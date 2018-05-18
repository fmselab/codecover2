/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.instrumentation;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.charset.IllegalCharsetNameException;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;

import org.codecover.instrumentation.exceptions.InstrumentationException;
import org.codecover.instrumentation.exceptions.InstrumentationFileNotFoundException;
import org.codecover.instrumentation.exceptions.InstrumentationIOException;
import org.codecover.instrumentation.exceptions.InstrumentationRuntimeException;
import org.codecover.instrumentation.exceptions.ParseException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.HierarchyLevelType;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.ProgressHandler;
import org.codecover.model.utils.criteria.Criterion;
import org.codecover.model.utils.file.SourceTargetContainer;

/**
 * This is an abstract Instrumenter implementing all the needed basic method. A
 * child should implement the {@link #instrumentThis(Reader, Writer, MASTBuilder, SourceFile, HierarchyLevelContainer, String, Map)} method.<br>
 * <br>
 * These methods can be overwritten by child classes to get informed of the steps of an instrumentation process:
 * <ul>
 * <li>{@link #notifyStart(File, File, Collection, HierarchyLevelContainer, MASTBuilder, String, Map)}</li>
 * <li>{@link #notifyEnd(File, File, Collection, HierarchyLevelContainer, MASTBuilder, String, Map)}</li>
 * <li>{@link #notifyBefore(SourceTargetContainer, HierarchyLevelContainer, MASTBuilder, String, Map)}</li>
 * <li>{@link #notifyAfter(SourceTargetContainer, HierarchyLevelContainer, MASTBuilder, String, Map)}</li> 
 * </ul>
 * <br>
 * <b>Attention</b>: before using {@link #instrument(File, File, Collection, MASTBuilder, Map)},
 * you have to check whether the instrumentation of more than one code file for
 * an instrumentation run is supported by this instrumenter:
 * {@link #allowsFileListInstrumentation()}. If <code>false</code>, this
 * method will throw an {@link InstrumentationRuntimeException}.<br>
 * The instrumenter has a <i>pretend mode</i> ({@link #pretend()}). If this mode 
 * is activated, the instrumenter will not create the target files, but start the
 * instrumentation process.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: Instrumenter.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public abstract class Instrumenter {

    private Charset charset;

    private Set<Criterion> criteria;

    private char[] sourceFileArray;

    private boolean pretend;

    private boolean verbose;

    private ProgressHandler progressHandler;

    /**
     * Protected constructor for initializing all member field.
     */
    protected Instrumenter() {
        this.charset = null;
        this.criteria = new TreeSet<Criterion>();
        this.sourceFileArray = null;
        this.pretend = false;
        this.verbose = false;
        this.progressHandler = null;
    }

    /**
     * @return the charset, which is now set; might be null
     */
    public Charset getCharset() {
        return this.charset;
    }

    /**
     * @param charset
     *            the new charset for the source and target files
     * 
     * @throws IllegalCharsetNameException
     *             If no support for the named charset is available in this
     *             instance of the Java virtual machine.
     * @see Charset#forName(String)
     */
    void setCharset(Charset charset) throws IllegalCharsetNameException {
        this.charset = charset;
    }

    /**
     * @param charsetName
     *            the name of the new charset for the source and target files
     */
    void setCharset(String charsetName) {
        this.charset = Charset.forName(charsetName);
    }

    /**
     * Is a given {@link Criterion} set?
     * 
     * @param criterion
     *            The specific {@link Criterion}.
     * 
     * @return true &rarr; this {@link Criterion} is set for instrumentation;
     *         false &rarr; this {@link Criterion} is not set for
     *         instrumentation.
     */
    public boolean isCriterionSet(Criterion criterion) {
        return this.criteria.contains(criterion);
    }

    /**
     * @return An <b>unmodifiable</b> set with all criteria set for
     *         instrumentation.
     */
    public Set<Criterion> getCriteria() {
        return Collections.unmodifiableSet(this.criteria);
    }

    /**
     * @param criterion
     *            A criterion the instrumenter should use.
     */
    void addCriterion(Criterion criterion) {
        this.criteria.add(criterion);
    }

    /**
     * Returns the current mode of the instrumenter: <i>pretend</i> or
     * <i>no pretend</i>.
     * 
     * @return true &rarr; <i>pretend mode</i> activated; false &rarr;
     *         <i>pretend mode</i> deactivated.
     */
    public boolean pretend() {
        return this.pretend;
    }

    /**
     * Set the pretend mode of the instrumenter.
     * 
     * @param pretend
     *            true &rarr; <i>pretend mode</i> activated; false &rarr;
     *            <i>pretend mode</i> deactivated.
     */
    void setPretendMode(boolean pretend) {
        this.pretend = pretend;
    }

    /**
     * Returns the current mode of the instrumenter: <i>verbose</i> or <i>no
     * verbose</i>.
     * 
     * @return true &rarr; <i>verbose mode</i> activated; false &rarr;
     *         <i>verbose mode</i> deactivated.
     */
    public boolean verbose() {
        return this.verbose;
    }

    /**
     * Set the verbose mode of the instrumenter.
     * 
     * @param verbose
     *            true &rarr; <i>verbose mode</i> activated; false &rarr;
     *            <i>verbose mode</i> deactivated.
     */
    void setVerboseMode(boolean verbose) {
        this.verbose  = verbose;
    }

    /**
     * Returns the progress handler of this instrumenter.
     * 
     * @return The {@link ProgressHandler} used to inform the caller of
     *            this instrumenter of the progress of the instrumentation.
     */
    public ProgressHandler getProgressHandler() {
        return this.progressHandler;
    }

    /**
     * Set the progress handler.
     * 
     * @param progressHandler
     *            A {@link ProgressHandler} used to inform the caller of
     *            this instrumenter of the progress of the instrumentation.
     */
    void setProgressHandler (ProgressHandler progressHandler) {
        this.progressHandler = progressHandler;
    }

    /**
     * Creates a new char array for {@link #sourceFileArray} or extends its
     * length to the given.
     */
    private void prepareSourceFileArray(int minLength) {
        if (this.sourceFileArray == null ||
                this.sourceFileArray.length < minLength) {
            this.sourceFileArray = new char[minLength];
        }
    }

    /**
     * <ol>
     * <li>This method creates the rootHierarchyLevelContainer, which is needed
     * for the
     * {@link #instrumentThis(Reader, Writer, MASTBuilder, SourceFile, HierarchyLevelContainer, String, Map)}.</li>
     * <li>Before the start of the instrumentation sequence,
     * {@link #notifyStart(File, File, Collection, HierarchyLevelContainer, MASTBuilder, String, Map)}
     * is called.</li>
     * <li>Before the start of a job,
     * {@link #notifyBefore(SourceTargetContainer, HierarchyLevelContainer, MASTBuilder, String, Map)}
     * is called.</li>
     * <li>Every job is handed over to
     * {@link #instrumentJob(SourceTargetContainer, MASTBuilder, HierarchyLevelContainer, String, Map)}.</li>
     * <li>If the instrumentation was successful, the {@link SourceFile} is
     * saved in a List.</li>
     * <li>If the instrumentation was not successful, all instrumented files
     * are deleted using
     * {@link #deleteFilesUntilJob(Collection, SourceTargetContainer)}.</li>
     * <li>After the end of a job,
     * {@link #notifyEnd(File, File, Collection, HierarchyLevelContainer, MASTBuilder, String, Map)}
     * is called.</li>
     * <li>After all job have been instrumented, a new
     * {@link TestSessionContainer} is created, using
     * {@link TestSessionContainer#TestSessionContainer(HierarchyLevel, Logger, List, Set, String, Date)}.</li>
     * <li>Finally,
     * {@link #notifyEnd(File, File, Collection, HierarchyLevelContainer, MASTBuilder, String, Map)}
     * is called.</li>
     * </ol>
     * 
     * @param rootFolder
     *            The source folder.
     * @param targetFolder
     *            The folder, all instrumented files are written to.
     * @param jobs
     *            The {@link Collection} of {@link SourceTargetContainer}s,
     *            that should be instrumented.
     * @param builder
     *            The {@link MASTBuilder} needed for logging and for creating
     *            MAST objects.
     * @param instrumenterDirectives
     *            Directives for this instrumenter to allow special features.
     *            Use <b>{@link InstrumenterDescriptor#getDefaultDirectiveValues()}
     *            for default</b>!
     * @return The created {@link TestSessionContainer} with all MAST elements.
     * @throws InstrumentationException
     *             If an error occurs during the instrumentation process.
     */
    public TestSessionContainer instrument(File rootFolder,
            File targetFolder,
            Collection<SourceTargetContainer> jobs,
            MASTBuilder builder,
            Map<String, Object> instrumenterDirectives) throws InstrumentationException {

        HierarchyLevelType rootType = getRootHierarchyLevelType(builder);
        HierarchyLevelType type = getPackageHierarchyLevelType(builder);
        
        //XXX: to add instrumentation root path (from rootFolder) , change this
        HierarchyLevelContainer rootHierarchyLevelContainer = new HierarchyLevelContainer(
                rootType.getInternalName(), rootType, type);
        List<SourceFile> sourceFileList = new Vector<SourceFile>(jobs.size());
        String testSessionContainerUID = (String) instrumenterDirectives.get(UUIDDirective.KEY);

        int fileCount = 0;

        // notify that the instrumentation is started
        notifyStart(rootFolder, targetFolder, jobs,
                rootHierarchyLevelContainer, builder, testSessionContainerUID, instrumenterDirectives);

        for (SourceTargetContainer job : jobs) {
            try {
                // notify that the job will be instrumented
                notifyBefore(job, rootHierarchyLevelContainer, builder, testSessionContainerUID, instrumenterDirectives);
                SourceFile thisSourceFile = instrumentJob(job,
                        builder,
                        rootHierarchyLevelContainer,
                        testSessionContainerUID,
                        instrumenterDirectives);
                notifyAfter(job, rootHierarchyLevelContainer, builder, testSessionContainerUID, instrumenterDirectives);
                // notify that the job has been instrumented

                sourceFileList.add(thisSourceFile);

                fileCount++;
                this.progressHandler.setProgress(((float)fileCount) / ((float)jobs.size()));
            } catch (InstrumentationException e) {
                // try to delete created files
                deleteFilesUntilJob(jobs, job);
                e.setFileOfException(job.getSource());
                throw e;
            }
        }

        try {
            // notify that the instrumentation is finished
            notifyEnd(rootFolder, targetFolder, jobs,
                    rootHierarchyLevelContainer, builder,
                    testSessionContainerUID, instrumenterDirectives);
        } catch (InstrumentationException e) {
            // try to delete created files
            deleteFilesUntilJob(jobs, null);
            throw e;
        }

        return new TestSessionContainer(
                rootHierarchyLevelContainer.transformToHierarchyLevel(builder),
                builder.getLogger(),
                sourceFileList,
                Collections.unmodifiableSet(this.criteria),
                testSessionContainerUID,
                new Date());
    }

    /**
     * Calls the child class to instrument the sourceFile of the given job.<br>
     * <br>
     * This method has the following flow:
     * <ol>
     * <li>open {@link InputStream} and {@link BufferedReader}</li>
     * <li>write it into a String and create a {@link SourceFile}</li>
     * <li>open {@link OutputStream} and {@link BufferedWriter}</li>
     * <li>if there occurs any exception till here, the instrumentation is not
     * started</li>
     * <li>otherwise the
     * {@link #instrumentThis(Reader, Writer, MASTBuilder, SourceFile, HierarchyLevelContainer, String, Map)}
     * method is called</li>
     * <li>finally all streams are tried to close</li>
     * </ol>
     * <br>
     * This method is synchronized to reuse {@link #sourceFileArray}.
     * 
     * @param job
     *            The {@link SourceTargetContainer} describing source and target.
     * @param builder
     *            The {@link MASTBuilder} needed for logging and for creating
     *            MAST objects.
     * @param rootHierarchyLevelContainer
     *            The {@link HierarchyLevelContainer} handed over to
     *            {@link #instrumentThis(Reader, Writer, MASTBuilder, SourceFile, HierarchyLevelContainer, String, Map)}.
     * @param testSessionContainerUID
     *            The UID of the {@link TestSessionContainer}.
     * @param instrumenterDirectives
     *            Directives for this instrumenter to allow special features.
     * 
     * @return The {@link SourceFile} that is instrumented. If there occurred
     *         any read error, the {@link SourceFile} is <code>null</code>.
     * 
     * @see #instrumentThis(Reader, Writer, MASTBuilder, SourceFile, HierarchyLevelContainer, String, Map)
     */
    private synchronized SourceFile instrumentJob(SourceTargetContainer job,
            MASTBuilder builder,
            HierarchyLevelContainer rootHierarchyLevelContainer,
            String testSessionContainerUID,
            Map<String, Object> instrumenterDirectives) throws InstrumentationException {
        SourceFile sourceFile;
        Writer targetWriter = null;

        try {
            // use the fileInputStream to get the content of the source file
            FileInputStream fileInputStream = new FileInputStream(job
                    .getSource());
            // if the source has a special charset, use this instead of the global
            Charset charsetForInput = job.getCharset();
            if (charsetForInput == null) {
                charsetForInput = this.charset;
            }
            InputStreamReader inputStreamReader = new InputStreamReader(
                    fileInputStream, charsetForInput);

            // the number of characters is appropriated with the length of the
            // source file -> the number of characters might be smaller if
            // UTF-8 is used
            int sourceFileLength = (int)job.getSource().length();
            // prepare an array, that is long enough
            prepareSourceFileArray(sourceFileLength);
            // read from the inputStreamReader and get the exact character count
            sourceFileLength = Math.max(inputStreamReader.read(
                    this.sourceFileArray, 0, sourceFileLength), 0);

            String sourceFileContent = new String(this.sourceFileArray, 0, sourceFileLength);
            String sourceFileName = job.getSource().getName();
            
            //XXX: to add Path to a SourceFile take it from job.getSource()
            sourceFile = builder.createSourceFile(sourceFileName, sourceFileContent);

            inputStreamReader.close();
        } catch (FileNotFoundException e) {
            throw new InstrumentationFileNotFoundException(e);
        } catch (IOException e) {
            throw new InstrumentationIOException(e);
        }

        // assert: the reading is successfully finished
        // sourceFile != null
        try {
            // use the String reader as input for instrumentation
            StringReader reader = new StringReader(sourceFile.getContent());

            if (pretend()) {
                targetWriter = NullWriter.INSTANCE;
            } else {
                // we have to ensure, that the parent directory exists
                job.getTarget().getParentFile().mkdirs();
                FileOutputStream fileOutputStream = new FileOutputStream(job
                        .getTarget());
                OutputStreamWriter outputStreamWriter = new OutputStreamWriter(
                        fileOutputStream, this.charset);
                targetWriter = new BufferedWriter(outputStreamWriter);
            }

            try {
                instrumentThis(reader,
                        targetWriter,
                        builder,
                        sourceFile,
                        rootHierarchyLevelContainer,
                        testSessionContainerUID,
                        instrumenterDirectives);
            } finally {
                targetWriter.flush();
                targetWriter.close();
            }

            // instrumentation successful and file is saved
        } catch (FileNotFoundException e) {
            throw new InstrumentationFileNotFoundException(e);
        } catch (IOException e) {
            throw new InstrumentationIOException(e);
        } catch (RuntimeException e) {
            throw new InstrumentationRuntimeException(e);
        }

        return sourceFile;
    }
    
    /**
     * Deletes all target files, of <code>jobs</code> until the target file of
     * the last job is deleted.
     * 
     * @param lastJob The job to be deleted at last. Can be <code>null</code>,
     * than all target files will be deleted.
     * 
     * @see #instrument(File, File, Collection, MASTBuilder, Map) where this method
     * is called
     */
    private void deleteFilesUntilJob(Collection<SourceTargetContainer> jobs,
            SourceTargetContainer lastJob) {
        for (SourceTargetContainer thisJob : jobs) {
            thisJob.getTarget().delete();
            if (thisJob == lastJob) {
                return;
            }
        }
    }

    /**
     * This method is called, after all {@link SourceTargetContainer}s have been
     * created but before the whole instrumentation process starts.
     * 
     * @param rootFolder
     *            The source folder.
     * @param targetFolder
     *            The folder, all instrumented files are written to.
     * @param jobs
     *          All jobs to be instrumented.
     * @param rootHierarchyLevelContainer
     *          The {@link HierarchyLevelContainer} used for the instrumentation.
     * @param builder
     *          The {@link MASTBuilder} use for creating the MAST.
     * @param testSessionContainerUID
     *          The generated UID of the {@link TestSessionContainer}.
     * @param instrumenterDirectives
     *          Directives for this instrumenter to allow special features.
     * @throws InstrumentationException 
     *          Can be thrown.
     */
    @SuppressWarnings("unused")
    protected void notifyStart(File rootFolder,
            File targetFolder,
            Collection<SourceTargetContainer> jobs,
            HierarchyLevelContainer rootHierarchyLevelContainer,
            MASTBuilder builder, String testSessionContainerUID,
            Map<String, Object> instrumenterDirectives)
    throws InstrumentationException {
        // can be overridden
    }

    /**
     * This method is called, after all {@link SourceTargetContainer}s have been
     * processed.
     * 
     * @param rootFolder
     *            The source folder.
     * @param targetFolder
     *            The folder, all instrumented files are written to.
     * @param jobs
     *          All jobs to be instrumented.
     * @param rootHierarchyLevelContainer
     *          The {@link HierarchyLevelContainer} used for the instrumentation.
     * @param builder
     *          The {@link MASTBuilder} use for creating the MAST.
     * @param testSessionContainerUID
     *          The generated UID of the {@link TestSessionContainer}.
     * @param instrumenterDirectives
     *          Directives for this instrumenter to allow special features.
     * @throws InstrumentationException 
     *          Can be thrown.
     */
    @SuppressWarnings("unused")
    protected void notifyEnd(File rootFolder,
            File targetFolder,
            Collection<SourceTargetContainer> jobs,
            HierarchyLevelContainer rootHierarchyLevelContainer,
            MASTBuilder builder, String testSessionContainerUID,
            Map<String, Object> instrumenterDirectives)
    throws InstrumentationException {
        // can be overridden
    }

    /**
     * This method is called, before a {@link SourceTargetContainer} is
     * instrumented.
     * 
     * @param job
     *            The job that will be instrumented.
     * @param rootHierarchyLevelContainer
     *            The {@link HierarchyLevelContainer} used for the
     *            instrumentation.
     * @param builder
     *            The {@link MASTBuilder} use for creating the MAST.
     * @param testSessionContainerUID
     *            The generated UID of the {@link TestSessionContainer}.
     * @param instrumenterDirectives
     *            Directives for this instrumenter to allow special features.
     * @throws InstrumentationException
     *             Can be thrown.
     */
    @SuppressWarnings("unused")
    protected void notifyBefore(SourceTargetContainer job,
            HierarchyLevelContainer rootHierarchyLevelContainer,
            MASTBuilder builder,
            String testSessionContainerUID,
            Map<String, Object> instrumenterDirectives)
    throws InstrumentationException {
        // can be overridden
    }

    /**
     * This method is called, after a {@link SourceTargetContainer} is
     * instrumented and no exception is thrown.
     * 
     * @param job
     *            The job that will be instrumented.
     * @param rootHierarchyLevelContainer
     *            The {@link HierarchyLevelContainer} used for the
     *            instrumentation.
     * @param builder
     *            The {@link MASTBuilder} use for creating the MAST.
     * @param testSessionContainerUID
     *            The generated UID of the {@link TestSessionContainer}.
     * @param instrumenterDirectives
     *            Directives for this instrumenter to allow special features.
     * @throws InstrumentationException
     *             Can be thrown.
     */
    @SuppressWarnings("unused")
    protected void notifyAfter(SourceTargetContainer job,
            HierarchyLevelContainer rootHierarchyLevelContainer,
            MASTBuilder builder,
            String testSessionContainerUID,
            Map<String, Object> instrumenterDirectives)
    throws InstrumentationException {
        // can be overridden
    }

    /**
     * The order to instrument the source and write the instrumented source code
     * in target.
     * 
     * @param source
     *            The source code to instrument.
     * @param target
     *            The instrumented source code.
     * @param builder
     *            The {@link MASTBuilder}
     * @param sourceFile
     *            The source file
     * @param rootContainer
     *            This is the {@link HierarchyLevelContainer}, where all top
     *            level {@link HierarchyLevel}s of the source file can be added
     *            using
     *            {@link HierarchyLevelContainer#addHierarchyLevels(Collection, LinkedList)}.
     * @param testSessionContainerUID
     *            The UID of the {@link TestSessionContainer}.
     * @param instrumenterDirectives
     *            Directives for this instrumenter to allow special features.
     * @throws ParseException
     *             If an exceptions occurs during the parsing.
     * @throws IOException
     *             On the occurrence of write exceptions.
     */
    protected abstract void instrumentThis(Reader source,
            Writer target,
            MASTBuilder builder,
            SourceFile sourceFile,
            HierarchyLevelContainer rootContainer,
            String testSessionContainerUID,
            Map<String, Object> instrumenterDirectives) throws ParseException,
            IOException;

    /**
     * Returns {@link HierarchyLevelType} which should be used to build the
     * {@link HierarchyLevel} tree out of the generated {@link HierarchyLevel}s
     * that are found during the
     * {@link #instrumentThis(Reader, Writer, MASTBuilder, SourceFile, HierarchyLevelContainer, String, Map)}.
     * 
     * @param builder
     *            The {@link MASTBuilder} for
     *            {@link MASTBuilder#createHierarchyLevelType(String, String)}.
     * 
     * @return The {@link HierarchyLevelType} used for packages in the given
     *         programming language.
     */
    protected abstract HierarchyLevelType getPackageHierarchyLevelType(MASTBuilder builder);

    /**
     * Returns {@link HierarchyLevelType} which should be used to build the root
     * of the {@link HierarchyLevel} tree out of the generated
     * {@link HierarchyLevel}s that are found during the
     * {@link #instrumentThis(Reader, Writer, MASTBuilder, SourceFile, HierarchyLevelContainer, String, Map)}.<br>
     * <br>
     * This method can be overwritten and will return
     * {@link #getPackageHierarchyLevelType(MASTBuilder)} per default.
     * 
     * @param builder
     *            The {@link MASTBuilder} for
     *            {@link MASTBuilder#createHierarchyLevelType(String, String)}.
     * 
     * @return The {@link HierarchyLevelType} used for the root package in the
     *         given programming language.
     */
    protected HierarchyLevelType getRootHierarchyLevelType(MASTBuilder builder) {
        return getPackageHierarchyLevelType(builder);
    }

    /**
     * States whether or whether not this {@link Instrumenter} allows the
     * instrumentation of more than one source files at a run.<br>
     * <br>
     * This information is needed, to know how the {@link HierarchyLevel} tree
     * is build and if {@link #instrument(File, File, Collection, MASTBuilder, Map)}
     * is allowed to be called with a Collection having more than one file.
     * 
     * @return true &rarr; this {@link Instrumenter} allows the instrumentation
     *         of more than one source file at a run; false &rarr; the
     *         {@link Instrumenter} does not allow.
     */
    public abstract boolean allowsFileListInstrumentation();
}
