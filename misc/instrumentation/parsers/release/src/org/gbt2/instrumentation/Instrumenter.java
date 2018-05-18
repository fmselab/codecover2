///////////////////////////////////////////////////////////////////////////////
//
// $Id: Instrumenter.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 24.02.2007 17:30:00
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.nio.charset.Charset;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;
import java.util.TreeSet;

import org.gbt2.instrumentation.cobol85.parser.CobolParser;
import org.gbt2.instrumentation.criteria.Criterion;
import org.gbt2.instrumentation.java15.parser.JavaParser;

/**
 * This is an abstract Instrumenter implementing all the needed basic method. A
 * child should implement the {@link #instrumentThis(Reader, Writer)} method.
 * 
 * @author Christoph Müller
 * @version 1.0 - 25.02.2007
 * 
 */
public abstract class Instrumenter {

    /**
     * The specific file separator of the operation system:<br>
     * <ul>
     * <li>"\" under Windows</li>
     * <li>"/" under Linux</li>
     * </ul>
     */
    public static final String FILE_SEPERATOR = System
            .getProperty("file.separator");

    private static final Set<String> IGNORED_FILE_NAMES = new HashSet<String>();

    static {
        IGNORED_FILE_NAMES.add(".svn");
    }

    private Charset charset;

    private Set<Criterion> criteria;

    /**
     * protectes constructor for initializing all member field.
     */
    protected Instrumenter() {
        this.charset = null;
        this.criteria = new TreeSet<Criterion>();
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
     */
    void setCharset(Charset charset) {
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
     * Instruments one single File.<br>
     * <br>
     * If there occur any exceptions during the instrumentation process, they
     * are logged to a list of {@link InstrumentationError}s.
     * 
     * @param file
     *            The single file that will be instrumented.
     * @param target
     *            The path where the instrumented file is written to.
     * @return A log with all {@link InstrumentationError}s that occur during
     *         the instrumentation process.
     * @throws IOException
     *             If there were IO Errors when instrumenting.
     * @see #instrumentFolder(File, boolean, Set, File) for instrumentation of a
     *      whole folder.
     */
    public Queue<InstrumentationError> instrument(File file, File target) {
        InstrumentationJob job = new InstrumentationJob(file, target);
        Queue<InstrumentationError> listErrors = new LinkedList<InstrumentationError>();

        instrumentJob(job, listErrors);

        return listErrors;
    }

    /**
     * Instruments a folder and possible subfolders.<br>
     * <br>
     * If there occur any exceptions during the instrumentation process, they
     * are logged to a list of {@link InstrumentationError}s. The
     * instrumentation of the remaining source files is not canceled.
     * 
     * @param sourceFolder
     *            The source folder.
     * @param recursive
     *            true &rarr; all subfolders are instrumented recursivly; false
     *            &rarr; just the files of this folder are instrumented.
     * @param extensions
     *            A set with possible extensions. Only files with one of these
     *            extensions are instrumented. If the set is null than all files
     *            are instrumented.
     * @param targetFolder
     *            The folder, all instrumented files are written to.
     * @return A log with all {@link InstrumentationError}s that occur during
     *         the instrumentation process.
     */
    public Queue<InstrumentationError> instrumentFolder(File sourceFolder,
            boolean recursive, Set<String> extensions, File targetFolder) {
        Queue<InstrumentationError> listErrors = new LinkedList<InstrumentationError>();

        if (sourceFolder == null) {
            throw new NullPointerException("sourceFolder == null");
        } else if (targetFolder == null) {
            throw new NullPointerException("targetFolder == null");
        } else if (!sourceFolder.isDirectory()) {
            Exception noDirExc = new IOException("The sourcefolder is no directory:\n"
                    + sourceFolder.getAbsolutePath());
            listErrors.add(new InstrumentationError(sourceFolder, targetFolder, noDirExc));
            return listErrors;
        }
        // sourceFolder != null && targetFolder != null &&
        // sourceFolder.isDirectory()

        // This filter contains a reference to all extensions and names, that
        // are accepted and rejected
        FileFilter fileFilter = new ExtensionFileFilter(extensions,
                recursive, IGNORED_FILE_NAMES);

        Queue<InstrumentationJob> jobs = new LinkedList<InstrumentationJob>();

        createJobs(sourceFolder, recursive, targetFolder, fileFilter, jobs);

        instrumentJobs(jobs, listErrors);

        return listErrors;
    }

    /**
     * This is an recursive method for filling a Queue of {@link InstrumentJob}.
     * 
     * @param sourceFolder
     *            The folder which files should be added.
     * @param recursive
     *            Call the method for subfolders too?
     * @param targetFolder
     *            The target folder for the instrumentation job.
     * @param fileFilter
     *            The {@link FileFilter} to use.
     * @param jobs
     *            This is an in/out variable used for storing all the created
     *            jobs.
     * 
     * @see ExtensionFileFilter
     */
    private void createJobs(File sourceFolder, boolean recursive,
            File targetFolder, FileFilter fileFilter,
            Queue<InstrumentationJob> jobs) {
        // assert sourceFolder != null && targetFolder != null

        // we start listing all the files and folders of the sourcefolder
        File[] filesOfFolder = sourceFolder.listFiles(fileFilter);

        for (File thisFile : filesOfFolder) {
            if (thisFile.isDirectory() && recursive) {
                // if it is a folder we call the recursion
                String targetFolderPath = targetFolder.getAbsolutePath()
                        + FILE_SEPERATOR + thisFile.getName();

                createJobs(thisFile, recursive, new File(targetFolderPath),
                        fileFilter, jobs);
            } else if (thisFile.isFile()) {
                // if it is a file, we create a new job
                String targetFilePath = targetFolder.getAbsolutePath()
                        + FILE_SEPERATOR + thisFile.getName();
                jobs.add(new InstrumentationJob(thisFile, new File(
                        targetFilePath)));
            }
        }
    }

    private void instrumentJobs(Queue<InstrumentationJob> jobs,
            Queue<InstrumentationError> listErrors) {
        for (InstrumentationJob job : jobs) {
            instrumentJob(job, listErrors);
        }
    }

    /**
     * Calls the child class to instrument the sourceFile of the given job.<br>
     * <br>
     * If there occur any exceptions during the instrumentation process, they
     * are logged to a list of {@link InstrumentationError}s.<br>
     * This method has the following flow:
     * <ol>
     * <li>open InputStreams and BufferedReader</li>
     * <li>open OutputStreams and BufferedWriter</li>
     * <li>if there occurs any exception till here, the instrumentation is not
     * started</li>
     * <li>otherwise the {@link #instrumentThis(Reader, Writer)} method is
     * called</li>
     * <li>if there occurs any {@link ParseException}, the target file will be
     * deleted</li>
     * <li>finally all streams are tried to close</li>
     * <li>...and the target file is possibly deleted</li>
     * </ol>
     * 
     * @param job
     *            The {@link InstrumentJob} describing source and target.
     * @param listErrors
     *            If there occur Exceptions during the instrumentation process
     *            they are not thrown but stored in this queue.
     * 
     * @see {@link #instrumentThis(Reader, Writer)}
     */
    private void instrumentJob(InstrumentationJob job,
            Queue<InstrumentationError> listErrors) {
        BufferedReader bufferedReader = null;
        BufferedWriter bufferedWriter = null;
        boolean deleteTargetFile = true;

        try {
            FileInputStream fileInputStream = new FileInputStream(job
                    .getSource());
            InputStreamReader inputStreamReader = new InputStreamReader(
                    fileInputStream, this.charset);
            bufferedReader = new BufferedReader(inputStreamReader);

            // wir müssen dafür sorgen, dass das Vaterverzeichnis der Zieldatei
            // auch wirklich existiert.
            job.getTarget().getParentFile().mkdirs();
            FileOutputStream fileOutputStream = new FileOutputStream(job
                    .getTarget());
            OutputStreamWriter outputStreamWriter = new OutputStreamWriter(
                    fileOutputStream, this.charset);
            bufferedWriter = new BufferedWriter(outputStreamWriter);

            try {
                instrumentThis(bufferedReader, bufferedWriter);
                deleteTargetFile = false;
            } catch (ParseException e) {
                listErrors.add(new InstrumentationError(job, e));
            } catch (IOException e) {
                listErrors.add(new InstrumentationError(job, e));
            }
        } catch (FileNotFoundException e) {
            listErrors.add(new InstrumentationError(job, e));
            return;
        }
        // The source and the target writers and readers are possibly created
        // without exceptions.
        // The instrumentation is not started or finished - with or without
        // exceptions.

        try {
            if (bufferedReader != null) {
                bufferedReader.close();
            }
            if (bufferedWriter != null) {
                bufferedWriter.flush();
                bufferedWriter.close();
            }
            if (deleteTargetFile) {
                job.getTarget().delete();
            }
        } catch (IOException e) {
            listErrors.add(new InstrumentationError(job, e));
        }
    }

    /**
     * The order to instrument the source and write the instrumented source code
     * in target.
     * 
     * @param source
     *            The source code to instrument.
     * @param target
     *            The instrumented source code.
     * @throws ParseException
     *             If an exceptions occurs during the parsing: e.g.
     *             {@link JavaParser#CompilationUnit()} or
     *             {@link CobolParser#CompilationUnit()}.
     * @throws IOException
     *             On the occurance of write exceptions.
     */
    protected abstract void instrumentThis(Reader source, Writer target)
            throws ParseException, IOException;
}