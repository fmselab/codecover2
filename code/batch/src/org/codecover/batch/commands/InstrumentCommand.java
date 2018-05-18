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

package org.codecover.batch.commands;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.Collection;
import java.util.List;

import org.codecover.utils.*;
import org.codecover.batch.BatchLogger;
import org.codecover.batch.BatchProgressHandler;
import org.codecover.batch.CommandLine;
import org.codecover.batch.Option;
import org.codecover.batch.OptionSet;
import org.codecover.batch.Options;
import org.codecover.batch.SimpleCommand;
import org.codecover.instrumentation.InstrumenterDescriptor;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileSaveException;
import org.codecover.model.extensions.*;
import org.codecover.model.utils.file.DefaultIgnores;
import org.codecover.model.utils.file.DirectoryScanner;
import org.codecover.model.utils.file.SourceTargetContainer;
import org.codecover.model.utils.file.listener.SourceTargetAllListener;
import org.codecover.model.utils.file.listener.SourceTargetIncludedListener;

/**
 * InstrumentCommand
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: InstrumentCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class InstrumentCommand extends SimpleCommand {

    private static final String DESCRIPTION = "instruments source files";

    private static final String DETAILED_DESCRIPTION = "instruments source files\n\n" +
        "This command requires a root directory with all source files, " +
        "a destination for the instrumented source files, " +
        "a container, where static information of the source files are placed - " +
        "the so called MAST (More Abstract Syntax Tree) and " +
        "the language of the source files - e.g. java or cobol.\n\n" +
        "To select the files to instrument more detailed, patters can be used. " +
        "There are include and exclude patterns. A file is instrumented, if it " +
        "fits one of the include patterns and none of the exclude patterns. " +
        "These patterns are relative paths under the root directory and can " +
        "include wildcards:\n" +
        " ? matches one character\n" +
        " * matches zero or more characters\n" +
        "** matches zero or more directories in a path\n" +
        "see http://ant.apache.org/manual/dirtasks.html#patterns for details";

    private static final InstrumentCommand instance = new InstrumentCommand();


    /**
     * Gets an instance of this class
     * 
     * @return the instance of the class
     */
    public static InstrumentCommand getInstance() {
        return instance;
    }

    private InstrumentCommand() {
        super("in",
              "instrument",
              DESCRIPTION,
              new OptionSet(
                // REQUIRED
                new Option[] {Options.container,
                              Options.destination,
                              Options.language,
                              Options.rootDirectory},

                // NOT REQUIRED
                new Option[] {Options.charset,
                              Options.copyUninstrumented,
                              Options.criterion,
                              Options.directive,
                              Options.exclude,
                              Options.excludesFile,
                              Options.include,
                              Options.includesFile,
                              Options.instrumenter}));
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.batch.Command#getDetailedDescription()
     */
    @Override
    public String getDetailedDescription() {
        return DETAILED_DESCRIPTION;
    }

    @Override
    protected int run(final CommandLine cl, final BatchLogger logger, final PluginManager pluginManager) {
        // instrument-command
        /*
         * you may want to use the attributes
         * - boolean isQuiet(cl)
         * - boolean isVerbose(cl)
         * (isQuiet(cl) AND isVerbose(cl) == false)
         * 
         * - boolean isPretend(cl)
         * - boolean isVersion(cl)
         */

        /*
         * required args:
         * - root-directory
         * - destination
         * - container
         * - language
         */

        // ////////////////////////////////////////////////////////////////////
        //
        // get values from options
        //
        // ////////////////////////////////////////////////////////////////////
        // required
        final String pRootDirectory = cl.getOptionValue(Options.rootDirectory);
        final String pDestination = cl.getOptionValue(Options.destination);
        final String pSessionContainer = cl.getOptionValue(Options.container);
        final String pLanguage = cl.getOptionValue(Options.language);

        // optional options
        final boolean pHasCriteria = cl.hasOption(Options.criterion);
        final List<String> pCriteria = cl.getOptionValues(Options.criterion);
        final boolean pHasCharset = cl.hasOption(Options.charset);
        final String pCharset = cl.getOptionValueOrNull(Options.charset);
        final boolean pCopyUninstrumented = cl.hasOption(Options.copyUninstrumented);
        final List<String> pDirectives = cl.getOptionValues(Options.directive);
        final boolean pHasInstrumenterKey = cl.hasOption(Options.instrumenter);
        final String pInstrumenterKey = cl.getOptionValueOrNull(Options.instrumenter);

        // includes
        final boolean pHasIncludes = cl.hasOption(Options.include);
        final List<String> pIncludePatterns = cl.getOptionValues(Options.include);
        final boolean pHasIncludesFile = cl.hasOption(Options.includesFile);
        final String pIncludesFile = cl.getOptionValueOrNull(Options.includesFile);

        // excludes
        final boolean pHasExcludes = cl.hasOption(Options.exclude);
        final List<String> pExcludePatterns = cl.getOptionValues(Options.exclude);
        final boolean pHasExcludesFile = cl.hasOption(Options.excludesFile);
        final String pExcludesFile = cl.getOptionValueOrNull(Options.excludesFile);

        final BatchProgressHandler progressHandler = BatchProgressHandler.
           createBatchProgressHandler(cl, logger);
        
        
        final InstrumentUtils.GetFilesCallback getFilesCallback = new InstrumentUtils.GetFilesCallback() {
                private Collection<SourceTargetContainer> filesToInstrument = null;
        
                private Collection<SourceTargetContainer> filesToCopy = null;

                public Collection<SourceTargetContainer> getFilesToInstrument(InstrumenterDescriptor descriptor, File rootFolderFile, File targetFolderFile) {
                    if (this.filesToInstrument == null) {
                        getValues(descriptor, rootFolderFile, targetFolderFile);
                    }
                    return this.filesToInstrument;
                }
                
                public Collection<SourceTargetContainer> getFilesToCopy(InstrumenterDescriptor descriptor, File rootFolderFile, File targetFolderFile) {
                    if (this.filesToCopy == null) {
                        getValues(descriptor, rootFolderFile, targetFolderFile);
                    }
                    return this.filesToCopy;
                }
                
                private void getValues(InstrumenterDescriptor descriptor, File rootFolderFile, File targetFolderFile) {
        // ////////////////////////////////////////////////////////////////////
        //
        // handling of includes / excludes / ignore
        //
        // ////////////////////////////////////////////////////////////////////
        DirectoryScanner directoryScanner = new DirectoryScanner();
        
        if (pHasIncludesFile) {
            logger.info("includes-file: " + new File(pIncludesFile).getAbsolutePath());

            // try to read the includes-file
            try {
                LineNumberReader reader = new LineNumberReader(new FileReader(pIncludesFile));
                String pattern;

                do {
                    pattern = reader.readLine();
                    logger.info("include pattern: " + pattern);
                    directoryScanner.addIncludePattern(pattern);
                } while (pattern != null);
                reader.close();
            } catch (FileNotFoundException e) {
                String message = e.getMessage();
                if (message != null) {
                    logger.fatal("The includes-file could not be found:\n"
                            + message);
                } else {
                    logger.fatal("The includes-file could not be found.");
                }
            } catch (IOException e) {
                String message = e.getMessage();
                if (message != null) {
                    logger.fatal("The includes-file could not be read:\n"
                            + message);
                } else {
                    logger.fatal("The includes-file could not be read.");
                }
            }
        }

        if (pHasIncludes) {
            // the option include has been used at least once

            for (String thisPattern : pIncludePatterns) {
                logger.info("include pattern: " + thisPattern);
                directoryScanner.addIncludePattern(thisPattern);
            }  
        }

        // when no include is set, we include everything
        if (directoryScanner.getIncludePatternCount() == 0) {
            directoryScanner.addIncludePattern("**");
        }

        if (pHasExcludesFile) {
            logger.info("excludes-file: " + new File(pExcludesFile).getAbsolutePath());

            // try to read the excludes-file
            try {
                LineNumberReader reader = new LineNumberReader(new FileReader(pExcludesFile));
                String pattern;
                
                pattern = reader.readLine();
                while (pattern != null) {
                    logger.info("exclude pattern: " + pattern);
                    directoryScanner.addExcludePattern(pattern);
                    pattern = reader.readLine();
                }
                reader.close();
            } catch (FileNotFoundException e) {
                String message = e.getMessage();
                if (message != null) {
                    logger.fatal("The excludes-file could not be found:\n"
                            + message);
                } else {
                    logger.fatal("The excludes-file could not be found.");
                }
            } catch (IOException e) {
                String message = e.getMessage();
                if (message != null) {
                    logger.fatal("The excludes-file could not be read:\n"
                            + message);
                } else {
                    logger.fatal("The excludes-file could not be read.");
                }
            }
        }

        if (pHasExcludes) {
            // the option exclude has been used at least once

            for (String thisPattern : pExcludePatterns) {
                logger.info("exclude pattern: " + thisPattern);
                directoryScanner.addExcludePattern(thisPattern);
            }  
        }

        // set default ignores
        directoryScanner.addIgnorePatterns(DefaultIgnores.getIgnorePatterns());
        // use the descriptor as a FileFilter
        directoryScanner.setFileFilter(descriptor);

        // ////////////////////////////////////////////////////////////////////
        //
        // scan
        //
        // ////////////////////////////////////////////////////////////////////

        // create the FileFoundListener informed by scan()
        SourceTargetIncludedListener fileFoundListener;
        if (pCopyUninstrumented) {
            // this listener collects included and not included files
            fileFoundListener = new SourceTargetAllListener(rootFolderFile,
                    targetFolderFile);
        } else {
            // this listener collects only included files
            fileFoundListener = new SourceTargetIncludedListener(rootFolderFile,
                    targetFolderFile);
        }
        // scan the root folder -> all files included (and not included) will
        // be collected by the listener
        try {
            directoryScanner.scan(rootFolderFile, fileFoundListener);
        } catch (IOException e) {
            logger.fatal("An error occured during listing the files:\n" + 
                    e.getMessage());
        }

        this.filesToInstrument = fileFoundListener.
            getIncludedSourceTargetContainers();
        this.filesToCopy = fileFoundListener.
            getNotIncludedSourceTargetContainers();
                }
            };
        
        final InstrumentUtils.SaveSessionContainerCallback saveSessionContainerCallback = new InstrumentUtils.SaveSessionContainerCallback() {
                public void saveSessionContainer(TestSessionContainer testSessionContainer) {
                    // save the testsessioncontainer only, if pretend mode is off
                    if (!isPretend(cl)) {
                        try {
                            logger.info("Storing test session container...");
        
                            testSessionContainer.save(pSessionContainer);
        
                            logger.info("Test session container stored.");
                        } catch (FileSaveException e) {
                            String message = e.getMessage();
                            if (message != null) {
                                logger.fatal("The session container file could not be found or "
                                             + "could not be used for reading/writing:\n" + message);
                            } else {
                                logger.fatal("The session container file could not be found or "
                                             + "could not be used for reading/writing.");
                            }
                        }
                    }
                }
            };
        
        InstrumentUtils.instrument(logger, pluginManager, progressHandler, isPretend(cl), pRootDirectory, pDestination, saveSessionContainerCallback, pLanguage, pHasCriteria, pCriteria, pHasCharset, pCharset, pCopyUninstrumented, pDirectives, pHasInstrumenterKey, pInstrumenterKey, getFilesCallback);
        
        progressHandler.complete();
        
        return 0;
    }
}
