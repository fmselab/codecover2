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

package org.codecover.utils;

import java.io.File;
import java.io.IOException;
import java.nio.charset.UnsupportedCharsetException;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.codecover.instrumentation.DefaultInstrumenterFactory;
import org.codecover.instrumentation.Instrumenter;
import org.codecover.instrumentation.InstrumenterDescriptor;
import org.codecover.instrumentation.InstrumenterDirective;
import org.codecover.instrumentation.InstrumenterFactory;
import org.codecover.instrumentation.exceptions.FactoryMisconfigurationException;
import org.codecover.instrumentation.exceptions.InstrumentationException;
import org.codecover.instrumentation.exceptions.InstrumentationFileNotFoundException;
import org.codecover.instrumentation.exceptions.InstrumentationIOException;
import org.codecover.instrumentation.exceptions.ParseException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.extensions.PluginManager;
import org.codecover.model.extensions.PluginUtils;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.ProgressHandler;
import org.codecover.model.utils.StringUtil;
import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.ConditionCoverage;
import org.codecover.model.utils.criteria.Criterion;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;
import org.codecover.model.utils.criteria.SynchronizedStatementCoverage;
import org.codecover.model.utils.criteria.QMOCoverage;
import org.codecover.model.utils.file.FileTool;
import org.codecover.model.utils.file.SourceTargetContainer;

/**
 * Instrumentation utility methods.
 *
 * @author Steffen Kieß
 * @version 1.0 ($Id: InstrumentUtils.java 158 2013-02-22 13:45:59Z hanikesn $)
 *
 */
public final class InstrumentUtils {

    private InstrumentUtils() {
        // private to prohibit external usage
    }

    // Here we have a map which contains shortcuts for instrumenter criteria
    // which are included in codecover
    public static final Map<String, Criterion> instrumenterCriteriaShortcuts;

    static {
        Map<String, Criterion> map = new TreeMap<String, Criterion>();

        // TODO add a field abbreviation for criteria and use this for this
        // shortcuts --> you have to change it in ant and batch too

        // ant and batch use this stuff, no need for a change there.
        // An abbreviation field might cause some trouble (like that it isn't
        // necessaryly unique).
        map.put("st", StatementCoverage.getInstance());
        map.put("br", BranchCoverage.getInstance());
        map.put("co", ConditionCoverage.getInstance());
        map.put("lo", LoopCoverage.getInstance());
        map.put("sy", SynchronizedStatementCoverage.getInstance());
        map.put("qm", QMOCoverage.getInstance());
        
        instrumenterCriteriaShortcuts = Collections.unmodifiableMap(map);
    }

    public static interface GetFilesCallback {
        public Collection<SourceTargetContainer> getFilesToInstrument(InstrumenterDescriptor descriptor, File rootFolderFile, File targetFolderFile);

        public Collection<SourceTargetContainer> getFilesToCopy(InstrumenterDescriptor descriptor, File rootFolderFile, File targetFolderFile);
    }

    public static interface SaveSessionContainerCallback {
        public void saveSessionContainer(TestSessionContainer testSessionContainer);
    }

    public static void instrument(Logger logger, PluginManager pluginManager,
            ProgressHandler progressHandler, boolean pretend,
            String pRootDirectory, String pDestination,
            SaveSessionContainerCallback saveSessionContainerCallback,
            String pLanguage, boolean pHasCriteria, List<String> pCriteria,
            boolean pHasCharset, String pCharset, boolean pCopyUninstrumented,
            List<String> pDirectives, boolean pHasInstrumenterKey,
            String pInstrumenterKey, GetFilesCallback getFilesCallback) {
        // ////////////////////////////////////////////////////////////////////
        //
        // logger, and InstrumenterDescriptor
        //
        // ////////////////////////////////////////////////////////////////////
        logger.info("Instrumentation:");
        logger.info("root-directory: " + new File(pRootDirectory).getAbsolutePath());
        logger.info("destination: " + new File(pDestination).getAbsolutePath());
        //logger.info("container: " + new File(pSessionContainer).getAbsolutePath());
        logger.info("language: " + pLanguage);

        InstrumenterDescriptor descriptor = null;

        if (pHasInstrumenterKey) {
            for (final InstrumenterDescriptor descr : PluginUtils.getExtensionObjects(pluginManager, logger, InstrumenterDescriptor.class)) {
                if (descr.getUniqueKey().equals(pInstrumenterKey)) {
                    if (descriptor == null) {
                        descriptor = descr;
                    } else {
                        logger.fatal("There are multiple instrumenters with the ID '" + pInstrumenterKey + "'");
                    }
                }
            }
            if (descriptor == null) {
                logger.fatal("No suitable instrumenter found for the key " +
                             pInstrumenterKey);
            }
        } else {
            Set<InstrumenterDescriptor> descriptors = new HashSet<InstrumenterDescriptor>();
            for (final InstrumenterDescriptor descr : PluginUtils.getExtensionObjects(pluginManager, logger, InstrumenterDescriptor.class)) {
                if (descr.isLanguageSupported(pLanguage)) {
                    descriptors.add(descr);
                }
            }
            if (descriptors.isEmpty()) {
                logger.fatal("No suitable instrumenter found for the " +
                             "programming language: " + pLanguage);
            } else if (descriptors.size() > 1) {
                final StringBuilder sb = new StringBuilder();
                sb.append("More than one suitable instrumenter found for the " +
                             "programming language: " + pLanguage + "\n" +
                             "Please use the command instrumenter-info to get " +
                             "to know the unique key of the instrumenter you " +
                             "prefer. Than use the option instrumenter for " +
                             "this command to exactly specify the instrumenter " +
                             "by its unique key.");
                for (InstrumenterDescriptor thisDescriptor : descriptors) {
                    String descriptorInfo = String.format("%s by %s (%s)",
                            thisDescriptor.getLanguageName(),
                            thisDescriptor.getAuthor(),
                            thisDescriptor.getClass().getName());
                    sb.append(descriptorInfo);
                    sb.append(thisDescriptor.getDescription());
                }
                logger.fatal(sb.toString());
                throw new RuntimeException();
            } else {
                // assert: only one InstrumenterDescriptor found
                descriptor = descriptors.iterator().next();
            }
        }

        logger.info("instrumenter key: " + descriptor.getUniqueKey());
        logger.info("instrumenter for: " + descriptor.getLanguageName());
        logger.info(descriptor.getDescription());

        // ////////////////////////////////////////////////////////////////////
        //
        // create the instrumenter
        //
        // ////////////////////////////////////////////////////////////////////
        InstrumenterFactory factory = new DefaultInstrumenterFactory();
        factory.setDescriptor(descriptor);

        // set criteria
        if (pHasCriteria) {
            for (final String criterionName : pCriteria) {
                if (criterionName.equals("all")) {
                    if (pCriteria.size() != 1) {
                        // "all" must be the only one criteria
                        logger.fatal("The criterion 'all' is given but it is not the only one");
                    }

                    // use all supported criteria
                    factory.addSupportedCriteria(descriptor);
                    logger.info("criteria: all");
                } else {
                    Criterion criterion = instrumenterCriteriaShortcuts.get(criterionName);
                    if (criterion == null) {
                        final int slashPos = criterionName.lastIndexOf("/");
                        if (slashPos == -1) {
                            logger.fatal("Unknown criterion " + criterionName);
                        }

                        final String pluginName = criterionName.substring(0, slashPos);
                        final String extensionName = criterionName.substring(slashPos + 1);

                        criterion = PluginUtils.getExtensionObjectByName(pluginManager, logger, Criterion.class, pluginName, extensionName);
                    }
                    if (!descriptor.isCriterionSupported(criterion)) {
                        logger.fatal("Criterion " + criterion.getName() + " is not supported by instrumenter " + descriptor.getUniqueKey());
                    }
                    factory.addCriterion(criterion);
                    logger.info("criterion: " + criterion.getName());
                }
            }
        } else {
            // use all supported criteria
            factory.addSupportedCriteria(descriptor);
            logger.info("using all criteria");
        }

        // set charset
        if (pHasCharset) {
            try {
                factory.setCharset(pCharset);
            } catch (UnsupportedCharsetException e) {
                logger.warning("Unknown charset " + pCharset
                        + ". It will be ignored.");
            }
            logger.info("charset: " + pCharset);
        } else {
            logger.info("charset: " + descriptor.getDefaultCharset());
        }

        factory.setPretendMode(pretend);
        if (pretend) {
            logger.info("pretend mode: on");
        }
        factory.setProgressHandler(progressHandler);
        if (pCopyUninstrumented) {
            logger.info("copy uninstrumented files: yes");
        } else {
            logger.info("copy uninstrumented files: no");
        }

        Instrumenter instrumenter = null;
        try {
            instrumenter = factory.getInstrumenter();
        } catch (FactoryMisconfigurationException e) {
            // shout not occur
            logger.fatal("A FactoryMisconfigurationException occured:\n"
                    + e.getMessage(), e);
        }

        // create the directives
        Map<String, Object> instrumenterDirectives = descriptor.getDefaultDirectiveValues();
        Map<String, InstrumenterDirective> registeredDirectives = descriptor.getRegisteredDirectives();
        for (String thisDirective : pDirectives) {
            int index = thisDirective.indexOf('=');
            if (index == -1) {
                logger.fatal("Wrong directive format: " + thisDirective);
            }

            String directiveKey = thisDirective.substring(0, index);
            String directiveValue = StringUtil.parseNonQuotedStringLiteral(thisDirective.substring(index+1));

            // is the key a valid directive
            InstrumenterDirective directive = registeredDirectives.get(directiveKey);
            if (directive != null) {
                Object parsedValue = null;
                try {
                  parsedValue = directive.parseValue(directiveValue);
                } catch (IllegalArgumentException e) {
                  logger.fatal("Cannot parse value for directive " + thisDirective + ", " +
                      "the value is malformed: " + directiveValue, e);
                }
                instrumenterDirectives.put(directiveKey, parsedValue);
            } else {
                logger.fatal("Directive " + thisDirective + " is unknown.");
            }
        }

        // print some directive value info
        if (instrumenterDirectives.isEmpty()) {
            logger.info("No directives set.");
        } else {
            logger.info("The following directives have been set:");
            for (Entry<String, Object> thisEntry : instrumenterDirectives.entrySet()) {
              logger.info(String.format("> %s = %s (%s)",
                  thisEntry.getKey(),
                  thisEntry.getValue().toString(),
                  thisEntry.getValue().getClass().getSimpleName()));
            }
        }

        // ////////////////////////////////////////////////////////////////////
        //
        // resolve the root and the destination
        //
        // ////////////////////////////////////////////////////////////////////
        File rootFolderFile;
        File targetFolderFile;
        try {
            rootFolderFile = new File(pRootDirectory).getCanonicalFile();
        } catch (IOException e) {
            logger.fatal("Could not resolve: " + pRootDirectory);
            throw new RuntimeException();
        }
        try {
            targetFolderFile = new File(pDestination).getCanonicalFile();
        } catch (IOException e) {
            logger.fatal("Could not resolve: " + pDestination);
            throw new RuntimeException();
        }
        if (!rootFolderFile.isDirectory()) {
            logger.fatal("The root-directory is no directory");
        }
        if (targetFolderFile.isFile()) {
            logger.fatal("The destination is a file");
        }
        if (rootFolderFile.equals(targetFolderFile)) {
            logger.fatal("The root-directory is the same than the destination. " +
            "This is not allowed to avoid overwriting of the source files.");
        }

        Collection<SourceTargetContainer> filesToInstrument = getFilesCallback.getFilesToInstrument(descriptor, rootFolderFile, targetFolderFile);

        Collection<SourceTargetContainer> filesToCopy = getFilesCallback.getFilesToCopy(descriptor, rootFolderFile, targetFolderFile);


        // ////////////////////////////////////////////////////////////////////
        //
        // instrument
        //
        // ////////////////////////////////////////////////////////////////////
        TestSessionContainer testSessionContainer;
        MASTBuilder builder = new MASTBuilder(logger);

        if (filesToInstrument.isEmpty()) {
            logger.warning("No files found to instrument.");
        } else if (filesToInstrument.size() == 1) {
            File theSourceFile = filesToInstrument.iterator().next().getSource();
            File theTargetFile = filesToInstrument.iterator().next().getTarget();
            logger.info("Instrument one single code file: " +
                        theSourceFile.getAbsolutePath() + " to\n" +
                        theTargetFile.getAbsolutePath());
        } else {
            if (!instrumenter.allowsFileListInstrumentation()) {
                logger.fatal("The instrumenter\n"
                        + descriptor.getDescription()
                        + "\ndoes not allow the instrumentation of a set of files. "
                        + "Use the options \"-i (include)\""
                        + "to specify one single source file.");
            }

            logger.info(filesToInstrument.size() +
                        " files selected for instrumentation.");
        }

        if (!filesToInstrument.isEmpty()) {
            try {
                builder.getLogger().info("Instrumentation starting...");
                testSessionContainer = instrumenter.instrument(
                        rootFolderFile,
                        targetFolderFile,
                        filesToInstrument,
                        builder,
                        instrumenterDirectives);
            } catch (InstrumentationException e) {
                handleInstrumentationException(e, logger);
                throw new RuntimeException();
            }
            logger.info("Instrumentation finished.");

            saveSessionContainerCallback.saveSessionContainer(testSessionContainer);
        }

        // ////////////////////////////////////////////////////////////////////
        //
        // copy uninstrumented
        //
        // ////////////////////////////////////////////////////////////////////
        if (!pCopyUninstrumented) {
            logger.info("No files will be copied");
            return;
        }

        logger.info(filesToCopy.size() + " files will be copied");
        if (filesToCopy.isEmpty()) {
          return;
        }

        logger.info("Copying starting...");
        try {
            int fileCount = 0;
            for (SourceTargetContainer container : filesToCopy) {
                if (pretend) {
                    // do not copy, but do some activity
                    FileTool.getContentFromFile(container.getSource());
                } else {
                    FileTool.copy(container.getSource(), container.getTarget());
                }
                fileCount++;
                progressHandler.setProgress(((float)fileCount) / ((float)filesToCopy.size()));
            }
        } catch (IOException e) {
            logger.fatal("A file could not be coppied:\n" + e.getMessage());
        }
        logger.info("Copying finished.");
    }


    private static void handleInstrumentationException(InstrumentationException e, Logger logger) {
        String message = e.getMessage();
        String fileOfException = "";

        if (e.getFileOfException() != null) {
            fileOfException = "\n" + e.getFileOfException().getAbsolutePath();
        }

        if (e instanceof InstrumentationFileNotFoundException) {
            if (message != null) {
                logger.fatal("A file was not found or could not be used for "
                             + "reading/writing:\n" + message + fileOfException, e);
            } else {
                logger.fatal(fileOfException + "A file was not found or could not be used "
                             + "for reading/writing." + fileOfException, e);
            }
        } else if (e instanceof InstrumentationIOException) {
            if (message != null) {
                logger.fatal("A file could not be used for reading/writing:\n"
                             + message + fileOfException, e);
            } else {
                logger.fatal("A file could not be used for reading/writing."
                             + fileOfException, e);
            }
        } else if (e instanceof ParseException) {
            if (message != null) {
                logger.fatal("A file could not be parsed:\n"
                             + message + fileOfException, e);
            } else {
                logger.fatal("A file could not be parsed."
                             + fileOfException, e);
            }
        } else {
            // should not occur
            // These are only RuntimeExceptions
            logger.fatal("An InstrumentationException occured:\n"
                    + e.getMessage() + fileOfException, e);
        }
        throw new RuntimeException();
    }
}
