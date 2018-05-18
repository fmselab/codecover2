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
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.UnsupportedCharsetException;
import java.util.Date;

import org.codecover.batch.BatchLogger;
import org.codecover.batch.CommandLine;
import org.codecover.batch.Option;
import org.codecover.batch.OptionSet;
import org.codecover.batch.Options;
import org.codecover.batch.SimpleCommand;
import org.codecover.instrumentation.measurement.CoverageResultLogReader;
import org.codecover.instrumentation.measurement.MeasurementConstants;
import org.codecover.instrumentation.measurement.parser.CoverageLogParser;
import org.codecover.instrumentation.measurement.parser.ParseException;
import org.codecover.instrumentation.measurement.parser.WrongUIDException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.exceptions.FileSaveException;

/**
 * AnalyseCommand
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: AnalyzeCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class AnalyzeCommand extends SimpleCommand {

    private static final AnalyzeCommand instance = new AnalyzeCommand();

    /**
     * Gets an instance of this class
     * 
     * @return the instance of the class
     */
    public static AnalyzeCommand getInstance() {
        return instance;
    }

    private AnalyzeCommand() {
        super("an",
              "analyze",
              "calculates coverage from instrumentation and coverage logs",
              new OptionSet(new Option[] {Options.coverageLog,
                                          Options.container,
                                          Options.name},
                            new Option[] {Options.comment, Options.charset}));
    }

    @Override
    protected int run(CommandLine cl, BatchLogger logger, org.codecover.model.extensions.PluginManager pluginManager) {
        File coverageLogFile = new File(cl.getOptionValue(Options.coverageLog));

        // Create the testSessionContainer and fill it with the data
        // from the given file
        TestSessionContainer testSessionContainer = null;
        String containerLocation = cl.getOptionValue(Options.container);
        MASTBuilder builder = new MASTBuilder(logger);

        try {
            testSessionContainer = TestSessionContainer.load(pluginManager, logger,
                                                             builder,
                                                             containerLocation);
        } catch (FileLoadException e) {
            logger.fatal("An error occured during loading", e);
        }

        if (testSessionContainer == null) {
            logger.fatal("testSessionContainer == null");
        }

        String testSessionName = cl.getOptionValue(Options.name);
        String testSessionComment = "";
        if (cl.hasOption(Options.comment)) {
            testSessionComment = cl.getOptionValue(Options.comment);
        }
        Charset coverageLogCharset;
        if (cl.hasOption(Options.charset)) {
            try {
                coverageLogCharset = Charset.forName(cl.getOptionValue(Options.charset));
            } catch (UnsupportedCharsetException e) {
                logger.error("The charset \""
                        + cl.getOptionValue(Options.charset)
                        + "\" is not supported", e);
                return 1;
            }
        } else {
            coverageLogCharset = MeasurementConstants.CHARSET;
        }

        TestSession testSession = testSessionContainer.createTestSession(testSessionName,
                                                                         testSessionComment,
                                                                         new Date());
        // If the name of the test session already existed, the user should
        // know, that the session is called something like
        // %test session name (1)%
        if (!testSessionName.equals(testSession.getName())) {
            logger.warning("A test session with the name \""
                    + testSessionName
                    + "\" already exists! The created test session can be found under the name: \""
                    + testSession.getName() + "\"");
        }

        try {
            CoverageLogParser logParser = new CoverageLogParser(coverageLogFile,
                                                                coverageLogCharset);

            CoverageResultLogReader coverageResultLogReader = new CoverageResultLogReader(testSession,
                                                                                          builder);

            logParser.CompilationUnit(coverageResultLogReader,
                                      testSessionContainer.getId());

            try {
                if (!isPretend(cl)) {
                    testSessionContainer.save(containerLocation);
                }
            } catch (FileSaveException e) {
                logger.fatal("An error occured during saving", e);
            }

        } catch (IOException e) {
            logger.fatal("Error accessing the coverage log file", e);
        } catch (WrongUIDException e) {
            logger.error("The coverage log file does not fit to the session container! Process aborted.",
                         e);
            return 1;
        } catch (ParseException e) {
            logger.fatal("Error parsing the coverage log", e);
        }

        return 0;
    }
}
