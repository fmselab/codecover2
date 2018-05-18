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
import java.util.List;

import org.codecover.batch.BatchLogger;
import org.codecover.batch.BatchProgressHandler;
import org.codecover.batch.CommandLine;
import org.codecover.batch.Option;
import org.codecover.batch.OptionSet;
import org.codecover.batch.Options;
import org.codecover.batch.SimpleCommand;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.utils.ReportUtils;

/**
 * ReportCommand
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: ReportCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
@SuppressWarnings("static-access")
public class ReportCommand extends SimpleCommand {

    private static final ReportCommand instance = new ReportCommand();
    
    /**
     * Gets an instance of this class
     * 
     * @return the instance of the class
     */
    public static ReportCommand getInstance() {
        return instance;
    }

    private ReportCommand() {
        super("re",
              "report",
              "generates a report based on a test session",
              new OptionSet(new Option[] {Options.container,
                                          Options.testSession,
                                          Options.template,
                                          Options.destination}, new Option[] {}));
    }

    @Override
    protected int run(CommandLine cl, BatchLogger logger, org.codecover.model.extensions.PluginManager pluginManager) {
        String outPath     = cl.getOptionValue(Options.destination);
        String template    = cl.getOptionValue(Options.template);
        String container   = cl.getOptionValue(Options.container);
        String sessionName = cl.getOptionValue(Options.testSession);

        final BatchProgressHandler progressHandler = BatchProgressHandler.createBatchProgressHandler(cl, logger);
        
        logger.info("Generating Report:");
        logger.info("container: " + new File(container).getAbsolutePath());
        logger.info("template: " + new File(template).getAbsolutePath());
        logger.info("session: \"" + sessionName + "\"");
        logger.info("destination: " + new File(outPath).getAbsolutePath());

        // Create the testSessionContainer and fill it with the data
        // from the given file
        TestSessionContainer testSessionContainer = null;
        try {
            MASTBuilder builder = new MASTBuilder(logger);
            testSessionContainer = TestSessionContainer.load(pluginManager, logger,
                                                             builder,
                                                             container);
        } catch (FileLoadException e) {
            logger.fatal("An error occured during loading: " + e.getMessage(), e);
        }

        if (testSessionContainer == null) {
            logger.fatal("testSessionContainer == null");
        }

        TestSession testSession = testSessionContainer.getTestSessionWithName(sessionName);
        if (testSession == null) {
            logger.fatal("testSession == null");
        }
        
        List<org.codecover.model.TestCase> testCases = testSession.getTestCases();
        if (testCases == null) {
            logger.fatal("testCases == null");
        }
        
        ReportUtils.report(logger, pluginManager, progressHandler, isPretend(cl), new File(outPath), new File(template), testCases);

        progressHandler.complete();
        
        return 0;
    }
}
