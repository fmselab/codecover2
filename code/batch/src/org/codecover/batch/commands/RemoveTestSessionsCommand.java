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

import java.util.List;
import java.util.Vector;

import org.codecover.batch.BatchLogger;
import org.codecover.batch.CommandLine;
import org.codecover.batch.Option;
import org.codecover.batch.OptionSet;
import org.codecover.batch.Options;
import org.codecover.batch.SimpleCommand;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.exceptions.FileSaveException;

/**
 * RemoveTestSessionsCommand
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: RemoveTestSessionsCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class RemoveTestSessionsCommand extends SimpleCommand {
    private static final RemoveTestSessionsCommand instance = new RemoveTestSessionsCommand();

    /**
     * Gets an instance of this class
     * 
     * @return the instance of the class
     */
    public static RemoveTestSessionsCommand getInstance() {
        return instance;
    }

    private RemoveTestSessionsCommand() {
        super("rs",
              "remove-sessions",
              "remove test sessions from session container",
              new OptionSet(new Option[] {Options.container, Options.testSessions},
                            new Option[] {}));
    }

    @Override
    protected int run(CommandLine cl, BatchLogger logger, org.codecover.model.extensions.PluginManager pluginManager) {
        // Create the testSessionContainer and fill it with the data
        // from the given file
        TestSessionContainer testSessionContainer = null;
        String containerLocation = cl.getOptionValue(Options.container);
        try {
            MASTBuilder builder = new MASTBuilder(logger);
            testSessionContainer = TestSessionContainer.load(pluginManager, logger,
                                                             builder,
                                                             containerLocation);
        } catch (FileLoadException e) {
            logger.fatal("An error occured during loading", e);
        }

        if (testSessionContainer == null) {
            logger.fatal("testSessionContainer == null");
        }

        List<String> notFoundTestSessionNameList = new Vector<String>();
        List<String> foundTestSessionNameList = new Vector<String>();

        List<String> givenSessionNames = cl.getOptionValues(Options.testSessions);

        for (String givenSessionName : givenSessionNames) {

            TestSession testSession = testSessionContainer.getTestSessionWithName(givenSessionName);

            if (testSession != null) {
                // A TestSession was found, so delete it.
                testSession.delete();
                foundTestSessionNameList.add(givenSessionName);
            } else {
                // For later usage add the session names
                // to a list, which do not correspond
                // to a test session in the current
                // testsessioncontainer.
                notFoundTestSessionNameList.add(givenSessionName);
            }
        }

        if (!foundTestSessionNameList.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            sb.append("Successfully deleted the following test sessions:");

            for (String foundSessionName : foundTestSessionNameList) {
                sb.append("\n- " + foundSessionName);
            }

            logger.info(sb.toString());
        } else {
            logger.warning("No test sessions removed, since no valid test case sessions were given!");
        }

        if (!notFoundTestSessionNameList.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            sb.append("\nCould not find the following test sessions:");
            for (String notFoundSessionName : notFoundTestSessionNameList) {
                sb.append("\n- " + notFoundSessionName);
            }
            logger.warning(sb.toString());
        }

        try {
            if (!isPretend(cl)) {
                testSessionContainer.save(containerLocation);
            }
        } catch (FileSaveException e) {
            logger.fatal("An error occured during saving", e);
        }

        return 0;
    }

}
