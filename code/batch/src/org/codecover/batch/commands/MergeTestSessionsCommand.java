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
import org.codecover.model.exceptions.MergeException;
import org.codecover.model.utils.NameReoccurenceHelper;

/**
 * MergeSessionCommand
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: MergeTestSessionsCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MergeTestSessionsCommand extends SimpleCommand {

    private static final MergeTestSessionsCommand instance = new MergeTestSessionsCommand();

    /**
     * Gets an instance of this class
     * 
     * @return the instance of the class
     */
    public static MergeTestSessionsCommand getInstance() {
        return instance;
    }

    private MergeTestSessionsCommand() {
        super("ms",
              "merge-sessions",
              "merge sessions within the same test session container",
              new OptionSet(new Option[] {Options.container,
                                          Options.testSessions,
                                          Options.name},
                            new Option[] {Options.removeOldTestSessions, Options.comment}));
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

        List<TestSession> testSessionList = new Vector<TestSession>();
        List<String> notFoundTestSessionNameList = new Vector<String>();
        List<String> foundTestSessionNameList = new Vector<String>();

        List<String> givenSessionNames = cl.getOptionValues(Options.testSessions);

        for (String givenSessionName : givenSessionNames) {

            TestSession testSession = testSessionContainer.getTestSessionWithName(givenSessionName);

            if (testSession != null) {
                testSessionList.add(testSession);
                foundTestSessionNameList.add(givenSessionName);
            } else {
                // For later usage add those test case names to a list, which do
                // not correspond to a test case in the current
                // testsessioncontainer.
                notFoundTestSessionNameList.add(givenSessionName);
            }
        }

        String mergedTestSessionName = cl.getOptionValue(Options.name);

        String escapedName = NameReoccurenceHelper.escapeName(testSessionContainer.getTestSessionNames(),
                                                              mergedTestSessionName);

        // The name of the merged test session is already present in the test
        // session container, so an (count) is appended
        if (!mergedTestSessionName.equals(escapedName)) {
            logger.warning("A test session with the name \""
                    + mergedTestSessionName
                    + "\" already exists! The merged test session is saved under the name \""
                    + escapedName + "\"");
        }

        String mergedTestSessionComment = "";

        if (cl.hasOption(Options.comment)) {
            mergedTestSessionComment = cl.getOptionValue(Options.comment);
        }

        try {
            if (!testSessionList.isEmpty()) {
                testSessionContainer.mergeTestSessions(testSessionList,
                                                       mergedTestSessionName,
                                                       mergedTestSessionComment);

                // If the remove-option was set, delete the merged test sessions
                if (cl.hasOption(Options.removeOldTestSessions)) {
                    for (TestSession testSession : testSessionList) {
                        testSession.delete();
                    }
                }

                StringBuilder sb = new StringBuilder();
                sb.append("Successfully merged the following test sessions:\n");
                for (String foundSessionName : foundTestSessionNameList) {
                    sb.append("- " + foundSessionName + "\n");
                }
                sb.append("into the test session with the name \""
                        + escapedName + "\"");
                logger.info(sb.toString());
            } else {
                logger.warning("No test sessions merged, since no valid test session names were given!");
            }

            if (!notFoundTestSessionNameList.isEmpty()) {
                StringBuilder sb = new StringBuilder();
                sb.append("Could not find the following test sessions:");
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
        } catch (MergeException e) {
            logger.fatal("An error occured during merging", e);
        }

        return 0;
    }
}
