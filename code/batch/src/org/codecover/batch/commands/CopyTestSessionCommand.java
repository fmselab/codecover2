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
import java.util.Date;
import java.util.List;
import java.util.Vector;

import org.codecover.batch.BatchLogger;
import org.codecover.batch.CommandLine;
import org.codecover.batch.Option;
import org.codecover.batch.OptionSet;
import org.codecover.batch.Options;
import org.codecover.batch.SimpleCommand;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.exceptions.FileSaveException;

/**
 * CopyTestSessionCommand
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: CopyTestSessionCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CopyTestSessionCommand extends SimpleCommand {
    private static final CopyTestSessionCommand instance = new CopyTestSessionCommand();

    /**
     * Gets an instance of this class
     * 
     * @return the instance of the class
     */
    public static CopyTestSessionCommand getInstance() {
        return instance;
    }

    private CopyTestSessionCommand() {
        super("cs",
              "copy-sessions",
              "copy session from one container to another",
              new OptionSet(new Option[] {Options.container,
                                          Options.testSessions,
                                          Options.destinationContainer}, new Option[] {}));
    }

    @Override
    protected int run(CommandLine cl, BatchLogger logger, org.codecover.model.extensions.PluginManager pluginManager) {
        // Create the testSessionContainer and fill it with the data
        // from the given file
        TestSessionContainer sourceTestSessionContainer = null;
        String sourceContainerLocation = cl.getOptionValue(Options.container);
        try {
            MASTBuilder builder = new MASTBuilder(logger);
            sourceTestSessionContainer = TestSessionContainer.load(pluginManager, logger,
                                                                   builder,
                                                                   sourceContainerLocation);
        } catch (FileLoadException e) {
            logger.fatal("An error occured during loading", e);
        }

        if (sourceTestSessionContainer == null) {
            logger.fatal("sourceTestSessionContainer == null");
        }

        String destinationContainerLocation = cl.getOptionValue(Options.destinationContainer);
        TestSessionContainer destinationTestSessionContainer = null;
        File destinationFile = new File(destinationContainerLocation);

        if (destinationFile.exists()) {
            try {
                MASTBuilder builder = new MASTBuilder(logger);
                destinationTestSessionContainer = TestSessionContainer.load(pluginManager, logger,
                                                                            builder,
                                                                            destinationFile);
            } catch (FileLoadException e) {
                logger.fatal("An error occured during loading", e);
            }

        } else {
            // If the destinationTestSessionContainer did not exist previously,
            // the mast from the sourceTestSessionContainer is copied into the
            // destinationTestSessionContainer, as well as the specified test
            // sessions.
            destinationTestSessionContainer = new TestSessionContainer(sourceTestSessionContainer.getCode(),
                                                                       sourceTestSessionContainer.getLogger(),
                                                                       sourceTestSessionContainer.getFiles(),
                                                                       sourceTestSessionContainer.getCriteria(),
                                                                       sourceTestSessionContainer.getId(),
                                                                       sourceTestSessionContainer.getDate());
        }

        if (destinationTestSessionContainer == null) {
            logger.fatal("destinationTestSessionContainer == null");
        }

        if (!sourceTestSessionContainer.isCompatible(destinationTestSessionContainer)) {
            logger.fatal("sourceTestSessionContainer is incompatible"
                    + "  with destinationTestSessionContainer");
        }

        List<TestSession> sessionList = new Vector<TestSession>();
        List<String> foundTestSessionNameList = new Vector<String>();
        List<String> notFoundTestSessionNameList = new Vector<String>();

        List<String> givenSessionNames = cl.getOptionValues(Options.testSessions);

        // Traverse the session in the sourceTestSessionContainer and compare
        // their names to the ones given by the user
        for (String givenSessionName : givenSessionNames) {

            TestSession testSession = sourceTestSessionContainer.getTestSessionWithName(givenSessionName);

            if (testSession != null) {
                sessionList.add(testSession);
                foundTestSessionNameList.add(givenSessionName);
            } else {
                // For later usage add the session names to a list, which do not
                // correspond to a test session in the current
                // testsessioncontainer.
                notFoundTestSessionNameList.add(givenSessionName);
            }

        }

        // traverse the found sessions of the sourceTestSessionContainer and
        // copy them into the destinationTestSessionContainer.
        for (TestSession sourceTestSession : sessionList) {
            String testSessionName = sourceTestSession.getName();
            String testSessionComment = sourceTestSession.getComment();
            Date testSessionDate = sourceTestSession.getDate();

            // Create the destination test session with the data from the old
            // test session
            TestSession destinationTestSession = destinationTestSessionContainer.createTestSession(testSessionName,
                                                                                                   testSessionComment,
                                                                                                   testSessionDate);

            // If the name of the test session already existed, the user should
            // know, that the session is called something like
            // %test session name (1)%
            if (!testSessionName.equals(destinationTestSession.getName())) {
                logger.warning("A test session with the name \""
                        + testSessionName
                        + "\" already exisited! The copied test session can be found under the name: \""
                        + destinationTestSession.getName() + "\"");
            }

            // Copy each test case from the source test session into the
            // destination test session
            for (TestCase sourceTestCase : sourceTestSession.getTestCases()) {
                destinationTestSession.copyTestCaseIntoTestSession(sourceTestCase);
            }
        }

        StringBuilder sb = new StringBuilder();

        sb.append("Successfully copied the following test sessions:");

        for (String foundTestSessionName : foundTestSessionNameList) {
            sb.append("\n-" + foundTestSessionName);
        }
        sb.append("\n into the test session container"
                + new File(destinationContainerLocation).getName());

        if (!notFoundTestSessionNameList.isEmpty()) {
            sb.append("\nCould not find the following test sessions:");
            for (String notFoundSessionName : notFoundTestSessionNameList) {
                sb.append("\n- " + notFoundSessionName);
            }
        }
        logger.info(sb.toString());

        try {
            if (!isPretend(cl)) {
                destinationTestSessionContainer.save(destinationContainerLocation);
            }
        } catch (FileSaveException e) {
            logger.fatal("An error occured during saving", e);
        }

        return 0;
    }
}
