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
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.exceptions.FileSaveException;
import org.codecover.model.exceptions.MergeException;

/**
 * MergeTestCasesCommand
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: MergeTestCasesCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MergeTestCasesCommand extends SimpleCommand {
    private static final MergeTestCasesCommand instance = new MergeTestCasesCommand();

    /**
     * Gets an instance of this class
     * 
     * @return the instance of the class
     */
    public static MergeTestCasesCommand getInstance() {
        return instance;
    }

    private MergeTestCasesCommand() {
        super("mt",
              "merge-test-cases",
              "merges test cases of the same session",
              new OptionSet(new Option[] {Options.container,
                                          Options.testSession,
                                          Options.testCases,
                                          Options.name},
                            new Option[] {Options.removeOldTestCases, Options.comment}));
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

        TestSession testSession = testSessionContainer.getTestSessionWithName(cl.getOptionValue(Options.testSession));
        if (testSession == null) {
            logger.fatal("testSession == null");
        }

        List<TestCase> testCaseList = new Vector<TestCase>();
        List<String> notFoundTestCaseNameList = new Vector<String>();
        List<String> foundTestCaseNameList = new Vector<String>();

        List<String> givenTestCaseNames = cl.getOptionValues(Options.testCases);

        for (String givenTestCaseName : givenTestCaseNames) {

            TestCase testCase = testSession.getTestCaseWithName(givenTestCaseName);

            if (testCase != null) {
                testCaseList.add(testCase);
                foundTestCaseNameList.add(givenTestCaseName);
            } else {
                // For later usage add those test case names to a list, which do
                // not correspond to a test case in the current
                // testsessioncontainer.
                notFoundTestCaseNameList.add(givenTestCaseName);
            }
        }

        String mergedTestCaseName = cl.getOptionValue(Options.name);
        String mergedTestCaseComment = "";

        if (cl.hasOption(Options.comment)) {
            mergedTestCaseComment = cl.getOptionValue(Options.comment);
        }

        try {
            if (!testCaseList.isEmpty()) {
                testSessionContainer.mergeTestCases(testCaseList,
                                                    mergedTestCaseName,
                                                    mergedTestCaseComment);

                // If the remove-option was set, delete the merged test cases
                if (cl.hasOption(Options.removeOldTestCases)) {
                    for (TestCase testCase : testCaseList) {
                        testCase.delete();
                    }
                }

                StringBuilder sb = new StringBuilder();
                sb.append("Successfully merged the following test cases:\n");

                for (String foundTestCaseName : foundTestCaseNameList) {
                    sb.append("- " + foundTestCaseName + "\n");
                }

                sb.append("into the test case with the name \""
                        + mergedTestCaseName + "\"");
                logger.info(sb.toString());
            } else {
                logger.warning("No test cases merged, since no valid test case names were given!");
            }

            if (!notFoundTestCaseNameList.isEmpty()) {
                StringBuilder sb = new StringBuilder();
                sb.append("\nCould not find the following test cases:");
                for (String notFoundSessionName : notFoundTestCaseNameList) {
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
            logger.fatal("Merging failed", e);
        }

        return 0;
    }
}
