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
import org.codecover.model.exceptions.NameAlreadyUsedException;

/**
 * AlterTestCaseCommand
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: AlterTestCaseCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class AlterTestCaseCommand extends SimpleCommand {
    private static final AlterTestCaseCommand instance = new AlterTestCaseCommand();

    /**
     * Gets an instance of this class
     * 
     * @return the instance of the class
     */
    public static AlterTestCaseCommand getInstance() {
        return instance;
    }

    private AlterTestCaseCommand() {
        super("at",
              "alter-test-case",
              "changes information about a test case",
              new OptionSet(new Option[] {Options.container,
                                          Options.testSession,
                                          Options.testCase},
                            new Option[] {Options.name, Options.comment}));
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

        boolean changedName = false;
        boolean changedComment = false;

        String oldTestCaseName = cl.getOptionValue(Options.testCase);
        TestCase testCase = testSession.getTestCaseWithName(oldTestCaseName);

        if (testCase != null) {
            if (cl.hasOption(Options.name)) {
                String newTestCaseName = cl.getOptionValue(Options.name);
                try {
                    testCase.setName(newTestCaseName);
                    changedName = true;
                    logger.info("Changed name of test case from \""
                            + oldTestCaseName + "\" to \"" + newTestCaseName
                            + "\".");
                } catch (NameAlreadyUsedException e) {
                    logger.warning("Name could not be set", e);
                }
            }
            if (cl.hasOption(Options.comment)) {
                String oldTestCaseComment = testCase.getComment();
                String newTestCaseComment = cl.getOptionValue(Options.comment);
                testCase.setComment(newTestCaseComment);
                changedComment = true;
                logger.info("Changed comment of test case from \""
                        + oldTestCaseComment + "\" to \"" + newTestCaseComment
                        + "\".");
            }
        }

        // Only save, if something has been changed.
        if (changedName || changedComment) {
            try {
                if (!isPretend(cl)) {
                    testSessionContainer.save(containerLocation);
                }
            } catch (FileSaveException e) {
                logger.fatal("An error occured during saving", e);
            }
        }

        return 0;
    }

}
