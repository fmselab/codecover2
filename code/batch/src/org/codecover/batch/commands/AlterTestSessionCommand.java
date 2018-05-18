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
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.exceptions.FileSaveException;
import org.codecover.model.exceptions.NameAlreadyUsedException;

/**
 * AlterTestSessionCommand
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: AlterTestSessionCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class AlterTestSessionCommand extends SimpleCommand {
    private static final AlterTestSessionCommand instance = new AlterTestSessionCommand();

    /**
     * Gets an instance of this class
     * 
     * @return the instance of the class
     */
    public static AlterTestSessionCommand getInstance() {
        return instance;
    }

    private AlterTestSessionCommand() {
        super("as",
              "alter-session",
              "change information of a test session",
              new OptionSet(new Option[] {Options.container, Options.testSession},
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

        String oldTestSessionName = cl.getOptionValue(Options.testSession);

        boolean changedName = false;
        boolean changedComment = false;

        TestSession testSession = testSessionContainer.getTestSessionWithName(oldTestSessionName);

        if (testSession != null) {
            if (cl.hasOption(Options.name)) {
                String newTestSessionName = cl.getOptionValue(Options.name);
                try {
                    testSession.setName(newTestSessionName);
                    changedName = true;
                    logger.info("Changed name of test session from \""
                            + oldTestSessionName + "\" to \""
                            + newTestSessionName + "\".");
                } catch (NameAlreadyUsedException e) {
                    logger.warning("Name could not be set", e);
                }
            }
            if (cl.hasOption(Options.comment)) {
                String oldTestSessionComment = testSession.getComment();
                String newTestSessionComment = cl.getOptionValue(Options.comment);
                testSession.setComment(newTestSessionComment);
                changedComment = true;
                logger.info("Changed comment of test session from \""
                        + oldTestSessionComment + "\" to \""
                        + newTestSessionComment + "\".");
            }
        } else {
            logger.warning("test session with the name: \""
                    + oldTestSessionName
                    + "\" does not exists in the given test session container");
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
