/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This file may be used, modifies and redistributed     *
 * under the terms of either the Eclipse Public License v1.0 which            *
 * accompanies this distribution and is available at                          *
 * http://www.eclipse.org/legal/epl-v10.html or the MIT license, available at *
 * http://www.opensource.org/licenses/mit-license.php                         *
 ******************************************************************************/

package org.codecover.batch;

import java.util.Collections;

/**
 * HelpCommand
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: HelpCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class HelpCommand extends Command {
    /**
     * @author Steffen Kieß
     * 
     * @version 1.0 ($Id: HelpCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public interface CommandCollectionProvider {
        /**
         * Gets the collection of commands
         * 
         * @return the command collection
         */
        CommandCollection get();
    }

    private final CommandCollectionProvider commandCollectionProvider;

    /**
     * Creates a HelpCommand
     * 
     * @param commandCollectionProvider
     *            the provider carrying the list of commands
     */
    public HelpCommand(CommandCollectionProvider commandCollectionProvider) {
        super("h", "help", "shows help-page", new OptionSet(Collections.<Option>emptySet(), Collections.<Option>emptySet()), 0, -1, false);

        if (commandCollectionProvider == null) {
            throw new NullPointerException("commandCollectionProvider == null");
        }

        this.commandCollectionProvider = commandCollectionProvider;
    }

    @Override
    protected int run(CommandLine cl, BatchLogger logger, org.codecover.model.extensions.PluginManager pluginManager) {
        CommandCollection commands = this.commandCollectionProvider.get();
        if (commands == null) {
            throw new NullPointerException();
        }

        if (cl.getNonOptionArgs().size() == 0) {
            Batch.printUsage(commands);
            return 0;
        } else {
            boolean error = false;
            for (String arg : cl.getNonOptionArgs()) {
                Command command = commands.getCommandByName(arg);
                if (command == null) {
                    System.err.println("Command " + arg + " not found.");
                    System.err.println();
                    error = true;
                } else {
                    command.printHelp();
                    System.err.println();
                }
            }
            return error ? 1 : 0;
        }
    }
}
