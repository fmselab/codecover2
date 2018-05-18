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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.Vector;

import org.codecover.model.utils.Logger;

/**
 * A collection of {@link Command}s.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: CommandCollection.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CommandCollection {
    private final Set<Command> commands;

    private final List<Command> commandsSorted;

    private final Map<String, Command> map;

    private final OptionSet mergedOptions;

    private CommandCollection(Set<Command> commands) {
        this.commands = Collections.unmodifiableSet(new HashSet<Command>(
                commands));

        final Map<String, Command> map = new TreeMap<String, Command>();
        final List<Command> commandsSorted = new Vector<Command>();
        for (Command command : this.commands) {
            commandsSorted.add(command);
            map.put(command.getName(), command);
            if (command.getShortName() != null) {
                map.put(command.getShortName(), command);
            }
        }
        this.map = Collections.unmodifiableMap(map);
        Collections.sort(commandsSorted, new Comparator<Command>() {
            public int compare(Command o1, Command o2) {
                return o1.getName().compareTo(o2.getName());
            }
        });
        this.commandsSorted = Collections.unmodifiableList(commandsSorted);

        final List<OptionSet> optionSets = new ArrayList<OptionSet>();
        for (Command command : this.commands) {
            optionSets.add(command.getOptions());
        }
        this.mergedOptions = OptionSet.mergeAndReplace(optionSets);
    }

    /**
     * Creates a new {@link CommandCollection} with the given set of
     * {@link Command}s
     * 
     * @param commands
     *                the set of {@link Command}s, to be contained in this
     *                {@link CommandCollection}
     * @return the {@link CommandCollection}
     */
    public static CommandCollection create(Set<Command> commands) {
        if (commands == null) {
            throw new NullPointerException("commands == null");
        }

        return new CommandCollection(commands);
    }

    /**
     * Gets a {@link Set} of all {@link Command}s in this
     * {@link CommandCollection}
     * 
     * @return the {@link Set} of {@link Command}s
     */
    public Set<Command> getAllCommands() {
        return this.commands;
    }

    /**
     * Gets a sorted {@link List} of all {@link Command}s in this
     * {@link CommandCollection}
     * 
     * @return the {@link List} of {@link Command}s
     */
    public List<Command> getSortedCommands() {
        return this.commandsSorted;
    }

    /**
     * Gets the {@link Command} with the given name
     * 
     * @param name
     *                the name of the {@link Command}
     * @return the {@link Command} with the given name, or <code>null</code>,
     *         if no such {@link Command} existed.
     */
    public Command getCommandByName(String name) {
        if (name == null) {
            throw new NullPointerException("name == null");
        }

        return this.map.get(name);
    }

    private static <T> List<T> removeFirstElement(List<T> list) {
        final List<T> newList = new ArrayList<T>(list);
        newList.remove(0);
        return Collections.unmodifiableList(newList);
    }

    /**
     * Trys and finds a Command in the args, which is executed, or, if no
     * command is found, trys and finds options valid without a command, which
     * are executed. If nothing is found or the arguments are wrong in another
     * way, an error message with the hint to the --help-option is written to
     * System.err
     * 
     * @param args
     *                the arguments to parse
     * @param logger
     *                the {@link Logger} to use.
     * @param helpCommand
     *                the command, that displays the help
     * @param versionCommand
     *                the command, that displays the version.
     * @return 0 if all went well, 1 if an error occurred
     */
    public int process(List<String> args, BatchLogger logger,
            Command helpCommand, Command versionCommand) {
        if (helpCommand != null && args.size() > 0
                && (args.get(0).equals("--help") || args.get(0).equals("-h"))) {
            return helpCommand.process(removeFirstElement(args), logger, false);
        }

        if (versionCommand != null
                && args.size() > 0
                && (args.get(0).equals("--version") || args.get(0).equals("-V"))) {
            return versionCommand.process(removeFirstElement(args), logger,
                    false);
        }

        // Parse options to get the first non-option argument
        CommandLine cl = OptionParser.parseAndPrintError(this.mergedOptions,
                args);
        if (cl == null) {
            // error, has already been printed
            return 1;
        }

        if (cl.getNonOptionArgs().size() == 0) {
            System.err.println("Error: No command given.");
            System.err.println("Use codecover --help to see all "
                    + "available commands.");
            return 1;
        }

        final Command command = getCommandByName(cl.getNonOptionArgs().get(0));
        if (command == null) {
            System.err.println(cl.getNonOptionArgs().get(0)
                    + " is no valid command. "
                    + "Use codecover --help to see all available commands.");
            return 1;
        }

        boolean hasHelpOption = false;
        for (Option option : cl.getOptionSet().getOptional()) {
            if (option.getShortOption() == 'h'
                    || "help".equals(option.getLongOption())) {
                if (cl.hasOption(option)) {
                    hasHelpOption = true;
                }
            }
        }
        if (hasHelpOption) {
            command.printHelp();
            return 0;
        }

        return command.process(args, logger, true);
    }

    /**
     * Gets the {@link OptionSet} of merged options.
     * 
     * @return the {@link OptionSet}
     */
    public OptionSet getMergedOptions() {
        return this.mergedOptions;
    }
}
