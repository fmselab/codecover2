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

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.codecover.model.extensions.PluginManager;
import org.codecover.model.extensions.PluginUtils;
import org.codecover.model.utils.FatalException;
import org.codecover.model.utils.LogLevel;
import org.codecover.model.utils.Logger;

/**
 * The abstract class Command provides everything a command needs except its
 * execution if it was called correctly. For the execution, the abstract
 * run-method is used.
 * 
 * @author Michael Starzmann, Markus Wittlinger
 * @version 1.0 ($Id: Command.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public abstract class Command {

    private final OptionSet options;

    private final String shortName;

    private final String name;

    private final String description;

    private final int minimumArguments;

    private final int maximumArguments;

    private final boolean hidden;

    /**
     * Creates a new Command
     * 
     * @param shortName
     *                the short name of the Command
     * @param name
     *                the long name of the Command
     * @param description
     *                the description of the Command for the help page
     * @param options
     *                the options of the Command
     * @throws IllegalArgumentException
     *                 if there is an non-valid character in opt
     */
    public Command(String shortName, String name, String description,
            OptionSet options) {
        this(shortName, name, description, options, 0, 0, false);
    }

    /**
     * Creates a new Command
     * 
     * @param shortName
     *                the short name of the Command
     * @param name
     *                the long name of the Command
     * @param description
     *                the description of the Command for the help page
     * @param minimumArguments
     *                the minimum amount of arguments for this command.
     * @param maximumArguments
     *                the maximum amount of arguments for this command.
     * @param options
     *                the options of the Command
     * @param hidden
     *                indicates, whether or not this command is hidden.
     * @throws IllegalArgumentException
     *                 if there is an non-valid character in opt
     */
    public Command(String shortName, String name, String description,
            OptionSet options, int minimumArguments, int maximumArguments,
            boolean hidden) {
        if (name == null) {
            throw new NullPointerException("name == null");
        }

        if (description == null) {
            throw new NullPointerException("description == null");
        }

        if (minimumArguments < 0) {
            throw new IllegalArgumentException("minimumArguments < 0");
        }

        if (maximumArguments < -1) {
            throw new IllegalArgumentException("maximumArguments < -1");
        }

        this.shortName = shortName;
        this.name = name;
        this.description = description;
        this.minimumArguments = minimumArguments;
        this.maximumArguments = maximumArguments;
        this.hidden = hidden;

        final Set<Option> optionalOptions = new HashSet<Option>(options
                .getOptional());
        optionalOptions.add(Options.help);
        this.options = new OptionSet(options.getRequired(), optionalOptions);
    }

    /**
     * Gets the short name of the command. Can be null
     * 
     * @return the short name
     */
    public String getShortName() {
        return this.shortName;
    }

    /**
     * Gets the name of the command
     * 
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    /**
     * Gets the description of the command.<br>
     * <br>
     * This description is showed at the <code>command overview</code> of
     * <code>codecover --help</code>.
     * 
     * @return the description
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * Returns a longer description of the command.<br>
     * <br>
     * This description is used for the help page of this command&mdash; e.g.
     * <code>codecover instrument --help</code>
     * 
     * @return a more detailed description
     */
    public String getDetailedDescription() {
        return getDescription();
    }

    /**
     * Gets the {@link OptionSet} of this {@link Command}
     * 
     * @return the {@link OptionSet}
     */
    public OptionSet getOptions() {
        return this.options;
    }

    /**
     * prints the options of this command to System.out
     */
    public void printHelp() {
        System.out.print(HelpFormatter.formatCommand(this));
    }

    private static <T> List<T> removeFirstElement(List<T> list) {
        final List<T> newList = new ArrayList<T>(list);
        newList.remove(0);
        return Collections.unmodifiableList(newList);
    }

    /**
     * Parses all previously set Options in args and, if sucessfull, calls
     * run(CommandLine cl, Logger cl) with the parsed CommandLine, or, if not
     * successful, prints error-messages to System.err
     * 
     * @param args
     *                maybe the command and the arguments this Command is called
     *                with
     * @param logger
     *                the logger to be used
     * @param argsContainCommand
     *                indicates, whether to arguments contain a command.
     * @return 0 if all went well, 1 if an error occurred
     */
    public int process(List<String> args, BatchLogger logger,
            boolean argsContainCommand) {
        CommandLine cmd = OptionParser.parseAndPrintError(getOptions(), args);

        if (cmd == null) {
            // an error has occurred during parsing the command line
            System.err.println("Use codecover " + getName()
                    + " --help to see a description of all options.");
            return 1;
        }

        // Remove first argument (the command) from the CommandLine
        if (argsContainCommand) {
            cmd = new CommandLine(cmd.getOptionSet(), removeFirstElement(cmd
                    .getNonOptionArgs()), cmd.getValues());
        }

        return process(cmd, logger);
    }

    /**
     * Processes the command on the given {@link CommandLine}.
     * 
     * @param cmd
     *                the given {@link CommandLine}
     * @param logger
     *                the {@link Logger} to use.
     * @return 0 if all went well, 1 if an error occurred
     */
    public int process(CommandLine cmd, BatchLogger logger) {
        if (cmd.hasOption(Options.help)) {
            printHelp();
            return 0;
        }

        if (cmd.getNonOptionArgs().size() < getMinimumArguments()) {
            System.err.println("Not enough arguments for command " + getName()
                    + ". Needed " + getMinimumArguments() + ", got "
                    + cmd.getNonOptionArgs().size());
            System.err.println("Use \"codecover " + getName()
                    + " --help\" to see a description of all options.");
            return 1;
        } else if (getMaximumArguments() != -1
                && cmd.getNonOptionArgs().size() > getMaximumArguments()) {
            System.err.println("Too many arguments for command " + getName()
                    + ". Allowed " + getMaximumArguments() + ", got "
                    + cmd.getNonOptionArgs().size());
            System.err.println("Use codecover " + getName()
                    + " --help to see a description of all options.");
            return 1;
        } else {
            if (isQuiet(cmd) && isVerbose(cmd)) {
                System.err.println("You cant't combine"
                        + " --quiet with --verbose.");
                return 1;
            } else {
                if (isQuiet(cmd)) {
                    logger.setLogLevel(LogLevel.ERROR);
                }
                if (isVerbose(cmd)) {
                    logger.setLogLevel(LogLevel.INFO);
                }
                if (cmd.hasOption(Options.showStackTrace)) {
                    logger.setShowStackTraces(true);
                }
                try {
                    final PluginManager pluginManager = PluginManager.create();
                    if (!cmd.hasOption(Options.noDefaultPluginDir)) {
                        File pluginDir = PluginUtils.getPluginDirectory(logger,
                                PluginUtils.getPathOfClass(logger,
                                        Command.class));
                        if (pluginDir != null) {
                            PluginUtils.loadPluginsFromDirectory(pluginManager,
                                    logger, pluginDir);
                        }
                    }
                    // TODO: We don't allow stuff like circular deps between
                    // directories here. Is that ok?
                    for (final String dir : cmd
                            .getOptionValues(Options.addPluginDir)) {
                        PluginUtils.loadPluginsFromDirectory(pluginManager,
                                logger, new File(dir));
                    }
                    return run(cmd, logger, pluginManager);
                } catch (FatalException e) {
                    // The error message was already written, so
                    // do nothing
                    return 1;
                } catch (OutOfMemoryError e) {
                    if (cmd.hasOption(Options.showStackTrace)) {
                        throw e;
                    }
                    System.err.println("*ERROR* Not enough Java heap space available to complete the operation.");
                    System.err.println("You can adjust the size of the heap in the wrapper file for calling CodeCover");
                    System.err.println("(codecover.sh or codecover.bat) by changing -Xmx512M to e.g. -Xmx1024M.");
                    return 1;
                }
            }
        }
    }

    /**
     * Gets, whether the quiet option was set
     * 
     * @param commandLine
     * @return true, if the option was set, false if not
     */
    public static boolean isQuiet(CommandLine commandLine) {
        return commandLine.hasOption(Options.quiet);
    }

    /**
     * Gets, whether the verbose option was set
     * 
     * @param commandLine
     * @return true, if the option was set, false if not
     */
    public static boolean isVerbose(CommandLine commandLine) {
        return commandLine.hasOption(Options.verbose);
    }

    /**
     * Gets, whether the pretend option was set
     * 
     * @param commandLine
     * @return true, if the option was set, false if not
     */
    public static boolean isPretend(CommandLine commandLine) {
        return commandLine.hasOption(Options.pretend);
    }

    /**
     * Gets, whether the version option was set
     * 
     * @param commandLine
     * @return true, if the option was set, false if not
     */
    public static boolean isVersion(CommandLine commandLine) {
        return commandLine.hasOption(Options.version);
    }

    /**
     * Gets the minimal amount of arguments for this {@link Command}
     * 
     * @return the minimal amount of arguments
     */
    public int getMinimumArguments() {
        return this.minimumArguments;
    }

    /**
     * Gets the maximal amount of arguments for this {@link Command}
     * 
     * @return the maximal amount of arguments
     */
    public int getMaximumArguments() {
        return this.maximumArguments;
    }

    /**
     * Returns true iff this command should not appear in the command overview.
     * 
     * @return true iff this command should not appear in the command overview.
     */
    public boolean isHidden() {
        return this.hidden;
    }

    /**
     * Gets a list of all aliases of this {@link Command}
     * 
     * @return the list of aliases.
     */
    public List<String> getAllAliases() {
        final List<String> list = new ArrayList<String>();
        list.add(getName());
        if (getShortName() != null) {
            list.add(getShortName());
        }
        return Collections.unmodifiableList(list);
    }

    /**
     * Defines what should happen if this Command was successfully parsed.<br>
     * You may want to use<br>
     * <ul>
     * <li> boolean quiet </li>
     * <li> boolean verbose (quiet AND verbose == false) </li>
     * <li> boolean pretend </li>
     * <li> boolean version </li>
     * </ul>
     * 
     * @param cl
     *                the parsed CommandLine
     * @param logger
     *                the {@link Logger} to use.
     * @param pluginManager
     *                the {@link PluginManager}
     * @return 0, if successful
     */
    protected abstract int run(CommandLine cl, BatchLogger logger,
            org.codecover.model.extensions.PluginManager pluginManager);

}
