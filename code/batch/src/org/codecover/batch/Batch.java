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

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.codecover.CodeCoverInfo;
import org.codecover.batch.commands.AlterTestCaseCommand;
import org.codecover.batch.commands.AlterTestSessionCommand;
import org.codecover.batch.commands.AnalyzeCommand;
import org.codecover.batch.commands.CopyTestSessionCommand;
import org.codecover.batch.commands.InfoCommand;
import org.codecover.batch.commands.InstrumentCommand;
import org.codecover.batch.commands.InstrumenterInfoCommand;
import org.codecover.batch.commands.MergeTestCasesCommand;
import org.codecover.batch.commands.MergeTestSessionsCommand;
import org.codecover.batch.commands.RemoveTestCasesCommand;
import org.codecover.batch.commands.RemoveTestSessionsCommand;
import org.codecover.batch.commands.ReportCommand;
import org.codecover.batch.commands.TouchCommand;

/**
 * The Batch interface uses opg.apache.commons.cli to parse the command line.
 * All Commands with their options are defined in this file.<br>
 * The commandline arguments are processed by finding the called Command and
 * letting this Command process the rest of the commandline. After the Command
 * has sucessfully parsed it's options, the abstract method run(CommandLine cl)
 * is called which controls the execution of the given Command.<br>
 * This class uses the Singleton Pattern.
 * 
 * @author Michael Starzmann, Tilmann Scheller, Markus Wittlinger
 * @version 1.0 ($Id: Batch.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class Batch {

    // the options standing alone, without a command:
    private final static Set<Option> optionsValidWithoutCommand;

    static {
        final Set<Option> mutableOptionsValidWithoutCommand = new HashSet<Option>();
        mutableOptionsValidWithoutCommand.add(Options.version);
        mutableOptionsValidWithoutCommand.add(Options.help);
        optionsValidWithoutCommand = Collections.unmodifiableSet(mutableOptionsValidWithoutCommand);
    }

    /**
     * Main entry point
     * 
     * @param args
     */
    public static void main(String[] args) {
        final int result = process(Arrays.asList(args));

        System.exit(result);
    }

    /**
     * Trys and finds a Command in the args, which is executed, or, if no
     * command is found, trys and finds options valid without a command, which
     * are executed. If nothing is found or the arguments are wrong in another
     * way, an error message with the hint to the --help-option is written to
     * System.err
     * 
     * @param args
     *            the arguments to parse
     * @return 0 if all went well, 1 if an error occured
     */
    public static int process(List<String> args) {
        final CommandCollection allCommands;

        final Set<Command> commands = new HashSet<Command>();

        commands.add(InstrumenterInfoCommand.getInstance());
        commands.add(InstrumentCommand.getInstance());
        commands.add(AnalyzeCommand.getInstance());
        commands.add(ReportCommand.getInstance());
        commands.add(InfoCommand.getInstance());
        commands.add(MergeTestSessionsCommand.getInstance());
        commands.add(AlterTestSessionCommand.getInstance());
        commands.add(CopyTestSessionCommand.getInstance());
        commands.add(RemoveTestSessionsCommand.getInstance());
        commands.add(MergeTestCasesCommand.getInstance());
        commands.add(AlterTestCaseCommand.getInstance());
        commands.add(RemoveTestCasesCommand.getInstance());
        commands.add(TouchCommand.getInstance());

        class MyCommandCollectionProvider implements
                HelpCommand.CommandCollectionProvider {
            public CommandCollection commands;

            public CommandCollection get() {
                return this.commands;
            }
        }
        MyCommandCollectionProvider commandCollectionProvider = new MyCommandCollectionProvider();
        final Command helpCommand = new HelpCommand(commandCollectionProvider);
        commands.add(helpCommand);
        commands.add(new CreateCompletionFileCommand(commandCollectionProvider));

        allCommands = CommandCollection.create(commands);
        
        final Command versionCommand = new Command("-V", "--version", "", OptionSet.EMPTY) {
                @Override
                protected int run(CommandLine cl, BatchLogger logger, org.codecover.model.extensions.PluginManager pluginManager) {
                    System.out.println("CodeCover version "
                                       + CodeCoverInfo.getVersion());
                    System.out.println("SVN revision: "
                                       + CodeCoverInfo.getRevision());
                    System.out.println("SVN date: " + CodeCoverInfo.getDate());
                    System.out.println("");
                    System.out.println("This program is available under the "
                                       + "terms of the Eclipse Public License (EPL).");
                    return 0;
                }
            };
        
        commandCollectionProvider.commands = allCommands;

        final BatchLogger logger = new BatchLogger();
        
        return allCommands.process(args, logger, helpCommand, versionCommand);
    }

    /**
     * prints the Usage-Information (all Commands and the options valid without
     * a command) to System.out
     * 
     * @param commands
     *            the collection of commands, whose information is printed
     */
    public static void printUsage(CommandCollection commands) {
        System.out.print(HelpFormatter.formatUsage(commands,
                                                   optionsValidWithoutCommand));
    }
}
