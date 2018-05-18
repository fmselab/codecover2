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

import java.util.HashSet;
import java.util.Set;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: SimpleCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public abstract class SimpleCommand extends Command {
    /**
     * Creates a new SimpleCommand
     * 
     * @param shortName
     *            the short name of the SimpleCommand
     * @param name
     *            the long name of the SimpleCommand
     * @param description
     *            the description of the SimpleCommand for the help page
     * @param options
     *            the options of the SimpleCommand
     * @throws IllegalArgumentException
     *             if there is an non-valid character in opt
     */
    public SimpleCommand(String shortName, String name, String description,
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
    @SuppressWarnings("static-access")
    public SimpleCommand(String shortName, String name, String description,
                         OptionSet options, int minimumArguments,
                         int maximumArguments, boolean hidden) {
        super(shortName, name, description, addOptions(options), minimumArguments, maximumArguments, hidden);
    }
    
    private static OptionSet addOptions(OptionSet options) {
        final Set<Option> optionalOptions = new HashSet<Option>(options.getOptional());
        optionalOptions.add(Options.verbose);
        optionalOptions.add(Options.quiet);
        optionalOptions.add(Options.pretend);
        optionalOptions.add(Options.progressBar);
        optionalOptions.add(Options.noProgressBar);
        optionalOptions.add(Options.addPluginDir);
        optionalOptions.add(Options.noDefaultPluginDir);
        optionalOptions.add(Options.showStackTrace);
        return new OptionSet(options.getRequired(), optionalOptions);
    }
}
