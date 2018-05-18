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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.codecover.model.utils.CollectionUtil;

/**
 * The result of parsing a command line.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: CommandLine.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public final class CommandLine {
    private final List<String> nonOptionArgs;

    private final Map<Option, List<String>> values;

    private final OptionSet optionSet;

    /**
     * Contructor
     * 
     * @param optionSet
     *            the given {@link OptionSet}
     * @param nonOptionArgs
     *            the given {@link List} of
     * @param values
     *            XXX what is this?
     */
    public CommandLine(OptionSet optionSet, List<String> nonOptionArgs,
            Map<Option, List<String>> values) {
        this.optionSet = optionSet;
        this.nonOptionArgs = CollectionUtil.copy(nonOptionArgs);

        final Map<Option, List<String>> mutableValues = new HashMap<Option, List<String>>();
        for (Map.Entry<Option, List<String>> entry : values.entrySet()) {
            mutableValues.put(entry.getKey(),
                              CollectionUtil.copy(entry.getValue()));
        }
        this.values = Collections.unmodifiableMap(mutableValues);
    }

    /**
     * Gets the non-option args
     * 
     * @return the non-option args
     */
    public List<String> getNonOptionArgs() {
        return this.nonOptionArgs;
    }

    /**
     * Gets the values
     * 
     * @return the values
     */
    public Map<Option, List<String>> getValues() {
        return this.values;
    }

    /**
     * Gets the {@link OptionSet}
     * 
     * @return the {@link OptionSet}
     */
    public OptionSet getOptionSet() {
        return this.optionSet;
    }

    /**
     * Checks, if this {@link CommandLine} contains the given {@link Option}
     * 
     * @param option
     *            the given {@link Option}
     * @return <code>true</code> &rarr; the {@link Option} is contained in
     *         this {@link CommandLine} <br>
     *         <code>false</code> &rarr; the {@link Option} is not contained
     *         in this {@link CommandLine}
     */
    public boolean hasOption(Option option) {
        return getValues().get(option) != null;
    }

    /**
     * Gets the {@link List} of values of the given {@link Option}
     * 
     * @param option
     *            the given {@link Option}
     * @return the {@link List} of values
     */
    public List<String> getOptionValues(Option option) {
        List<String> values = getValues().get(option);
        if (values == null) {
            return Collections.<String> emptyList();
        }
        return values;
    }

    /**
     * Gets the value of the given {@link Option}
     * 
     * @param option
     *            the given {@link Option}
     * @return the value
     */
    public String getOptionValue(Option option) {
        List<String> values = getOptionValues(option);
        if (values.isEmpty()) {
            throw new IllegalArgumentException("Option " + option.getName()
                    + " was not set.");
        } else if (values.size() > 1) {
            throw new IllegalArgumentException("Option " + option.getName()
                    + " appeared " + values.size() + " times.");
        }
        return values.get(0);
    }

    /**
     * Gets the value of the given {@link Option} or {@code null} if the option 
     * was not given.
     * 
     * @param option
     *            the given {@link Option} or {@code null}
     * @return the value
     */
    public String getOptionValueOrNull(Option option) {
        List<String> values = getOptionValues(option);
        if (values.isEmpty()) {
            return null;
        } else if (values.size() > 1) {
            throw new IllegalArgumentException("Option " + option.getName()
                    + " appeared " + values.size() + " times.");
        }
        return values.get(0);
    }
}
