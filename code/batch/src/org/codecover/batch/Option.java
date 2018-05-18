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

import java.util.Set;

import org.codecover.model.utils.CollectionUtil;

/**
 * An Option of a {@link Command}.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: Option.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public final class Option {
    private final char shortOption;

    private final String longOption;

    private final String description;

    private final boolean hasArgument;

    private final String argumentName;

    private final boolean canAppearMultipleTimes;

    private final Set<Option> excludedOptions;

    private final ArgumentType argumentType;

    /**
     * Constructor
     * 
     * @param shortOption
     *                the {@link Character} holding the short name of the
     *                {@link Option}
     * @param longOption
     *                the {@link String} holding the long name of the
     *                {@link Option}
     * @param description
     *                the {@link String} holding the description of the
     *                {@link Option}
     * @param hasArgument
     *                indicates, whether the {@link Option} has arguments
     * @param argumentName
     *                the {@link String} holding the name of that argument
     * @param canAppearMultipleTimes
     *                indicates, whether the {@link Option} can appear more than
     *                once
     * @param excludedOptions
     *                the {@link Set} of excluded {@link Option}
     * @param argumentType
     *                the {@link ArgumentType} of the {@link Option}
     */
    public Option(char shortOption, String longOption, String description,
            boolean hasArgument, String argumentName,
            boolean canAppearMultipleTimes, Set<Option> excludedOptions,
            ArgumentType argumentType) {
        if (shortOption == '\0' && longOption == null) {
            throw new IllegalArgumentException(
                    "Neither shortOption nor longOption is given.");
        }

        if (!hasArgument && argumentName != null) {
            throw new IllegalArgumentException(
                    "!hasArgument && argumentName != null");
        }

        if (excludedOptions == null) {
            throw new NullPointerException("excludedOptions == null");
        }

        if (hasArgument && argumentType == null) {
            throw new IllegalArgumentException(
                    "hasArgument && argumentType == null");
        }

        if (!hasArgument && argumentType != null) {
            throw new IllegalArgumentException(
                    "ArgumentType is set but option has no argument");
        }

        this.shortOption = shortOption;
        this.longOption = longOption;
        this.description = description;
        this.hasArgument = hasArgument;
        this.argumentName = argumentName;
        this.canAppearMultipleTimes = canAppearMultipleTimes;
        this.excludedOptions = CollectionUtil.copy(excludedOptions);
        this.argumentType = argumentType;
    }

    /**
     * Gets the {@link Character} of the short option
     * 
     * @return the short option
     */
    public char getShortOption() {
        return this.shortOption;
    }

    /**
     * Gets the {@link String} of the long option
     * 
     * @return the long option
     */
    public String getLongOption() {
        return this.longOption;
    }

    /**
     * Gets the description
     * 
     * @return the description
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * Indicates, whether the {@link Option} has arguments
     * 
     * @return whether the {@link Option} has arguments
     */
    public boolean getHasArgument() {
        return this.hasArgument;
    }

    /**
     * Gets the name of the argument
     * 
     * @return the name of the argument
     */
    public String getArgumentName() {
        return this.argumentName;
    }

    /**
     * Indicates, whether the {@link Option} can appear more than once
     * 
     * @return whether the {@link Option} can appear more than once
     */
    public boolean getCanAppearMultipleTimes() {
        return this.canAppearMultipleTimes;
    }

    /**
     * Gets the name of the {@link Option}
     * 
     * @return the name
     */
    public String getName() {
        return getName(getLongOption() != null);
    }

    /**
     * Gets either the short name or the long name of the {@link Option}
     * 
     * @param useLongOption
     *                indicates what name is wanted
     * @return the name
     */
    public String getName(boolean useLongOption) {
        if (useLongOption && getLongOption() == null) {
            throw new IllegalArgumentException();
        }

        if (!useLongOption && getShortOption() == '\0') {
            throw new IllegalArgumentException();
        }

        if (useLongOption) {
            return "--" + getLongOption();
        } else {
            return "-" + getShortOption();
        }
    }

    /**
     * Gets whether or not this {@link Option} excludes the given {@link Option}
     * 
     * @param option
     *                the given {@link Option}
     * @return true iff this {@link Option} excludes the given one.
     */
    public boolean excludes(Option option) {
        if (option == null) {
            throw new NullPointerException("option == null");
        }

        return this.excludedOptions.contains(option)
                || option.excludedOptions.contains(this);
    }

    /**
     * Gets the {@link ArgumentType} of this {@link Option}
     * 
     * @return the {@link ArgumentType}
     */
    public ArgumentType getArgumentType() {
        if (this.argumentType == null) {
            throw new UnsupportedOperationException("Option has no argument");
        }

        return this.argumentType;
    }
}
