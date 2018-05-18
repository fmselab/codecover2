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

import java.util.*;

/**
 * A class for building {@link Option}s.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: OptionBuilder.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public final class OptionBuilder {
    private char shortOption = '\0';

    private String longOption = null;

    private String description = null;

    private boolean hasArgument = false;

    private String argumentName = null;

    private boolean canAppearMultipleTimes = false;

    private final Set<Option> excludedOptions = new HashSet<Option>();

    private ArgumentType argumentType = null;

    /**
     * Sets the long option of this {@link OptionBuilder}
     * 
     * @param longOption
     *                the value to set
     * @return this instance of {@link OptionBuilder}
     */
    public OptionBuilder withLongOpt(String longOption) {
        this.longOption = longOption;
        return this;
    }

    /**
     * Sets the short option of this {@link OptionBuilder}
     * 
     * @param shortOption
     *                the value to set
     * @return this instance of {@link OptionBuilder}
     */
    public OptionBuilder withShortOpt(char shortOption) {
        this.shortOption = shortOption;
        return this;
    }

    /**
     * Sets the has arguments value of this {@link OptionBuilder}
     * 
     * @return this instance of {@link OptionBuilder}
     */
    public OptionBuilder hasArg() {
        this.hasArgument = true;
        return this;
    }

    /**
     * Sets the name of the argument of this {@link OptionBuilder}
     * 
     * @param argumentName
     *                the value to set
     * @return this instance of {@link OptionBuilder}
     */
    public OptionBuilder withArgName(String argumentName) {
        this.argumentName = argumentName;
        return this;
    }

    /**
     * Sets the description of this {@link OptionBuilder}
     * 
     * @param description
     *                the value to set
     * @return this instance of {@link OptionBuilder}
     */
    public OptionBuilder withDescription(String description) {
        this.description = description;
        return this;
    }

    /**
     * Sets the canAppearMultipleTimes value of this {@link OptionBuilder}
     * 
     * @return this instance of {@link OptionBuilder}
     */
    public OptionBuilder multipleTimes() {
        this.canAppearMultipleTimes = true;
        return this;
    }

    /**
     * Adds the Option to the excludedOptions of this {@link OptionBuilder}
     * 
     * @param option
     *                the {@link Option} to add.
     * 
     * @return this instance of {@link OptionBuilder}
     */
    public OptionBuilder exclude(Option option) {
        if (option == null) {
            throw new NullPointerException("option == null");
        }

        this.excludedOptions.add(option);
        return this;
    }

    /**
     * Sets the {@link ArgumentType} of this {@link OptionBuilder}
     * 
     * @param argumentType
     *                the {@link ArgumentType} to set.
     * 
     * @return this instance of {@link OptionBuilder}
     */
    public OptionBuilder withArgumentType(ArgumentType argumentType) {
        if (argumentType == null) {
            throw new NullPointerException("argumentType == null");
        }

        this.argumentType = argumentType;
        return this;
    }

    /**
     * Creates the {@link Option} from the set data
     * 
     * @return the created instance of {@link Option}
     */
    public Option create() {
        final ArgumentType myArgumentType;

        if (this.hasArgument) {
            if (this.argumentType == null) {
                myArgumentType = new ArgumentType.Opaque();
            } else {
                myArgumentType = this.argumentType;
            }
        } else {
            if (this.argumentType == null) {
                myArgumentType = null;
            } else {
                throw new IllegalArgumentException(
                        "ArgumentType is set but option has no argument");
            }
        }

        return new Option(this.shortOption, this.longOption, this.description,
                this.hasArgument, this.argumentName,
                this.canAppearMultipleTimes, this.excludedOptions,
                myArgumentType);
    }
}
