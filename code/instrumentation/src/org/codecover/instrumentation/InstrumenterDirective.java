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

package org.codecover.instrumentation;


/**
 * Represents a directive for a {@link Instrumenter}.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: InstrumenterDirective.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @see InstrumenterDescriptor#registerDirective(InstrumenterDirective)
 */
public abstract class InstrumenterDirective implements
  Comparable<InstrumenterDirective> {

    private final String key;

    private final String description;

    /**
     * Constructs a new {@link InstrumenterDirective}
     * 
     * @param key
     *            The key of the directive.
     * @param description
     *            A description for verbose mode.
     */
    public InstrumenterDirective(String key, String description) {
        this.key = key;
        this.description = description;
    }
    
    /**
     * Returns the key of the directive.
     * 
     * @return The key Of the directive.
     */
    public String getKey() {
        return this.key;
    }

    /**
     * A description of the directive for a verbose mode.
     * 
     * @return A description.
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * Parses a String to get an object, which can be used for the user of the
     * directive. 
     * 
     * @param value The value to parse.
     * @return The parsed Object.
     * 
     * @throws IllegalArgumentException If the value has a wrong format.
     */
    public abstract Object parseValue(String value)
            throws IllegalArgumentException;

    /**
     * Returns an object, which is used as the default value for this directive.<br>
     * <br>
     * Can be overwritten by child classes.
     * 
     * @return The default Object.
     */
    public Object getDefaultValue() {
        return null;
    }

    public int compareTo(InstrumenterDirective other) {
        return getKey().compareTo(other.getKey());
    }
}
