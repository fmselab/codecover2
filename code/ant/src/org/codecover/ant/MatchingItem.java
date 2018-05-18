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

package org.codecover.ant;

import java.util.regex.Pattern;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.DataType;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: MatchingItem.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class MatchingItem extends DataType {
    String name;

    String pattern;

    boolean prepared = false;

    String constantName;

    Pattern compiledPattern;

    /**
     * Sets the name.
     * 
     * @param name
     *                the name to set
     */
    public void setName(String name) {
        if (this.pattern != null) {
            throw new BuildException("Only name or pattern must be set");
        }
        this.name = name;
    }

    /**
     * Sets the pattern.
     * 
     * @param pattern
     *                the pattern to set
     */
    public void setPattern(String pattern) {
        if (this.name != null) {
            throw new BuildException("Only name or pattern must be set");
        }
        this.pattern = pattern;
    }

    /**
     * Prepares the {@link MatchingItem}
     */
    public void prepare() {
        if (this.prepared) {
            return;
        }

        if (this.name == null && this.pattern == null) {
            throw new BuildException("One of name or pattern must be set");
        }

        if (this.name != null) {
            this.constantName = this.name;
        } else {
            this.compiledPattern = Pattern
                    .compile(this.pattern, Pattern.DOTALL);
        }

        this.prepared = true;
    }

    /**
     * Evaluates the given {@link String}
     * 
     * @param s
     *                the {@link String} to evaluate.
     * @return the result of the evaluation.
     */
    public boolean evaluate(String s) {
        if (!this.prepared) {
            throw new RuntimeException();
        }

        if (this.constantName != null) {
            return s.equals(this.constantName);
        } else {
            return this.compiledPattern.matcher(s).matches();
        }
    }
}
