/******************************************************************************
 * Copyright (c) 2008 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/
package org.codecover.ant;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.DataType;

/**
 * An Extension for {@link InstrumentCommand} to represent Directives.
 *
 * @author Christoph Müller
 *
 * @version 1.0 ($Id$)
 */
public class Directive extends DataType {

    private String key;

    private String value;

    public String getKey() {
        if (this.key == null) {
            throw new BuildException("The attribute 'key' is missing.");
        }
        return this.key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getValue() {
        if (this.value == null) {
            throw new BuildException("The attribute 'value' is missing.");
        }
        return this.value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return this.key + "=" + this.value;
    }

    @Override
    public boolean equals(Object other) {
        if ((this == other)) {
            return true;
        }
        if ((other == null)) {
            return false;
        }
        if (!(other instanceof Directive)) {
            return false;
        }
        Directive castOther = (Directive) other;

        return getKey().equals(castOther.getKey());
    }

    @Override
    public int hashCode() {
        return getKey().hashCode();
    }
}
