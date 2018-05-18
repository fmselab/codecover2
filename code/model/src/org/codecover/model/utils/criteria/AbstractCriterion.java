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

package org.codecover.model.utils.criteria;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: AbstractCriterion.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class AbstractCriterion implements Criterion {
    private final int hashcode;

    private final String name;

    private final String extensionName;

    private final String pluginName;

    /**
     * Constructor.
     * 
     * @param name
     *            the name of the {@link Criterion} .
     * @param extensionName
     *            the extensionName of the {@link Criterion}.
     * @param pluginName
     *            the name of the plugin the {@link Criterion} is contained in.
     */
    public AbstractCriterion(String name, String extensionName,
            String pluginName) {
        if (name == null) {
            throw new NullPointerException("name == null");
        }
        if (extensionName == null) {
            throw new NullPointerException("extensionName == null");
        }
        if (pluginName == null) {
            throw new NullPointerException("pluginName == null");
        }

        this.hashcode = name.hashCode() ^ extensionName.hashCode()
                ^ pluginName.hashCode();
        this.name = name;
        this.extensionName = extensionName;
        this.pluginName = pluginName;
    }

    /**
     * Compares this object with another {@link Criterion}.
     * 
     * @param otherCriterion
     *            the other criterion
     * @return Returns a negative integer, zero, or a positive integer as this
     *         object is less than, equal to, or greater than the specified
     *         object.
     */
    public final int compareTo(Criterion otherCriterion) {
        if (this == otherCriterion) {
            return 0;
        }

        int result = getPluginName().compareTo(otherCriterion.getPluginName());

        if (result == 0) {
            result = getExtensionName().compareTo(
                    otherCriterion.getExtensionName());
        }

        if (result == 0) {
            throw new RuntimeException("Got two criteria with plugin name '"
                    + getPluginName() + "' and extension name '"
                    + getExtensionName() + "'");
        }

        return result;
    }

    public final String getName() {
        return this.name;
    }

    public final String getPluginName() {
        return this.pluginName;
    }

    public final String getExtensionName() {
        return this.extensionName;
    }

    @Override
    public int hashCode() {
        return this.hashcode;
    }

    @Override
    public String toString() {
        return getName();
    }
}
