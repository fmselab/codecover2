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
 * This interface describes a criterion for code coverage.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: Criterion.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface Criterion extends Comparable<Criterion> {

    /**
     * Gets he name of the {@link Criterion}
     * 
     * @return The name of the {@link Criterion}.
     */
    public String getName();

    /**
     * Gets the name of the plugin the {@link Criterion} belongs to.
     * 
     * @return The name of the plugin the {@link Criterion} belongs to.
     */
    public String getPluginName();

    /**
     * Gets the extension name of the {@link Criterion}.
     * 
     * @return The extension name of the {@link Criterion}.
     */
    public String getExtensionName();
}
