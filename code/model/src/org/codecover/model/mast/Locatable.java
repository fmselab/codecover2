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

package org.codecover.model.mast;

/**
 * A Locatable is an object with one or multiple locations. The method
 * getLocation() can be used to get this locations.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: Locatable.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface Locatable {
    /**
     * Gets the list of {@link Location}s of this {@link Locatable} object.
     * 
     * @return the list containing the {@link Location}s
     */
    public LocationList getLocation();
}
