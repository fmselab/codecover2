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

package org.codecover.model;

/**
 * This interface is implemented by classes which allow to store additional meta
 * data (TestSession and TestCase). This meta data might be e.g. the Eclipse
 * project associated with a test session.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: MetaDataProvider.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public interface MetaDataProvider extends ReadOnlyMetaDataProvider {
    /**
     * Adds a given object as metadata to the provider, with the given name as
     * the key to retrieve it.
     * 
     * @param name
     *            the key used in storing the object
     * @param value
     *            the object, that is stored as metadata.
     */
    public void setMetaData(String name, Object value);
}
