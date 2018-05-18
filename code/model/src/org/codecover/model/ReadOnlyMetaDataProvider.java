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
 * This interface is implemented by classes which allow a read only access to
 * the meta data stored into a MetaDataProvider.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: ReadOnlyMetaDataProvider.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface ReadOnlyMetaDataProvider {
    /**
     * Retrieves the object, that was added as metadata to the provider under
     * the given key, or returns null, if no such object was found
     * 
     * @param name
     *            the key used to retrieve the object
     * @return the retrieved object or <code>null</code>, if no object was
     *         stored under the given key.
     */
    public Object getMetaData(String name);
}
