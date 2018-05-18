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

import java.util.*;

import org.codecover.model.utils.*;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: AbstractMetaDataProvider.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
abstract class AbstractMetaDataProvider implements MetaDataProvider {
    private final Map<String, Object> metaData = new TreeMap<String, Object>();
    
    /**
     * Ensures that the object isn't deleted yet.
     *
     * This method is called by all other methods.
     */
    /*
     * We still allow setting metadata while deleting.
     */
    protected abstract void assertNotDeleted();
    
    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.model.MetaDataProvider#setMetaData(java.lang.String,
     *      java.lang.Object)
     */
    public void setMetaData(String name, Object value) {
        if (name == null) {
            throw new NullPointerException("name == null");
        }
        
        //assertNotDeleted();
        
        synchronized (this.metaData) {
            this.metaData.put(name, value);
        }
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.model.ReadOnlyMetaDataProvider#getMetaData(java.lang.String)
     */
    public Object getMetaData(String name) {
        if (name == null) {
            throw new NullPointerException("name == null");
        }

        //assertNotDeleted();

        synchronized (this.metaData) {
            return this.metaData.get(name);
        }
    }

    Set<Map.Entry<String, Object>> getMetaDataMapEntries() {
        //assertNotDeleted();

        synchronized (this.metaData) {
            // We have to return a copy due to threading issues
            return CollectionUtil.copy(this.metaData.entrySet());
        }
    }
}
