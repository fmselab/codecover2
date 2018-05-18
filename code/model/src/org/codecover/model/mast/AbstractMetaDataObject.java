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
 * @author Steffen Kieß
 * @version 1.0 ($Id: AbstractMetaDataObject.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
abstract class AbstractMetaDataObject implements MetaDataObject {
    private final MetaData metaData = new MetaData();

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.model.mast.MetaDataObject#getMetaData()
     */
    public MetaData getMetaData() {
        return this.metaData;
    }

    /**
     * Returns a hash code value for the object. This method is supported for
     * the benefit of hashtables such as those provided by
     * <code>java.util.Hashtable</code>.
     * 
     * @return a hash code value for this object.
     */
    @Override
    public abstract int hashCode();
}
