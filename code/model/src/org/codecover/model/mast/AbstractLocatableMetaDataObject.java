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
 * @version 1.0 ($Id: AbstractLocatableMetaDataObject.java 73 2010-07-30 19:42:18Z schmidberger $)
 */
public abstract class AbstractLocatableMetaDataObject extends AbstractMetaDataObject
        implements Locatable {
    private final LocationList location;

    protected AbstractLocatableMetaDataObject(LocationList location) {
        if (location == null) {
            throw new NullPointerException("location == null");
        }

        this.location = location;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.model.mast.Locatable#getLocation()
     */
    public LocationList getLocation() {
        return this.location;
    }

    /**
     * Returns a debug representation of the object.
     *
     * @see LocationList#toString()
     */
    @Override
    public String toString() {
        return getClass().toString() + " -- " + getLocation().toString();
    }
    
    /**
     * RS, 26.07.10, in most child classes overwritten
     * @return
     */
    public CoverableItem getCoverableItem() {
    	return null;
    }
    	 
}
