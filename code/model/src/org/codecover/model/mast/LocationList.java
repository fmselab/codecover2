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

import java.util.*;

import org.codecover.model.utils.*;

/**
 * A LocationList is a list of Locations. This is necessary since some AST
 * elements can have mulitple locations (e.g. a partial class in C#).
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: LocationList.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public final class LocationList {
    private final List<Location> locations;

    LocationList(List<Location> locations) {
        if (locations == null) {
            throw new NullPointerException("locations == null");
        }
        
        this.locations = CollectionUtil.copy(locations);
    }

    /**
     * @return the locations
     */
    public List<Location> getLocations() {
        return this.locations;
    }

    /**
     * Returns a String containing the containing a debug representation of all
     * {@link Location}s in this {@link LocationList}
     * 
     * @return a debug representation of each {@link Location} otherwise.
     */
    @Override
    public String toString() {
        if (this.locations.isEmpty()) {
            return "";
        }

        if (this.locations.size() == 1) {
            return this.locations.get(0).toString();
        }

        StringBuilder sb = new StringBuilder();
        Iterator<Location> iterator = this.locations.iterator();

        while (iterator.hasNext()) {
            sb.append(iterator.next());

            if (iterator.hasNext()) {
                sb.append(",\n");
            }
        }

        return sb.toString();
    }
}
