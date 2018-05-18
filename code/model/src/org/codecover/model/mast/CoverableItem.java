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
 * A CoverableItem represents a coverable item which can be covered in a test
 * case. Instances of this class can be passed to the method
 * getCoverageCount(...) of the class TestCase.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: CoverableItem.java 22 2008-05-25 20:08:53Z ahija $)
 */
public class CoverableItem implements Comparable<CoverableItem> {
    private final String prefix;

    private final String id;

    CoverableItem(String prefix, String id) {

        if (id == null) {
            throw new NullPointerException("id == null");
        }

        if (prefix == null) {
            throw new NullPointerException("prefix == null");
        }

        this.prefix = prefix;
        this.id = id;
    }

    /**
     * @return the id
     */
    public String getId() {
        return this.id;
    }

    /**
     * @return the prefix
     */
    public String getPrefix() {
        return this.prefix;
    }

    /**
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return getId().hashCode() + getPrefix().hashCode();
    }

    /**
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object o) {
        if (!(o instanceof CoverableItem)) {
            return false;
        }

        CoverableItem item = (CoverableItem) o;

        return getPrefix().equals(item.getPrefix())
                && getId().equals(item.getId());
    }

    /**
     * Compares two {@link CoverableItem}s.
     * 
     * @param coverableItem
     *            the {@link CoverableItem} this instance is compared to
     * @return the comparison result
     */
    public int compareTo(CoverableItem coverableItem) {
        if (coverableItem == null) {
            throw new NullPointerException("o == null");
        }

        final int result = getPrefix().compareTo(coverableItem.getPrefix());

        if (result == 0) {
            return getId().compareTo(coverableItem.getId());
        } else {
            return result;
        }
    }

    /**
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "PREFIX: " + getPrefix() + ", ID: " + getId();
    }
}
