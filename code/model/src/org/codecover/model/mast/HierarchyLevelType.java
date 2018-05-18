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
 * A HierarchyLevelType represents the type of a HierarchyLevel. It contains an
 * English name which can be used in texts shown to the user (this name might be
 * e.g. \package" oder \class") and an internal name which can be used e.g. for
 * choosing a icon for the HierarchyLevel.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: HierarchyLevelType.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class HierarchyLevelType {
    private final String englishName;

    private final String internalName;

    HierarchyLevelType(String englishName, String internalName) {
        if (englishName == null) {
            throw new NullPointerException("englishName == null");
        }

        if (internalName == null) {
            throw new NullPointerException("internalName == null");
        }

        this.englishName = englishName;
        this.internalName = internalName;
    }

    /**
     * @return the englishName
     */
    public String getEnglishName() {
        return this.englishName;
    }

    /**
     * @return the internalName
     */
    public String getInternalName() {
        return this.internalName;
    }

    /**
     * Gets the internal name of the {@link HierarchyLevelType}
     * 
     * @return the internal name.
     */
    @Override
    public String toString() {
        return getInternalName();
    }

    /**
     * Returns a hash code value for the object. This method is supported for
     * the benefit of hashtables such as those provided by
     * <code>java.util.Hashtable</code>.
     * 
     * @return a hash code value for this object.
     */
    @Override
    public int hashCode() {
        return getEnglishName().hashCode() + getInternalName().hashCode();
    }

    /**
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof HierarchyLevelType)) {
            return false;
        }

        HierarchyLevelType that = (HierarchyLevelType) obj;

        if (getEnglishName().equals(that.getEnglishName())
                && getInternalName().equals(that.getInternalName())) {
            return true;
        }

        return false;
    }
}
