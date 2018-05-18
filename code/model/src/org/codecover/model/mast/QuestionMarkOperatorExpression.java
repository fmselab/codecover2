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

import org.codecover.model.mast.BooleanTerm.Visitor;

/**
  * 
 * @author RS
 * @version 1.08
 */
public final class QuestionMarkOperatorExpression extends AbstractLocatableMetaDataObject {
		
    private final CoverableItem coverableItem;

    public QuestionMarkOperatorExpression(LocationList location, CoverableItem coverableItem) {
        super(location);

        if (coverableItem == null) {
            throw new NullPointerException("coverableItem == null");
        }
        
        this.coverableItem = coverableItem;
    }

    /**
     * @return the coverableItem
     */
    public CoverableItem getCoverableItem() {
        return this.coverableItem;
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
        int result = 0;
        for (Location location : getLocation().getLocations()) {
            result += location.getStartOffset();
            result += location.getEndOffset();
            result += location.getFile().hashCode();
            result += location.getFile().getContent().hashCode();
        }

        return result;
    }
    
}

