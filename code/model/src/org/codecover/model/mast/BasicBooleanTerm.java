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
 * A BasicBooleanTerm represents a basic boolean term which is considered
 * atomic.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: BasicBooleanTerm.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class BasicBooleanTerm extends BooleanTerm {

    BasicBooleanTerm(LocationList location) {
        super(location, 1);
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

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.model.mast.BooleanTerm#accept(org.codecover.model.mast.BooleanTerm.Visitor,
     *      org.codecover.model.mast.BooleanTerm.Visitor)
     */
    @Override
    public void accept(Visitor pre, Visitor post) {
        if (pre != null) {
            pre.visit(this);
        }
        if (post != null) {
            post.visit(this);
        }
    }
}
