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

import java.util.Set;

import org.codecover.model.mast.Statement.Visitor;


/**
  * 
 * @author RS
 * @version 1.08
 */
public final class SynchronizedStatement extends ComplexStatement {
		
	private final CoverableItem coverableItem0;
	private final CoverableItem coverableItem1;
	private final CoverableItem coverableItem2;

	public SynchronizedStatement(LocationList location, CoverableItem coverableItem, Location keyword, CoverableItem coverableItem0, CoverableItem coverableItem1, CoverableItem coverableItem2, Set<QuestionMarkOperator> questionMarkOperators) {
		super(location, keyword, coverableItem, null, questionMarkOperators);

		if (coverableItem0 == null || coverableItem1 == null || coverableItem2 == null) {
			throw new NullPointerException("coverableItem == null");
		}

		this.coverableItem0 = coverableItem0;
		this.coverableItem1 = coverableItem1;
		this.coverableItem2 = coverableItem2;
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
    
    public CoverableItem getCoverableItem(int id) {
    	if(id == 0) return coverableItem0;
    	if(id == 1) return coverableItem1;
    	return coverableItem2;
    }
    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.model.mast.Statement#accept(org.codecover.model.mast.Statement.Visitor,
     *      org.codecover.model.mast.Statement.Visitor,
     *      org.codecover.model.mast.RootTerm.Visitor,
     *      org.codecover.model.mast.RootTerm.Visitor,
     *      org.codecover.model.mast.BooleanTerm.Visitor,
     *      org.codecover.model.mast.BooleanTerm.Visitor)
     */
    @Override
    public void accept(Visitor pre, Visitor post, RootTerm.Visitor rootTermPre,
            RootTerm.Visitor rootTermPost, BooleanTerm.Visitor termPre,
            BooleanTerm.Visitor termPost, QuestionMarkOperator.Visitor qmoVisitor) {
        if (pre != null) {
            pre.visit(this);
        }
        super.accept(pre, post, rootTermPre, rootTermPost, termPre, termPost, null);
    }

}

