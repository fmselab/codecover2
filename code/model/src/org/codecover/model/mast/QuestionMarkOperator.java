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
public final class QuestionMarkOperator extends AbstractLocatableMetaDataObject {
		
	private QuestionMarkOperatorExpression expression1;
	private QuestionMarkOperatorExpression expression2;
	
    private final CoverableItem coverableItem;

    public QuestionMarkOperator(LocationList location, CoverableItem coverableItem, QuestionMarkOperatorExpression expr1, QuestionMarkOperatorExpression expr2) {
        super(location);

        if (coverableItem == null) {
            throw new NullPointerException("coverableItem == null");
        }
        
        this.coverableItem = coverableItem;
        
        this.expression1 = expr1;
        this.expression2 = expr2;
    }

    /**
     * @return the coverableItem
     */
    public CoverableItem getCoverableItem() {
        return this.coverableItem;
    }
    
    public QuestionMarkOperatorExpression getQuestionMarkOperatorExpression1() {
    	return expression1;
    }

    public QuestionMarkOperatorExpression getQuestionMarkOperatorExpression2() {
    	return expression2;
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
    public void accept(Visitor qmoVisitor) {
    	qmoVisitor.visit(this);
    }

    /**
     * The {@link org.codecover.model.mast.RootTerm.Visitor Visitor} to be used
     * for all {@link QuestionMarkOperator}s
     * 
     * @author ES
     * @version 1.0 ($Id: RootTerm.java 53 2009-06-05 14:09:26Z t-scheller $)
     */
    public static interface Visitor {
        /**
         * Visits a {@link RootTerm}
         * 
         * @param term
         *            the given {@link RootTerm}
         */
        void visit(QuestionMarkOperator qmo);
    }

    /**
     * A default, empty implementation of the
     * {@link org.codecover.model.mast.QuestionMarkOperator.Visitor Visitor}
     * 
     * @author Steffen Kieß, Markus Wittlinger
     * @version 1.0 ($Id: RootTerm.java 53 2009-06-05 14:09:26Z t-scheller $)
     */
    public static class DefaultVisitor implements Visitor {
        /**
         * (non-Javadoc)
         * 
         * @see org.codecover.model.mast.QuestionMarkOperator.Visitor#visit(org.codecover.model.mast.QuestionMarkOperator)
         */
        public void visit(QuestionMarkOperator qmo) {
            // Do nothing.
        }
    }
}

