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
 * A OperatorTerm represents a boolean term which consists of an operator
 * connect zero, one or more BooleanTerms. It contains a reference to the
 * BooleanOperator used and the list of the operands (which are BooleanTerms).
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: OperatorTerm.java 71 2010-04-14 18:28:46Z schmidberger $)
 */
public class OperatorTerm extends BooleanTerm {
    private final BooleanOperator operator;

    private final List<BooleanTerm> operands;

    OperatorTerm(LocationList location, BooleanOperator operator,
            List<BooleanTerm> operands) {
        super(location, addBasicBooleanTerms(operands));

        if (operator == null) {
            throw new NullPointerException("operator == null");
        }
        
        if (operands == null) {
            throw new NullPointerException("operands == null");
        }
        
        this.operator = operator;
        this.operands = CollectionUtil.copy(operands);
    }

    private static int addBasicBooleanTerms(List<BooleanTerm> terms) {
        int result = 0;
        for (BooleanTerm term : terms) {
            result += term.getBasicBooleanTerms();
        }
        return result;
    }
    
    public final String getShortNameOfOperator() {
    	
    	String operator = this.getLocation().getLocations().get(0).getContent();
    	if("&&".equals(operator)) {
    		return "AND";
    	}
    	if("||".equals(operator)) {
    		return "OR";
    	}
        return operator;
    }
    

    /**
     * @return the operands
     */
    public List<BooleanTerm> getOperands() {
        return this.operands;
    }

    /**
     * @return the operator
     */
    public BooleanOperator getOperator() {
        return this.operator;
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
        if (pre != null || post != null) {
            for (BooleanTerm term : getOperands()) {
                term.accept(pre, post);
            }
        }
        if (post != null) {
            post.visit(this);
        }
    }
}
