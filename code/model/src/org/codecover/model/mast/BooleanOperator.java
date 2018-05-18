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
 * A BooleanOperator is a function with a given arity arity which maps a
 * arity-tuple of boolean values to a boolean value. The object contains the
 * arity, a map which maps the assignments to the result and a name.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: BooleanOperator.java 71 2010-04-14 18:28:46Z schmidberger $)
 */
public class BooleanOperator {
    private final int arity;

    private final Map<BooleanAssignment, Boolean> possibleAssignments;

    private final String name;

    BooleanOperator(int arity,
            Map<BooleanAssignment, Boolean> possibleAssignments, String name) {
        if (possibleAssignments == null) {
            throw new NullPointerException("possibleAssignments == null");
        }
        
        if (name == null) {
            throw new NullPointerException("name == null");
        }

        this.arity = arity;
        this.possibleAssignments = CollectionUtil.copy(possibleAssignments);
        this.name = name;
    }

    /**
     * @return the arity
     */
    public int getArity() {
        return this.arity;
    }

    /**
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    /**
     * returns the decision table for the operator
     * 
     * Example: {[TRUE, FALSE]=false, [FALSE, NOT_EVALUATED]=false, [TRUE, TRUE]=true}
     * 
     * @return the possibleAssignments
     */
    public Map<BooleanAssignment, Boolean> getPossibleAssignments() {
        return this.possibleAssignments;
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
        int result = getPossibleAssignments().size();

        for (BooleanAssignment booleanAssignment : getPossibleAssignments()
                .keySet()) {
            result *= 3;
            result += booleanAssignment.hashCode();
        }

        result += this.name.hashCode();

        return result;
    }

    /**
     * Compares the given object with this instance.
     * 
     * @param obj
     *            the given object
     * @return <code>true</code> &rarr; if the given object is a
     *         BooleanOperator and it and this instance contain the same data.<br>
     *         <code>false</code> &rarr; otherwise
     */
    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof BooleanOperator)) {
            return false;
        }

        BooleanOperator that = (BooleanOperator) obj;

        if (this.getArity() == that.getArity()
                && this.getName().equals(that.getName())
                && this.getPossibleAssignments().equals(
                        that.getPossibleAssignments())) {
            return true;
        }

        return false;
    }
}
