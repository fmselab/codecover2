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
package org.codecover.instrumentation.xampil.parser;

import java.util.Vector;

/**
 * @author Christoph Müller, Stefan Franke
 */
public class InstrumentableItemCounter {
    private int statementCount = 0;

    private int branchCount = 0;

    private int loopCount = 0;

    private Vector<Integer> conditions = new Vector<Integer>();

    /**
     * Returns the statement count for the entire source code file.
     * 
     * @return statement count
     */
    public int getStatementCount() {
        return this.statementCount;
    }

    /**
     * Increments the statement count by one.
     */
    public void incrementStatementCount() {
        this.statementCount++;
    }

    /**
     * Returns the branch count for the entire source code file.
     * 
     * @return branch count
     */
    public int getBranchCount() {
        return this.branchCount;
    }

    /**
     * Increments the branch count by one.
     */
    public void incrementBranchCount() {
        this.branchCount++;
    }

    /**
     * Returns the loop count for the entire source code file.
     * 
     * @return loop count
     */
    public int getLoopCount() {
        return this.loopCount;
    }

    /**
     * Increments the loop count by one.
     */
    public void incrementLoopCount() {
        this.loopCount++;
    }

    /**
     * Returns the condition count for the entire source code file.
     * 
     * @return condition count
     */
    public int getConditionCount() {
        return this.conditions.size();
    }

    /**
     * Returns the number of BasicBoolean terms of a selected condition.
     * 
     * @param conditionIndex The index of the condition;
     * 
     * @return Its number of BasicBoolean terms.
     * 
     * @see #getConditionCount() to get to know the range.
     */
    public int getBasicBooleanCount(int conditionIndex) {
        return this.conditions.get(conditionIndex).intValue();
    }

    /**
     * Increments the condition count by one and sets its BasicBoolean count to
     * the given value. 
     * 
     * @param basicBooleanCount The number of basic booleans of this condition. 
     */
    public void incrementConditionCount(int basicBooleanCount) {
        this.conditions.add(Integer.valueOf(basicBooleanCount));
    }
}