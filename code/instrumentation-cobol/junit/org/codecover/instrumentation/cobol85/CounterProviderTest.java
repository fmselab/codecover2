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

package org.codecover.instrumentation.cobol85;

import junit.framework.TestCase;

/**
 * @author Stefan Franke
 * @version 1.0 ($Id: CounterProviderTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CounterProviderTest extends TestCase {
    
    CounterProvider counterProvider;
    
    @Override
    protected void setUp() {
        this.counterProvider = new CounterProvider();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementConditionCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementConditionCounter();
        this.counterProvider.incrementLoopCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementConditionCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementProgramUnitCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementConditionCounter();
        this.counterProvider.incrementLoopCounter();
        this.counterProvider.incrementConditionCounter();
        this.counterProvider.incrementLoopCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementTestCaseCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementConditionCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementBranchCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementStatementCounter();
        this.counterProvider.incrementProgramUnitCounter();
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#getBranchCounter()}.
     */
    public void testGetBranchCounter() {
        assertEquals(17, this.counterProvider.getBranchCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#getBranchCounterID()}.
     */
    public void testGetBranchCounterID() {
        assertEquals("B17", this.counterProvider.getBranchCounterID());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#getConditionCounter()}.
     */
    public void testGetConditionCounter() {
        assertEquals(6, this.counterProvider.getConditionCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#getConditionCounterID()}.
     */
    public void testGetConditionCounterID() {
        assertEquals("C6", this.counterProvider.getConditionCounterID());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#getLoopCounter()}.
     */
    public void testGetLoopCounter() {
        assertEquals(3, this.counterProvider.getLoopCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#getLoopCounterID()}.
     */
    public void testGetLoopCounterID() {
        assertEquals("L3-", this.counterProvider.getLoopCounterID());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#getStatementCounter()}.
     */
    public void testGetStatementCounter() {
        assertEquals(37, this.counterProvider.getStatementCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#getStatementCounterID()}.
     */
    public void testGetStatementCounterID() {
        assertEquals("S37", this.counterProvider.getStatementCounterID());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#getTestCaseCounter()}.
     */
    public void testGetTestCaseCounter() {
        assertEquals(1, this.counterProvider.getTestCaseCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#getTestCaseCounterID()}.
     */
    public void testGetTestCaseCounterID() {
        assertEquals("test case 1", this.counterProvider.getTestCaseCounterID());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#incrementBranchCounter()}.
     */
    public void testIncrementBranchCounter() {
        int value = this.counterProvider.getBranchCounter();
        this.counterProvider.incrementBranchCounter();
        assertEquals(value + 1, this.counterProvider.getBranchCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#incrementConditionCounter()}.
     */
    public void testIncrementConditionCounter() {
        int value = this.counterProvider.getConditionCounter();
        this.counterProvider.incrementConditionCounter();
        assertEquals(value + 1, this.counterProvider.getConditionCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#incrementLoopCounter()}.
     */
    public void testIncrementLoopCounter() {
        int value = this.counterProvider.getLoopCounter();
        this.counterProvider.incrementLoopCounter();
        assertEquals(value + 1, this.counterProvider.getLoopCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#incrementStatementCounter()}.
     */
    public void testIncrementStatementCounter() {
        int value = this.counterProvider.getStatementCounter();
        this.counterProvider.incrementStatementCounter();
        assertEquals(value + 1, this.counterProvider.getStatementCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#incrementTestCaseCounter()}.
     */
    public void testIncrementTestCaseCounter() {
        int value = this.counterProvider.getTestCaseCounter();
        this.counterProvider.incrementTestCaseCounter();
        assertEquals(value + 1, this.counterProvider.getTestCaseCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.CounterProvider#incrementProgramUnitCounter()}.
     */
    public void testIncrementProgramUnitCounter() {
        int value = this.counterProvider.getProgramUnitCounter();
        this.counterProvider.incrementProgramUnitCounter();
        assertEquals(value + 1, this.counterProvider.getProgramUnitCounter());
    }

}
