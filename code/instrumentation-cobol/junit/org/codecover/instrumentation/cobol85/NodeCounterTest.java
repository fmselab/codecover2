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
 * @version 1.0 ($Id: NodeCounterTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class NodeCounterTest extends TestCase {
    
    NodeCounter nodeCounter;

    /**
     *
     * @throws java.lang.Exception
     */
    protected void setUp() {
        this.nodeCounter = NodeCounter.getInstance();
        this.nodeCounter.reset();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementBranchCounter();
        this.nodeCounter.incrementBranchCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementConditionCounter();
        this.nodeCounter.incrementBasicBooleanCounter();
        this.nodeCounter.incrementBasicBooleanCounter();
        this.nodeCounter.incrementBasicBooleanCounter();
        this.nodeCounter.incrementBranchCounter(2);
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementBranchCounter();
        this.nodeCounter.incrementBranchCounter();
        this.nodeCounter.incrementBranchCounter();
        this.nodeCounter.incrementBranchCounter();
        this.nodeCounter.incrementBranchCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementConditionCounter();
        this.nodeCounter.incrementBasicBooleanCounter();
        this.nodeCounter.incrementLoopCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementConditionCounter();
        this.nodeCounter.incrementBasicBooleanCounter();
        this.nodeCounter.incrementBasicBooleanCounter();
        this.nodeCounter.incrementBasicBooleanCounter();
        this.nodeCounter.incrementBranchCounter(2);
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementBranchCounter();
        this.nodeCounter.incrementBranchCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.newProgramUnit();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementConditionCounter();
        this.nodeCounter.incrementBasicBooleanCounter();
        this.nodeCounter.incrementLoopCounter();
        this.nodeCounter.incrementConditionCounter();
        this.nodeCounter.incrementBasicBooleanCounter();
        this.nodeCounter.incrementBasicBooleanCounter();
        this.nodeCounter.incrementLoopCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.setStartTestCase();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementBranchCounter();
        this.nodeCounter.incrementBranchCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementConditionCounter();
        this.nodeCounter.incrementBasicBooleanCounter();
        this.nodeCounter.incrementBasicBooleanCounter();
        this.nodeCounter.incrementBranchCounter(2);
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.newProgramUnit();
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#reset()}.
     */
    public void testReset() {
        this.nodeCounter.reset();
        assertEquals(0, this.nodeCounter.getStatementCounter());
        assertEquals(0, this.nodeCounter.getBranchCounter());
        assertEquals(0, this.nodeCounter.getConditionCounter());
        assertEquals(0, this.nodeCounter.getLoopCounter());
        assertEquals(true, this.nodeCounter.getStartTestCase(0));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#getInstance()}.
     */
    public void testGetInstance() {
        assertNotNull(NodeCounter.getInstance());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#getBasicBooleanCounter(int)}.
     */
    public void testGetBasicBooleanCounter() {
        assertEquals(0, this.nodeCounter.getBasicBooleanCounter(0));
        assertEquals(3, this.nodeCounter.getBasicBooleanCounter(1));
        assertEquals(1, this.nodeCounter.getBasicBooleanCounter(2));
        assertEquals(3, this.nodeCounter.getBasicBooleanCounter(3));
        assertEquals(1, this.nodeCounter.getBasicBooleanCounter(4));
        assertEquals(2, this.nodeCounter.getBasicBooleanCounter(5));
        assertEquals(2, this.nodeCounter.getBasicBooleanCounter(6));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#getBranchCounter()}.
     */
    public void testGetBranchCounter() {
        assertEquals(17, this.nodeCounter.getBranchCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#getBranchCounterBegin(int)}.
     */
    public void testGetBranchCounterBegin() {
        assertEquals(0, this.nodeCounter.getBranchCounterBegin(1));
        assertEquals(13, this.nodeCounter.getBranchCounterBegin(2));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#getBranchCounterEnd(int)}.
     */
    public void testGetBranchCounterEnd() {
        assertEquals(13, this.nodeCounter.getBranchCounterEnd(1));
        assertEquals(17, this.nodeCounter.getBranchCounterEnd(2));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#getConditionCounter()}.
     */
    public void testGetConditionCounter() {
        assertEquals(6, this.nodeCounter.getConditionCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#getConditionCounterBegin(int)}.
     */
    public void testGetConditionCounterBegin() {
        assertEquals(0, this.nodeCounter.getConditionCounterBegin(1));
        assertEquals(3, this.nodeCounter.getConditionCounterBegin(2));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#getConditionCounterEnd(int)}.
     */
    public void testGetConditionCounterEnd() {
        assertEquals(3, this.nodeCounter.getConditionCounterEnd(1));
        assertEquals(6, this.nodeCounter.getConditionCounterEnd(2));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#getLoopCounter()}.
     */
    public void testGetLoopCounter() {
        assertEquals(3, this.nodeCounter.getLoopCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#getLoopCounterBegin(int)}.
     */
    public void testGetLoopCounterBegin() {
        assertEquals(0, this.nodeCounter.getLoopCounterBegin(1));
        assertEquals(1, this.nodeCounter.getLoopCounterBegin(2));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#getLoopCounterEnd(int)}.
     */
    public void testGetLoopCounterEnd() {
        assertEquals(1, this.nodeCounter.getLoopCounterEnd(1));
        assertEquals(3, this.nodeCounter.getLoopCounterEnd(2));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#getStartTestCase(int)}.
     */
    public void testGetStartTestCase() {
        assertEquals(true, this.nodeCounter.getStartTestCase(0));
        assertEquals(true, this.nodeCounter.getStartTestCase(1));
        assertEquals(false, this.nodeCounter.getStartTestCase(2));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#getStatementCounter()}.
     */
    public void testGetStatementCounter() {
        assertEquals(37, this.nodeCounter.getStatementCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#getStatementCounterBegin(int)}.
     */
    public void testGetStatementCounterBegin() {
        assertEquals(0, this.nodeCounter.getStatementCounterBegin(1));
        assertEquals(22, this.nodeCounter.getStatementCounterBegin(2));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#getStatementCounterEnd(int)}.
     */
    public void testGetStatementCounterEnd() {
        assertEquals(22, this.nodeCounter.getStatementCounterEnd(1));
        assertEquals(37, this.nodeCounter.getStatementCounterEnd(2));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#incrementBasicBooleanCounter()}.
     */
    public void testIncrementBasicBooleanCounter() {
        // Must be the last condition, because incrementBasicBooleanCounter() works 
        // for the last condition.
        int condition = 6;
        int value = this.nodeCounter.getBasicBooleanCounter(condition);
        this.nodeCounter.incrementBasicBooleanCounter();
        assertEquals(value + 1, this.nodeCounter.getBasicBooleanCounter(condition));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#incrementBranchCounter()}.
     */
    public void testIncrementBranchCounter() {
        int value = this.nodeCounter.getBranchCounter();
        this.nodeCounter.incrementBranchCounter();
        assertEquals(value + 1, this.nodeCounter.getBranchCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#incrementBranchCounter(int)}.
     */
    public void testIncrementBranchCounterInt() {
        int value = this.nodeCounter.getBranchCounter();
        int branches = 45;
        this.nodeCounter.incrementBranchCounter(branches);
        assertEquals(value + branches, this.nodeCounter.getBranchCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#incrementConditionCounter()}.
     */
    public void testIncrementConditionCounter() {
        int value = this.nodeCounter.getConditionCounter();
        this.nodeCounter.incrementConditionCounter();
        assertEquals(value + 1, this.nodeCounter.getConditionCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#incrementLoopCounter()}.
     */
    public void testIncrementLoopCounter() {
        int value = this.nodeCounter.getLoopCounter();
        this.nodeCounter.incrementLoopCounter();
        assertEquals(value + 1, this.nodeCounter.getLoopCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#incrementStatementCounter()}.
     */
    public void testIncrementStatementCounter() {
        int value = this.nodeCounter.getStatementCounter();
        this.nodeCounter.incrementStatementCounter();
        assertEquals(value + 1, this.nodeCounter.getStatementCounter());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#newProgramUnit()}.
     */
    public void testNewProgramUnit() {
        int statements = this.nodeCounter.getStatementCounter();
        int branches = this.nodeCounter.getBranchCounter();
        int conditions = this.nodeCounter.getConditionCounter();
        int loops = this.nodeCounter.getLoopCounter();

        this.nodeCounter.setStartTestCase();
        this.nodeCounter.incrementConditionCounter();
        this.nodeCounter.incrementBasicBooleanCounter();
        this.nodeCounter.incrementLoopCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.incrementBranchCounter();
        this.nodeCounter.incrementStatementCounter();
        this.nodeCounter.newProgramUnit();
        
        assertEquals(statements, this.nodeCounter.getStatementCounterBegin(3));
        assertEquals(statements + 2, this.nodeCounter.getStatementCounterEnd(3));
        assertEquals(branches, this.nodeCounter.getBranchCounterBegin(3));
        assertEquals(branches + 1, this.nodeCounter.getBranchCounterEnd(3));
        assertEquals(conditions, this.nodeCounter.getConditionCounterBegin(3));
        assertEquals(conditions + 1, this.nodeCounter.getConditionCounterEnd(3));
        assertEquals(loops, this.nodeCounter.getLoopCounterBegin(3));
        assertEquals(loops + 1, this.nodeCounter.getLoopCounterEnd(3));
        assertEquals(false, this.nodeCounter.getStartTestCase(3));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.NodeCounter#setStartTestCase()}.
     */
    public void testSetStartTestCase() {
        this.nodeCounter.setStartTestCase();
        this.nodeCounter.newProgramUnit();
        assertEquals(false, this.nodeCounter.getStartTestCase(3));
        
    }

}
