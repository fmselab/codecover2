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

package org.codecover.instrumentation.java.measurement;

import java.util.Vector;
import java.util.Map.Entry;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJMeasure.TestCoverageCounterLog;
/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: MediumConditionCounterTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MediumConditionCounterTest extends TestCase {

    private static final String fullClassName = "java.util.Collections";

    private static final String varname = "C3";

    private MediumConditionCounter conditionCounter;

    private TestCoverageCounterLog counterLog;

    public void setUp() {
        this.conditionCounter = new MediumConditionCounter(fullClassName, varname);
        this.counterLog = new TestCoverageCounterLog();
    }

    protected void tearDown() throws Exception {
        this.conditionCounter.reset();
        this.conditionCounter = null;
        this.counterLog.clear();
        this.counterLog = null;
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.LargeConditionCounter#incrementCounterOfBitMask(int[])}.
     * 
     * one entry once
     */
    public void testIncrementCounterOfBitMaskIntArray1() {
        String conditionID = "100000";
        int bitMask = Integer.parseInt(conditionID, 2);
        String covItem = varname + "-" + conditionID;

        this.conditionCounter.incrementCounterOfBitMask(bitMask, 3);
        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(1, this.counterLog.counters.size());
        Vector entrySet = new Vector(this.counterLog.counters.entrySet());
        Assert.assertEquals(covItem, ((Entry)entrySet.get(0)).getKey());
        Assert.assertEquals(Long.valueOf(1), ((Entry)entrySet.get(0)).getValue());

        this.counterLog.clear();
        this.conditionCounter.serializeAndReset(this.counterLog);
        Assert.assertEquals(0, this.counterLog.counters.size());

        this.conditionCounter.incrementCounterOfBitMask(bitMask, 3);
        this.conditionCounter.reset();
        this.counterLog.clear();
        this.conditionCounter.serializeAndReset(this.counterLog);
        Assert.assertEquals(0, this.counterLog.counters.size());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.LargeConditionCounter#incrementCounterOfBitMask(int[])}.
     * 
     * one entry twice (the same)
     */
    public void testIncrementCounterOfBitMaskIntArray2() {
        String conditionID = "101000";
        int bitMask = Integer.parseInt(conditionID, 2);
        String covItem = varname + "-" + conditionID;

        this.conditionCounter.incrementCounterOfBitMask(bitMask, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask, 3);
        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(1, this.counterLog.counters.size());
        Vector entrySet = new Vector(this.counterLog.counters.entrySet());
        Assert.assertEquals(covItem, ((Entry)entrySet.get(0)).getKey());
        Assert.assertEquals(Long.valueOf(2), ((Entry)entrySet.get(0)).getValue());

        this.counterLog.clear();
        this.conditionCounter.serializeAndReset(this.counterLog);
        Assert.assertEquals(0, this.counterLog.counters.size());

        this.conditionCounter.incrementCounterOfBitMask(bitMask, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask, 3);
        this.conditionCounter.reset();
        this.counterLog.clear();
        this.conditionCounter.serializeAndReset(this.counterLog);
        Assert.assertEquals(0, this.counterLog.counters.size());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.LargeConditionCounter#incrementCounterOfBitMask(int[])}.
     * 
     * leading "00" in the second int
     */
    public void testIncrementCounterOfBitMaskIntArray3() {
        String conditionID1 = "100000";
        int bitMask1 = Integer.parseInt(conditionID1, 2);
        String covItem1 = varname + "-" + conditionID1;
        String conditionID2 = "101000";
        int bitMask2 = Integer.parseInt(conditionID2, 2);
        String covItem2 = varname + "-" + conditionID2;

        this.conditionCounter.incrementCounterOfBitMask(bitMask1, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask2, 3);
        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(2, this.counterLog.counters.size());
        Vector entrySet = new Vector(this.counterLog.counters.entrySet());
        Assert.assertEquals(covItem1, ((Entry)entrySet.get(0)).getKey());
        Assert.assertEquals(Long.valueOf(1), ((Entry)entrySet.get(0)).getValue());
        Assert.assertEquals(covItem2, ((Entry)entrySet.get(1)).getKey());
        Assert.assertEquals(Long.valueOf(1), ((Entry)entrySet.get(1)).getValue());

        this.counterLog.clear();
        this.conditionCounter.serializeAndReset(this.counterLog);
        Assert.assertEquals(0, this.counterLog.counters.size());

        this.conditionCounter.incrementCounterOfBitMask(bitMask1, 3);
        this.conditionCounter.reset();
        this.counterLog.clear();
        this.conditionCounter.serializeAndReset(this.counterLog);
        Assert.assertEquals(0, this.counterLog.counters.size());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.LargeConditionCounter#incrementCounterOfBitMask(int[])}.
     * 
     * a lot of entries
     */
    public void testIncrementCounterOfBitMaskIntArray4() {
        String conditionID1 = "100000";
        int bitMask1 = Integer.parseInt(conditionID1, 2);
        String covItem1 = varname + "-" + conditionID1;
        String conditionID2 = "101000";
        int bitMask2 = Integer.parseInt(conditionID2, 2);
        String covItem2 = varname + "-" + conditionID2;
    
        this.conditionCounter.incrementCounterOfBitMask(bitMask1, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask1, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask2, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask2, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask2, 3);
        this.conditionCounter.serializeAndReset(this.counterLog);
    
        Assert.assertEquals(2, this.counterLog.counters.size());
        Vector entrySet = new Vector(this.counterLog.counters.entrySet());
        Assert.assertEquals(covItem1, ((Entry)entrySet.get(0)).getKey());
        Assert.assertEquals(Long.valueOf(2), ((Entry)entrySet.get(0)).getValue());
        Assert.assertEquals(covItem2, ((Entry)entrySet.get(1)).getKey());
        Assert.assertEquals(Long.valueOf(3), ((Entry)entrySet.get(1)).getValue());
    
        this.counterLog.clear();
        this.conditionCounter.serializeAndReset(this.counterLog);
        Assert.assertEquals(0, this.counterLog.counters.size());
    
        this.conditionCounter.incrementCounterOfBitMask(bitMask1, 3);
        this.conditionCounter.reset();
        this.counterLog.clear();
        this.conditionCounter.serializeAndReset(this.counterLog);
        Assert.assertEquals(0, this.counterLog.counters.size());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.LargeConditionCounter#incrementCounterOfBitMask(int[])}.
     * 
     * a lot of entries
     */
    public void testIncrementCounterOfBitMaskIntArray5() {
        String conditionID1 = "100000";
        int bitMask1 = Integer.parseInt(conditionID1, 2);
        String covItem1 = varname + "-" + conditionID1;
        String conditionID2 = "101000";
        int bitMask2 = Integer.parseInt(conditionID2, 2);
        String covItem2 = varname + "-" + conditionID2;

        this.conditionCounter.incrementCounterOfBitMask(bitMask1, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask2, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask1, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask1, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask1, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask2, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask2, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask2, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask2, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask1, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask1, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask1, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask2, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask2, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask2, 3);
        this.conditionCounter.incrementCounterOfBitMask(bitMask2, 3);
        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(2, this.counterLog.counters.size());
        Vector entrySet = new Vector(this.counterLog.counters.entrySet());
        Assert.assertEquals(covItem1, ((Entry)entrySet.get(0)).getKey());
        Assert.assertEquals(Long.valueOf(7), ((Entry)entrySet.get(0)).getValue());
        Assert.assertEquals(covItem2, ((Entry)entrySet.get(1)).getKey());
        Assert.assertEquals(Long.valueOf(9), ((Entry)entrySet.get(1)).getValue());

        this.counterLog.clear();
        this.conditionCounter.serializeAndReset(this.counterLog);
        Assert.assertEquals(0, this.counterLog.counters.size());

        this.conditionCounter.incrementCounterOfBitMask(bitMask1, 3);
        this.conditionCounter.reset();
        this.counterLog.clear();
        this.conditionCounter.serializeAndReset(this.counterLog);
        Assert.assertEquals(0, this.counterLog.counters.size());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.LargeConditionCounter#incrementCounterOfBitMask(int[])}.
     * 
     * a lot of entries
     */
    public void testIncrementCounterOfBitMaskIntArrayException() {
        try {
            this.conditionCounter.incrementCounterOfBitMask(1, 0);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            // expected
        }

        try {
            this.conditionCounter.incrementCounterOfBitMask(1, 1);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            // expected
        }

        try {
            this.conditionCounter.incrementCounterOfBitMask(1, 2);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            // expected
        }

        try {
            this.conditionCounter.incrementCounterOfBitMask(1, 3);
        } catch (RuntimeException e) {
            Assert.fail("RuntimeException expected");
            // expected
        }

        try {
            this.conditionCounter.incrementCounterOfBitMask(1, 15);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            // expected
        }

        try {
            this.conditionCounter.incrementCounterOfBitMask(1, MediumConditionCounter.MAX_TERM_COUNT);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            // expected
        }

        try {
            this.conditionCounter.incrementCounterOfBitMask(1, MediumConditionCounter.MAX_TERM_COUNT + 1);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            // expected
        }

        try {
            this.conditionCounter.incrementCounterOfBitMask(new int[]{1}, 1);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            // expected
        }
    }
}
