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

import java.nio.CharBuffer;
import java.util.Vector;
import java.util.Map.Entry;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJMeasure.TestCoverageCounterLog;
/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: LargeConditionCounterTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class LargeConditionCounterTest extends TestCase {

    private static final String fullClassName = "java.util.Collections";

    private static final String varname = "C3";

    private LargeConditionCounter conditionCounter;

    private TestCoverageCounterLog counterLog;

    public void setUp() {
        this.conditionCounter = new LargeConditionCounter(fullClassName, varname);
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

        int int1 = 1 << 31;
        int int2 = Integer.parseInt("1000001000000000", 2);
        int[] bitMasks1 = new int[]{int1, int2};
        String conditionID = varname + "-" + "10000000000000000000000000000000" + "1000001000000000";

        this.conditionCounter.incrementCounterOfBitMask(bitMasks1, 24);
        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(1, this.counterLog.counters.size());
        Vector entrySet = new Vector(this.counterLog.counters.entrySet());
        Assert.assertEquals(conditionID, ((Entry)entrySet.get(0)).getKey());
        Assert.assertEquals(Long.valueOf(1), ((Entry)entrySet.get(0)).getValue());

        this.counterLog.clear();
        this.conditionCounter.serializeAndReset(this.counterLog);
        Assert.assertEquals(0, this.counterLog.counters.size());

        this.conditionCounter.incrementCounterOfBitMask(bitMasks1, 24);
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

        int int1 = 1 << 31;
        int int2 = Integer.parseInt("1000001000000000", 2);
        int[] bitMasks1 = new int[]{int1, int2};
        String conditionID = varname + "-" + "10000000000000000000000000000000" + "1000001000000000";

        this.conditionCounter.incrementCounterOfBitMask(bitMasks1, 24);
        this.conditionCounter.incrementCounterOfBitMask(bitMasks1, 24);
        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(1, this.counterLog.counters.size());
        Vector entrySet = new Vector(this.counterLog.counters.entrySet());
        Assert.assertEquals(conditionID, ((Entry)entrySet.get(0)).getKey());
        Assert.assertEquals(Long.valueOf(2), ((Entry)entrySet.get(0)).getValue());

        this.counterLog.clear();
        this.conditionCounter.serializeAndReset(this.counterLog);
        Assert.assertEquals(0, this.counterLog.counters.size());

        this.conditionCounter.incrementCounterOfBitMask(bitMasks1, 24);
        this.conditionCounter.incrementCounterOfBitMask(bitMasks1, 24);
        this.conditionCounter.reset();
        this.counterLog.clear();
        this.conditionCounter.serializeAndReset(this.counterLog);
        Assert.assertEquals(0, this.counterLog.counters.size());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.LargeConditionCounter#incrementCounterOfBitMask(int[])}.
     * 
     * one entry twice equal
     */
    public void testIncrementCounterOfBitMaskIntArray3() {

        int int1 = 1 << 31;
        int int2 = Integer.parseInt("1000001000000000", 2);
        int[] bitMasks1 = new int[]{int1, int2};
        int[] bitMasks2 = new int[]{int1, int2};
        String conditionID = varname + "-" + "10000000000000000000000000000000" + "1000001000000000";

        this.conditionCounter.incrementCounterOfBitMask(bitMasks1, 24);
        this.conditionCounter.incrementCounterOfBitMask(bitMasks2, 24);
        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(1, this.counterLog.counters.size());
        Vector entrySet = new Vector(this.counterLog.counters.entrySet());
        Assert.assertEquals(conditionID, ((Entry)entrySet.get(0)).getKey());
        Assert.assertEquals(Long.valueOf(2), ((Entry)entrySet.get(0)).getValue());

        this.counterLog.clear();
        this.conditionCounter.serializeAndReset(this.counterLog);
        Assert.assertEquals(0, this.counterLog.counters.size());

        this.conditionCounter.incrementCounterOfBitMask(bitMasks1, 24);
        this.conditionCounter.incrementCounterOfBitMask(bitMasks2, 24);
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
    public void testIncrementCounterOfBitMaskIntArray4() {

        int int1 = 1 << 31;
        int int2 = Integer.parseInt("0010001000000000", 2);
        int[] bitMasks1 = new int[]{int1, int2};
        int[] bitMasks2 = new int[]{int1, int2};
        String conditionID = varname + "-" + "10000000000000000000000000000000" + "0010001000000000";

        this.conditionCounter.incrementCounterOfBitMask(bitMasks1, 24);
        this.conditionCounter.incrementCounterOfBitMask(bitMasks2, 24);
        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(1, this.counterLog.counters.size());
        Vector entrySet = new Vector(this.counterLog.counters.entrySet());
        Assert.assertEquals(conditionID, ((Entry)entrySet.get(0)).getKey());
        Assert.assertEquals(Long.valueOf(2), ((Entry)entrySet.get(0)).getValue());

        this.counterLog.clear();
        this.conditionCounter.serializeAndReset(this.counterLog);
        Assert.assertEquals(0, this.counterLog.counters.size());

        this.conditionCounter.incrementCounterOfBitMask(bitMasks1, 24);
        this.conditionCounter.incrementCounterOfBitMask(bitMasks2, 24);
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
        int[] bitMasks;

        bitMasks = new int[]{1 << 31, 1 << 7 | 1 << 6};
        String conditionID1 = varname + "-" + "10000000000000000000000000000000" + "0000000011000000";
        this.conditionCounter.incrementCounterOfBitMask(bitMasks, 24);

        bitMasks = new int[]{1 << 31 | 1 << 30, 1 << 7 | 1 << 6};
        String conditionID2 = varname + "-" + "11000000000000000000000000000000" + "0000000011000000";
        this.conditionCounter.incrementCounterOfBitMask(bitMasks, 24);

        bitMasks = new int[]{1 << 31 | 1 << 30 | 1 << 29 | 1 << 28, 1 << 7 | 1 << 6};
        String conditionID3 = varname + "-" + "11110000000000000000000000000000" + "0000000011000000";
        this.conditionCounter.incrementCounterOfBitMask(bitMasks, 24);

        bitMasks = new int[]{1 << 31 | 1 << 30 | 1 << 29 | 1 << 28, 1 << 13 | 1 << 11 | 1 << 7 | 1 << 6};
        String conditionID4 = varname + "-" + "11110000000000000000000000000000" + "0010100011000000";
        this.conditionCounter.incrementCounterOfBitMask(bitMasks, 24);

        bitMasks = new int[]{1 << 31 | 1 << 30 | 1 << 29 | 1 << 28, 1 << 7 | 1 << 6};
        this.conditionCounter.incrementCounterOfBitMask(bitMasks, 24);

        bitMasks = new int[]{1 << 31 | 1 << 30 | 1 << 29 | 1 << 28, 1 << 13 | 1 << 11 | 1 << 7 | 1 << 6};
        this.conditionCounter.incrementCounterOfBitMask(bitMasks, 24);

        bitMasks = new int[]{1 << 31 | 1 << 30, 1 << 7 | 1 << 6};
        this.conditionCounter.incrementCounterOfBitMask(bitMasks, 24);

        bitMasks = new int[]{1 << 31, 1 << 7 | 1 << 6};
        this.conditionCounter.incrementCounterOfBitMask(bitMasks, 24);

        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(4, this.counterLog.counters.size());
        Assert.assertEquals(2, ((Long)this.counterLog.counters.get(conditionID1)).longValue());
        Assert.assertEquals(2, ((Long)this.counterLog.counters.get(conditionID2)).longValue());
        Assert.assertEquals(2, ((Long)this.counterLog.counters.get(conditionID3)).longValue());
        Assert.assertEquals(2, ((Long)this.counterLog.counters.get(conditionID4)).longValue());
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
            this.conditionCounter.incrementCounterOfBitMask(new int[]{1}, 1);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            // expected
        }

        try {
            this.conditionCounter.incrementCounterOfBitMask(new int[]{1}, 2);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            // expected
        }

        try {
            this.conditionCounter.incrementCounterOfBitMask(new int[]{1}, 3);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            // expected
        }

        try {
            this.conditionCounter.incrementCounterOfBitMask(new int[]{1}, MediumConditionCounter.MAX_TERM_COUNT);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            // expected
        }

        try {
            this.conditionCounter.incrementCounterOfBitMask(new int[]{1}, MediumConditionCounter.MAX_TERM_COUNT + 1);
        } catch (RuntimeException e) {
            Assert.fail("No RuntimeException expected: " + e.getMessage());
        }

        try {
            this.conditionCounter.incrementCounterOfBitMask(new int[]{1}, MediumConditionCounter.MAX_TERM_COUNT + 2);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            // expected
        }
    }

    /**
     * tests {@link LargeConditionCounter.ArrayComparator}
     */
    public void testArrayComparator() {
        LargeConditionCounter.ArrayComparator comp = new LargeConditionCounter.ArrayComparator();
        
        Object o1;
        Object o2;

        o1 = new Object();
        o2 = new Object();
        try {
            comp.compare(o1, o2);
            Assert.fail();
        } catch (RuntimeException e) {
            // expected
        }
        
        o1 = new Object();
        o2 = new String();
        try {
            comp.compare(o1, o2);
            Assert.fail();
        } catch (RuntimeException e) {
            // expected
        }

        o1 = new int[0];
        o2 = new String();
        try {
            comp.compare(o1, o2);
            Assert.fail();
        } catch (RuntimeException e) {
            // expected
        }
        
        o1 = CharBuffer.wrap("Hello");
        o2 = new int[0];
        try {
            comp.compare(o1, o2);
            Assert.fail();
        } catch (RuntimeException e) {
            // expected
        }
        
        o1 = new int[0];
        o2 = new int[0];
        Assert.assertTrue(comp.compare(o1, o2) == 0);
        
        o1 = new int[1];
        o2 = new int[0];
        try {
            comp.compare(o1, o2);
            Assert.fail();
        } catch (RuntimeException e) {
            // expected
        }
        
        o1 = new int[1];
        o2 = new int[2];
        try {
            comp.compare(o1, o2);
            Assert.fail();
        } catch (RuntimeException e) {
            // expected
        }
        
        o1 = new int[]{20};
        o2 = new int[]{10};
        Assert.assertTrue(comp.compare(o1, o2) > 0);

        o1 = new int[]{20};
        o2 = new int[]{Integer.MAX_VALUE};
        Assert.assertTrue(comp.compare(o1, o2) < 0);

        o1 = new int[]{Integer.MIN_VALUE};
        o2 = new int[]{Integer.MIN_VALUE};
        Assert.assertTrue(comp.compare(o1, o2) == 0);

        o1 = new int[]{3, 8};
        o2 = new int[]{3, 8};
        Assert.assertTrue(comp.compare(o1, o2) == 0);
        
        o1 = new int[]{3, 8};
        o2 = new int[]{3, 7};
        Assert.assertTrue(comp.compare(o1, o2) > 0);
        
        o1 = new int[]{3, 8};
        o2 = new int[]{3, 9};
        Assert.assertTrue(comp.compare(o1, o2) < 0);
        
        o1 = new int[]{3, 8};
        o2 = new int[]{2, 8};
        Assert.assertTrue(comp.compare(o1, o2) > 0);
        
        o1 = new int[]{3, 8};
        o2 = new int[]{2, 8};
        Assert.assertTrue(comp.compare(o1, o2) > 0);
        
        o1 = new int[]{1, 8};
        o2 = new int[]{2, 8};
        Assert.assertTrue(comp.compare(o1, o2) < 0);
    }
}
