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

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJMeasure.TestCoverageCounterLog;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: SmallTwoConditionCounterTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class SmallTwoConditionCounterTest extends TestCase {

    private static final String fullClassName = "java.util.Collections";

    private static final String varname = "C12";

    private SmallTwoConditionCounter conditionCounter;

    private TestCoverageCounterLog counterLog;

    public void setUp() {
        this.conditionCounter = new SmallTwoConditionCounter(fullClassName, varname);
        this.counterLog = new TestCoverageCounterLog();
    }

    protected void tearDown() throws Exception {
        this.conditionCounter.reset();
        this.conditionCounter = null;
        this.counterLog.clear();
        this.counterLog = null;
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.SmallTwoConditionCounter#incrementCounterOfBitMask(int, int)}.
     * 
     * one int once
     */
    public void testIncrementCounterOfBitMaskIntInt1() {
        String conditionID1 = varname + "-" + "1000";

        this.conditionCounter.incrementCounterOfBitMask(1 << 3, 2);
        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(1, this.counterLog.counters.size());
        Assert.assertEquals(1, ((Long)this.counterLog.counters.get(conditionID1)).longValue());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.SmallTwoConditionCounter#incrementCounterOfBitMask(int, int)}.
     * 
     * one int twice
     */
    public void testIncrementCounterOfBitMaskIntInt2() {
        String conditionID1 = varname + "-" + "1000";

        this.conditionCounter.incrementCounterOfBitMask(1 << 3, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3, 2);
        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(1, this.counterLog.counters.size());
        Assert.assertEquals(2, ((Long)this.counterLog.counters.get(conditionID1)).longValue());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.SmallTwoConditionCounter#incrementCounterOfBitMask(int, int)}.
     * 
     * two int once
     */
    public void testIncrementCounterOfBitMaskIntInt3() {
        String conditionID1 = varname + "-" + "1000";
        String conditionID2 = varname + "-" + "1100";

        this.conditionCounter.incrementCounterOfBitMask(1 << 3, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2, 2);
        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(2, this.counterLog.counters.size());
        Assert.assertEquals(1, ((Long)this.counterLog.counters.get(conditionID1)).longValue());
        Assert.assertEquals(1, ((Long)this.counterLog.counters.get(conditionID2)).longValue());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.SmallTwoConditionCounter#incrementCounterOfBitMask(int, int)}.
     * 
     * various
     */
    public void testIncrementCounterOfBitMaskIntInt4() {
        String conditionID1000 = varname + "-" + "1000";
        String conditionID1010 = varname + "-" + "1010";
        String conditionID1011 = varname + "-" + "1011";
        String conditionID1100 = varname + "-" + "1100";
        String conditionID1110 = varname + "-" + "1110";
        String conditionID1111 = varname + "-" + "1111";

        this.conditionCounter.incrementCounterOfBitMask(1 << 3, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 1, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 1 | 1 << 0, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 1 | 1 << 0, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 1 | 1 << 0, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 1 | 1 << 0, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 1 | 1 << 0, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 1, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2 | 1 << 1, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 1 | 1 << 0, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2 | 1 << 1 | 1 << 0, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2 | 1 << 1, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2 | 1 << 1, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 1, 2);
        this.conditionCounter.incrementCounterOfBitMask(1 << 3 | 1 << 2 | 1 << 1, 2);
        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(6, this.counterLog.counters.size());
        Assert.assertEquals(9, ((Long)this.counterLog.counters.get(conditionID1000)).longValue());
        Assert.assertEquals(3, ((Long)this.counterLog.counters.get(conditionID1010)).longValue());
        Assert.assertEquals(6, ((Long)this.counterLog.counters.get(conditionID1011)).longValue());
        Assert.assertEquals(12, ((Long)this.counterLog.counters.get(conditionID1100)).longValue());
        Assert.assertEquals(4, ((Long)this.counterLog.counters.get(conditionID1110)).longValue());
        Assert.assertEquals(1, ((Long)this.counterLog.counters.get(conditionID1111)).longValue());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.SmallTwoConditionCounter#incrementCounterOfBitMask(int, int)}.
     * 
     * test some exceptions
     */
    public void testIncrementCounterOfBitMaskExceptions() {
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
        } catch (RuntimeException e) {
            Assert.fail("No RuntimeException expected: " + e.getMessage());
        }

        try {
            this.conditionCounter.incrementCounterOfBitMask(1, 3);
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
