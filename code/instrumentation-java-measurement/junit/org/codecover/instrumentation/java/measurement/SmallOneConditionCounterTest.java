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
 * @version 1.0 ($Id: SmallOneConditionCounterTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class SmallOneConditionCounterTest extends TestCase {

    private static final String fullClassName = "java.util.Collections";

    private static final String varname = "C12";

    private SmallOneConditionCounter conditionCounter;

    private TestCoverageCounterLog counterLog;

    public void setUp() {
        this.conditionCounter = new SmallOneConditionCounter(fullClassName, varname);
        this.counterLog = new TestCoverageCounterLog();
    }

    protected void tearDown() throws Exception {
        this.conditionCounter.reset();
        this.conditionCounter = null;
        this.counterLog.clear();
        this.counterLog = null;
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.SmallOneConditionCounter#incrementCounterOfBitMask(int, int)}.
     * 
     * test one int once
     */
    public void testIncrementCounterOfBitMaskIntInt1() {
        String conditionID = varname + "-" + "10";

        this.conditionCounter.incrementCounterOfBitMask(1 << 1, 1);
        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(1, this.counterLog.counters.size());
        Assert.assertEquals(1, ((Long)this.counterLog.counters.get(conditionID)).longValue());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.SmallOneConditionCounter#incrementCounterOfBitMask(int, int)}.
     * 
     * test one int twice
     */
    public void testIncrementCounterOfBitMaskIntInt2() {
        String conditionID = varname + "-" + "10";

        this.conditionCounter.incrementCounterOfBitMask(1 << 1, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1, 1);
        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(1, this.counterLog.counters.size());
        Assert.assertEquals(2, ((Long)this.counterLog.counters.get(conditionID)).longValue());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.SmallOneConditionCounter#incrementCounterOfBitMask(int, int)}.
     * 
     * test two int once
     */
    public void testIncrementCounterOfBitMaskIntInt3() {
        String conditionID1 = varname + "-" + "10";
        String conditionID2 = varname + "-" + "11";

        this.conditionCounter.incrementCounterOfBitMask(1 << 1, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1 | 1 << 0, 1);
        this.conditionCounter.serializeAndReset(this.counterLog);

        Assert.assertEquals(2, this.counterLog.counters.size());
        Assert.assertEquals(1, ((Long)this.counterLog.counters.get(conditionID1)).longValue());
        Assert.assertEquals(1, ((Long)this.counterLog.counters.get(conditionID2)).longValue());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.SmallOneConditionCounter#incrementCounterOfBitMask(int, int)}.
     * 
     * test two int various
     */
    public void testIncrementCounterOfBitMaskIntInt4() {
        String conditionID1 = varname + "-" + "10";
        String conditionID2 = varname + "-" + "11";

        this.conditionCounter.incrementCounterOfBitMask(1 << 1 | 1 << 0, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1 | 1 << 0, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1 | 1 << 0, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1 | 1 << 0, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1 | 1 << 0, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1 | 1 << 0, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1 | 1 << 0, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1 | 1 << 0, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1, 1);
        this.conditionCounter.incrementCounterOfBitMask(1 << 1, 1);
        this.conditionCounter.serializeAndReset(this.counterLog);

        this.conditionCounter.incrementCounterOfBitMask(1 << 1 | 1 << 0, 1);
        Assert.assertEquals(2, this.counterLog.counters.size());
        Assert.assertEquals(7, ((Long)this.counterLog.counters.get(conditionID1)).longValue());
        Assert.assertEquals(8, ((Long)this.counterLog.counters.get(conditionID2)).longValue());
    }

    /**
     * Test method for {@link org.codecover.instrumentation.java15.measurement.SmallOneConditionCounter#incrementCounterOfBitMask(int, int)}.
     * 
     * test some exceptions
     */
    public void testIncrementCounterOfBitMaskException() {
        try {
            this.conditionCounter.incrementCounterOfBitMask(1, 0);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            // expected
        }

        try {
            this.conditionCounter.incrementCounterOfBitMask(1, 1);
        } catch (RuntimeException e) {
            Assert.fail("No RuntimeException expected: " + e.getMessage());
        }

        try {
            this.conditionCounter.incrementCounterOfBitMask(1, 2);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            // expected
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
