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

import org.codecover.instrumentation.measurement.CoverageCounterLog;

/**
 * This is a special {@link ConditionCounter}, which is used for <b>two</b>
 * InstrBasicBooleanTerms.<br>
 * <br>
 * Just four long counters represent the counters for the evaluation of the
 * InstrBasicBooleanTerms to true and false.
 * 
 * @see ConditionCounter
 * @author Christoph Müller
 * @version 1.0 ($Id: SmallTwoConditionCounter.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class SmallTwoConditionCounter extends ConditionCounter {

    private static final int BIT_MASK_FOR_1000 = 8;

    private static final int BIT_MASK_FOR_1010 = 8 | 2;

    private static final int BIT_MASK_FOR_1011 = 8 | 2 | 1;

    private static final int BIT_MASK_FOR_1100 = 8 | 4;

    private static final int BIT_MASK_FOR_1110 = 8 | 4 | 2;

    private static final int BIT_MASK_FOR_1111 = 8 | 4 | 2 | 1;

    private long counter1000;

    private long counter1010;

    private long counter1011;

    private long counter1100;

    private long counter1110;

    private long counter1111;

    /**
     * Constructor for a new {@link SmallTwoConditionCounter}.
     * 
     * @param fullClassName
     *            The full name of the instrumented class, this condition
     *            counter is placed in.
     * 
     * @param varname
     *            The name of the counter ID, that is passed to the
     *            {@link CoverageCounterLog}.
     */
    public SmallTwoConditionCounter(String fullClassName, String varname) {
        super(fullClassName, varname);

        reset();
    }

    public synchronized boolean incrementCounterOfBitMask(int bitMask, int termCount) {
        if (termCount != 2) {
            throw new RuntimeException("Do not use SmallTwoConditionCounter with "
                + termCount + " terms");
        }
        switch (bitMask) {
        case BIT_MASK_FOR_1000:
            this.counter1000++;
            break;

        case BIT_MASK_FOR_1010:
            this.counter1010++;
            break;

        case BIT_MASK_FOR_1011:
            this.counter1011++;
            break;

        case BIT_MASK_FOR_1100:
            this.counter1100++;
            break;

        case BIT_MASK_FOR_1110:
            this.counter1110++;
            break;

        case BIT_MASK_FOR_1111:
            this.counter1111++;
            break;
        }

        return true;
    }

    public synchronized boolean incrementCounterOfBitMask(int[] bitMasks, int termCount) {
        throw new RuntimeException(
        "Do not use int[] as a bit mask for SmallTwoConditionCounter");
    }

    public void reset() {
        this.counter1000 = 0L;
        this.counter1010 = 0L;
        this.counter1011 = 0L;
        this.counter1100 = 0L;
        this.counter1110 = 0L;
        this.counter1111 = 0L;
    }

    public void serializeAndReset(CoverageCounterLog log) {
        String varname = super.getVarname();

        if (this.counter1000 > 0) {
            log.passCounter(varname + "-1000", this.counter1000);
        }
        if (this.counter1010 > 0) {
            log.passCounter(varname + "-1010", this.counter1010);
        }
        if (this.counter1011 > 0) {
            log.passCounter(varname + "-1011", this.counter1011);
        }
        if (this.counter1100 > 0) {
            log.passCounter(varname + "-1100", this.counter1100);
        }
        if (this.counter1110 > 0) {
            log.passCounter(varname + "-1110", this.counter1110);
        }
        if (this.counter1111 > 0) {
            log.passCounter(varname + "-1111", this.counter1111);
        }

        reset();
    }
}
