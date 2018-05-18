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
 * This is a special {@link ConditionCounter}, which is used for <b>one</b>
 * InstrBasicBooleanTerm.<br>
 * <br>
 * Just two long represent the counters for the evaluation of the
 * InstrBasicBooleanTerm to true and false.
 * 
 * @see ConditionCounter
 * @author Christoph Müller
 * @version 1.0 ($Id: SmallOneConditionCounter.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class SmallOneConditionCounter extends ConditionCounter {

    private static final int BIT_MASK_FOR_11 = 2 | 1;

    private long counterFalse;

    private long counterTrue;

    /**
     * Constructor for a new {@link SmallOneConditionCounter}.
     * 
     * @param fullClassName
     *            The full name of the instrumented class, this condition
     *            counter is placed in.
     * 
     * @param varname
     *            The name of the counter ID, that is passed to the
     *            {@link CoverageCounterLog}.
     */
    public SmallOneConditionCounter(String fullClassName, String varname) {
        super(fullClassName, varname);

        reset();
    }

    public synchronized boolean incrementCounterOfBitMask(int bitMask, int termCount) {
        if (termCount != 1) {
            throw new RuntimeException("Do not use SmallOneConditionCounter with "
                    + termCount + " terms");
        }
        if (bitMask == BIT_MASK_FOR_11) {
            this.counterTrue++;
        } else {
            this.counterFalse++;
        }

        return true;
    }

    public synchronized boolean incrementCounterOfBitMask(int[] bitMasks, int termCount) {
        throw new RuntimeException(
        "Do not use int[] as a bit mask for SmallOneConditionCounter");
    }

    public void reset() {
        this.counterFalse = 0L;
        this.counterTrue = 0L;
    }

    public void serializeAndReset(CoverageCounterLog log) {
        String varname = super.getVarname();

        if (this.counterFalse > 0) {
        log.passCounter(varname + "-10", this.counterFalse);
        }
        if (this.counterTrue > 0) {
            log.passCounter(varname + "-11", this.counterTrue);
        }

        reset();
    }
}
