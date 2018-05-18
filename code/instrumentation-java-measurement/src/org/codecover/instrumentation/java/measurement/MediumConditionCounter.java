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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map.Entry;

import org.codecover.instrumentation.measurement.CoverageCounterLog;

/**
 * This is a special {@link ConditionCounter}, which is used for a number of
 * <b>3</b> to <b>{@value #MAX_TERM_COUNT}</b> InstrBasicBooleanTerms.<br>
 * <br>
 * Then a mapping from a bit mask to a counter is saved using a {@link HashMap}.
 * 
 * @see ConditionCounter
 * @author Christoph Müller
 * @version 1.0 ($Id: MediumConditionCounter.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MediumConditionCounter extends ConditionCounter {

    /**
     * How many InstrBasicBooleanTerm can be used with this class?<br>
     * <br>
     * {@value}
     */
    public static final int MAX_TERM_COUNT = 16;

    private HashMap counters;

    private int savedTermCount = -1;

    /**
     * Constructor for a new {@link MediumConditionCounter}.<br>
     * <br>
     * The HashMap is initialized.
     * 
     * @param fullClassName
     *            The full name of the instrumented class, this condition
     *            counter is placed in.
     * 
     * @param varname
     *            The name of the counter ID, that is passed to the
     *            {@link CoverageCounterLog}.
     */
    public MediumConditionCounter(String fullClassName, String varname) {
        super(fullClassName, varname);
        this.counters = new HashMap(17);
    }

    /**
     * Add a bit mask.<br>
     * <br>
     * If this bit mask is not in the HashMap, it is added with counter 1. Else
     * the counter is incremented.
     * 
     * @param bitMask
     *            The assigments of the the basic boolean terms in a bit mask.
     * @param termCount
     *            The number of terms.
     *
     * @return Always true.
     */
    public synchronized boolean incrementCounterOfBitMask(int bitMask, int termCount) {
        if (termCount <= 2 || termCount > MAX_TERM_COUNT) {
            throw new RuntimeException("Do not use MediumConditionCounter with "
                    + termCount + " terms");
        }
        if (this.savedTermCount == -1) {
            this.savedTermCount = termCount;
        } else if (this.savedTermCount != termCount) {
            throw new RuntimeException("Do not use this MediumConditionCounter with "
                    + "different terms " + this.savedTermCount + " vs. " + termCount);
        }

        Integer integerKey = new Integer(bitMask);
        LongContainer counter = (LongContainer) this.counters.get(integerKey);
        if (counter == null) {
            this.counters.put(integerKey, new LongContainer(1L));
        } else {
            counter.increment();
        }

        return true;
    }

    public synchronized boolean incrementCounterOfBitMask(int[] bitMasks, int termCount) {
        throw new RuntimeException(
        "Do not use int[] as a bit mask for MediumConditionCounter");
    }

    public void reset() {
        this.counters.clear();
    }

    public void serializeAndReset(CoverageCounterLog log) {
        String varname = super.getVarname();

        Iterator iterator =  this.counters.entrySet().iterator();
        while (iterator.hasNext()) {
            Entry thisEntry = (Entry) iterator.next();
            Integer integerKey = (Integer) thisEntry.getKey();
            LongContainer counter = (LongContainer) thisEntry.getValue();
            log.passCounter(varname + "-" + toBinString(integerKey.intValue(),
                    this.savedTermCount * 2), counter.getValue());
        }

        reset();
    }
}
