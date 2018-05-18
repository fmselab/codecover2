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

import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.codecover.instrumentation.measurement.CoverageCounterLog;

/**
 * This is a special {@link ConditionCounter}, which is used for more than 16
 * InstrBasicBooleanTerms.<br>
 * <br>
 * A mapping from a bit mask to a counter is saved using a {@link HashMap}.
 * 
 * @see ConditionCounter
 * @author Christoph Müller
 * @version 1.0 ($Id: LargeConditionCounter.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class LargeConditionCounter extends ConditionCounter {

    /** An instance of ArrayComparator */
    public static final ArrayComparator ARRAY_COMPARATOR = new ArrayComparator();

    /**
     * How many InstrBasicBooleanTerm can be used for each int of the
     * int array?
     */
    public static final int TERMS_PER_INT = 16;

    private Map counters;

    private int savedTermCount = -1;

    /**
     * Constructor for a new {@link LargeConditionCounter}.<br>
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
    public LargeConditionCounter(String fullClassName, String varname) {
        super(fullClassName, varname);
        this.counters = new TreeMap(ARRAY_COMPARATOR);
    }

    public synchronized boolean incrementCounterOfBitMask(int bitMask, int termCount) {
        throw new RuntimeException(
                "Do not use an int as a bit mask for LargeConditionCounter");
    }

    public synchronized boolean incrementCounterOfBitMask(int[] bitMasks, int termCount) {
        if (termCount <= MediumConditionCounter.MAX_TERM_COUNT) {
            throw new RuntimeException("Do not use LargeConditionCounter with "
                    + termCount + " terms");
        }
        if (this.savedTermCount  == -1) {
            this.savedTermCount = termCount;
        } else if (this.savedTermCount != termCount) {
            throw new RuntimeException("Do not use this LargeConditionCounter with "
                    + "different terms " + this.savedTermCount + " vs. " + termCount);
        }

        LongContainer counter = (LongContainer) this.counters.get(bitMasks);
        if (counter == null) {
            this.counters.put(bitMasks, new LongContainer(1L));
        } else {
            counter.increment();
        }

        return true;
    }

    public void reset() {
        this.counters.clear();
    }

    public void serializeAndReset(CoverageCounterLog log) {
        String varname = super.getVarname();

        Iterator iterator =  this.counters.entrySet().iterator();
        while (iterator.hasNext()) {
            Entry thisEntry = (Entry) iterator.next();

            // this int[] contains ints which are bitmasks
            // at int[0] and the highest bit we have the most left basic boolean
            // term
            int[] intArrayKey = (int[]) thisEntry.getKey();
            int thisBitCount = this.savedTermCount * 2;
            StringBuffer conditionSubKey = new StringBuffer();
            for (int i = 0; i < intArrayKey.length; i++) {
                conditionSubKey.append(toBinString(intArrayKey[i], Math.min(32, thisBitCount)));
                thisBitCount -= 32;
            }

            LongContainer counter = (LongContainer) thisEntry.getValue();
            log.passCounter(varname + "-" + conditionSubKey, counter.getValue());
        }

        reset();
    }

    /**
     * This is a {@link Comparator} to use int[] in {@link TreeMap}s. Otherwise
     * two different int[] instances with the same values are treated different.
     * 
     * @author Christoph Müller
     * 
     * @version 1.0 ($Id: LargeConditionCounter.java 1 2007-12-12 17:37:26Z t-scheller $)
     * 
     * TODO test
     */
    static class ArrayComparator implements Comparator {

        public int compare(Object o1, Object o2) {
            if (!(o1 instanceof int[]) || !(o2 instanceof int[])) {
                throw new RuntimeException("!(o1 instanceof int[]) || !(o2 instanceof int[])");
            }
            
            int[] bitMasks1 = (int[]) o1;
            int[] bitMasks2 = (int[]) o2;

            if (bitMasks1.length != bitMasks2.length) {
                throw new RuntimeException("bitMasks1.length != bitMasks2.length");
            }
            int compareResult = 0;

            for (int i = 0; compareResult == 0 && i < bitMasks1.length; i++) {
                compareResult = bitMasks1[i] - bitMasks2[i];
            }

            return compareResult;
        }
    }
}
