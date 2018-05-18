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
 * This is a special {@link CounterContainer}, which is used for boolean
 * assignments.<br>
 * <br>
 * These assignments are coded using an integer bit mask. Than
 * {@link #incrementCounterOfBitMask(int, int) or #incrementCounterOfBitMask(int[], int)}
 * is used to increment the counter of the bit mask.<br>
 * According to the number of basic boolean terms, different child classes can
 * be used.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: ConditionCounter.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @see SmallOneConditionCounter <b>1</b> InstrBasicBooleanTerm
 * @see SmallTwoConditionCounter <b>2</b> InstrBasicBooleanTerms
 * @see MediumConditionCounter <b>3</b> to <b>16</b> InstrBasicBooleanTerms
 * @see LargeConditionCounter more than <b>16</b> InstrBasicBooleanTerms
 */
public abstract class ConditionCounter extends CounterContainer {

    /** @see SmallOneConditionCounter */
    public static final byte CONDITION_COUNTER_TYPE_SMALL_ONE = 1;
    
    /** @see SmallTwoConditionCounter */
    public static final byte CONDITION_COUNTER_TYPE_SMALL_TWO = 2;

    /** @see MediumConditionCounter */
    public static final byte CONDITION_COUNTER_TYPE_MEDIUM = 16;

    /** @see LargeConditionCounter */
    public static final byte CONDITION_COUNTER_TYPE_LARGE = 17;

    /**
     * The name of the method {@link #incrementCounterOfBitMask(int, int)}.
     */
    public static final String INCREMENT_COUNTER_FOR_BIT_MASK_METHOD_NAME = "incrementCounterOfBitMask";

    private static final String ZEROS = "00000000000000000000000000000000";

    private String varname;

    /**
     * Constructor for a new {@link ConditionCounter}.
     * 
     * @param fullClassName
     *            The full name of the instrumented class, this condition
     *            counter is placed in.
     * 
     * @param varname
     *            The name of the counter ID, that is passed to the
     *            {@link CoverageCounterLog}.
     */
    public ConditionCounter(String fullClassName, String varname) {
        super(fullClassName + "." + varname);
        this.varname = varname;
    }

    /**
     * @return The varname of this {@link ConditionCounter}
     */
    public String getVarname() {
        return this.varname;
    }

    /**
     * Adds the counter of the current given bit mask.
     * 
     * @param bitMask
     *            The bit mask, which has been captured.
     * @param termCount
     *            The number of terms.
     *
     * @return Always true.
     */
    public abstract boolean incrementCounterOfBitMask(int bitMask,
            int termCount);

    /**
     * Adds the counter of the current given bit mask.<br>
     * <br>
     * This bit mask is the concatenation of the integers in the int array
     * bitMasks. The first int bitMask is located at bitMasks[0].
     * 
     * @param bitMasks
     *            The bit masks, which have been captured in a int array.
     * @param termCount
     *            The number of terms.
     *
     * @return Always true.
     */
    public abstract boolean incrementCounterOfBitMask(int[] bitMasks,
            int termCount);

    /**
     * Transforms a bitmask to a binary String.<br>
     * <br>Calls {@link Integer#toBinaryString(int)} and adds additional
     * "0" at the left side to extend the String to a given size.
     * 
     * @param bitMask
     *          The bitMask.
     * @param size
     *          The target size of the String. <code>2 * termCount</code>
     * 
     * @return The binary String.
     */
    public static String toBinString(int bitMask, int size) {
        String binString = Integer.toBinaryString(bitMask);
        if (binString.length() < size) {
            return ZEROS.substring(0, size - binString.length()) + binString;
        }

        return binString;
    }
}
