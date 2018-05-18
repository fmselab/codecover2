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

import java.util.TreeSet;

import org.codecover.instrumentation.measurement.CoverageCounterLog;

/**
 * This abstract class is used for inner classes which contain coverage results.<br>
 * <br>
 * These inner classes have serialize their coverage results and have to reset
 * the counters on demand.<br>
 * <br>
 * The implementation of {@link Comparable} is needed to use objects of this
 * kind in {@link TreeSet}s. It is bases on the comparaision of the field
 * {@link #className}.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: CounterContainer.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public abstract class CounterContainer implements Comparable {
    /** This is the name of {@link #reset()}. */
    public static final String RESET_METHOD_NAME = "reset";

    /** This is the name of {@link #serializeAndReset(CoverageCounterLog)}. */
    public static final String SERIALIZE_AND_RESET_METHOD_NAME = "serializeAndReset";

    private String className;

    /**
     * Constructor of the {@link CounterContainer}.
     * 
     * @param className
     *            The name of the class, this counter container is inside. It is
     *            needed for comparation.
     */
    protected CounterContainer(String className) {
        this.className = className;
    }

    /**
     * Resets all the counters.
     */
    public abstract void reset();

    /**
     * Passes all non zero counters to the {@link CoverageCounterLog} and resets
     * the counters afterwards.
     * 
     * @param log
     *            The target {@link CoverageCounterLog}.
     */
    public abstract void serializeAndReset(CoverageCounterLog log);

    /**
     * Can compare this counter container with the other.
     * 
     * @param other
     *            The other object.
     * 
     * @return The integer as a compare result, telling whether this object is
     *         smaller (<0) equal (==0) or larger (>0) than the other object.
     */
    public int compareTo(Object other) {
        if (other instanceof CounterContainer) {
            CounterContainer otherCC = (CounterContainer) other;
            return this.className.compareTo(otherCC.className);
        }

        // should not be reached
        return this.getClass().getName().compareTo(other.getClass().getName());
    }
}
