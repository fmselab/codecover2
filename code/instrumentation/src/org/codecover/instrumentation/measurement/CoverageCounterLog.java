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

package org.codecover.instrumentation.measurement;

/**
 * This is an interface where coverage counters can be written to.<br>
 * <br>
 * For example, this interface is expanded to the {@link CoverageResultLog}<br>
 * <br>
 * This log file accepts the the start of a section and long counters.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: CoverageCounterLog.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface CoverageCounterLog {
    /** This is the name of {@link #passCounter(String, long)}. */
    public static final String PASS_COUNTER_METHOD_NAME = "passCounter";

    /** This is the name of {@link #startNamedSection(String)}. */
    public static final String START_SECTION_METHOD_NAME = "startNamedSection";

    /**
     * Notifies the start of a section.
     * 
     * @param sectionName
     *            The name of the section.
     */
    public void startNamedSection(String sectionName);

    /**
     * Passes the ID and the value of a counter.
     * 
     * @param counterID
     *            The ID of the counter
     * @param counterValue
     *            The value of the counter.
     */
    public void passCounter(String counterID, long counterValue);
}
