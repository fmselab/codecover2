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

package org.codecover.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;

/**
 * This is an interface for loop manipulators. A loop manipulator 
 * should be used to insert counters into the source code. The counter 
 * variable declaration is also placed in a statement manipulator.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: LoopManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public interface LoopManipulator {

    /**
     * Generates all loop counter that appears in the given program unit.
     * Writes results to the output writer.
     * 
     * @param programUnit the active program unit
     * @param printWriter the output writer
     */
    void generateLoopCounter(int programUnit, PrintWriter printWriter);

    /**
     * Generates all auxiliary loop counter that appears in the given program unit.
     * Writes results to the output writer.
     * 
     * @param programUnit the active program unit
     * @param printWriter the output writer
     */
    void generateAuxiliaryLoopCounter(int programUnit, PrintWriter printWriter);

    /**
     * Generates auxiliary loop counter head.
     * 
     * @param printWriter the output writer
     */
    void generateAuxiliaryLoopCounterHeader(PrintWriter printWriter);

    /**
     * Manipulates the source code for loop coverage criteria.
     * 
     * @param printWriter the output writer
     * @param loopCounter the loop number
     */
    void manipulate(PrintWriter printWriter, int loopCounter);

    /**
     * Manipulates the source code for loop coverage criteria.
     * 
     * @param printWriter the output writer
     * @param loopCounter the loop number
     */
    void increment(PrintWriter printWriter, int loopCounter);

    /**
     * Manipulates the source code for loop coverage criteria.
     * 
     * @param printWriter the output writer
     * @param loopCounter the loop number
     */
    void evaluate(PrintWriter printWriter, int loopCounter);
}
