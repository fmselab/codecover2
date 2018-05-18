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
 * This is a dummy implementation of the loop manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: DummyLoopManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public class DummyLoopManipulator implements LoopManipulator {

    public void generateLoopCounter(int programUnit, PrintWriter printWriter) {
        // The dummy does not write
    }

    public void evaluate(PrintWriter printWriter, int loopCounter) {
        // The dummy does not write
    }

    public void generateAuxiliaryLoopCounter(int programUnit, PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateAuxiliaryLoopCounterHeader(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void increment(PrintWriter printWriter, int loopCounter) {
        // The dummy does not write
    }

    public void manipulate(PrintWriter printWriter, int loopCounter) {
        // The dummy does not write
    }

}
