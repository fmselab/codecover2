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
 * This is a dummy implementation of the statement manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: DummyStatementManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public class DummyStatementManipulator implements StatementManipulator {

    public void generateEndAdd(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateEndCall(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateEndCompute(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateEndDelete(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateEndDivide(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateEndEvaluate(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateEndIf(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateEndMultiply(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateEndRead(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateEndRewrite(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateEndSearch(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateEndStart(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateEndString(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateEndSubtract(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateEndUnstring(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateEndWrite(PrintWriter writer) {
        // The dummy does not write
    }

    public void generateStatementCounter(int programUnit, PrintWriter printWriter) {
        // The dummy does not write
    }

    public void manipulate(PrintWriter printWriter) {
        // The dummy does not write
    }

}
