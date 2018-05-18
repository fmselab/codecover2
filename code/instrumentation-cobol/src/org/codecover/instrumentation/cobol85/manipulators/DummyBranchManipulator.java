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

import org.codecover.instrumentation.cobol85.syntaxtree.NodeSequence;
import org.codecover.instrumentation.cobol85.visitor.Visitor;

/**
 * This is a dummy implementation of the branch manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: DummyBranchManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public class DummyBranchManipulator implements BranchManipulator {

    public void generateBranchCounter(int programUnit, PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateElseBranch(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateEndEvaluate(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateEndIf(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateWhenOtherBranch(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void manipulate(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateNextSentenceGoTo(Visitor visitor, NodeSequence nodeSequence, PrintWriter printWriter) {
        // The dummy just visit the next sentence node
        nodeSequence.accept(visitor);
    }

    public void generateNextSentenceMark(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void resetNextSentenceMark() {
        // The dummy does not write
    }
}
