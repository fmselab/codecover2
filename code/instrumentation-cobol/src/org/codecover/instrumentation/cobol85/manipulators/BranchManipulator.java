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
import org.codecover.instrumentation.cobol85.visitor.InstrumentationVisitor;
import org.codecover.instrumentation.cobol85.visitor.Visitor;

/**
 * This is an interface for branch manipulators. A branch manipulator 
 * should be used to insert counters into the source code. The counter 
 * variable declaration is also placed in a statement manipulator.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: BranchManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public interface BranchManipulator {

    /**
     * Generates all branch counter that appears in the given program unit.
     * Writes results to the output writer.
     * 
     * @param programUnit the active program unit
     * @param printWriter the output writer
     */
    void generateBranchCounter(int programUnit, PrintWriter printWriter);

    /**
     * Generates an else-branch which includes a branch counter.
     * 
     * @param printWriter the output writer
     */
    void generateElseBranch(PrintWriter printWriter);

    /**
     * Generates the end evaluate keyword.
     * 
     * @param printWriter the output writer
     */
    void generateEndEvaluate(PrintWriter printWriter);

    /**
     * Generates the end if keyword.
     * 
     * @param printWriter the output writer
     */
    void generateEndIf(PrintWriter printWriter);

    /**
     * Generates an when-other-branch which includes a branch counter.
     * 
     * @param printWriter the output writer
     */
    void generateWhenOtherBranch(PrintWriter printWriter);

    /**
     * Generates the goto statement to jump to next sentence.
     * 
     * @param visitor the visitor
     * @param nodeSequence the {@link NodeSequence}
     * @param printWriter the output writer
     */
    void generateNextSentenceGoTo(Visitor visitor, NodeSequence nodeSequence, PrintWriter printWriter);

    /**
     * Generates the marker after next sentence starts.
     * 
     * @param printWriter the output writer
     */
    void generateNextSentenceMark(PrintWriter printWriter);

    /**
     * Manipulates the source code for branch coverage criteria.
     * 
     * @param printWriter the output writer
     */
    void manipulate(PrintWriter printWriter);

    /**
     * Resets the next sentence mark.
     */
    void resetNextSentenceMark();

}
