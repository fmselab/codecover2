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
 * This is an interface for statement manipulators. A statement manipulator 
 * should be used to insert counters into the source code. The counter 
 * variable declaration is also placed in a statement manipulator.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: StatementManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public interface StatementManipulator {

    /**
     * Manipulates the source code for statement coverage criteria.
     * 
     * @param printWriter the output writer
     */
    void manipulate(PrintWriter printWriter);

    /**
     * Generates all statement counter that appears in the given program unit.
     * Writes results to the output writer.
     * 
     * @param programUnit the active program unit
     * @param printWriter the output writer
     */
    void generateStatementCounter(int programUnit, PrintWriter printWriter);
    
    /**
     * Generates the end-add keyword.
     * 
     * @param writer the output writer
     */
    void generateEndAdd(PrintWriter writer);
    
    /**
     * Generates the end-call keyword.
     * 
     * @param writer the output writer
     */
    void generateEndCall(PrintWriter writer);
    
    /**
     * Generates the end-compute keyword.
     * 
     * @param writer the output writer
     */
    void generateEndCompute(PrintWriter writer);
    
    /**
     * Generates the end-delete keyword.
     * 
     * @param writer the output writer
     */
    void generateEndDelete(PrintWriter writer);
    
    /**
     * Generates the end-divide keyword.
     * 
     * @param writer the output writer
     */
    void generateEndDivide(PrintWriter writer);
    
    /**
     * Generates the end-evaluate keyword.
     * 
     * @param writer the output writer
     */
    void generateEndEvaluate(PrintWriter writer);
    
    /**
     * Generates the end-if keyword.
     * 
     * @param writer the output writer
     */
    void generateEndIf(PrintWriter writer);
    
    /**
     * Generates the end-multiply keyword.
     * 
     * @param writer the output writer
     */
    void generateEndMultiply(PrintWriter writer);
    
    /**
     * Generates the end-read keyword.
     * 
     * @param writer the output writer
     */
    void generateEndRead(PrintWriter writer);
    
    /**
     * Generates the end-rewrite keyword.
     * 
     * @param writer the output writer
     */
    void generateEndRewrite(PrintWriter writer);
    
    /**
     * Generates the end-search keyword.
     * 
     * @param writer the output writer
     */
    void generateEndSearch(PrintWriter writer);
    
    /**
     * Generates the end-start keyword.
     * 
     * @param writer the output writer
     */
    void generateEndStart(PrintWriter writer);
    
    /**
     * Generates the end-string keyword.
     * 
     * @param writer the output writer
     */
    void generateEndString(PrintWriter writer);
    
    /**
     * Generates the end-subtract keyword.
     * 
     * @param writer the output writer
     */
    void generateEndSubtract(PrintWriter writer);
    
    /**
     * Generates the end-unstring keyword.
     * 
     * @param writer the output writer
     */
    void generateEndUnstring(PrintWriter writer);
    
    /**
     * Generates the end-write keyword.
     * 
     * @param writer the output writer
     */
    void generateEndWrite(PrintWriter writer);

}
