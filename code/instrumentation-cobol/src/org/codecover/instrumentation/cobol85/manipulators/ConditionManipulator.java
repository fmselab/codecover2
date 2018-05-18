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
import java.util.List;

import org.codecover.instrumentation.booleanterms.InstrBasicBooleanTerm;

/**
 * This is an interface for condition manipulators. A condition manipulator 
 * should be used to insert counters into the source code. The counter 
 * variable declaration is also placed in a statement manipulator.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: ConditionManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public interface ConditionManipulator {

    /**
     * Generates all condition counter that appears in the given program unit.
     * Writes results to the output writer.
     * 
     * @param programUnit the active program unit
     * @param printWriter the output writer
     */
    void generateConditionCounter(int programUnit, PrintWriter printWriter);

    /**
     * Manipulates the source code for condition coverage criteria.
     * 
     * @param printWriter the output writer
     * @param basicBooleanTerms a queue of basic boolean terms
     */
    void manipulate(PrintWriter printWriter, List<InstrBasicBooleanTerm> basicBooleanTerms);

    /**
     * Manipulates the source code for condition coverage criteria.
     * 
     * @param printWriter the output writer
     * @param basicBooleanTerms a queue of basic boolean terms
     * @param conditionCounter the condition number
     */
    void manipulate(PrintWriter printWriter, List<InstrBasicBooleanTerm> basicBooleanTerms, int conditionCounter);

}
