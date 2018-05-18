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
 * This is a dummy implementation of the condition manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: DummyConditionManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public class DummyConditionManipulator implements ConditionManipulator {

    public void generateConditionCounter(int programUnit, PrintWriter printWriter) {
        // The dummy does not write
    }

    public void manipulate(PrintWriter printWriter, List<InstrBasicBooleanTerm> basicBooleanTerms) {
        // The dummy does not write
    }

    public void manipulate(PrintWriter printWriter, List<InstrBasicBooleanTerm> basicBooleanTerms, int conditionCounter) {
        // The dummy does not write
    }

}
