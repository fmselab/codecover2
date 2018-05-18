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

package org.codecover.instrumentation.xampil.manipulator;

import java.util.List;

import org.codecover.instrumentation.booleanterms.InstrBasicBooleanTerm;
import org.codecover.instrumentation.xampil.parser.InstrumentableItemCounter;

/**
 * @author Stefan Franke
 */
public class DummyConditionManipulator extends AbstractDummyManipulator
        implements ConditionManipulator {

    public void manipulate(String ifConditionID, List<InstrBasicBooleanTerm> termList) {
        // nothing to instrument
    }
    
    public void writeCoverageLogFileOutput(InstrumentableItemCounter counter) {
        // nothing to write
    }

    public void writeDeclarations(InstrumentableItemCounter counter) {
        // nothing to instrument
    }
}
