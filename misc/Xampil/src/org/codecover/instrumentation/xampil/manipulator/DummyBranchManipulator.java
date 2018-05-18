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

import org.codecover.instrumentation.xampil.syntaxtree.IfStatement;
import org.codecover.instrumentation.xampil.syntaxtree.SwitchStatement;


/**
 * @author Christoph Müller
 */
public class DummyBranchManipulator extends AbstractDummyManipulator implements
        BranchManipulator {

    public void writeDeclarations(int branchCount) {
        // nothing to declare
    }

    public void writeCoverageLogFileOutput(int branchCount) {
        // nothing to write
    }

    public void manipulateIf(IfStatement n, String ifBranchID) {
        // nothing to instrument
    }

    public void manipulateElse(IfStatement n, String elseBranchID, boolean isImplicit) {
        // nothing to instrument
    }

    public void manipulateCase(SwitchStatement n, String caseBranchID) {
        // nothing to instrument
    }

    public void manipulateDefault(SwitchStatement n, String defaultBranchID,
            boolean isImplicit) {
        // nothing to instrument
    }
}
