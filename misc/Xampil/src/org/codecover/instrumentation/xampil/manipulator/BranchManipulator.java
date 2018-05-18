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

import java.io.PrintWriter;

import org.codecover.instrumentation.xampil.syntaxtree.IfStatement;
import org.codecover.instrumentation.xampil.syntaxtree.SwitchStatement;
import org.codecover.instrumentation.xampil.visitor.InstrumentationVisitor;

/**
 * This Manipulator is used for instrumentation of branches.
 * 
 * @see DummyBranchManipulator
 * @see DefaultBranchManipulator
 * 
 * @author Christoph Müller
 */
public interface BranchManipulator extends Manipulator {

    public void writeDeclarations(int branchCount);
    
    public void writeCoverageLogFileOutput(int branchCount);

    public void manipulateIf(IfStatement n, String ifBranchID);

    public void manipulateElse(IfStatement n, String elseBranchID,
            boolean isImplicit);

    public void manipulateCase(SwitchStatement n, String caseBranchID);

    public void manipulateDefault(SwitchStatement n, String defaultBranchID,
            boolean isImplicit);
}
