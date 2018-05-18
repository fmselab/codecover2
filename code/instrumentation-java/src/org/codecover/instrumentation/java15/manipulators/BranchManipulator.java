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

package org.codecover.instrumentation.java15.manipulators;

import java.io.IOException;

import org.codecover.instrumentation.java15.syntaxtree.IfStatement;
import org.codecover.instrumentation.java15.syntaxtree.SwitchLabel;
import org.codecover.instrumentation.java15.syntaxtree.SwitchStatement;
import org.codecover.instrumentation.java15.syntaxtree.TryStatement;
import org.codecover.instrumentation.java15.syntaxtree.Type;
import org.codecover.instrumentation.java15.visitor.InstrumentationVisitor;

/**
 * This Manipulator is used for instrumentation of branches.<br>
 * </br> A object of this interface is called by the
 * {@link InstrumentationVisitor}. This can either be an
 * {@link DummyBranchManipulator} or an {@link LongBranchManipulator}.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: BranchManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @see DummyBranchManipulator
 * @see ArrayBranchManipulator
 */
@SuppressWarnings("all")
public interface BranchManipulator extends Manipulator {

    public void manipulateIf(IfStatement n, String ifBranchID)
            throws IOException;

    public void manipulateElse(IfStatement n, String elseBranchID,
            boolean implicitElse) throws IOException;

    public void manipulateSwitchCase(SwitchLabel switchLabel,
            String caseBranchID) throws IOException;

    public void manipulateSwitchDefault(SwitchStatement n,
            String defaultBranchID, boolean implicitDefault) throws IOException;

    public void manipulateHelperDeclaration(TryStatement n, String branchID,
            String tryBranchID) throws IOException;

    public void manipulateTry(TryStatement n, String branchID,
            String tryBranchID) throws IOException;

    public void manipulateCatch(TryStatement n, Type catchType,
            String catchBranchId, String tryBranchID) throws IOException;

    public void manipulateFinally(TryStatement n, String branchID,
            String tryBranchID, boolean implicitFinally) throws IOException;
}
