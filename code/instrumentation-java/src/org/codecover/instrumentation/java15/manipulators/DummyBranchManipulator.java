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

import org.codecover.instrumentation.java15.syntaxtree.IfStatement;
import org.codecover.instrumentation.java15.syntaxtree.SwitchLabel;
import org.codecover.instrumentation.java15.syntaxtree.SwitchStatement;
import org.codecover.instrumentation.java15.syntaxtree.TryStatement;
import org.codecover.instrumentation.java15.syntaxtree.Type;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: DummyBranchManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DummyBranchManipulator extends AbstractDummyManipulator implements
        BranchManipulator {

    public void manipulateIf(IfStatement n, String ifBranchID) {
        // do not add anything;
    }

    public void manipulateElse(IfStatement n, String elseBranchID,
            boolean implicitElse) {
        // do not add anything;
    }

    public void manipulateSwitchCase(SwitchLabel switchLabel,
            String caseBranchID) {
        // do not add anything;
    }

    public void manipulateSwitchDefault(SwitchStatement n,
            String defaultBranchID, boolean implicitDefault) {
        // do not add anything;
    }

    public void manipulateHelperDeclaration(TryStatement n, String branchID,
            String tryBranchID) {
        // do not add anything;
    }

    public void manipulateTry(TryStatement n, String branchID,
            String tryBranchID) {
        // do not add anything;
    }

    public void manipulateCatch(TryStatement n, Type catchType,
            String catchBranchId, String tryBranchID) {
        // do not add anything;
    }

    public void manipulateFinally(TryStatement n, String branchID,
            String tryBranchID, boolean implicitFinally) {
        // do not add anything;
    }
}
