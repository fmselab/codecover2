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

import org.codecover.instrumentation.java15.syntaxtree.DoStatement;
import org.codecover.instrumentation.java15.syntaxtree.ForStatement;
import org.codecover.instrumentation.java15.syntaxtree.WhileStatement;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: DummyLoopManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DummyLoopManipulator extends AbstractDummyManipulator
        implements LoopManipulator {

    public void manipulateBefore(ForStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateInner(ForStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateAfter(ForStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateBefore(WhileStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateInner(WhileStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateAfter(WhileStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateBefore(DoStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateInner(DoStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateAfter(DoStatement n, String primaryLoopID) {
        // do not add anything;
    }
 }
