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

import org.codecover.instrumentation.xampil.syntaxtree.WhileStatement;


/**
 * @author Christoph Müller
 */
public class DummyLoopManipulator extends AbstractDummyManipulator
        implements LoopManipulator {

    public void writeDeclarations(int loopCount) {
        // nothing to declare
    }

    public void writeCoverageLogFileOutput(int loopCount) {
        // nothing to write
    }

    public void manipulateBeforeWhile(WhileStatement n, String loopID) {
        // nothing to manipulate
    }

    public void manipulateInWhile(WhileStatement n, String loopID) {
        // nothing to manipulate
    }
    
    public void manipulateAfterWhile(WhileStatement n, String loopID) {
        // nothing to manipulate
    }
}
