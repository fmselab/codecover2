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
 * This Manipulator is used for instrumentation of loops.
 * 
 * @see DummyLoopManipulator
 * @see LongLoopManipulator
 * 
 * @author Christoph Müller
 */
public interface LoopManipulator extends Manipulator {

    public void writeDeclarations(int loopCount);
    
    public void writeCoverageLogFileOutput(int loopCount);

    public void manipulateBeforeWhile(WhileStatement n, String loopID);

    public void manipulateInWhile(WhileStatement n, String loopID);

    public void manipulateAfterWhile(WhileStatement n, String loopID);
}
