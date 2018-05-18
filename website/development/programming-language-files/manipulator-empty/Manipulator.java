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

/**
 * This is an interface for manipulators.<br>
 * <br>
 * It allows to set a {@link PrintWriter}.
 * 
 * @see StatementManipulator
 * @see BranchManipulator
 * @see ConditionManipulator
 * @see LoopManipulator
 * @see AbstractDummyManipulator
 * @see AbstractDefaultManipulator
 * 
 * @author Christoph Müller
 */
public interface Manipulator {
    /**
     * Sets the writer for incrementing statements and modifications.
     * 
     * @param writer
     */
    public void setWriter(PrintWriter writer);
}
