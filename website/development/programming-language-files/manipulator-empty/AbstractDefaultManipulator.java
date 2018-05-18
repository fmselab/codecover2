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
 * An abstract {@link Manipulator} which can be inherited by Default
 * Manipulators.
 * 
 * @see Manipulator
 * @see LongStatementManipulator
 * @see LongBranchManipulator
 * @see DefaultConditionManipulator
 * @see LongLoopManipulator
 * 
 * @author Christoph Müller
 */
public abstract class AbstractDefaultManipulator implements Manipulator {
    private PrintWriter writer = null;

    public void setWriter(PrintWriter writer) {
        this.writer = writer;
    }

    /**
     * @return The writer.
     */
    protected PrintWriter getWriter() {
        return this.writer;
    }
}
