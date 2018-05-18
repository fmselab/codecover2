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

import org.codecover.instrumentation.java15.counter.CounterIDManager;
import org.codecover.instrumentation.java15.counter.CounterManager;
import org.codecover.instrumentation.java15.visitor.TreeDumperWithException;

/**
 * An abstract {@link Manipulator} which can be inherited by Dummy Manipulators.<br>
 * <br>
 * All required methods do nothing and all boolean functions return false.
 * 
 * @see Manipulator
 * @see DummyStatementManipulator
 * @see DummyBranchManipulator
 * @see DummyConditionManipulator
 * @see DummyLoopManipulator
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: AbstractDummyManipulator.java 22 2008-05-25 20:08:53Z ahija $)
 */
public abstract class AbstractDummyManipulator implements Manipulator,
        CounterManager {

    public void setTreeDumper(TreeDumperWithException treeDumper) {
        // we needn't save it
    }

    /**
     * Always false.
     * 
     * @return false
     */
    public boolean requiresBlockExpansionsForBranches() {
        return false;
    }

    /**
     * Always false.
     * 
     * @return false
     */
    public boolean requiresBlockExpansionsForLoops() {
        return false;
    }

    public void setCounterIDManager(CounterIDManager counterIDManager) {
        // we needn't save it
    }

    /**
     * Nothing to be reinitialized.
     */
    public void reinit() {
        // has to do nothing
    }

    /**
     * Nothing to write.
     */
    public void writeDeclarations() {
        // has to do nothing
    }

    /**
     * Nothing to write.
     */
    public void writeReset() throws IOException {
        // has to do nothing
    }

    /**
     * Nothing to write.
     */
    public void writeSerialzeAndReset() throws IOException {
        // has to do nothing
    }
}
