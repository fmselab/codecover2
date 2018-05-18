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

package org.codecover.instrumentation.java15.counter;

import java.io.IOException;

import org.codecover.instrumentation.java15.manipulators.Manipulator;

/**
 * This class represents a manager for counters.<br>
 * <br>
 * This manager can write the declaration(s), write a block for
 * resetting all counters and write a block for serializing and resetting all
 * counters. This interface is used by the {@link CounterIDManager}.<br>
 * 
 * @see Manipulator requires this interface.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: CounterManager.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface CounterManager {
    /**
     * This is the {@link CounterIDManager}, that can be used for manipulating.<br>
     * <br>
     * E.g. {@link CounterIDManager#getInnerClassName()}
     * 
     * @param counterIDManager The new {@link CounterIDManager}.
     */
    public void setCounterIDManager(CounterIDManager counterIDManager);

    /**
     * Writes the declarations for the statement counter(s).
     * 
     * @throws IOException
     */
    public void writeDeclarations() throws IOException;

    /**
     * Writes the block to reset the statement counters.
     * 
     * @throws IOException
     */
    public void writeReset() throws IOException;

    /**
     * Writes the block to serialize and reset the statement counters.
     * 
     * @throws IOException
     */
    public void writeSerialzeAndReset() throws IOException;
}