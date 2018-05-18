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

import org.codecover.instrumentation.java15.syntaxtree.DoStatement;
import org.codecover.instrumentation.java15.syntaxtree.ForStatement;
import org.codecover.instrumentation.java15.syntaxtree.WhileStatement;
import org.codecover.instrumentation.java15.visitor.InstrumentationVisitor;

/**
 * This Manipulator is used for instrumentation of loops.<br>
 * </br> A object of this interface is called by the
 * {@link InstrumentationVisitor}. This can either be an
 * {@link DummyLoopManipulator} or an {@link LongLoopManipulator}.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: LoopManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @see DummyLoopManipulator
 * @see ArrayLoopManipulator
 */
@SuppressWarnings("all")
public interface LoopManipulator extends Manipulator {

    public void manipulateBefore(ForStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateInner(ForStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateAfter(ForStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateBefore(WhileStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateInner(WhileStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateAfter(WhileStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateBefore(DoStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateInner(DoStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateAfter(DoStatement n, String primaryLoopID)
            throws IOException;
}
