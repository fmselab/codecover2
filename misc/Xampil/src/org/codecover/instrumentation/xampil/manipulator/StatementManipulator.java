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

import org.codecover.instrumentation.xampil.CounterIDProvider;
import org.codecover.instrumentation.xampil.parser.InstrumentableItemCounter;
import org.codecover.instrumentation.xampil.parser.XampilParser;
import org.codecover.instrumentation.xampil.syntaxtree.Statement;
import org.codecover.instrumentation.xampil.visitor.InstrumentationVisitor;

/**
 * This Manipulator is used for instrumentation of statements.
 * 
 * @see DummyStatementManipulator
 * @see DefaultStatementManipulator
 * 
 * @author Christoph Müller
 */
public interface StatementManipulator extends Manipulator {

    /**
     * Tells the {@link Manipulator} to write all required declarations for the
     * counters now.
     * 
     * @param statementCount
     *                The number of statements found during
     *                {@link XampilParser#CompilationUnit(InstrumentableItemCounter)}.
     */
    public void writeDeclarations(int statementCount);

    /**
     * Write out the FILE statements to push the counters into a coverage log
     * file.<br>
     * <br>
     * Use
     * {@link InstrumentationVisitor#writeFileStatement(PrintWriter, boolean, CharSequence)}
     * for this purpose.
     * 
     * @param statementCount
     *                The number of statements found during
     *                {@link XampilParser#CompilationUnit(InstrumentableItemCounter)}.
     */
    public void writeCoverageLogFileOutput(int statementCount);

    public void manipulate(Statement n, String statementID);
}
