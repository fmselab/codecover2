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
import org.codecover.instrumentation.xampil.syntaxtree.Statement;

import static org.codecover.instrumentation.xampil.visitor.InstrumentationVisitor.writeFileStatement;

/**
 * This is a {@link StatementManipulator}.
 * 
 * @see StatementManipulator
 * 
 * @author Christoph Müller
 */
public class DefaultStatementManipulator extends AbstractDefaultManipulator
        implements StatementManipulator {

    public void writeDeclarations(int statementCount) {
        PrintWriter writer = super.getWriter();
        for (int i = 1; i <= statementCount; i++) {
            writer.printf("    INTEGER %s%s := 0%n",
                    CounterIDProvider.VARIABLE_PREFIX,
                    CounterIDProvider.generateStatementID(i));
        }
        writer.printf("%n");
    }
    
    public void writeCoverageLogFileOutput(int statementCount) {
        for (int i = 1; i <= statementCount; i++) {
            writeFileStatement(super.getWriter(), false, 
                    String.format("\"%2$s \" + %1$s%2$s",
                                  CounterIDProvider.VARIABLE_PREFIX,
                                  CounterIDProvider.generateStatementID(i)));
        }
    }

    public void manipulate(Statement n, String statementID) {
        super.getWriter().printf("%1$s%2$s := %1$s%2$s + 1%n    ",
                                 CounterIDProvider.VARIABLE_PREFIX,
                                 statementID);
    }
}
