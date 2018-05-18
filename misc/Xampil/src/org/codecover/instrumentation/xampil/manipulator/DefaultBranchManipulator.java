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

import static org.codecover.instrumentation.xampil.visitor.InstrumentationVisitor.writeFileStatement;

import java.io.PrintWriter;

import org.codecover.instrumentation.xampil.CounterIDProvider;
import org.codecover.instrumentation.xampil.syntaxtree.IfStatement;
import org.codecover.instrumentation.xampil.syntaxtree.SwitchStatement;

/**
 * This is a {@link BranchManipulator}.
 * 
 * @see BranchManipulator
 * 
 * @author Christoph Müller
 */
public class DefaultBranchManipulator extends AbstractDefaultManipulator
        implements BranchManipulator {

    public void writeDeclarations(int branchCount) {
        PrintWriter writer = super.getWriter();
        for (int i = 1; i <= branchCount; i++) {
            writer.printf("    INTEGER %s%s := 0%n",
                    CounterIDProvider.VARIABLE_PREFIX,
                    CounterIDProvider.generateBranchID(i));
        }
        writer.printf("%n");
    }

    public void writeCoverageLogFileOutput(int branchCount) {
        for (int i = 1; i <= branchCount; i++) {
            writeFileStatement(super.getWriter(), false, 
                    String.format("\"%2$s \" + %1$s%2$s",
                                  CounterIDProvider.VARIABLE_PREFIX,
                                  CounterIDProvider.generateBranchID(i)));
        }
    }

    public void manipulateIf(IfStatement n, String ifBranchID) {
        super.getWriter().printf("%1$s%2$s := %1$s%2$s + 1%n    ",
                CounterIDProvider.VARIABLE_PREFIX,
                ifBranchID);
    }
    
    public void manipulateElse(IfStatement n, String elseBranchID, boolean isImplicit) {
        PrintWriter writer = super.getWriter();
        if (isImplicit) {
            writer.printf("ELSE%n");
        }
        writer.printf("%1$s%2$s := %1$s%2$s + 1%n        ",
                CounterIDProvider.VARIABLE_PREFIX,
                elseBranchID);
    }

    public void manipulateCase(SwitchStatement n, String caseBranchID) {
        super.getWriter().printf("%1$s%2$s := %1$s%2$s + 1%n    ",
                CounterIDProvider.VARIABLE_PREFIX,
                caseBranchID);
    }

    public void manipulateDefault(SwitchStatement n, String defaultBranchID,
            boolean isImplicit) {
        PrintWriter writer = super.getWriter();
        if (isImplicit) {
            writer.printf("DEFAULT :%n");
        }
        writer.printf("%1$s%2$s := %1$s%2$s + 1%n        ",
                CounterIDProvider.VARIABLE_PREFIX,
                defaultBranchID);
        if (isImplicit) {
            writer.printf("ENDCASE%n    ");
        }
    }
}
