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
import org.codecover.instrumentation.xampil.syntaxtree.WhileStatement;

/**
 * This is a {@link LoopManipulator}.
 * 
 * @see LoopManipulator
 * 
 * @author Christoph Müller
 */
public class DefaultLoopManipulator extends AbstractDefaultManipulator
        implements LoopManipulator {

    private static final String TEMP_COUNTER_SUFFIX = "_temp";

    public void writeDeclarations(int loopCount) {
        PrintWriter writer = super.getWriter();
        for (int i = 1; i <= loopCount; i++) {
            String thisLoopID = CounterIDProvider.generateStatementID(i);
            writer.printf("    INTEGER %1$s%2$s%3$s%n" +
                          "    INTEGER %1$s%4$s := 0%n" +
                          "    INTEGER %1$s%5$s := 0%n" +
                          "    INTEGER %1$s%6$s := 0%n",
                CounterIDProvider.VARIABLE_PREFIX,
                thisLoopID,
                TEMP_COUNTER_SUFFIX,
                CounterIDProvider.generateLoopSubIDZero(thisLoopID).replace('-', '_'),
                CounterIDProvider.generateLoopSubIDOne(thisLoopID).replace('-', '_'),
                CounterIDProvider.generateLoopSubIDAbove(thisLoopID).replace('-', '_'));
        }
        writer.printf("%n");
    }

    public void writeCoverageLogFileOutput(int loopCount) {
        PrintWriter writer = super.getWriter();
        for (int i = 1; i <= loopCount; i++) {
            String thisLoopID = CounterIDProvider.generateStatementID(i);
            String loopIDZero = CounterIDProvider.generateLoopSubIDZero(thisLoopID);
            String loopIDOne = CounterIDProvider.generateLoopSubIDOne(thisLoopID);
            String loopIDAbove = CounterIDProvider.generateLoopSubIDAbove(thisLoopID);
            writeFileStatement(writer, false, 
                    String.format("\"%s \" + %s%s",
                                  loopIDZero,
                                  CounterIDProvider.VARIABLE_PREFIX,
                                  loopIDZero.replace('-', '_')));
            writeFileStatement(writer, false, 
                    String.format("\"%s \" + %s%s",
                            loopIDOne,
                            CounterIDProvider.VARIABLE_PREFIX,
                            loopIDOne.replace('-', '_')));
            writeFileStatement(writer, false, 
                    String.format("\"%s \" + %s%s",
                            loopIDAbove,
                            CounterIDProvider.VARIABLE_PREFIX,
                            loopIDAbove.replace('-', '_')));
        }
    }

    public void manipulateBeforeWhile(WhileStatement n, String loopID) {
        super.getWriter().printf("    %s%s%s := 0%n",
                                 CounterIDProvider.VARIABLE_PREFIX,
                                 loopID,
                                 TEMP_COUNTER_SUFFIX);
    }

    public void manipulateInWhile(WhileStatement n, String loopID) {
        super.getWriter().printf("    %1$s%2$s%3$s := %1$s%2$s%3$s + 1%n",
                CounterIDProvider.VARIABLE_PREFIX,
                loopID,
                TEMP_COUNTER_SUFFIX);
    }

    public void manipulateAfterWhile(WhileStatement n, String loopID) {
        super.getWriter().printf("    SWITCH %1$s%2$s%3$s%n" +
        		         "        CASE 0 : %1$s%4$s := %1$s%4$s + 1%n" +
        		         "        ENDCASE%n" +
        		         "        CASE 1 : %1$s%5$s := %1$s%5$s + 1%n" +
        		         "        ENDCASE%n" +
        		         "        DEFAULT : %1$s%6$s := %1$s%6$s + 1%n" +
        		         "        ENDCASE%n" +
        		         "    ENDSWITCH%n",
                       /* 1$ */  CounterIDProvider.VARIABLE_PREFIX,
                       /* 2$ */  loopID,
                       /* 3$ */  TEMP_COUNTER_SUFFIX,
                       /* 4$ */  CounterIDProvider.generateLoopSubIDZero(loopID).replace('-', '_'),
                       /* 5$ */  CounterIDProvider.generateLoopSubIDOne(loopID).replace('-', '_'),
                       /* 6$ */  CounterIDProvider.generateLoopSubIDAbove(loopID).replace('-', '_'));
    }
}
