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

import static org.codecover.instrumentation.java15.counter.CounterIDManager.LOG_NAME;
import static org.codecover.instrumentation.java15.counter.CounterIDManager.getNumberFromStatementID;
import static org.codecover.instrumentation.java15.visitor.TreeDumperWithException.LINE_SEPARATOR;
import static org.codecover.model.utils.criteria.QMOCoverage.ID_PREFIX;
import static org.codecover.model.utils.criteria.QMOCoverage.ID_SUFFIX_ONE;
import static org.codecover.model.utils.criteria.QMOCoverage.ID_SUFFIX_ZERO;

import java.io.IOException;
import java.io.Writer;

import org.codecover.instrumentation.java15.counter.CounterManager;
import org.codecover.instrumentation.java15.syntaxtree.ConditionalOrExpression;
import org.codecover.instrumentation.java15.visitor.TreeDumperWithException;
import org.codecover.instrumentation.measurement.CoverageCounterLog;
/**
 * 
 * @see StatementManipulator
 * 
 * @author Christoph Müller, Stefan Franke
 * @version 1.0 ($Id: ArrayStatementManipulator.java 22 2008-05-25 20:08:53Z ahija $)
 * 
 */
public class ArrayQMOManipulator extends AbstractDefaultManipulator
implements QMOManipulator, CounterManager {

    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for Question mark operator ? :
    //
    // /////////////////////////////////////////////////////////////////////////

    private static final String ARRAY_NAME = "qmo";

    private static final String COUNTER = "%1$s." + ARRAY_NAME + "[%2$d]";

    private static final String COUNTER_DECLARATION = "public static long[] "
        + ARRAY_NAME + " = new long[%1$d];";

    private static final String COUNTER_FOR_LOOP = "for (int i = 0; i < %1$d; i++)";

    private static final String COUNTER_RESET = ARRAY_NAME + "[i] = 0L;";

    private static final String COUNTER_SERIALIZE_IF_ZERO = "if ("
        + ARRAY_NAME + "[i * 2] != 0L)";
   
    private static final String COUNTER_SERIALIZE_PASS_COUNTER_ZERO = LOG_NAME
        + "." + CoverageCounterLog.PASS_COUNTER_METHOD_NAME + "(\""
        + ID_PREFIX + "\" + i + \"" + ID_SUFFIX_ZERO + "\", " + ARRAY_NAME
        + "[i * 2]);";

    
    private static final String COUNTER_SERIALIZE_IF_ONE = "if ("
        + ARRAY_NAME + "[i * 2 + 1] != 0L)";
   
    private static final String COUNTER_SERIALIZE_PASS_COUNTER_ONE = LOG_NAME
        + "." + CoverageCounterLog.PASS_COUNTER_METHOD_NAME + "(\""
        + ID_PREFIX + "\" + i + \"" + ID_SUFFIX_ONE + "\", " + ARRAY_NAME
        + "[i * 2 + 1]);";
    
    private static final String COUNTER_RESET_ZERO = ARRAY_NAME
    + "[i * 2] = 0L;";

    private static final String COUNTER_RESET_ONE = ARRAY_NAME
    + "[i * 2 + 1] = 0L;";

    
    // /////////////////////////////////////////////////////////////////////////
    //
    // private members
    //
    // /////////////////////////////////////////////////////////////////////////

    private int maxStatementID;

    // /////////////////////////////////////////////////////////////////////////
    //
    // constructor, public and private methods
    //
    // /////////////////////////////////////////////////////////////////////////

    /**
     * Constructor for a new {@link ArrayStatementManipulator}.
     */
    public ArrayQMOManipulator() {
        this.maxStatementID = 0;
    }

    // /////////////////////////////////////////////////////////////////////////
    //
    // methods for the interface StatementManipulator
    //
    // /////////////////////////////////////////////////////////////////////////

    /**
     * Always true.
     * 
     * @return true;
     */
    public boolean requiresBlockExpansionsForBranches() {
        return true;
    }

    public void manipulateBefore(ConditionalOrExpression n, String qmpID)
    throws IOException {
        Writer writer = super.getWriter();

        writer.write("(");
    }

    public void manipulateAfter(ConditionalOrExpression n, String qmpID)
    throws IOException {
        Writer writer = super.getWriter();

        int ID = getNumberFromStatementID(qmpID);

        String counterVariable1 = String.format(COUNTER,
                super.getCounterIDManager().getInnerClassName(),
                new Integer(ID * 2));

        String counterVariable2 = String.format(COUNTER,
                super.getCounterIDManager().getInnerClassName(),
                new Integer(ID * 2 + 1));

        this.maxStatementID = Math.max(this.maxStatementID, ID);
        
        
        writer.write(" ? true | " + counterVariable1 + " == " + counterVariable1 + "++ : false  & " + counterVariable2 + " != " + counterVariable2 + "++) ");
    }


    public void writeDeclarations() throws IOException {
        Writer writer = super.getWriter();

        writer.write("    ");
        writer.write(String.format(COUNTER_DECLARATION,
                new Integer((this.maxStatementID + 1) * 2)));
        writer.write(LINE_SEPARATOR);
    }

    public void writeReset() throws IOException {
        Writer writer = super.getWriter();

        writer.write("      ");
        writer.write(String.format(COUNTER_FOR_LOOP, new Integer((this.maxStatementID + 1) * 2)));
        writer.write(" {");
        writer.write(LINE_SEPARATOR);
        writer.write("        ");
        writer.write(COUNTER_RESET);
        writer.write(LINE_SEPARATOR);
        writer.write("      }");
        writer.write(LINE_SEPARATOR);
    }

    public void writeSerialzeAndReset() throws IOException {
        Writer writer = super.getWriter();

        writer.write("      ");
        writer.write(String.format(COUNTER_FOR_LOOP, new Integer(
        		this.maxStatementID + 1)));
        writer.write(" {");
        writer.write(LINE_SEPARATOR);

        // 
        writer.write("        ");
        writer.write(COUNTER_SERIALIZE_IF_ZERO);
        writer.write(" {");
        writer.write(LINE_SEPARATOR);
        writer.write("          ");
        writer.write(COUNTER_SERIALIZE_PASS_COUNTER_ZERO);
        writer.write(LINE_SEPARATOR);
        writer.write("          ");
        writer.write(COUNTER_RESET_ZERO);
        writer.write(LINE_SEPARATOR);
        writer.write("        }");
        writer.write(LINE_SEPARATOR);

        // 
        writer.write("        ");
        writer.write(COUNTER_SERIALIZE_IF_ONE);
        writer.write(" {");
        writer.write(LINE_SEPARATOR);
        writer.write("          ");
        writer.write(COUNTER_SERIALIZE_PASS_COUNTER_ONE);
        writer.write(LINE_SEPARATOR);
        writer.write("          ");
        writer.write(COUNTER_RESET_ONE);
        writer.write(LINE_SEPARATOR);
        writer.write("        }");
        writer.write(LINE_SEPARATOR);

        // "}" of for loop
        writer.write("      }");
        writer.write(LINE_SEPARATOR);
     }


    public boolean requiresBlockExpansionsForLoops() {
        // TODO Automatisch erstellter Methoden-Stub
        return false;
    }
}