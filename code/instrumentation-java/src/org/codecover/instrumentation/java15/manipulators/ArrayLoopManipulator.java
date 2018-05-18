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
import static org.codecover.instrumentation.java15.counter.CounterIDManager.getNumberFromPrimaryLoopID;
import static org.codecover.instrumentation.java15.visitor.TreeDumperWithException.LINE_SEPARATOR;
import static org.codecover.model.utils.criteria.LoopCoverage.ID_PREFIX;
import static org.codecover.model.utils.criteria.LoopCoverage.ID_SUFFIX_ABOVE;
import static org.codecover.model.utils.criteria.LoopCoverage.ID_SUFFIX_ONE;
import static org.codecover.model.utils.criteria.LoopCoverage.ID_SUFFIX_ZERO;

import java.io.IOException;
import java.io.Writer;

import org.codecover.instrumentation.java15.counter.CounterManager;
import org.codecover.instrumentation.java15.syntaxtree.DoStatement;
import org.codecover.instrumentation.java15.syntaxtree.ForStatement;
import org.codecover.instrumentation.java15.syntaxtree.WhileStatement;
import org.codecover.instrumentation.java15.visitor.TreeDumperWithException;
import org.codecover.instrumentation.measurement.CoverageCounterLog;

/**
 * This is a {@link LoopManipulator} using a long array.
 * 
 * @see LoopManipulator
 * @author Christoph Müller
 * @version 1.0 ($Id: ArrayLoopManipulator.java 22 2008-05-25 20:08:53Z ahija $)
 */
public class ArrayLoopManipulator extends AbstractDefaultManipulator
        implements LoopManipulator, CounterManager {

    private static final String ARRAY_NAME = "loops";

    private static final String COUNTER_INCREMENTING = "%1$s." + ARRAY_NAME
            + "[%2$d]++;";

    private static final String COUNTER_DECREMENTING = "%1$s." + ARRAY_NAME
    + "[%2$d]--;";

    private static final String COUNTER_DECLARATION = "public static long[] "
            + ARRAY_NAME + " = new long[%1$d];";

    private static final String COUNTER_FOR_LOOP = "for (int i = 1; i <= %1$d; i++)";

    private static final String COUNTER_RESET = ARRAY_NAME + "[i] = 0L;";

    private static final String COUNTER_SERIALIZE_IF_ZERO = "if ("
            + ARRAY_NAME + "[i * 3 - 2] != 0L)";

    private static final String COUNTER_SERIALIZE_PASS_COUNTER_ZERO = LOG_NAME
            + "." + CoverageCounterLog.PASS_COUNTER_METHOD_NAME + "(\""
            + ID_PREFIX + "\" + i + \"" + ID_SUFFIX_ZERO + "\", " + ARRAY_NAME
            + "[i * 3 - 2]);";

    private static final String COUNTER_RESET_ZERO = ARRAY_NAME
            + "[i * 3 - 2] = 0L;";

    private static final String COUNTER_SERIALIZE_IF_ONE = "if ( " + ARRAY_NAME
            + "[i * 3 - 1] != 0L)";

    private static final String COUNTER_SERIALIZE_PASS_COUNTER_ONE = LOG_NAME
            + "." + CoverageCounterLog.PASS_COUNTER_METHOD_NAME + "(\""
            + ID_PREFIX + "\" + i + \"" + ID_SUFFIX_ONE + "\", " + ARRAY_NAME
            + "[i * 3 - 1]);";

    private static final String COUNTER_RESET_ONE = ARRAY_NAME
            + "[i * 3 - 1] = 0L;";

    private static final String COUNTER_SERIALIZE_IF_ABOVE = "if ( "
            + ARRAY_NAME + "[i * 3] != 0L)";

    private static final String COUNTER_SERIALIZE_PASS_COUNTER_ABOVE = LOG_NAME
            + "." + CoverageCounterLog.PASS_COUNTER_METHOD_NAME + "(\""
            + ID_PREFIX + "\" + i + \"" + ID_SUFFIX_ABOVE + "\", "
            + ARRAY_NAME + "[i * 3]);";

    private static final String COUNTER_RESET_ABOVE = ARRAY_NAME
            + "[i * 3] = 0L;";

    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for the loop helper
    //
    // /////////////////////////////////////////////////////////////////////////

    private static final String LOOP_HELPER_ID_FORMAT = "CodeCoverLoopChoiceHelper_%1$s";

    private static final String COUNTER_INCREMENTING_FOR_LOOP_HELPER = LOOP_HELPER_ID_FORMAT
            + "++;";

    private static final String COUNTER_DECLARATION_FOR_LOOP_HELPER = "byte "
            + LOOP_HELPER_ID_FORMAT + " = 0;";

    private static final String LOOP_HELPER_EVALUATION_ZERO = "if ("
            + LOOP_HELPER_ID_FORMAT + " == 0)";

    private static final String LOOP_HELPER_EVALUATION_ONE = " else if ("
            + LOOP_HELPER_ID_FORMAT + " == 1)";

    // /////////////////////////////////////////////////////////////////////////
    //
    // private members
    //
    // /////////////////////////////////////////////////////////////////////////

    private int maxLoopID;

    // /////////////////////////////////////////////////////////////////////////
    //
    // constructor, public and private methods
    //
    // /////////////////////////////////////////////////////////////////////////

    /**
     * Constructor for a new {@link ArrayLoopManipulator}.
     */
    public ArrayLoopManipulator() {
        this.maxLoopID = 0;
    }

    /**
     * Writes a statement for setting the help counter of a primary loopID to
     * zero and incrementing the counter representing the LoopSubIDZero.
     * 
     * <pre>
     * helper = 0;
     * counterZero++;
     * </pre>
     * 
     * @param primaryLoopID
     *            The corresponding primary loopID.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    private void writeBeforeLoop(String primaryLoopID)
            throws IOException {
        TreeDumperWithException treeDumper = super.getTreeDumper();
        Writer writer = super.getWriter();
        int ID = getNumberFromPrimaryLoopID(primaryLoopID);
        this.maxLoopID = Math.max(this.maxLoopID, ID);

        writer.write(String.format(COUNTER_DECLARATION_FOR_LOOP_HELPER, primaryLoopID));
        writer.write(LINE_SEPARATOR);
        treeDumper.addInstrumentationBetween(String.format(COUNTER_INCREMENTING,
                super.getCounterIDManager().getInnerClassName(),
                new Integer(ID * 3 - 2)));
        treeDumper.addInstrumentationBetween(LINE_SEPARATOR);
    }

    /**
     * <pre>
     * if (helper == 0) {
     *   helper++;
     *   counterZero--;
     *   counterOne++;
     * } else if (helper == 1) {
     *   helper++;
     *   counterOne--;
     *   counterAbove++;
     * }
     * </pre>
     * 
     * @param primaryLoopID
     *            The corresponding primary loopID.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    private void writeInLoop(String primaryLoopID)
            throws IOException {
        Writer writer = super.getWriter();
        int ID = getNumberFromPrimaryLoopID(primaryLoopID);
        this.maxLoopID = Math.max(this.maxLoopID, ID);

        writer.write(String.format(LOOP_HELPER_EVALUATION_ZERO, primaryLoopID));
        writer.write(" {");
        writer.write(LINE_SEPARATOR);
        writer.write("  ");
        writer.write(String.format(COUNTER_INCREMENTING_FOR_LOOP_HELPER,
                primaryLoopID));
        writer.write(LINE_SEPARATOR);
        writer.write("  ");
        writer.write(String.format(COUNTER_DECREMENTING,
                super.getCounterIDManager().getInnerClassName(),
                new Integer(ID * 3 - 2)));
        writer.write(LINE_SEPARATOR);
        writer.write("  ");
        writer.write(String.format(COUNTER_INCREMENTING,
                super.getCounterIDManager().getInnerClassName(),
                new Integer(ID * 3 - 1)));
        writer.write(LINE_SEPARATOR);
        writer.write("}");

        writer.write(String.format(LOOP_HELPER_EVALUATION_ONE, primaryLoopID));
        writer.write(" {");
        writer.write(LINE_SEPARATOR);
        writer.write("  ");
        writer.write(String.format(COUNTER_INCREMENTING_FOR_LOOP_HELPER,
                primaryLoopID));
        writer.write(LINE_SEPARATOR);
        writer.write("  ");
        writer.write(String.format(COUNTER_DECREMENTING,
                super.getCounterIDManager().getInnerClassName(),
                new Integer(ID * 3 - 1)));
        writer.write(LINE_SEPARATOR);
        writer.write("  ");
        writer.write(String.format(COUNTER_INCREMENTING,
                super.getCounterIDManager().getInnerClassName(),
                new Integer(ID * 3)));
        writer.write(LINE_SEPARATOR);
        writer.write("}");
    }

    /**
     * Always true.<br>
     * <br>
     * This is needed, because we extend a while by adding a preceding
     * declaration. 
     * 
     * @return true;
     */
    public boolean requiresBlockExpansionsForBranches() {
        return true;
    }

    /**
     * Always true.
     * 
     * @return true;
     */
    public boolean requiresBlockExpansionsForLoops() {
        return true;
    }

    public void manipulateBefore(ForStatement n, String primaryLoopID)
            throws IOException {
        Writer writer = super.getWriter();

        writer.write(LINE_SEPARATOR);
        writeBeforeLoop(primaryLoopID);
        writer.write(LINE_SEPARATOR);
    }

    public void manipulateInner(ForStatement n, String primaryLoopID)
            throws IOException {
        super.getWriter().write(LINE_SEPARATOR);
        writeInLoop(primaryLoopID);
    }

    public void manipulateAfter(ForStatement n, String primaryLoopID)
            throws IOException {
        // not needed
    }

    public void manipulateBefore(WhileStatement n, String primaryLoopID)
            throws IOException {
        Writer writer = super.getWriter();

        writer.write(LINE_SEPARATOR);
        writeBeforeLoop(primaryLoopID);
        writer.write(LINE_SEPARATOR);
    }

    public void manipulateInner(WhileStatement n, String primaryLoopID)
            throws IOException {
        super.getWriter().write(LINE_SEPARATOR);
        writeInLoop(primaryLoopID);
    }

    public void manipulateAfter(WhileStatement n, String primaryLoopID)
            throws IOException {
        // not needed
    }

    public void manipulateBefore(DoStatement n, String primaryLoopID)
            throws IOException {
        Writer writer = super.getWriter();

        writer.write(LINE_SEPARATOR);
        writeBeforeLoop(primaryLoopID);
        writer.write(LINE_SEPARATOR);
    }

    public void manipulateInner(DoStatement n, String primaryLoopID)
            throws IOException {
        super.getWriter().write(LINE_SEPARATOR);
        writeInLoop(primaryLoopID);
    }

    public void manipulateAfter(DoStatement n, String primaryLoopID)
            throws IOException {
        // not needed
    }

    // /////////////////////////////////////////////////////////////////////////
    //
    // methods for the interface CounterManager
    //
    // /////////////////////////////////////////////////////////////////////////

    public void writeDeclarations() throws IOException {
        // we need three counters for every loop ID

        Writer writer = super.getWriter();
        writer.write("    ");
        writer.write(String.format(COUNTER_DECLARATION,
                new Integer(this.maxLoopID * 3 + 1)));
        writer.write(LINE_SEPARATOR);
    }

    public void writeReset() throws IOException {
        Writer writer = super.getWriter();

        writer.write("      ");
        writer.write(String.format(COUNTER_FOR_LOOP,
                new Integer(this.maxLoopID * 3)));
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
                this.maxLoopID)));
        writer.write(" {");
        writer.write(LINE_SEPARATOR);

        // the if for zero loops -> loops[i * 3 - 2]
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

        // the if for one loop -> loops[i * 3 - 1]
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

        // the if for more than one loop -> loops[i * 3]
        writer.write("        ");
        writer.write(COUNTER_SERIALIZE_IF_ABOVE);
        writer.write(" {");
        writer.write(LINE_SEPARATOR);
        writer.write("          ");
        writer.write(COUNTER_SERIALIZE_PASS_COUNTER_ABOVE);
        writer.write(LINE_SEPARATOR);
        writer.write("          ");
        writer.write(COUNTER_RESET_ABOVE);
        writer.write(LINE_SEPARATOR);
        writer.write("        }");
        writer.write(LINE_SEPARATOR);

        // "}" of for loop
        writer.write("      }");
        writer.write(LINE_SEPARATOR);
    }
}
