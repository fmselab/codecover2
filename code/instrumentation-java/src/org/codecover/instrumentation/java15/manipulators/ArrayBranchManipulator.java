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
import static org.codecover.instrumentation.java15.counter.CounterIDManager.getNumberFromBranchID;
import static org.codecover.instrumentation.java15.visitor.TreeDumperWithException.LINE_SEPARATOR;
import static org.codecover.model.utils.criteria.BranchCoverage.ID_PREFIX;

import java.io.IOException;
import java.io.Writer;

import org.codecover.instrumentation.java15.counter.CounterIDManager;
import org.codecover.instrumentation.java15.counter.CounterManager;
import org.codecover.instrumentation.java15.syntaxtree.IfStatement;
import org.codecover.instrumentation.java15.syntaxtree.SwitchLabel;
import org.codecover.instrumentation.java15.syntaxtree.SwitchStatement;
import org.codecover.instrumentation.java15.syntaxtree.TryStatement;
import org.codecover.instrumentation.java15.syntaxtree.Type;
import org.codecover.instrumentation.measurement.CoverageCounterLog;

/**
 * This is a {@link BranchManipulator} using a long array for every branch.
 * 
 * @see BranchManipulator
 * @author Christoph Müller
 * @version 1.0 ($Id: ArrayBranchManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ArrayBranchManipulator extends AbstractDefaultManipulator
        implements BranchManipulator, CounterManager {

    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for branch coverage
    //
    // /////////////////////////////////////////////////////////////////////////

    private static final String ARRAY_NAME = "branches";

    private static final String COUNTER_INCREMENTING = "%1$s." + ARRAY_NAME
            + "[%2$d]++;";

    private static final String COUNTER_DECLARATION = "public static long[] "
            + ARRAY_NAME + " = new long[%1$d];";

    private static final String COUNTER_FOR_LOOP = "for (int i = 1; i <= %1$d; i++)";

    private static final String COUNTER_RESET = ARRAY_NAME + "[i] = 0L;";

    private static final String COUNTER_SERIALIZE_IF = "if (" + ARRAY_NAME
            + "[i] != 0L)";

    private static final String COUNTER_SERIALIZE_PASS_COUNTER = LOG_NAME + "."
            + CoverageCounterLog.PASS_COUNTER_METHOD_NAME
            + "(\"" + ID_PREFIX + "\"+ i, "
            + ARRAY_NAME + "[i]);";

    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for try catch coverage
    //
    // /////////////////////////////////////////////////////////////////////////

    private static final String TRY_BRANCH_BOOLEAN_FORMAT = "CodeCoverTryBranchHelper_%1$s";

    private static final String TRY_BRANCH_BOOLEAN_DECLARATION = "boolean "
            + TRY_BRANCH_BOOLEAN_FORMAT + " = false;";

    private static final String TRY_BRANCH_BOOLEAN_SET_TRUE = TRY_BRANCH_BOOLEAN_FORMAT
            + " = true;";

    private static final String TRY_BRANCH_BOOLEAN_SET_FALSE = TRY_BRANCH_BOOLEAN_FORMAT
            + " = false;";

    private static final String TRY_BRANCH_BOOLEAN_FINALLY = "if ( "
            + TRY_BRANCH_BOOLEAN_FORMAT + " )";

    // /////////////////////////////////////////////////////////////////////////
    //
    // private members
    //
    // /////////////////////////////////////////////////////////////////////////

    private int maxBranchID;

    // /////////////////////////////////////////////////////////////////////////
    //
    // constructor, public and private methods
    //
    // /////////////////////////////////////////////////////////////////////////

    /**
     * Constructor for a new {@link ArrayBranchManipulator}.
     */
    public ArrayBranchManipulator() {
        this.maxBranchID = -1;
    }

    /**
     * Writes a statement for incrementing the counter of a branchID.<br>
     * <br>
     * The branchID is saved for {@link #writeDeclarations()}.
     * 
     * @param branchID
     *            The ID of the branch.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    private void writeCounterIncrementingForBranch(String branchID)
            throws IOException {
        int ID = getNumberFromBranchID(branchID);
        super.getWriter().write(String.format(COUNTER_INCREMENTING,
                super.getCounterIDManager().getInnerClassName(),
                new Integer(ID)));
        this.maxBranchID = Math.max(this.maxBranchID, ID);
    }

    private void writeTryBranchHelperDeclaration(String tryBranchID)
            throws IOException {
        super.getWriter().write(String.format(TRY_BRANCH_BOOLEAN_DECLARATION,
                tryBranchID));
    }

    /**
     * Writes a statement for setting the boolean of the TryBranchID to true.
     * 
     * @param tryBranchID
     *            The TryBranchID of the try branch. Got with
     *            {@link CounterIDManager#nextTryBranchID()}.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    private void writeTryBranchSetTrue(String tryBranchID) throws IOException {
        super.getWriter().write(String.format(TRY_BRANCH_BOOLEAN_SET_TRUE,
                tryBranchID));
    }

    /**
     * Writes a statement for setting the boolean of the TryBranchID to false.
     * 
     * @param tryBranchID
     *            The TryBranchID of the try branch. Got with
     *            {@link CounterIDManager#nextTryBranchID()}.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    private void writeTryBranchSetFalse(String tryBranchID) throws IOException {
        super.getWriter().write(String.format(TRY_BRANCH_BOOLEAN_SET_FALSE,
                tryBranchID));
    }

    /**
     * Writes a statement for incrementing the counter of a branchID of a the
     * try branch.<br>
     * <br>
     * The branchID is saved for {@link #writeDeclarations()}.
     * 
     * @param branchID
     *            The branchID of the try branch. Got with
     *            {@link CounterIDManager#nextBranchID()}.
     * 
     * @param tryBranchID
     *            The TryBranchID of the try branch. Got with
     *            {@link CounterIDManager#nextTryBranchID()}.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    private void writeTryBranchFinally(String branchID, String tryBranchID)
            throws IOException {
        int ID = getNumberFromBranchID(branchID);
        this.maxBranchID = Math.max(this.maxBranchID, ID);
        Writer writer = super.getWriter();
        writer.write(
                String.format(TRY_BRANCH_BOOLEAN_FINALLY, tryBranchID));
        writer.write(" {");
        writer.write(LINE_SEPARATOR);
        writer.write("  ");
        writer.write(String.format(COUNTER_INCREMENTING,
                super.getCounterIDManager().getInnerClassName(),
                new Integer(ID)));
        writer.write(LINE_SEPARATOR);
        writer.write("}");
    }

    // /////////////////////////////////////////////////////////////////////////
    //
    // methods for the interface BranchManipulator
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

    /**
     * Always true.
     * 
     * @return true;
     */
    public boolean requiresBlockExpansionsForLoops() {
        return true;
    }

    public void manipulateIf(IfStatement n, String ifBranchID)
            throws IOException {
        writeCounterIncrementingForBranch(ifBranchID);
    }

    public void manipulateElse(IfStatement n, String elseBranchID,
            boolean implicitElse) throws IOException {
        if (implicitElse) {
            Writer writer = super.getWriter();
            // if the else branch is not present, we have to create an implicit
            // branch
            writer.write(" else {");
            writer.write(LINE_SEPARATOR);
            writer.write("  ");
            writeCounterIncrementingForBranch(elseBranchID);
            writer.write("}");
        } else {
            writeCounterIncrementingForBranch(elseBranchID);
        }
    }

    public void manipulateSwitchCase(SwitchLabel switchLabel,
            String caseBranchID) throws IOException {
        super.getWriter().write(LINE_SEPARATOR);
        writeCounterIncrementingForBranch(caseBranchID);
    }

    public void manipulateSwitchDefault(SwitchStatement n,
            String defaultBranchID, boolean implicitDefault) throws IOException {
        if (implicitDefault) {
            // if the default branch is not present, we have to create an
            // implicit branch

            super.getWriter().write(" default : ");
            writeCounterIncrementingForBranch(defaultBranchID);
        } else {
            super.getWriter().write(LINE_SEPARATOR);
            writeCounterIncrementingForBranch(defaultBranchID);
        }
    }

    public void manipulateHelperDeclaration(TryStatement n, String branchID,
            String tryBranchID) throws IOException {
        super.getWriter().write(LINE_SEPARATOR);
        writeTryBranchHelperDeclaration(tryBranchID);
    }

    public void manipulateTry(TryStatement n, String branchID,
            String tryBranchID) throws IOException {
        super.getWriter().write(LINE_SEPARATOR);
        writeTryBranchSetTrue(tryBranchID);
    }

    public void manipulateCatch(TryStatement n, Type catchType,
            String catchBranchId, String tryBranchID) throws IOException {

        super.getWriter().write(LINE_SEPARATOR);
        writeTryBranchSetFalse(tryBranchID);
        super.getWriter().write(LINE_SEPARATOR);
        writeCounterIncrementingForBranch(catchBranchId);
    }

    public void manipulateFinally(TryStatement n, String branchID,
            String tryBranchID, boolean implicitFinally) throws IOException {
        Writer writer = super.getWriter();
        if (implicitFinally) {
            writer.write(" finally {");
            writer.write(LINE_SEPARATOR);
            writer.write("    ");
            writeTryBranchFinally(branchID, tryBranchID);
            writer.write(LINE_SEPARATOR);
            writer.write("  }");

        } else {
            writer.write(LINE_SEPARATOR);
            writeTryBranchFinally(branchID, tryBranchID);
        }
    }

    // /////////////////////////////////////////////////////////////////////////
    //
    // methods for the interface CounterManager
    //
    // /////////////////////////////////////////////////////////////////////////

    public void writeDeclarations() throws IOException {
        Writer writer = super.getWriter();

        writer.write("    ");
        writer.write(String.format(COUNTER_DECLARATION,
                new Integer(this.maxBranchID + 1)));
        writer.write(LINE_SEPARATOR);
    }

    public void writeReset() throws IOException {
        Writer writer = super.getWriter();

        writer.write("      ");
        writer.write(String.format(COUNTER_FOR_LOOP,
                new Integer(this.maxBranchID)));
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
        writer.write(String.format(COUNTER_FOR_LOOP,
                new Integer(this.maxBranchID)));
        writer.write(" {");
        writer.write(LINE_SEPARATOR);
        writer.write("        ");
        writer.write(COUNTER_SERIALIZE_IF);
        writer.write(" {");
        writer.write(LINE_SEPARATOR);
        writer.write("          ");
        writer.write(COUNTER_SERIALIZE_PASS_COUNTER);
        writer.write(LINE_SEPARATOR);
        writer.write("          ");
        writer.write(COUNTER_RESET);
        writer.write(LINE_SEPARATOR);
        writer.write("        }");
        writer.write(LINE_SEPARATOR);
        writer.write("      }");
        writer.write(LINE_SEPARATOR);
    }
}
