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
import static org.codecover.model.utils.criteria.StatementCoverage.ID_PREFIX;

import java.io.IOException;
import java.io.Writer;

import org.codecover.instrumentation.java15.counter.CounterManager;
import org.codecover.instrumentation.java15.syntaxtree.BreakStatement;
import org.codecover.instrumentation.java15.syntaxtree.ContinueStatement;
import org.codecover.instrumentation.java15.syntaxtree.DoStatement;
import org.codecover.instrumentation.java15.syntaxtree.EmptyStatement;
import org.codecover.instrumentation.java15.syntaxtree.ExplicitConstructorInvocation;
import org.codecover.instrumentation.java15.syntaxtree.FieldDeclaration;
import org.codecover.instrumentation.java15.syntaxtree.ForStatement;
import org.codecover.instrumentation.java15.syntaxtree.IfStatement;
import org.codecover.instrumentation.java15.syntaxtree.LocalVariableDeclaration;
import org.codecover.instrumentation.java15.syntaxtree.ReturnStatement;
import org.codecover.instrumentation.java15.syntaxtree.StatementExpression;
import org.codecover.instrumentation.java15.syntaxtree.SwitchStatement;
import org.codecover.instrumentation.java15.syntaxtree.ThrowStatement;
import org.codecover.instrumentation.java15.syntaxtree.TryStatement;
import org.codecover.instrumentation.java15.syntaxtree.WhileStatement;
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
public class ArrayStatementManipulator extends AbstractDefaultManipulator
        implements StatementManipulator, CounterManager {

    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for statement coverage
    //
    // /////////////////////////////////////////////////////////////////////////

    private static final String ARRAY_NAME = "statements";

    private static final String COUNTER_INCREMENTING = "%1$s." + ARRAY_NAME
            + "[%2$d]++;";

    private static final String COUNTER_DECLARATION = "public static long[] "
            + ARRAY_NAME + " = new long[%1$d];";

    private static final String COUNTER_FOR_LOOP = "for (int i = 1; i <= %1$d; i++)";

    private static final String COUNTER_RESET = ARRAY_NAME + "[i] = 0L;";

    private static final String COUNTER_SERIALIZE_IF = "if (" + ARRAY_NAME
            + "[i] != 0L)";

    private static final String COUNTER_SERIALIZE_PASS_COUNTER = LOG_NAME + "."
            + CoverageCounterLog.PASS_COUNTER_METHOD_NAME + "(\"" + ID_PREFIX + "\" + i, "
            + ARRAY_NAME + "[i]);";

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
    public ArrayStatementManipulator() {
        this.maxStatementID = -1;
    }

    /**
     * Writes a statement for incrementing the counter of a statementID.<br>
     * <br>
     * The statementID is saved for {@link #writeDeclarations()}.
     * 
     * @param statementID
     *            The ID of the statement.
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    private void writeCounterIncrementingForStatement(String statementID) {
        TreeDumperWithException treeDumper = super.getTreeDumper();

        int ID = getNumberFromStatementID(statementID);
        treeDumper.addInstrumentationBetween(String.format(COUNTER_INCREMENTING,
                super.getCounterIDManager().getInnerClassName(),
                new Integer(ID)));
        treeDumper.addInstrumentationBetween(LINE_SEPARATOR);
        this.maxStatementID = Math.max(this.maxStatementID, ID);
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

    /**
     * Always true.
     * 
     * @return true;
     */
    public boolean requiresBlockExpansionsForLoops() {
        return true;
    }

    public void manipulate(StatementExpression n, String statementID)
            throws IOException {
        writeCounterIncrementingForStatement(statementID);
    }

    public void manipulate(EmptyStatement emptyStatement, String statementID)
            throws IOException {
        writeCounterIncrementingForStatement(statementID);
    }

    public void manipulate(BreakStatement breakStatement, String statementID)
            throws IOException {
        writeCounterIncrementingForStatement(statementID);
    }

    public void manipulate(ContinueStatement continueStatement,
            String statementID) throws IOException {
        writeCounterIncrementingForStatement(statementID);
    }

    public void manipulate(ReturnStatement returnStatement,
            String statementID) throws IOException {
        writeCounterIncrementingForStatement(statementID);
    }

    public void manipulate(ThrowStatement throwStatement,
            String statementID) throws IOException {
        writeCounterIncrementingForStatement(statementID);
    }

    public void manipulate(LocalVariableDeclaration declaration,
            String statementID) throws IOException {
        writeCounterIncrementingForStatement(statementID);
    }

    public void manipulate(FieldDeclaration fieldDeclaration,
            boolean isStaticPresent, String statementID) throws IOException {
        TreeDumperWithException treeDumper = super.getTreeDumper();

        int ID = getNumberFromStatementID(statementID);

        if (isStaticPresent) {
            treeDumper.addInstrumentationBetween("static ");
        }
        treeDumper.addInstrumentationBetween("{ ");
        treeDumper.addInstrumentationBetween(String.format(COUNTER_INCREMENTING,
                super.getCounterIDManager().getInnerClassName(),
                new Integer(ID)));
        treeDumper.addInstrumentationBetween(" }" + LINE_SEPARATOR);

        this.maxStatementID = Math.max(this.maxStatementID, ID);
    }

    public void manipulate(SwitchStatement switchStatement, String statementID) throws IOException {
        writeCounterIncrementingForStatement(statementID);
    }

    public void manipulate(WhileStatement switchStatement, String statementID) throws IOException {
        writeCounterIncrementingForStatement(statementID);
    }

    public void manipulate(IfStatement whileStatement, String statementID) throws IOException {
        writeCounterIncrementingForStatement(statementID);
    }

    public void manipulate(DoStatement whileStatement, String statementID) throws IOException {
        writeCounterIncrementingForStatement(statementID);
    }

    public void manipulate(ForStatement doStatement, String statementID) throws IOException {
        writeCounterIncrementingForStatement(statementID);
    }

    public void manipulate(TryStatement breakStatement, String statementID) throws IOException {
        writeCounterIncrementingForStatement(statementID);
    }

    public void manipulate(ExplicitConstructorInvocation explicitConstructorInvocation,
            String statementID) throws IOException {
        writeCounterIncrementingForStatement(statementID);
    }

    public void writeDeclarations() throws IOException {
        Writer writer = super.getWriter();

        writer.write("    ");
        writer.write(String.format(COUNTER_DECLARATION,
                new Integer(this.maxStatementID + 1)));
        writer.write(LINE_SEPARATOR);
    }

    public void writeReset() throws IOException {
        Writer writer = super.getWriter();

        writer.write("      ");
        writer.write(String.format(COUNTER_FOR_LOOP, new Integer(
                this.maxStatementID)));
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
                this.maxStatementID)));
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
