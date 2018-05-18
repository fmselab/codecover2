///////////////////////////////////////////////////////////////////////////////
//
// $Id: LongStatementCounterManager.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 15.04.2007 15:51:37
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.counter;

import static org.gbt2.instrumentation.java15.visitor.TreeDumperWithException.LINE_SEPERATOR;

import java.io.IOException;
import java.io.Writer;
import java.util.LinkedList;
import java.util.Queue;

import org.gbt2.instrumentation.java15.measurement.CoverageCounterLog;

import static org.gbt2.instrumentation.java15.counter.CounterIDManager.INNER_CLASS_NAME;
import static org.gbt2.instrumentation.java15.counter.CounterIDManager.LOG_NAME;

/**
 * This class implements the interface {@link StatementCounterManager} using
 * longs as counter.
 * 
 * @author Christoph Müller
 */
class LongStatementCounterManager implements StatementCounterManager {
    private int statementCount;

    private Queue<String> usedStatmentIDs;

    private Writer writer;

    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for statement coverage
    //
    // /////////////////////////////////////////////////////////////////////////

    private static final String STATEMENT_ID_FORMAT = "S%d";

    private static final String COUNTER_INCREMENTING = INNER_CLASS_NAME
            + ".%s++;";

    private static final String COUNTER_DECLARATION = "public static long %s = 0L;";

    private static final String COUNTER_RESET = "%s = 0L;";

    private static final String COUNTER_SERIALIZE_IF = "if ( %s != 0L)";

    private static final String COUNTER_SERIALIZE_PASS_COUNTER = LOG_NAME
            + "." + CoverageCounterLog.PASS_COUNTER_METHOD_NAME
            + "(\"%1$s\", %1$s);";

    /**
     * Creates a new {@link LongStatementCounterManager}.
     * 
     * @param writer
     *            The writer, using for the output of the declarations and
     *            counters.
     */
    public LongStatementCounterManager(Writer writer) {
        this.writer = writer;
        this.statementCount = 0;
        this.usedStatmentIDs = new LinkedList<String>();
    }

    public String nextStatementID() {
        this.statementCount++;
        return String.format(STATEMENT_ID_FORMAT, new Integer(
                this.statementCount));
    }

    public void writeCounterIncrementingForStatement(String statementID)
            throws IOException {
        this.writer.write(String.format(COUNTER_INCREMENTING, statementID));
        this.usedStatmentIDs.add(statementID);
    }

    public void writeDeclarations() throws IOException {
        for (String thisStatementID : this.usedStatmentIDs) {
            this.writer.write("    ");
            this.writer.write(String.format(COUNTER_DECLARATION,
                    thisStatementID));
            this.writer.write(LINE_SEPERATOR);
        }
    }

    public void writeReset() throws IOException {
        for (String thisStatementID : this.usedStatmentIDs) {
            this.writer.write("      ");
            this.writer.write(String.format(COUNTER_RESET,
                    thisStatementID));
            this.writer.write(LINE_SEPERATOR);
        }
    }

    public void writeSerialzeAndReset() throws IOException {
        if (this.usedStatmentIDs.size() > 0) {
            this.writer.write(LINE_SEPERATOR);
        }
        for (String thisStatementID : this.usedStatmentIDs) {
            this.writer.write("      ");
            this.writer.write(String.format(COUNTER_SERIALIZE_IF,
                    thisStatementID));
            this.writer.write(" {");
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("        ");
            this.writer.write(String.format(COUNTER_SERIALIZE_PASS_COUNTER,
                    thisStatementID));
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("        ");
            this.writer.write(String.format(COUNTER_RESET,
                    thisStatementID));
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("      }");
            this.writer.write(LINE_SEPERATOR);
        }
    }
}
