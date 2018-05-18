///////////////////////////////////////////////////////////////////////////////
//
// $Id: ArrayStatementCounterManager.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 15.04.2007 15:51:37
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.counter;

import static org.gbt2.instrumentation.java15.counter.CounterIDManager.INNER_CLASS_NAME;
import static org.gbt2.instrumentation.java15.counter.CounterIDManager.LOG_NAME;
import static org.gbt2.instrumentation.java15.visitor.TreeDumperWithException.LINE_SEPERATOR;

import java.io.IOException;
import java.io.Writer;

import org.gbt2.instrumentation.java15.measurement.CoverageCounterLog;

/**
 * This class implements the interface {@link StatementCounterManager} using a
 * long array as counter.
 * 
 * @author Christoph Müller
 * 
 */
class ArrayStatementCounterManager implements StatementCounterManager {

    private int statementCount;

    private Writer writer;

    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for statement coverage
    //
    // /////////////////////////////////////////////////////////////////////////

    private static final String STATEMENT_ID_FORMAT = "S%1$d";
    
    private static final String ARRAY_NAME = "statements";

    private static final String COUNTER_INCREMENTING = INNER_CLASS_NAME
            + "." + ARRAY_NAME + "[%1$d]++;";

    private static final String COUNTER_DECLARATION = "public static long[] " + ARRAY_NAME + " = new long[%1$d];";

    private static final String COUNTER_FOR_LOOP = "for (int i = 0; i <= %1$d; i++)";

    private static final String COUNTER_RESET = ARRAY_NAME + "[i] = 0L;";

    private static final String COUNTER_SERIALIZE_IF = "if ( " + ARRAY_NAME + "[i] != 0L)";

    private static final String COUNTER_SERIALIZE_PASS_COUNTER = LOG_NAME + "."
            + CoverageCounterLog.PASS_COUNTER_METHOD_NAME
            + "(\"S\" + i, " + ARRAY_NAME + "[i]);";

    /**
     * Creates a new {@link ArrayStatementCounterManager}.
     * 
     * @param writer
     *            The writer, using for the output of the declarations and
     *            counters.
     */
    public ArrayStatementCounterManager(Writer writer) {
        this.writer = writer;
        this.statementCount = 0;
    }

    public String nextStatementID() {
        this.statementCount++;
        return String.format(STATEMENT_ID_FORMAT, new Integer(
                this.statementCount));
    }

    public void writeCounterIncrementingForStatement(String statementID)
            throws IOException {
        Integer ID = Integer.valueOf(statementID.substring(1), 10);
        this.writer.write(String.format(COUNTER_INCREMENTING, ID));
    }

    public void writeDeclarations() throws IOException {
        this.writer.write("    ");
        this.writer.write(String.format(COUNTER_DECLARATION, new Integer(
                this.statementCount + 1)));
        this.writer.write(LINE_SEPERATOR);
    }

    public void writeReset() throws IOException {
        this.writer.write("      ");
        this.writer.write(String.format(COUNTER_FOR_LOOP, new Integer(
                this.statementCount)));
        this.writer.write(" {");
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("        ");
        this.writer.write(COUNTER_RESET);
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("      }");
        this.writer.write(LINE_SEPERATOR);
    }

    public void writeSerialzeAndReset() throws IOException {
        this.writer.write("      ");
        this.writer.write(String.format(COUNTER_FOR_LOOP, new Integer(
                this.statementCount)));
        this.writer.write(" {");
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("        ");
        this.writer.write(COUNTER_SERIALIZE_IF);
        this.writer.write(" {");
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("          ");
        this.writer.write(COUNTER_SERIALIZE_PASS_COUNTER);
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("          ");
        this.writer.write(COUNTER_RESET);
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("        }");
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("      }");
        this.writer.write(LINE_SEPERATOR);
    }
}
