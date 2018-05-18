package org.codecover.instrumentation.java15.manipulators;

import static org.codecover.instrumentation.java15.counter.CounterIDManager.LOG_NAME;
import static org.codecover.instrumentation.java15.counter.CounterIDManager.getNumberFromSyncStatementID;
import static org.codecover.instrumentation.java15.visitor.TreeDumperWithException.LINE_SEPARATOR;
import static org.codecover.model.utils.criteria.SynchronizedStatementCoverage.ID_SUFFIX_ABOVE;
import static org.codecover.model.utils.criteria.SynchronizedStatementCoverage.ID_SUFFIX_ONE;
import static org.codecover.model.utils.criteria.SynchronizedStatementCoverage.ID_SUFFIX_ZERO;
import static org.codecover.model.utils.criteria.SynchronizedStatementCoverage.ID_PREFIX;

import java.io.IOException;
import java.io.Writer;

import org.codecover.instrumentation.java15.counter.CounterManager;
import org.codecover.instrumentation.java15.syntaxtree.SynchronizedStatement;
import org.codecover.instrumentation.measurement.CoverageCounterLog;

/**
 * @author Rainer Schmidberger
 *
 * @version 1.0 ($Id$)
 */
public class ArraySynchronizedManipulator extends AbstractDefaultManipulator
implements SynchronizedManipulator, CounterManager {

    //array syncs:
    //offset description
    //0      wait counter; temporary variable for counting the number of waiting threads; needs to be in a "global" scope
    //1      number of threads passed through synchronized block without waiting
    //2      number of threads passed through synchronized block with waiting for one additional thread waiting
    //3      number of threads passed through synchronized block with more than one additional threads waiting 
    private static final String ARRAY_NAME = "syncs";
    private static final String WAIT_ARRAY_NAME = "syncWaits";

    private static final String SYNC_TMP_VARIABLE = "__codeCover_Tmp_SyncObjectRef%1$d";
    private static final String SYNC_TMP_COUNTER = "__codeCover_Tmp_SyncCounter%1$d";

    private static final String COUNTER_DECLARATION = "public static java.util.concurrent.atomic.AtomicLong[] "
        + ARRAY_NAME + " = new java.util.concurrent.atomic.AtomicLong[%1$d];";
    private static final String WAIT_COUNTER_DECLARATION = "public static java.util.Hashtable[] "
        + WAIT_ARRAY_NAME + " = new java.util.Hashtable[%1$d];";

    private static final String COUNTER_INITIALIZE = ARRAY_NAME + "[i] = new java.util.concurrent.atomic.AtomicLong(0L);";
    private static final String WAIT_COUNTER_INITIALIZE = WAIT_ARRAY_NAME + "[i] = new java.util.Hashtable();";

    private static final String COUNTER_INCREMENTING_COUNTER_CHECK = "_waitCounter = _waitCounter > 3 ? 3 : _waitCounter;";
    private static final String COUNTER_INCREMENTING = "%1$s." + ARRAY_NAME
    + "[%2$d + _waitCounter - 1].incrementAndGet();";

    //private static final String COUNTER_DECREMENTING = "%1$s." + ARRAY_NAME
    //        + "[%2$d].decrementAndGet();";

    private static final String WAIT_COUNTER_TABLE = "%1$s." + WAIT_ARRAY_NAME;
    
 
    private static final String COUNTER_FOR_LOOP = "for (int i = 0; i <= %1$d; i++)";

    private static final String COUNTER_RESET = ARRAY_NAME + "[i].set(0L);";
    private static final String WAIT_COUNTER_RESET = WAIT_ARRAY_NAME + "[i].clear();";

    private static final String COUNTER_SERIALIZE_IF_ZERO = "if ("
        + ARRAY_NAME + "[i * 3].get() != 0L)";
    private static final String COUNTER_SERIALIZE_PASS_COUNTER_ZERO = LOG_NAME
    + "." + CoverageCounterLog.PASS_COUNTER_METHOD_NAME + "(\""
    + ID_PREFIX + "\" + i + \"" + ID_SUFFIX_ZERO + "\", " + ARRAY_NAME
    + "[i * 3].get());";
    private static final String COUNTER_RESET_ZERO = ARRAY_NAME
    + "[i * 3].set(0L);";

    private static final String COUNTER_SERIALIZE_IF_ONE = "if (" + ARRAY_NAME
    + "[i * 3 + 1].get() != 0L)";
    private static final String COUNTER_SERIALIZE_PASS_COUNTER_ONE = LOG_NAME
    + "." + CoverageCounterLog.PASS_COUNTER_METHOD_NAME + "(\""
    + ID_PREFIX + "\" + i + \"" + ID_SUFFIX_ONE + "\", " + ARRAY_NAME
    + "[i * 3 + 1].get());";
    private static final String COUNTER_RESET_ONE = ARRAY_NAME
    + "[i * 3 + 1].set(0L);";

    private static final String COUNTER_SERIALIZE_IF_ABOVE = "if ("
        + ARRAY_NAME + "[i * 3 + 2].get() != 0L)";
    private static final String COUNTER_SERIALIZE_PASS_COUNTER_ABOVE = LOG_NAME
    + "." + CoverageCounterLog.PASS_COUNTER_METHOD_NAME + "(\""
    + ID_PREFIX + "\" + i + \"" + ID_SUFFIX_ABOVE + "\", "
    + ARRAY_NAME + "[i * 3 + 2].get());";
    private static final String COUNTER_RESET_ABOVE = ARRAY_NAME
    + "[i * 3 + 2].set(0L);";

    private int maxSyncID;

    /**
     * Constructor
     */
    public ArraySynchronizedManipulator() {
        this.maxSyncID = 0;
    }


    public void manipulatePart1(SynchronizedStatement n, String statementID)
    throws IOException {
        Writer writer = super.getWriter();

        int ID = getNumberFromSyncStatementID(statementID);
        this.maxSyncID = Math.max(this.maxSyncID, ID);



        writer.write(LINE_SEPARATOR);
        writer.write("  Object ");

        writer.write(String.format(SYNC_TMP_VARIABLE, new Integer(this.maxSyncID)));
        writer.write(" = ");

    }


    public void manipulatePart2(SynchronizedStatement n, String statementID)
    throws IOException {
        Writer writer = super.getWriter();
        int ID = getNumberFromSyncStatementID(statementID);
        this.maxSyncID = Math.max(this.maxSyncID, ID);

        // given the following code:
        // syncronized(expr) { ... }
        //
        //      AtomicLong y = null;
        //      synchronized(yTable) {        
        //        y = (AtomicLong)_yTable.get(_tmpRef);
        //        if(y == null) {
        //          y = new AtomicLong();
        //          yTable.put(tmpRef, y);
        //        }
        //      }


        writer.write(";");
        
        writer.write(LINE_SEPARATOR);
        writer.write(LINE_SEPARATOR);

        writer.write("java.util.concurrent.atomic.AtomicLong ");        
        writer.write(String.format(SYNC_TMP_COUNTER, new Integer(this.maxSyncID)));        
        writer.write(" = null;");
        writer.write(LINE_SEPARATOR);
        
        writer.write("synchronized(");
        writer.write(String.format(WAIT_COUNTER_TABLE,
                super.getCounterIDManager().getInnerClassName()));
        writer.write(String.format("[%1$d]", new Integer(this.maxSyncID)));
                writer.write(") {");
        

        writer.write(LINE_SEPARATOR);
        writer.write(String.format(SYNC_TMP_COUNTER, new Integer(this.maxSyncID)));
        writer.write(" = (java.util.concurrent.atomic.AtomicLong)");
        
        
        
        writer.write(String.format(WAIT_COUNTER_TABLE,
                super.getCounterIDManager().getInnerClassName()));

        writer.write(String.format("[%1$d]", new Integer(this.maxSyncID)));
        
        writer.write(".get(");

        writer.write(String.format(SYNC_TMP_VARIABLE, new Integer(this.maxSyncID)));
        writer.write(");");


        writer.write("");
        
        
        writer.write(LINE_SEPARATOR);
        
        writer.write("if(");
        writer.write(String.format(SYNC_TMP_COUNTER, new Integer(this.maxSyncID)));
        writer.write(" == null) {");

        writer.write(LINE_SEPARATOR);
        
        writer.write(String.format(SYNC_TMP_COUNTER, new Integer(this.maxSyncID)));
        writer.write(" = new java.util.concurrent.atomic.AtomicLong(); ");

        writer.write(LINE_SEPARATOR);

        writer.write(String.format(WAIT_COUNTER_TABLE,
                super.getCounterIDManager().getInnerClassName()));
        writer.write(String.format("[%1$d]", new Integer(this.maxSyncID)));
        writer.write(".put(");
        writer.write(String.format(SYNC_TMP_VARIABLE, new Integer(this.maxSyncID)));
        writer.write(", ");
        writer.write(String.format(SYNC_TMP_COUNTER, new Integer(this.maxSyncID)));
        
        writer.write(");");

        writer.write(LINE_SEPARATOR);

        writer.write("}");
        writer.write(LINE_SEPARATOR);
    
        writer.write("}");
        writer.write(LINE_SEPARATOR);
    }

    public void manipulatePart3(SynchronizedStatement n, String statementID)
    throws IOException {
        Writer writer = super.getWriter();
        int ID = getNumberFromSyncStatementID(statementID);
        this.maxSyncID = Math.max(this.maxSyncID, ID);

        writer.write(LINE_SEPARATOR);
        writer.write("synchronized(");
        writer.write(String.format(SYNC_TMP_VARIABLE, new Integer(this.maxSyncID)));
        writer.write(")");
    }


    public void manipulateBefore(SynchronizedStatement n, String statementID)
    throws IOException {
        Writer writer = super.getWriter();

        int ID = getNumberFromSyncStatementID(statementID);
        this.maxSyncID = Math.max(this.maxSyncID, ID);

        //int _waitCounter = syncWaits[ID].incrementAndGet(); 
        //_waitCounter = _waitCounter > 3 ? 3 : _waitCounter;
        //syncs[ID * 3 + _waitCounter - 1].incrementAndGet();
        
        
//        int actualY = y.incrementAndGet()
//        actualY = _actualY > 2 ? 2 : _actualY;
//        yP[actualY].incrementAndGet();

        
        
        writer.write(LINE_SEPARATOR);
        writer.write("{");
        writer.write(LINE_SEPARATOR);

        writer.write("int _waitCounter = (int)");
        writer.write(String.format(SYNC_TMP_COUNTER, new Integer(this.maxSyncID))); 
        writer.write(".incrementAndGet();");
        writer.write(LINE_SEPARATOR);

        
        writer.write("  ");
        writer.write(COUNTER_INCREMENTING_COUNTER_CHECK);
        writer.write(LINE_SEPARATOR);
        writer.write("  ");
        writer.write(String.format(COUNTER_INCREMENTING,
                super.getCounterIDManager().getInnerClassName(),
                new Integer(ID * 3)));
        writer.write(LINE_SEPARATOR);
        writer.write("}");
        writer.write(LINE_SEPARATOR);
    }

    public void manipulateInnerBefore(SynchronizedStatement n, String statementID)
    throws IOException {
        Writer writer = super.getWriter();

        writer.write(LINE_SEPARATOR);
        writer.write("try {");
    }

    public void manipulateInnerAfter(SynchronizedStatement n, String statementID)
    throws IOException {
        Writer writer = super.getWriter();
        int ID = getNumberFromSyncStatementID(statementID);
        this.maxSyncID = Math.max(this.maxSyncID, ID);
        writer.write(LINE_SEPARATOR);
        writer.write("} finally {");
        writer.write(LINE_SEPARATOR);
        writer.write("  ");
        writer.write(String.format(SYNC_TMP_COUNTER, new Integer(this.maxSyncID)));
        writer.write(".decrementAndGet();");
        writer.write(LINE_SEPARATOR);
        writer.write("}");
        writer.write(LINE_SEPARATOR);
    }

    public boolean requiresBlockExpansionsForBranches() {
        return false;
    }

    public boolean requiresBlockExpansionsForLoops() {
        return false;
    }

    public void writeDeclarations() throws IOException {
        // we need three counters for every synchronized ID

        Writer writer = super.getWriter();
        writer.write("    ");
        writer.write(String.format(COUNTER_DECLARATION,
                new Integer((this.maxSyncID + 1) * 3))); // public static java.util.concurrent.atomic.AtomicInteger[] syncWaits = new java.util.concurrent.atomic.AtomicInteger[2];

        writer.write(LINE_SEPARATOR);
        writer.write("    ");
        writer.write("static {");
        writer.write(LINE_SEPARATOR);
        writer.write("      ");
        writer.write(String.format(COUNTER_FOR_LOOP,
                new Integer((this.maxSyncID + 1) * 3 - 1))); // <=, so - 1
        writer.write(" {");
        writer.write(LINE_SEPARATOR);
        writer.write("        ");
        writer.write(COUNTER_INITIALIZE);
        writer.write(LINE_SEPARATOR);
        writer.write("      }");
        writer.write(LINE_SEPARATOR);
        writer.write("    ");
        writer.write("}");
        writer.write(LINE_SEPARATOR);
        writer.write("    ");
        writer.write(String.format(WAIT_COUNTER_DECLARATION,
                new Integer(this.maxSyncID + 1)));
        writer.write(LINE_SEPARATOR);
        writer.write("    ");
        writer.write("static {");
        writer.write(LINE_SEPARATOR);
        writer.write("      ");
        writer.write(String.format(COUNTER_FOR_LOOP,
                new Integer(this.maxSyncID))); // <=, so not + 1
        writer.write(" {");
        writer.write(LINE_SEPARATOR);
        writer.write("        ");
        writer.write(WAIT_COUNTER_INITIALIZE);
        writer.write(LINE_SEPARATOR);
        writer.write("      }");
        writer.write(LINE_SEPARATOR);
        writer.write("    ");
        writer.write("}");
        writer.write(LINE_SEPARATOR);

    }

    public void writeReset() throws IOException {
        Writer writer = super.getWriter();

        writer.write("      ");
        writer.write(String.format(COUNTER_FOR_LOOP,
                new Integer((this.maxSyncID + 1) * 3 - 1)));
        writer.write(" {");
        writer.write(LINE_SEPARATOR);
        writer.write("        ");
        writer.write(COUNTER_RESET);
        writer.write(LINE_SEPARATOR);
        writer.write("      }");
        writer.write(LINE_SEPARATOR);

        /* --> 19.01.10, RS: decrementing is done in the finally
        writer.write("      ");
        writer.write(String.format(COUNTER_FOR_LOOP,
                new Integer(this.maxSyncID)));
        writer.write(" {");
        writer.write(LINE_SEPARATOR);
        writer.write("        ");
        writer.write(WAIT_COUNTER_RESET);
        writer.write(LINE_SEPARATOR);
        writer.write("      }");
        writer.write(LINE_SEPARATOR);
         */
    }

    public void writeSerialzeAndReset() throws IOException {
        Writer writer = super.getWriter();

        writer.write("      ");
        writer.write(String.format(COUNTER_FOR_LOOP, new Integer(
                (this.maxSyncID))));
        writer.write(" {");
        // writer.write(LINE_SEPARATOR);

        // writer.write("        ");
        // writer.write(WAIT_COUNTER_RESET);
        writer.write(LINE_SEPARATOR);

        // the if for zero threads -> syncs[i * 3]
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

        // the if for one thread -> syncs[i * 3 + 1]
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

        // the if for more than one thread -> syncs[i * 3 + 2]
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
