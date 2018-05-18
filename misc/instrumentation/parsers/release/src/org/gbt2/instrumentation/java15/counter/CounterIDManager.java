///////////////////////////////////////////////////////////////////////////////
//
// $Id: CounterIDManager.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 18:40:42
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.counter;

import static org.gbt2.instrumentation.java15.visitor.TreeDumperWithException.LINE_SEPERATOR;

import java.io.IOException;
import java.io.Writer;
import java.util.LinkedList;
import java.util.Queue;

import org.gbt2.instrumentation.java15.measurement.CounterContainer;
import org.gbt2.instrumentation.java15.measurement.CoverageCounterLog;
import org.gbt2.instrumentation.java15.measurement.CoverageResultLog;
import org.gbt2.instrumentation.java15.measurement.ProtocolImpl;

/**
 * This class manages all the counters and IDs needed for instrumentation.<br>
 * <br>
 * A CounterIDManager is only useable for one java source file. It allows to
 * create new counters and can write counter incrementing statements to the
 * target file. Moreover this file can create the inner class
 * Gbt2CoverageCounter at the end of the target file.<br>
 * Every statement and branch has an ID. Using this ID a statement incrementing
 * the related counter can be created by this class - e.g.:
 * 
 * <pre>
 * Gbt2CoverageCounter.S1++;
 * </pre>
 * 
 * @author Christoph Müller, Stefan Franke
 * 
 * TODO handle "too many constants" compile error
 */
public class CounterIDManager {
    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for the inner counting container class,
    // which is added when instrumenting
    //
    // /////////////////////////////////////////////////////////////////////////

    static final String INNER_CLASS_NAME = "Gbt2CoverageCounter";

    static final String LOG_NAME = "log";

    /**
     * <pre>
     *          private static class Gbt2CoverageCounter implements org.gbt2.instrumentation.java1_5.measurement.CounterContainer
     * </pre>
     */
    private static final String INNER_CLASS_HEADER = "private static class "
            + INNER_CLASS_NAME + " extends " + CounterContainer.class.getName();

    /**
     * This is the constructor of the inner class. It is needed to call the
     * super constructor: {@link CounterContainer}.
     * 
     * <pre>
     *       public Gbt2CoverageCounter();
     * </pre>
     */
    private static final String INNER_CLASS_CONSTRUCTOR1 = "public "
            + INNER_CLASS_NAME + " ()";

    /**
     * This is the constructor of the inner class. It is needed to call the
     * super constructor: {@link CounterContainer}.
     * 
     * <pre>
     * super(&quot;%s&quot;);
     * </pre>
     */
    private static final String INNER_CLASS_CONSTRUCTOR2 = "super(\"%s\");";

    /**
     * The header for the method required by {@link CounterContainer#reset()}.
     */
    private static final String INNER_CLASS_RESET_METHOD_HEADER = "public void "
            + CounterContainer.RESET_METHOD_NAME + "()";

    /**
     * The header for the method required by
     * {@link CounterContainer#serializeAndReset(CoverageCounterLog)}.
     */
    private static final String INNER_CLASS_SERIALZE_METHOD_HEADER = "public void "
            + CounterContainer.SERIALIZE_AND_RESET_METHOD_NAME
            + "("
            + CoverageCounterLog.class.getName() + " " + LOG_NAME + ")";

    /**
     * The call of the {@link CoverageCounterLog#startNamedSection(String)} in
     * the {@link CounterContainer#serializeAndReset(CoverageCounterLog)} method
     * of the inner class.
     */
    private static final String INNER_CLASS_START_SECTION = LOG_NAME + "."
            + CoverageCounterLog.START_SECTION_METHOD_NAME + "(\"%s\");";

    /**
     * <pre>
     *       static {
     *         org.gbt2.measurement.ProtocolImpl.getInstance(%s.getInstance()).addObservedContainer(new Gbt2CoverageCounter());
     *       }
     * </pre>
     */
    private static final String STATIC_BLOCK = "  static {" + LINE_SEPERATOR
            + "    " + ProtocolImpl.class.getName() + "."
            + ProtocolImpl.GET_INSTANCE_METHOD_NAME + "(%s."
            + CoverageResultLog.GET_INSTANCE_METHOD_NAME + "())."
            + ProtocolImpl.ADD_OBSERVED_CONTAINER_METHOD_NAME + "(new "
            + INNER_CLASS_NAME + "());" + LINE_SEPERATOR + "  }";

    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for branch coverage
    //
    // /////////////////////////////////////////////////////////////////////////

    private static final String BRANCH_ID_FORMAT = "B%1$d";

    private static final String COUNTER_INCREMENTING_FOR_BRANCH = INNER_CLASS_NAME
            + ".%s++;";

    private static final String COUNTER_DECLARATION_FOR_BRANCH = "public static long %s = 0L;";

    private static final String COUNTER_RESET_FOR_BRANCH = "%s = 0L;";

    private static final String COUNTER_SERIALIZE1_FOR_BRANCH = "if ( %s != 0L)";

    private static final String COUNTER_SERIALIZE2_FOR_BRANCH = LOG_NAME + "."
            + CoverageCounterLog.PASS_COUNTER_METHOD_NAME + "(\"%1$s\", %1$s);";

    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for try catch coverage
    //
    // /////////////////////////////////////////////////////////////////////////

    private static final String TRY_BRANCH_ID_FORMAT = "Try%1$d";

    private static final String TRY_BRANCH_BOOLEAN_SET_TRUE = INNER_CLASS_NAME
            + ".%s = true;";

    private static final String TRY_BRANCH_BOOLEAN_SET_FALSE = INNER_CLASS_NAME
            + ".%s = false;";

    private static final String TRY_BRANCH_BOOLEAN_DECLARATION = "public static boolean %s = false;";

    private static final String TRY_BRANCH_BOOLEAN_FINALLY = "if ( "
            + INNER_CLASS_NAME + ".%s )";

    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for loop coverage
    //
    // /////////////////////////////////////////////////////////////////////////

    private static final String LOOP_PRIMARY_ID_FORMAT = "L%1$d";

    private static final String LOOP_SUB_ID_FORMAT_ZERO = "%s_0";

    private static final String LOOP_SUB_ID_FORMAT_ONE = "%s_1";

    private static final String LOOP_SUB_ID_FORMAT_ABOVE = "%s_2";

    private static final String COUNTER_INCREMENTING_FOR_LOOP = INNER_CLASS_NAME
            + ".%s++;";

    private static final String COUNTER_DECLARATION_FOR_LOOP = "public static long %s = 0L;";

    private static final String COUNTER_RESET_FOR_LOOP = "%s = 0L;";

    private static final String COUNTER_SERIALIZE1_FOR_LOOP = "if ( %s != 0L)";

    private static final String COUNTER_SERIALIZE2_FOR_LOOP = LOG_NAME + "."
            + CoverageCounterLog.PASS_COUNTER_METHOD_NAME + "(\"%1$s\", %2$s);";

    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for the loop helper
    //
    // /////////////////////////////////////////////////////////////////////////

    private static final String LOOP_HELPER_ID_FORMAT = "%1$s_helper";

    private static final String COUNTER_INCREMENTING_FOR_LOOP_HELPER = INNER_CLASS_NAME
            + ".%s++;";

    private static final String COUNTER_SET_ZERO_FOR_LOOP_HELPER = INNER_CLASS_NAME
            + ".%s = 0L;";

    private static final String COUNTER_DECLARATION_FOR_LOOP_HELPER = "public static long %s = 0L;";

    private static final String LOOP_HELPER_EVALUATION_ZERO = "if ("
            + INNER_CLASS_NAME + "." + LOOP_HELPER_ID_FORMAT + " == 0)";

    private static final String LOOP_HELPER_EVALUATION_SINGLE_ONE = "if ("
            + INNER_CLASS_NAME + "." + LOOP_HELPER_ID_FORMAT + " == 1)";

    private static final String LOOP_HELPER_EVALUATION_ONE = " else if ("
            + INNER_CLASS_NAME + "." + LOOP_HELPER_ID_FORMAT + " == 1)";

    private static final String LOOP_HELPER_EVALUATION_ABOVE = " else";

    // /////////////////////////////////////////////////////////////////////////
    //
    // private member fields
    //
    // /////////////////////////////////////////////////////////////////////////

    private final String fullClassName;

    private Writer writer;

    /**
     * A class that is used as the {@link CoverageResultLog} when instumenting.
     */
    private Class<? extends CoverageResultLog> coverageResultLog;

    private StatementCounterManager statementCounterManager;
    
    private int branchCount;

    private int tryBranchCount;

    private int loopCount;

    private int conditionCount;

    private int methodCount;

    private Queue<String> usedBranchIDs;
    
    private Queue<String> usedTryBranchIDs;

    private Queue<String> usedLoopSubIDs;

    private Queue<String> usedLoopHelpers;

    /**
     * Constructs a new CounterManager.
     * 
     * @param fullClassName
     *            The name of the class, that is instrumented using this
     *            {@link CounterIDManager}. This name is the results of
     *            {@link Class#getName()}.
     * @param writer
     *            The writer for all incrementing statements.
     * @param coverageResultLog
     *            A class that is used as the {@link CoverageResultLog} when
     *            instumenting.
     */
    public CounterIDManager(String fullClassName, Writer writer,
            Class<? extends CoverageResultLog> coverageResultLog) {
        this.fullClassName = fullClassName;
        this.writer = writer;
        this.coverageResultLog = coverageResultLog;

        this.statementCounterManager = new ArrayStatementCounterManager(
                this.writer);
        this.branchCount = 0;
        this.tryBranchCount = 0;
        this.loopCount = 0;
        this.conditionCount = 0;
        this.methodCount = 0;
        this.usedBranchIDs = new LinkedList<String>();
        this.usedTryBranchIDs = new LinkedList<String>();
        this.usedLoopSubIDs = new LinkedList<String>();
        this.usedLoopHelpers = new LinkedList<String>();
    }

    /**
     * Increments the StatementID.
     * 
     * @return The incremented and formatted StatementID.
     */
    public String nextStatementID() {
        return this.statementCounterManager.nextStatementID();
    }

    /**
     * Increments the BranchID.
     * 
     * @return The incremented and formatted BranchID.
     */
    public String nextBranchID() {
        this.branchCount++;
        return String.format(BRANCH_ID_FORMAT, new Integer(this.branchCount));
    }

    /**
     * Increments the TryBranchID.
     * 
     * @return The incremented and formatted TryBranch ID;
     */
    public String nextTryBranchID() {
        this.tryBranchCount++;
        return String.format(TRY_BRANCH_ID_FORMAT, new Integer(
                this.tryBranchCount));
    }

    /**
     * Increments the loopID and returns it formatted.<br>
     * <br>
     * A loopID consists of a <b>primary ID</b>, a minus and a <i>sub counter</i>&mdash;e.g.:
     * 
     * <pre>
     *        L&lt;b&gt;1&lt;/b&gt;-&lt;i&gt;0&lt;/i&gt;
     *        L&lt;b&gt;1&lt;/b&gt;-&lt;i&gt;1&lt;/i&gt;
     *        L&lt;b&gt;1&lt;/b&gt;-&lt;i&gt;2&lt;/i&gt;
     *        
     *        L&lt;b&gt;2&lt;/b&gt;-&lt;i&gt;0&lt;/i&gt;
     *        L&lt;b&gt;2&lt;/b&gt;-&lt;i&gt;1&lt;/i&gt;
     * </pre>
     * 
     * @return The incremented and formatted primary loopID.
     */
    public String nextLoopID() {
        this.loopCount++;

        return String.format(LOOP_PRIMARY_ID_FORMAT,
                new Integer(this.loopCount));
    }

    /**
     * Writes a statement for incrementing the counter of a statementID.<br>
     * <br>
     * The statementID is saved for {@link #writeInnerClass()}.
     * 
     * @param statementID
     *            The ID of the statement.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    public void writeCounterIncrementingForStatement(String statementID)
            throws IOException {
        this.statementCounterManager
                .writeCounterIncrementingForStatement(statementID);
    }

    /**
     * Writes a statement for incrementing the counter of a branchID.<br>
     * <br>
     * The branchID is saved for {@link #writeInnerClass()}.
     * 
     * @param branchID
     *            The ID of the branch.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    public void writeCounterIncrementingForBranch(String branchID)
            throws IOException {
        this.writer.write(String.format(COUNTER_INCREMENTING_FOR_BRANCH,
                branchID));
        this.usedBranchIDs.add(branchID);
    }

    /**
     * Writes a statement for setting the boolean of the TryBranchID to true.
     * 
     * @param tryBranchID
     *            The TryBranchID of the try branch. Got with
     *            {@link #nextTryBranchID()}.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    public void writeTryBranchSetTrue(String tryBranchID) throws IOException {
        this.writer.write(String.format(TRY_BRANCH_BOOLEAN_SET_TRUE,
                tryBranchID));
        this.usedTryBranchIDs.add(tryBranchID);
    }

    /**
     * Writes a statement for setting the boolean of the TryBranchID to false.
     * 
     * @param tryBranchID
     *            The TryBranchID of the try branch. Got with
     *            {@link #nextTryBranchID()}.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    public void writeTryBranchSetFalse(String tryBranchID) throws IOException {
        this.writer.write(String.format(TRY_BRANCH_BOOLEAN_SET_FALSE,
                tryBranchID));
    }

    /**
     * Writes a statement for incrementing the counter of a branchID of a the
     * try branch.<br>
     * <br>
     * The branchID is saved for {@link #writeInnerClass()}.
     * 
     * @param branchID
     *            The branchID of the try branch. Got with
     *            {@link #nextBranchID()}.
     * 
     * @param tryBranchID
     *            The TryBranchID of the try branch. Got with
     *            {@link #nextTryBranchID()}.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    public void writeTryBranchFinally(String branchID, String tryBranchID)
            throws IOException {
        this.writer.write(String
                .format(TRY_BRANCH_BOOLEAN_FINALLY, tryBranchID));
        this.writer.write(" {");
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("  ");
        this.writer.write(String.format(COUNTER_INCREMENTING_FOR_BRANCH,
                branchID));
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("}");

        this.usedBranchIDs.add(branchID);
    }

    /**
     * Writes a statement for setting the help counter of a primary loopID to
     * zero.
     * 
     * @param primaryLoopID
     *            The corresponding primary loopID.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    public void writeLoopHelperSetZero(String primaryLoopID) throws IOException {
        String loopHelper = String.format(LOOP_HELPER_ID_FORMAT, primaryLoopID);

        this.writer.write(String.format(COUNTER_SET_ZERO_FOR_LOOP_HELPER,
                loopHelper));

        this.usedLoopHelpers.add(loopHelper);
    }

    /**
     * Writes a statement for incrementing the help counter of a primary loopID.
     * 
     * @param primaryLoopID
     *            The corresponding primary loopID.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    public void writeLoopHelperIncrementing(String primaryLoopID)
            throws IOException {
        String loopHelper = String.format(LOOP_HELPER_ID_FORMAT, primaryLoopID);

        this.writer.write(String.format(COUNTER_INCREMENTING_FOR_LOOP_HELPER,
                loopHelper));
    }

    /**
     * Writes if blocks for evalutation the helper counter of a primary loopID.<br>
     * <br>
     * Within these if blocks the state of the helper counter is evaluated and
     * the corresponding subID counter of the primary loopID is incremented.
     * 
     * @param primaryLoopID
     *            The corresponding primary loopID.
     * @param Mode012
     *            <ul>
     *            <li>true &rarr; this loop has the modes 0 times, 1 time, more
     *            than 1 time executed;</li>
     *            <li>false &rarr; the loop has only the modes 1 time and more
     *            than 1 time executed
     *            </ul>
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    public void writeHelperEvaluationForLoop(String primaryLoopID,
            boolean Mode012) throws IOException {
        if (Mode012) {
            String loopCounter0 = String.format(LOOP_SUB_ID_FORMAT_ZERO,
                    primaryLoopID);
            String loopCounter1 = String.format(LOOP_SUB_ID_FORMAT_ONE,
                    primaryLoopID);
            String loopCounter2 = String.format(LOOP_SUB_ID_FORMAT_ABOVE,
                    primaryLoopID);

            this.writer.write(String.format(LOOP_HELPER_EVALUATION_ZERO,
                    primaryLoopID));
            this.writer.write(" {");
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("  ");
            this.writer.write(String.format(COUNTER_INCREMENTING_FOR_LOOP,
                    loopCounter0));
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("}");

            this.writer.write(String.format(LOOP_HELPER_EVALUATION_ONE,
                    primaryLoopID));
            this.writer.write(" {");
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("  ");
            this.writer.write(String.format(COUNTER_INCREMENTING_FOR_LOOP,
                    loopCounter1));
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("}");

            this.writer.write(String.format(LOOP_HELPER_EVALUATION_ABOVE,
                    primaryLoopID));
            this.writer.write(" {");
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("  ");
            this.writer.write(String.format(COUNTER_INCREMENTING_FOR_LOOP,
                    loopCounter2));
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("}");

            this.usedLoopSubIDs.add(loopCounter0);
            this.usedLoopSubIDs.add(loopCounter1);
            this.usedLoopSubIDs.add(loopCounter2);
        } else {
            String loopCounter1 = String.format(LOOP_SUB_ID_FORMAT_ONE,
                    primaryLoopID);
            String loopCounter2 = String.format(LOOP_SUB_ID_FORMAT_ABOVE,
                    primaryLoopID);

            this.writer.write(String.format(LOOP_HELPER_EVALUATION_SINGLE_ONE,
                    primaryLoopID));
            this.writer.write(" {");
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("  ");
            this.writer.write(String.format(COUNTER_INCREMENTING_FOR_LOOP,
                    loopCounter1));
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("}");

            this.writer.write(String.format(LOOP_HELPER_EVALUATION_ABOVE,
                    primaryLoopID));
            this.writer.write(" {");
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("  ");
            this.writer.write(String.format(COUNTER_INCREMENTING_FOR_LOOP,
                    loopCounter2));
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("}");

            this.usedLoopSubIDs.add(loopCounter1);
            this.usedLoopSubIDs.add(loopCounter2);
        }
    }

    /**
     * Writes the static block at the top of a class&mdash;e.g.:
     * 
     * <pre>
     * static {
     *     org.gbt2.measurement.ProtocolImpl
     *             .addObservedContainer(new Gbt2CoverageCounter());
     * }
     * </pre>
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    public void writeStaticBlock() throws IOException {
        this.writer.write(LINE_SEPERATOR);
        this.writer.write(String.format(STATIC_BLOCK, this.coverageResultLog
                .getName()));
        this.writer.write(LINE_SEPERATOR);
    }

    /**
     * Creates the inner class.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    public void writeInnerClass() throws IOException {
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("  ");
        this.writer.write(INNER_CLASS_HEADER);
        this.writer.write(" {");
        this.writer.write(LINE_SEPERATOR);

        // /////////////////////////////
        // declarations
        // /////////////////////////////

        // the declarations for the statement counters
        this.statementCounterManager.writeDeclarations();

        // the declarations for the branch counters
        if (this.usedBranchIDs.size() > 0) {
            this.writer.write(LINE_SEPERATOR);
        }
        for (String thisBranchID : this.usedBranchIDs) {
            this.writer.write("    ");
            this.writer.write(String.format(COUNTER_DECLARATION_FOR_BRANCH,
                    thisBranchID));
            this.writer.write(LINE_SEPERATOR);
        }

        // the declarations for the try branch booleans
        if (this.usedTryBranchIDs.size() > 0) {
            this.writer.write(LINE_SEPERATOR);
        }
        for (String thisTryBranchID : this.usedTryBranchIDs) {
            this.writer.write("    ");
            this.writer.write(String.format(TRY_BRANCH_BOOLEAN_DECLARATION,
                    thisTryBranchID));
            this.writer.write(LINE_SEPERATOR);
        }

        // the declarations for the loop counters
        if (this.usedLoopSubIDs.size() > 0) {
            this.writer.write(LINE_SEPERATOR);
        }
        for (String thisLoopSubID : this.usedLoopSubIDs) {
            this.writer.write("    ");
            this.writer.write(String.format(COUNTER_DECLARATION_FOR_LOOP,
                    thisLoopSubID));
            this.writer.write(LINE_SEPERATOR);
        }

        // the declarations for the loop help counters
        if (this.usedLoopHelpers.size() > 0) {
            this.writer.write(LINE_SEPERATOR);
        }
        for (String thisLoopHelper : this.usedLoopHelpers) {
            this.writer.write("    ");
            this.writer.write(String.format(
                    COUNTER_DECLARATION_FOR_LOOP_HELPER, thisLoopHelper));
            this.writer.write(LINE_SEPERATOR);
        }

        // /////////////////////////////
        // the constructor
        // /////////////////////////////

        this.writer.write(LINE_SEPERATOR);
        this.writer.write("    ");
        this.writer.write(INNER_CLASS_CONSTRUCTOR1);
        this.writer.write(" {");
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("      ");
        this.writer.write(String.format(INNER_CLASS_CONSTRUCTOR2,
                this.fullClassName));
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("    ");
        this.writer.write("}");
        this.writer.write(LINE_SEPERATOR);

        // /////////////////////////////
        // the reset method
        // /////////////////////////////
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("    @Override");
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("    ");
        this.writer.write(INNER_CLASS_RESET_METHOD_HEADER);
        this.writer.write(" {");
        this.writer.write(LINE_SEPERATOR);

        // the reseter for the statement counters
        this.statementCounterManager.writeReset();

        // the reseter for the branch counters
        if (this.usedBranchIDs.size() > 0) {
            this.writer.write(LINE_SEPERATOR);
        }
        for (String thisBranchID : this.usedBranchIDs) {
            this.writer.write("      ");
            this.writer.write(String.format(COUNTER_RESET_FOR_BRANCH,
                    thisBranchID));
            this.writer.write(LINE_SEPERATOR);
        }

        // the reseter for the loop counters
        if (this.usedLoopSubIDs.size() > 0) {
            this.writer.write(LINE_SEPERATOR);
        }
        for (String thisLoopSubID : this.usedLoopSubIDs) {
            this.writer.write("      ");
            this.writer.write(String.format(COUNTER_RESET_FOR_LOOP,
                    thisLoopSubID));
            this.writer.write(LINE_SEPERATOR);
        }

        // end } reset()
        this.writer.write("    }");

        // /////////////////////////////
        // the serializeAndReset method
        // /////////////////////////////
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("    @Override");
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("    ");
        this.writer.write(INNER_CLASS_SERIALZE_METHOD_HEADER);
        this.writer.write(" {");
        this.writer.write(LINE_SEPERATOR);
        this.writer.write("      ");
        this.writer.write(String.format(INNER_CLASS_START_SECTION,
                this.fullClassName));
        this.writer.write(LINE_SEPERATOR);

        // serializeAndReset for the statement counters
        this.statementCounterManager.writeSerialzeAndReset();

        // serializeAndReset for the branch counters
        if (this.usedBranchIDs.size() > 0) {
            this.writer.write(LINE_SEPERATOR);
        }
        for (String thisBranchID : this.usedBranchIDs) {
            this.writer.write("      ");
            this.writer.write(String.format(COUNTER_SERIALIZE1_FOR_BRANCH,
                    thisBranchID));
            this.writer.write(" {");
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("        ");
            this.writer.write(String.format(COUNTER_SERIALIZE2_FOR_BRANCH,
                    thisBranchID));
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("        ");
            this.writer
                    .write(String.format(COUNTER_RESET_FOR_BRANCH, thisBranchID));
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("      }");
            this.writer.write(LINE_SEPERATOR);
        }

        // serializeAndReset for the loop counters
        if (this.usedLoopSubIDs.size() > 0) {
            this.writer.write(LINE_SEPERATOR);
        }
        for (String thisLoopSubID : this.usedLoopSubIDs) {
            String loopSubIDMinus = thisLoopSubID.replace('_', '-');
            
            this.writer.write("      ");
            this.writer.write(String.format(COUNTER_SERIALIZE1_FOR_LOOP,
                    thisLoopSubID));
            this.writer.write(" {");
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("        ");
            this.writer.write(String.format(COUNTER_SERIALIZE2_FOR_LOOP,
                    loopSubIDMinus, thisLoopSubID));
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("        ");
            this.writer.write(String.format(COUNTER_RESET_FOR_LOOP,
                    thisLoopSubID));
            this.writer.write(LINE_SEPERATOR);
            this.writer.write("      }");
            this.writer.write(LINE_SEPERATOR);
        }

        // end } serializeAnd reset
        this.writer.write("    }");
        this.writer.write(LINE_SEPERATOR);

        // end } inner class
        this.writer.write("  }");
        this.writer.write(LINE_SEPERATOR);
    }
}
