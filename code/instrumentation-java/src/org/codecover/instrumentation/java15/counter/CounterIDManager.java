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

package org.codecover.instrumentation.java15.counter;

import static org.codecover.instrumentation.java15.visitor.TreeDumperWithException.LINE_SEPARATOR;

import java.io.IOException;
import java.io.Writer;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.util.Formatter;
import java.util.LinkedList;
import java.util.Queue;

import org.codecover.instrumentation.java.measurement.CounterContainer;
import org.codecover.instrumentation.java.measurement.CoverageResultLogFile;
import org.codecover.instrumentation.java.measurement.ProtocolImpl;
import org.codecover.instrumentation.java15.syntaxtree.Expression;
import org.codecover.instrumentation.measurement.CoverageCounterLog;
import org.codecover.instrumentation.measurement.CoverageResultLog;
import org.codecover.instrumentation.measurement.MeasurementConstants;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.ConditionCoverage;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;
import org.codecover.model.utils.criteria.SynchronizedStatementCoverage;
import org.codecover.model.utils.criteria.QMOCoverage;

/**
 * This class manages all the counters and IDs needed for instrumentation.<br>
 * <br>
 * A CounterIDManager is only usable for one java source file. It is managing
 * the number of IDs for counters of all criteria and allows to create an ID.<br>
 * Moreover this file can create the inner class CodeCoverCoverageCounter at the
 * end of the target file.
 * 
 * @author Christoph Müller, Stefan Franke
 * @version 1.0 ($Id: CounterIDManager.java 69 2010-01-27 19:31:18Z schmidberger $)
 * 
 */
public class CounterIDManager {
    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for the inner counting container class,
    // which is added when instrumenting
    //
    // /////////////////////////////////////////////////////////////////////////

    /**
     * The name of the inner class, containing all the counters.<br>
     * <br>
     * Here: <b>{@value #INNER_CLASS_PREFIX}</b>
     */
    private static final String INNER_CLASS_PREFIX = "CodeCoverCoverageCounter$";

    /**
     * The name of the parameter of the serialize and reset method.<br>
     * <br>
     * Here: <b>{@value #LOG_NAME}</b>
     */
    public static final String LOG_NAME = "log";

    /**
     * <pre>
     * class CodeCoverCoverageCounter implements org.codecover.instrumentation.java15.measurement.CounterContainer
     * </pre>
     */
    private static final String INNER_CLASS_HEADER = "class %s extends "
            + CounterContainer.class.getName();

    /**
     * This is the constructor of the inner class. It is needed to call the
     * super constructor: {@link CounterContainer}.
     * 
     * <pre>
     *             public CodeCoverCoverageCounter();
     * </pre>
     */
    private static final String INNER_CLASS_CONSTRUCTOR1 = "public %s ()";

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
     * The method, called by every class, when loaded.
     */
    private static final String INNER_CLASS_PING_METHOD = "public static void ping() " +
                "{/* nothing to do*/}";

    /**
     * The call in the static block, calling the ping method
     */
    private static final String PING_METHOD_CALLING = "%s.ping();";

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
     *  static {
     *    org.codecover.measurement.ProtocolImpl.getInstance(%s.getInstance()).addObservedContainer(new CodeCoverCoverageCounter());
     *  }
     * </pre>
     */
    private static final String STATIC_BLOCK = "  static {" + LINE_SEPARATOR
            + "    " + ProtocolImpl.class.getName() + "."
            + ProtocolImpl.GET_INSTANCE_METHOD_NAME + "(%1$s."
            + CoverageResultLog.GET_INSTANCE_METHOD_NAME + "(%2$s), \"%3$s\")."
            + ProtocolImpl.ADD_OBSERVED_CONTAINER_METHOD_NAME + "(new %4$s ());" + LINE_SEPARATOR + "  }";

    /**
     * This is the Format String for statementIDs.<br>
     * 
     * @see Formatter
     * @see #generateStatementID(int)
     * @see StatementCoverage#ID_PREFIX
     */
    private static final String STATEMENT_ID_FORMAT = StatementCoverage.ID_PREFIX
            + "%d";

    /**
     * This is the Format String for statementIDs.<br>
     * 
     * @see Formatter
     * @see #generateBranchID(int)
     * @see BranchCoverage#ID_PREFIX
     */
    private static final String BRANCH_ID_FORMAT = BranchCoverage.ID_PREFIX + "%1$d";

    private static final String TRY_BRANCH_ID_FORMAT = "Try%1$d";

    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for condition coverage
    //
    // /////////////////////////////////////////////////////////////////////////

    /**
     * This is the Format String for primary condition IDs.<br>
     * 
     * @see Formatter
     * @see #generateConditionPrimaryID(int)
     * @see ConditionCoverage#ID_PREFIX
     */
    private static final String CONDITION_PRIMARY_ID_FORMAT = ConditionCoverage.ID_PREFIX
            + "%1$d";

    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for loop coverage
    //
    // /////////////////////////////////////////////////////////////////////////

    /**
     * This is the Format String for primary loop IDs.<br>
     * 
     * @see Formatter
     * @see #generateLoopPrimaryID(int)
     * @see LoopCoverage#ID_PREFIX
     */
    private static final String LOOP_PRIMARY_ID_FORMAT = LoopCoverage.ID_PREFIX
            + "%1$d";

    /**
     * This is the Format String for the zero sub loop ID.<br>
     * 
     * @see Formatter
     * @see #generateLoopSubIDZero(String)
     */
    private static final String LOOP_SUB_ID_FORMAT_ZERO = "%s"
        + MeasurementConstants.ID_ASSIGNMENT_SEPERATOR + "0";

    /**
     * This is the Format String for the one sub loop ID.<br>
     * 
     * @see Formatter
     * @see #generateLoopSubIDOne(String)
     */
    private static final String LOOP_SUB_ID_FORMAT_ONE = "%s"
        + MeasurementConstants.ID_ASSIGNMENT_SEPERATOR + "1";

    /**
     * This is the Format String for the above sub loop ID.<br>
     * 
     * @see Formatter
     * @see #generateLoopSubIDAbove(String)
     */
    private static final String LOOP_SUB_ID_FORMAT_ABOVE = "%s"
        + MeasurementConstants.ID_ASSIGNMENT_SEPERATOR + "2";
    
    private static final String SYNC_STATEMENT_ID_FORMAT = SynchronizedStatementCoverage.ID_PREFIX + "%1$d";
    
    private static final String QMO_ID_FORMAT = QMOCoverage.ID_PREFIX + "%1$d";

    private static final Charset CHARSET_FOR_CREATING_NAME = Charset.forName("UTF-8");
    
    private static final CharsetEncoder ENCODER_FOR_NAME = CHARSET_FOR_CREATING_NAME.newEncoder();
    
    private static CharBuffer charBuffer = CharBuffer.allocate(64);

    // /////////////////////////////////////////////////////////////////////////
    //
    // private member fields
    //
    // /////////////////////////////////////////////////////////////////////////

    private final String fullSourceFileName;

    private String innerClassName;
    
    private Writer writer;

    /**
     * A class that is used as the {@link CoverageResultLog} when instrumenting.
     */
    private Class<? extends CoverageResultLog> coverageResultLog;

    private Queue<CounterManager> listCounterManager;

    private int statementCount;

    private int branchCount;

    private int tryBranchCount;

    private int conditionCount;

    private int loopCount;
    
    private int syncStatementCount;

    private int qmoCount; // Question mark operator

    private final String testSessionContainerUID;

    /**
     * Generates a statementID out of the statement number using
     * {@link #STATEMENT_ID_FORMAT} as a format String.
     * 
     * @param statementNumber
     *            The number of the statement as an integer.
     * 
     * @return The formatted statement ID.
     */
    public static String generateStatementID(int statementNumber) {
        return String.format(STATEMENT_ID_FORMAT, new Integer(statementNumber));
    }

    /**
     * Gets the number of a statement out of a statement ID&mdash;e.g. <b>S17</b>;
     * 
     * @param statementID
     *            The statementID.
     * 
     * @return The number of the statement ID&mdash;e.g. <b>17</b>.
     */
    public static int getNumberFromStatementID(String statementID) {
        return Integer.parseInt(statementID.substring(StatementCoverage.ID_PREFIX.length()), 10);
    }

    /**
     * Generates a branchID out of the branch number using
     * {@link #BRANCH_ID_FORMAT} as a format String.
     * 
     * @param branchNumber
     *            The number of the branch as an integer.
     * 
     * @return The formatted branchID.
     */
    public static String generateBranchID(int branchNumber) {
        return String.format(BRANCH_ID_FORMAT, new Integer(branchNumber));
    }

    /**
     * Gets the number of a branch out of a branch ID&mdash;e.g. <b>B17</b>;
     * 
     * @param branchID
     *            The branchID.
     * 
     * @return The number of the branch ID&mdash;e.g. <b>17</b>.
     */
    public static int getNumberFromBranchID(String branchID) {
        return Integer.parseInt(branchID.substring(BranchCoverage.ID_PREFIX.length()), 10);
    }

    /**
     * Generates a loopID out of the loop number using
     * {@link #LOOP_PRIMARY_ID_FORMAT} as a format String.
     * 
     * @param conditionNumber
     *            The number of the {@link Expression} as an integer.
     * 
     * @return The formatted loopID.
     */
    public static String generateConditionPrimaryID(int conditionNumber) {
        return String.format(CONDITION_PRIMARY_ID_FORMAT, new Integer(
                conditionNumber));
    }

    /**
     * Gets the number of a condition out of a condition ID&mdash;e.g. <b>C17</b>.
     * 
     * @param conditionID
     *            The statementID.
     * 
     * @return The number of the condition ID&mdash;e.g. <b>17</b>.
     */
    public static int getNumberFromConditionID(String conditionID) {
        return Integer.parseInt(conditionID.substring(ConditionCoverage.ID_PREFIX.length()), 10);
    }

    /**
     * Generates a loopID out of the loop number using
     * {@link #LOOP_PRIMARY_ID_FORMAT} as a format String.
     * 
     * @param loopNumber
     *            The number of the loop as an integer.
     * 
     * @return The formatted loopID.
     */
    public static String generateLoopPrimaryID(int loopNumber) {
        return String.format(LOOP_PRIMARY_ID_FORMAT, new Integer(loopNumber));
    }

    /**
     * Gets the number of a loop out of a loop ID&mdash;e.g. <b>L17</b>.
     * 
     * @param primaryLoopID
     *            The primaryLoopID.
     * 
     * @return The number of the loop ID&mdash;e.g. <b>17</b>.
     */
    public static int getNumberFromPrimaryLoopID(String primaryLoopID) {
        return Integer.parseInt(primaryLoopID.substring(LoopCoverage.ID_PREFIX.length()), 10);
    }

    /**
     * Generates a loopSubIDZero out of the primary loop ID
     * {@link #LOOP_SUB_ID_FORMAT_ZERO} as a format String.
     * 
     * @param primaryLoopID
     *            The primary loop ID.
     * 
     * @return The formatted loopSubIDZero.
     */
    public static String generateLoopSubIDZero(String primaryLoopID) {
        return String.format(LOOP_SUB_ID_FORMAT_ZERO, primaryLoopID);
    }

    /**
     * Generates a loopSubIDOne out of the primary loop ID
     * {@link #LOOP_SUB_ID_FORMAT_ONE} as a format String.
     * 
     * @param primaryLoopID
     *            The primary loop ID.
     * 
     * @return The formatted loopSubIDZero.
     */
    public static String generateLoopSubIDOne(String primaryLoopID) {
        return String.format(LOOP_SUB_ID_FORMAT_ONE, primaryLoopID);
    }

    /**
     * Generates a loopSubIDAove out of the primary loop ID
     * {@link #LOOP_SUB_ID_FORMAT_ABOVE} as a format String.
     * 
     * @param primaryLoopID
     *            The primary loop ID.
     * 
     * @return The formatted loopSubIDZero.
     */
    public static String generateLoopSubIDAbove(String primaryLoopID) {
        return String.format(LOOP_SUB_ID_FORMAT_ABOVE, primaryLoopID);
    }
    
    /**
     * 
     * @param statementNumber
     * @return
     */
    public static String generateSyncStatementID(int statementNumber) {
        return String.format(SYNC_STATEMENT_ID_FORMAT, new Integer(statementNumber));
    }

    /**
     * 
     * @param statementID
     * @return
     */
    public static int getNumberFromSyncStatementID(String statementID) {
        //TODO: SynchronizedCoverage
        return Integer.parseInt(statementID.substring(SynchronizedStatementCoverage.ID_PREFIX.length()), 10);
    }

    /**
     * Constructs a new CounterManager.
     * 
     * @param sourceFileName
     *            The name of the {@link SourceFile}&mdash;e.g.
     *            <code>Main.java</code>.
     * @param fullSourceFileName
     *            The full Name of the {@link SourceFile}, that is instrumented
     *            using this {@link CounterIDManager}. This contains the name
     *            of the package and the name of the source file&mdash;e.g.
     *            <code>org.codecover.Main.java</code>.
     * @param writer
     *            The writer for all incrementing statements.
     * @param coverageResultLog
     *            A class that is used as the {@link CoverageResultLog} when
     *            instumenting.
     * @param testSessionContainerUID
     *            The UID of the {@link TestSessionContainer}.
     */
    public CounterIDManager(String sourceFileName, 
            String fullSourceFileName,
            Writer writer,
            Class<? extends CoverageResultLog> coverageResultLog,
            String testSessionContainerUID) {
        this.fullSourceFileName = fullSourceFileName;
        this.innerClassName = createInnerClassName(sourceFileName);
        this.writer = writer;
        this.coverageResultLog = coverageResultLog;
        this.testSessionContainerUID = testSessionContainerUID;
        this.listCounterManager = new LinkedList<CounterManager>();

        this.statementCount = 0;
        this.branchCount = 0;
        this.tryBranchCount = 0;
        this.loopCount = 0;
        this.conditionCount = 0;
        this.syncStatementCount = 0;
    }

    /**
     * @return The {@link #fullSourceFileName} as the name of the section in the
     *         {@link CoverageResultLogFile}.
     */
    public String getSectionName() {
        return this.fullSourceFileName;
    }

    /**
     * @return The {@link #innerClassName}.
     */
    public String getInnerClassName() {
        return this.innerClassName;
    }

    /**
     * Adds a {@link CounterManager} to the list of the counter managers, these
     * are called for creating declarations and blocks in the reset and
     * serializeAndreset method.
     * 
     * @param counterManager
     *            The {@link CounterManager} to add.
     */
    public void addCounterManager(CounterManager counterManager) {
        this.listCounterManager.add(counterManager);
        counterManager.setCounterIDManager(this);
    }

    
    
    /**
     * Increments the StatementID.
     * 
     * @return The incremented and formatted StatementID.
     */
    public String nextStatementID() {
        this.statementCount++;
        return generateStatementID(this.statementCount);
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
     * Increments the conditionID.
     * 
     * @return The incremented and formatted conditionID;
     */
    public String nextConditionID() {
        this.conditionCount++;
        return String.format(CONDITION_PRIMARY_ID_FORMAT, new Integer(
                this.conditionCount));
    }

    /**
     * Increments the loopID and returns it formatted.<br>
     * <br>
     * A loopID consists of a <b>primary ID</b>, a minus and a <i>sub counter</i>&mdash;e.g.:
     * 
     * <pre>
     *              L&lt;b&gt;1&lt;/b&gt;-&lt;i&gt;0&lt;/i&gt;
     *              L&lt;b&gt;1&lt;/b&gt;-&lt;i&gt;1&lt;/i&gt;
     *              L&lt;b&gt;1&lt;/b&gt;-&lt;i&gt;2&lt;/i&gt;
     *              
     *              L&lt;b&gt;2&lt;/b&gt;-&lt;i&gt;0&lt;/i&gt;
     *              L&lt;b&gt;2&lt;/b&gt;-&lt;i&gt;1&lt;/i&gt;
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
     * 
     * @return
     */
    public String nextSyncStatementID() {
        this.syncStatementCount++;
        return String.format(SYNC_STATEMENT_ID_FORMAT, 
                new Integer(this.syncStatementCount - 1));
    }

    /**
     * the question mark operator
     * @return
     */
    public String nextQMOStatementID() {
        this.qmoCount++;
        return String.format(QMO_ID_FORMAT, 
                new Integer(this.qmoCount - 1)); // start with Q0
    }
    
    
    /**
     * Writes the calling of the inner class to start their static block, which
     * inserts the inner class to the {@link ProtocolImpl} list of observed
     * {@link CounterContainer}s.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    public void writeStaticBlock() throws IOException {
        this.writer.write(LINE_SEPARATOR);
        this.writer.write("  static {");
        this.writer.write(LINE_SEPARATOR);
        this.writer.write("    ");
        this.writer.write(String.format(PING_METHOD_CALLING,
                getInnerClassName()));
        this.writer.write(LINE_SEPARATOR);
        this.writer.write("  }");
        this.writer.write(LINE_SEPARATOR);
    }
    
    /**
     * Creates the inner class.<br>
     * <br>
     * This class contains a constructor, a reset method and a serialize and
     * reset method.
     *
     * @param preferredCoverageLogPath <code>null</code> or an absolute path
     *                                 to the coverage log file got by directive.
     * 
     * @see CounterContainer
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    public void writeInnerClass(String preferredCoverageLogPath) throws IOException {
        this.writer.write(LINE_SEPARATOR);
        this.writer.write(String.format(INNER_CLASS_HEADER,
                getInnerClassName()));
        this.writer.write(" {");
        this.writer.write(LINE_SEPARATOR);

        // /////////////////////////////
        // the static block
        // /////////////////////////////
        if (preferredCoverageLogPath == null) {
            preferredCoverageLogPath = "null";
        } else {
            preferredCoverageLogPath = "\"" +
                MeasurementConstants.escapeName(preferredCoverageLogPath) +
                "\"";
        }
        this.writer.write(LINE_SEPARATOR);
        this.writer.write(String.format(STATIC_BLOCK,
                this.coverageResultLog.getName(),
                preferredCoverageLogPath,
                this.testSessionContainerUID,
                getInnerClassName()));
        this.writer.write(LINE_SEPARATOR);

        // /////////////////////////////
        // the declarations from the CounterManager
        // /////////////////////////////

        for (CounterManager counterManager : this.listCounterManager) {
            counterManager.writeDeclarations();
        }

        // /////////////////////////////
        // the constructor
        // /////////////////////////////
        this.writer.write(LINE_SEPARATOR);
        this.writer.write("  ");
        this.writer.write(String.format(INNER_CLASS_CONSTRUCTOR1,
                getInnerClassName()));
        this.writer.write(" {");
        this.writer.write(LINE_SEPARATOR);
        this.writer.write("    ");
        this.writer.write(String.format(INNER_CLASS_CONSTRUCTOR2,
                this.fullSourceFileName));
        this.writer.write(LINE_SEPARATOR);
        this.writer.write("  ");
        this.writer.write("}");
        this.writer.write(LINE_SEPARATOR);

        // /////////////////////////////
        // the ping method
        // /////////////////////////////
        this.writer.write(LINE_SEPARATOR);
        this.writer.write("  ");
        this.writer.write(INNER_CLASS_PING_METHOD);
        this.writer.write(LINE_SEPARATOR);

        // /////////////////////////////
        // the reset method
        // /////////////////////////////
        this.writer.write(LINE_SEPARATOR);
        this.writer.write("  ");
        this.writer.write(INNER_CLASS_RESET_METHOD_HEADER);
        this.writer.write(" {");
        this.writer.write(LINE_SEPARATOR);

        // the block of the reset method from the CounterManager
        for (CounterManager counterManager : this.listCounterManager) {
            counterManager.writeReset();
        }

        // end } reset()
        this.writer.write("  }");
        this.writer.write(LINE_SEPARATOR);

        // /////////////////////////////
        // the serializeAndReset method
        // /////////////////////////////
        this.writer.write(LINE_SEPARATOR);
        this.writer.write("  ");
        this.writer.write(INNER_CLASS_SERIALZE_METHOD_HEADER);
        this.writer.write(" {");
        this.writer.write(LINE_SEPARATOR);
        this.writer.write("    ");
        this.writer.write(String.format(INNER_CLASS_START_SECTION,
                this.fullSourceFileName));
        this.writer.write(LINE_SEPARATOR);

        // the block of the serializeAndReset method from the CounterManager
        for (CounterManager counterManager : this.listCounterManager) {
            counterManager.writeSerialzeAndReset();
        }

        // end } serializeAnd reset
        this.writer.write("  }");
        this.writer.write(LINE_SEPARATOR);

        // end } inner class
        this.writer.write("}");
        this.writer.write(LINE_SEPARATOR);
    }

    private synchronized String createInnerClassName(String sourceFileName) {
        try {
            if (charBuffer.capacity() < sourceFileName.length()) {
                charBuffer = CharBuffer.allocate(sourceFileName.length());
            }
            // pos = 0
            charBuffer.clear();
            charBuffer.put(sourceFileName);
            // limit = pos, pos = 0
            charBuffer.flip();

            ByteBuffer byteBuffer = ENCODER_FOR_NAME.encode(charBuffer);

            BigInteger intRepresentation = BigInteger.ZERO;
            while(byteBuffer.hasRemaining()) {
                intRepresentation = intRepresentation.shiftLeft(8);
                intRepresentation = intRepresentation.add(BigInteger.valueOf(byteBuffer.get()));
            }

            return INNER_CLASS_PREFIX + intRepresentation.toString(36);
        } catch (CharacterCodingException e) {
            throw new RuntimeException(e);
        }
    }
}
