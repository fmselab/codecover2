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

package org.codecover.instrumentation.xampil;

import java.util.Formatter;

import org.codecover.instrumentation.measurement.MeasurementConstants;
import org.codecover.instrumentation.xampil.syntaxtree.Expression;
import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.ConditionCoverage;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;

/**
 * This class manages all the counters and IDs needed for instrumentation.<br>
 * <br>
 * A {@link CounterIDProvider} is only usable for one java source file. It allows to
 * create new counters and can write counter incrementing statements to the
 * target file. Moreover this file can create the inner class
 * CodeCoverCoverageCounter at the end of the target file.<br>
 * Every statement and branch has an ID. Using this ID a statement incrementing
 * the related counter can be created by this class - e.g.:
 * 
 * <pre>
 * CodeCoverCoverageCounter.S1++;
 * </pre>
 * 
 * @author Christoph Müller, Stefan Franke
 * @version 1.0 ($Id: CounterIDProvider.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CounterIDProvider {
    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants
    //
    // /////////////////////////////////////////////////////////////////////////

    /**
     * The prefix, that is used for counter variables to make them unique.
     * <br>
     * Here: <b>{@value #VARIABLE_PREFIX}</b>
     */
    public static final String VARIABLE_PREFIX = "CodeCoverCoverageCounter_";

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

    /**
     * This is the Format String for primary condition IDs.<br>
     * 
     * @see Formatter
     * @see #generateConditionPrimaryID(int)
     * @see ConditionCoverage#ID_PREFIX
     */
    private static final String CONDITION_PRIMARY_ID_FORMAT = ConditionCoverage.ID_PREFIX
            + "%1$d";

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
    
    // /////////////////////////////////////////////////////////////////////////
    //
    // private member fields
    //
    // /////////////////////////////////////////////////////////////////////////

    private int statementCount;

    private int branchCount;

    private int conditionCount;

    private int loopCount;

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
     * Constructs a new CounterManager.
     */
    public CounterIDProvider() {
        this.statementCount = 0;
        this.branchCount = 0;
        this.loopCount = 0;
        this.conditionCount = 0;
    }

    /**
     * Increments the StatementID.
     * 
     * @return The incremented and formatted StatementID.
     */
    public String nextStatementID() {
        return generateStatementID(++this.statementCount);
    }

    /**
     * Increments the BranchID.
     * 
     * @return The incremented and formatted BranchID.
     */
    public String nextBranchID() {
        return generateBranchID(++this.branchCount);
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
        return generateLoopPrimaryID(++this.loopCount);
    }

    /**
     * Increments the conditionID.
     * 
     * @return The incremented and formatted conditionID;
     */
    public String nextConditionID() {
        return generateConditionPrimaryID(++this.conditionCount);
    }
}
