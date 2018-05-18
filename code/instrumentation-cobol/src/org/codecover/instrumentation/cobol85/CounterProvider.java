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

package org.codecover.instrumentation.cobol85;

import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.ConditionCoverage;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;

/**
 * This class provides counters for all coverage criteria. In addition, it
 * generates the counter variable declaration.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: CounterProvider.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CounterProvider {

    private static final String STATEMENT_COUNTER_ID = StatementCoverage.ID_PREFIX + "%d";

    private static final String BRANCH_COUNTER_ID = BranchCoverage.ID_PREFIX + "%d";

    private static final String CONDITION_COUNTER_ID = ConditionCoverage.ID_PREFIX + "%d";

    private static final String LOOP_COUNTER_ID = LoopCoverage.ID_PREFIX + "%d-";

    // String includes 1 "%d" place holder
    private static final String TEST_CASE_ID = "test case %d";

    private int statementCounter;

    private int branchCounter;

    private int conditionCounter;

    private int loopCounter;

    private int testCaseCounter;

    private int programUnitCounter;

    /**
     * Constructor
     */
    public CounterProvider() {
        this.statementCounter = 0;
        this.branchCounter = 0;
        this.conditionCounter = 0;
        this.loopCounter = 0;
        this.testCaseCounter = 0;
        this.programUnitCounter = 0;
    }

    /**
     * Returns the branch counter.
     * 
     * @return the branch counter
     */
    public int getBranchCounter() {
        return this.branchCounter;
    }

    /**
     * Returns the branch counter ID as formated string.
     * 
     * @return the branch counter
     */
    public String getBranchCounterID() {
        return String.format(BRANCH_COUNTER_ID, new Integer(
                this.branchCounter));
    }

    /**
     * Returns the condition counter.
     * 
     * @return the condition counter
     */
    public int getConditionCounter() {
        return this.conditionCounter;
    }

    /**
     * Returns the condition counter ID as formated string.
     * 
     * @return the condition counter
     */
    public String getConditionCounterID() {
        return String.format(CONDITION_COUNTER_ID, new Integer(
                this.conditionCounter));
    }

    /**
     * Returns the loop counter.
     * 
     * @return the loop counter
     */
    public int getLoopCounter() {
        return this.loopCounter;
    }

    /**
     * Returns the loop counter ID as formated string.
     * 
     * @return the loop counter
     */
    public String getLoopCounterID() {
        return String.format(LOOP_COUNTER_ID, new Integer(
                this.loopCounter));
    }

    /**
     * Returns the program unit counter.
     * 
     * @return the program unit counter
     */
    public int getProgramUnitCounter() {
        return this.programUnitCounter;
    }

    /**
     * Returns the statement counter.
     * 
     * @return the statement counter
     */
    public int getStatementCounter() {
        return this.statementCounter;
    }

    /**
     * Returns the statement counter ID as formated string.
     * 
     * @return the statement counter
     */
    public String getStatementCounterID() {
        return String.format(STATEMENT_COUNTER_ID, new Integer(
                this.statementCounter));
    }

    /**
     * Returns the test case counter.
     * 
     * @return the test case counter
     */
    public int getTestCaseCounter() {
        return this.testCaseCounter;
    }

    /**
     * Returns the test case counter ID as formated string.
     * 
     * @return the test case counter
     */
    public String getTestCaseCounterID() {
        return String.format(TEST_CASE_ID, new Integer(
                this.testCaseCounter));
    }

    /**
     * Increment the branch counter by one.
     */
    public void incrementBranchCounter() {
        this.branchCounter++;
    }

    /**
     * Increment the condition counter by one.
     */
    public void incrementConditionCounter() {
        this.conditionCounter++;
    }

    /**
     * Increment the loop counter by one.
     */
    public void incrementLoopCounter() {
        this.loopCounter++;
    }
    
    /**
     * Increment the program unit counter by one.
     */
    public void incrementProgramUnitCounter() {
        this.programUnitCounter++;
    }

    /**
     * Increment the statement counter by one.
     */
    public void incrementStatementCounter() {
        this.statementCounter++;
    }

    /**
     * Increment the test case counter by one.
     */
    public void incrementTestCaseCounter() {
        this.testCaseCounter++;
    }

}
