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

import java.util.Vector;

/**
 * The object of this class is used to count statements, branches, conditions
 * and loops during the parser creates the abstract syntax tree. That way, the
 * information (e.g. how many counters have to be declared) is available before
 * the instrument visitor starts.
 * 
 * The singleton pattern is used.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: NodeCounter.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class NodeCounter {

    private static NodeCounter theInstance = null;

    private int statementCounter;

    private int branchCounter;

    private Vector<Condition> conditions;

    private int loopCounter;

    private Vector<ProgUnit> progUnits;

    private boolean startTestCase;

    private NodeCounter() {
        this.reset();
    }

    /**
     * Resets the node counter for new instrumentation.
     */
    public void reset() {
        this.statementCounter = 0;
        this.branchCounter = 0;
        Condition condition = new Condition();
        this.conditions = new Vector<Condition>();
        this.conditions.add(condition);
        this.loopCounter = 0;
        ProgUnit progUnit = new ProgUnit(0, 0, 0, 0, true);
        this.progUnits = new Vector<ProgUnit>();
        this.progUnits.add(progUnit);
        this.startTestCase = true;
    }

    /**
     * Return the single object of this class.
     * 
     * @return the NodeCounter-object
     */
    public static NodeCounter getInstance() {
        if (theInstance == null) {
            return theInstance = new NodeCounter();
        }
        return theInstance;
    }

    /**
     * Returns the number of basic boolean terms that the given condition
     * contains.
     * 
     * @param condition
     *            the condition number
     * @return the conditionCounter if condition number is valid, else -1.
     */
    public int getBasicBooleanCounter(int condition) {
        if ( condition >= 0 && condition < this.conditions.size() ) {
            return this.conditions.elementAt(condition).basicBooleans;
        }
        return -1;
    }

    /**
     * Returns the branch counter for the entire source code file.
     * 
     * @return branch counter
     */
    public int getBranchCounter() {
        return this.branchCounter;
    }

    /**
     * Returns the number of that branch which is the first in the given program
     * unit number.
     * 
     * @param programUnit
     *            the program unit number
     * @return the branchCounter
     */
    public int getBranchCounterBegin(int programUnit) {
        ProgUnit progUnit = this.progUnits.get(programUnit - 1);
        return progUnit.branchCount;
    }

    /**
     * Returns the number of that branch which is the last in the given program
     * unit number.
     * 
     * @param programUnit
     *            the program unit number
     * @return the branchCounter
     */
    public int getBranchCounterEnd(int programUnit) {
        ProgUnit progUnit = this.progUnits.get(programUnit);
        return progUnit.branchCount;
    }

    /**
     * Returns the condition counter for the entire source code file.
     * 
     * @return condition counter
     */
    public int getConditionCounter() {
        return this.conditions.size() - 1;
    }

    /**
     * Returns the number of that condition which is the first in the given
     * program unit number.
     * 
     * @param programUnit
     *            the program unit number
     * @return the conditionCounter
     */
    public int getConditionCounterBegin(int programUnit) {
        ProgUnit progUnit = this.progUnits.get(programUnit - 1);
        return progUnit.conditionCount;
    }

    /**
     * Returns the number of that condition which is the last in the given
     * program unit number.
     * 
     * @param programUnit
     *            the program unit number
     * @return the conditionCounter
     */
    public int getConditionCounterEnd(int programUnit) {
        ProgUnit progUnit = this.progUnits.get(programUnit);
        return progUnit.conditionCount;
    }

    /**
     * Returns the loop counter for the entire source code file.
     * 
     * @return loop counter
     */
    public int getLoopCounter() {
        return this.loopCounter;
    }

    /**
     * Returns the number of that loop which is the first in the given program
     * unit number.
     * 
     * @param programUnit
     *            the program unit number
     * @return the loopCounter
     */
    public int getLoopCounterBegin(int programUnit) {
        ProgUnit progUnit = this.progUnits.get(programUnit - 1);
        return progUnit.loopCount;
    }

    /**
     * Returns the number of that loop which is the last in the given program
     * unit number.
     * 
     * @param programUnit
     *            the program unit number
     * @return the loopCounter
     */
    public int getLoopCounterEnd(int programUnit) {
        ProgUnit progUnit = this.progUnits.get(programUnit);
        return progUnit.loopCount;
    }

    /**
     * Returns true if a test case starts with the start of the 
     * given program unit number.
     * 
     * @param programUnit the program unit number
     * @return true if a test case start with the start of the given program unit
     */
    public boolean getStartTestCase(int programUnit) {
        ProgUnit progUnit = this.progUnits.get(programUnit);
        return progUnit.sTestCase;
    }

    /**
     * Returns the statement counter for the entire source code file.
     * 
     * @return statement counter
     */
    public int getStatementCounter() {
        return this.statementCounter;
    }

    /**
     * Returns the number of that statement which is the first in the given
     * program unit number.
     * 
     * @param programUnit
     *            the program unit number
     * @return the statementCounter
     */
    public int getStatementCounterBegin(int programUnit) {
        ProgUnit progUnit = this.progUnits.get(programUnit - 1);
        return progUnit.statementCount;
    }

    /**
     * Returns the number of that statement which is the last in the given
     * program unit number.
     * 
     * @param programUnit
     *            the program unit number
     * @return the statementCounter
     */
    public int getStatementCounterEnd(int programUnit) {
        ProgUnit progUnit = this.progUnits.get(programUnit);
        return progUnit.statementCount;
    }

    /**
     * Increments the basic boolean counter by one for the last created 
     * condition.
     */
    public void incrementBasicBooleanCounter() {
        this.conditions.lastElement().incrementBasicBooleanCounter();
    }

    /**
     * Increments the branch counter by one.
     */
    public void incrementBranchCounter() {
        this.branchCounter++;
    }

    /**
     * Increments the branch counter by given integer.
     * 
     * @param branches the number of branches
     */
    public void incrementBranchCounter(int branches) {
        this.branchCounter = this.branchCounter + branches;
    }

    /**
     * Increments the condition counter by one and saves the given integer of
     * basic boolean terms.
     */
    public void incrementConditionCounter() {
        this.conditions.add(new Condition());
    }

    /**
     * Increments the loop counter by one.
     */
    public void incrementLoopCounter() {
        this.loopCounter++;
    }

    /**
     * Increments the statement counter by one.
     */
    public void incrementStatementCounter() {
        this.statementCounter++;
    }

    /**
     * Sets the active program unit. This method closes the old program unit and
     * saves which counters belongs to it.
     */
    public void newProgramUnit() {
        ProgUnit progUnit = new ProgUnit(this.statementCounter,
                this.branchCounter, this.conditions.size() - 1, this.loopCounter,
                this.startTestCase);
        this.progUnits.add(progUnit);
        this.startTestCase = true;
    }
    
    /**
     * Sets the start test case variable to false. This specifies that a test
     * case does not start with the start of the program unit (there is at least
     * one test case definded in the source code).
     */
    public void setStartTestCase() {
        this.startTestCase = false;
    }

    /**
     * Saves the number of basic boolean terms.
     * 
     * @author Stefan Franke
     * @version 1.0 ($Id: NodeCounter.java 1 2007-12-12 17:37:26Z t-scheller $)
     * 
     */
    private class Condition {
        int basicBooleans;

        /**
         * Constructor
         */
        public Condition() {
            this.basicBooleans = 0;
        }

        /**
         * Increments the basic boolean counter for this condition.
         */
        public void incrementBasicBooleanCounter() {
            this.basicBooleans++;
        }
    }

    /**
     * Saves the counters and old program unit.
     * 
     * @author Stefan Franke
     * @version 1.0 ($Id: NodeCounter.java 1 2007-12-12 17:37:26Z t-scheller $)
     * 
     */
    private class ProgUnit {

        int statementCount;

        int branchCount;

        int conditionCount;

        int loopCount;

        boolean sTestCase;

        /**
         * Constructor
         * 
         * @param statementCount
         * @param branchCount
         * @param conditionCount
         * @param loopCount
         * @param sTestCase
         */
        public ProgUnit(int statementCount, int branchCount,
                int conditionCount, int loopCount, boolean sTestCase) {
            this.statementCount = statementCount;
            this.branchCount = branchCount;
            this.conditionCount = conditionCount;
            this.loopCount = loopCount;
            this.sTestCase = sTestCase;
        }

    }

}
