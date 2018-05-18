///////////////////////////////////////////////////////////////////////////////
//
// $Id: NodeCounter.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 27.03.2007 19:28:43
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85;

import java.util.Vector;

import org.gbt2.instrumentation.cobol85.syntaxtree.ProgramUnit;

/**
 * The object of this class is used to count statements, branches, conditions
 * and loops during the parser creates the abstract syntax tree. That way, the
 * information (e.g. how many counters have to be declared) is available before
 * the instrument visitor starts.
 * 
 * The singleton pattern is used.
 * 
 * @author Stefan Franke
 * @version 1.0 27.03.2007
 * 
 */
public class NodeCounter {

    private int statementCounter;

    private int branchCounter;

    private Vector<Condition> conditions;

    private int loopCounter;

    private Vector<ProgUnit> progUnits;

    private ProgUnit lastProgUnit;

    private boolean startTestCase;

    private static NodeCounter theInstance = null;

    private NodeCounter() {
        this.statementCounter = 0;
        this.branchCounter = 0;
        Condition condition = new Condition();
        this.conditions = new Vector<Condition>();
        this.conditions.add(condition);
        this.loopCounter = 0;
        ProgUnit progUnit = new ProgUnit(0, 0, 0, 0, true, null);
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

    private ProgUnit searchProgUnit(ProgramUnit programUnit) {
        if (this.lastProgUnit == null || this.lastProgUnit.programUnit != programUnit) {
            for (ProgUnit progUnit : this.progUnits) {
                if (progUnit.programUnit == programUnit) {
                    this.lastProgUnit = progUnit;
                    return progUnit;
                }
            }
        }
        return this.lastProgUnit;
    }
    
    private ProgUnit getPreviousProgUnit(ProgUnit progUnit) {
        int index = this.progUnits.indexOf(progUnit);
        return this.progUnits.elementAt(index - 1);
    }

    /**
     * Returns the number of that branch which is the first in the given program
     * unit.
     * 
     * @param programUnit
     *            the program unit
     * @return the branchCounter
     */
    public int getBranchCounterBegin(ProgramUnit programUnit) {
        ProgUnit progUnit = searchProgUnit(programUnit);
        return getPreviousProgUnit(progUnit).branchCount;
    }

    /**
     * Returns the number of that branch which is the last in the given program
     * unit.
     * 
     * @param programUnit
     *            the program unit
     * @return the branchCounter
     */
    public int getBranchCounterEnd(ProgramUnit programUnit) {
        ProgUnit progUnit = searchProgUnit(programUnit);
        return progUnit.branchCount;
    }

    /**
     * Returns the number of that condition which is the first in the given
     * program unit.
     * 
     * @param programUnit
     *            the program unit
     * @return the conditionCounter
     */
    public int getConditionCounterBegin(ProgramUnit programUnit) {
        ProgUnit progUnit = searchProgUnit(programUnit);
        return getPreviousProgUnit(progUnit).conditionCount;
    }

    /**
     * Returns the number of that condition which is the last in the given
     * program unit.
     * 
     * @param programUnit
     *            the program unit
     * @return the conditionCounter
     */
    public int getConditionCounterEnd(ProgramUnit programUnit) {
        ProgUnit progUnit = searchProgUnit(programUnit);
        return progUnit.conditionCount;
    }

    /**
     * Returns the number of that loop which is the first in the given program
     * unit.
     * 
     * @param programUnit
     *            the program unit
     * @return the loopCounter
     */
    public int getLoopCounterBegin(ProgramUnit programUnit) {
        ProgUnit progUnit = searchProgUnit(programUnit);
        return getPreviousProgUnit(progUnit).loopCount;
    }

    /**
     * Returns the number of that loop which is the last in the given program
     * unit.
     * 
     * @param programUnit
     *            the program unit
     * @return the loopCounter
     */
    public int getLoopCounterEnd(ProgramUnit programUnit) {
        ProgUnit progUnit = searchProgUnit(programUnit);
        return progUnit.loopCount;
    }

    /**
     * Returns the number of that statement which is the first in the given
     * program unit.
     * 
     * @param programUnit
     *            the program unit
     * @return the statementCounter
     */
    public int getStatementCounterBegin(ProgramUnit programUnit) {
        ProgUnit progUnit = searchProgUnit(programUnit);
        return getPreviousProgUnit(progUnit).statementCount;
    }

    /**
     * Returns the number of that statement which is the last in the given
     * program unit.
     * 
     * @param programUnit
     *            the program unit
     * @return the statementCounter
     */
    public int getStatementCounterEnd(ProgramUnit programUnit) {
        ProgUnit progUnit = searchProgUnit(programUnit);
        return progUnit.statementCount;
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
     * Returns true if a test case starts with the start of the 
     * given program unit.
     * 
     * @param programUnit the program unit
     * @return true if a test case start with the start of the given program unit
     */
    public boolean getStartTestCase(ProgramUnit programUnit) {
        ProgUnit progUnit = searchProgUnit(programUnit);
        return progUnit.sTestCase;
    }

    /**
     * Increments the statement counter by one.
     */
    public void incrementStatementCounter() {
        this.statementCounter++;
    }

    /**
     * Increments the branch counter by one.
     */
    public void incrementBranchCounter() {
        this.branchCounter++;
    }

    /**
     * Increments the branch counter by given integer.
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
     * Increments the basic boolean counter by one for the last created 
     * condition.
     */
    public void incrementBasicBooleanCounter() {
        this.conditions.lastElement().incrementBasicBooleanCounter();
    }

    /**
     * Increments the loop counter by one.
     */
    public void incrementLoopCounter() {
        this.loopCounter++;
    }

    /**
     * Sets the active program unit. This method closes the old program unit and
     * saves which counters belongs to it.
     * 
     * @param programUnit
     *            the active program unit.
     */
    public void setProgramUnit(ProgramUnit programUnit) {
        ProgUnit progUnit = new ProgUnit(this.statementCounter,
                this.branchCounter, this.conditions.size() - 1, this.loopCounter,
                this.startTestCase, programUnit);
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
     * Saves the counters and old program unit.
     * 
     * @author Stefan Franke
     * @version 1.0 - 28.03.2007
     * 
     */
    private class ProgUnit {

        private int statementCount;

        private int branchCount;

        private int conditionCount;

        private int loopCount;

        private boolean sTestCase;

        private ProgramUnit programUnit;

        /**
         * Constructor
         * 
         * @param statementCount
         * @param branchCount
         * @param conditionCount
         * @param loopCount
         * @param sTestCase
         * @param programUnit
         */
        public ProgUnit(int statementCount, int branchCount,
                int conditionCount, int loopCount, boolean sTestCase,
                ProgramUnit programUnit) {
            this.statementCount = statementCount;
            this.branchCount = branchCount;
            this.conditionCount = conditionCount;
            this.loopCount = loopCount;
            this.sTestCase = sTestCase;
            this.programUnit = programUnit;
        }

    }

    /**
     * Saves the number of basic boolean terms.
     * 
     * @author Stefan Franke
     * @version 1.0 - 28.03.2007
     * 
     */
    private class Condition {
        private int basicBooleans;
        
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
}
