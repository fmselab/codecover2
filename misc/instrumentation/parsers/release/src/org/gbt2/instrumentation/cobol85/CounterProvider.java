///////////////////////////////////////////////////////////////////////////////
//
// $Id: CounterProvider.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 22:32:13
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85;

import java.io.PrintWriter;

import org.gbt2.instrumentation.cobol85.syntaxtree.ProgramUnit;

/**
 * This class provides counter for all coverage criteria. In addition, it
 * generates the counter variable declaration.
 * 
 * @author Stefan Franke
 * @version 1.0 - 27.03.2007
 * 
 */
public class CounterProvider {

    private static final String ONE = "1";

    private static final String ZERO = "0";

    private int statementCounter;

    private int branchCounter;

    private int conditionCounter;

    private int loopCounter;

    private int testCaseCounter;

    private static NodeCounter nodeCounter = NodeCounter.getInstance();

    private static final String NEW_LINE_IN_COVERAGE_FILE = "    10 COVERAGE-TXT-NEW-LINE PIC X VALUE X\"0A\".%n";

    // String includes 1 "%d" place holder
    private static final String COUNT_STATEMENT = "COUNT-STATEMENT-%d";

    // String includes 1 "%d" place holder
    private static final String STATEMENT_COUNTER_WITHOUT_POINT = "%nADD 1 TO "
            + COUNT_STATEMENT + "%n";

    // String includes 4 "%d" place holder
    private static final String STATEMENT_COUNTER = "    10 " + COUNT_STATEMENT
            + "-TXT PIC X(%d) VALUE \"S-%d \".%n" + "    10 " + COUNT_STATEMENT
            + " PIC 9(18) VALUE ZERO.%n" + NEW_LINE_IN_COVERAGE_FILE;

    // String includes 1 "%d" place holder
    private static final String COUNT_BRANCH = "COUNT-BRANCH-%d";

    // String includes 1 "%d" place holder
    private static final String BRANCH_COUNTER_WITHOUT_POINT = "%nADD 1 TO "
            + COUNT_BRANCH + "%n";

    // String includes 4 "%d" place holder
    private static final String BRANCH_COUNTER = "    10 " + COUNT_BRANCH
            + "-TXT PIC X(%d) VALUE \"B-%d \".%n" + "    10 " + COUNT_BRANCH
            + " PIC 9(18) VALUE ZERO.%n" + NEW_LINE_IN_COVERAGE_FILE;

    // String includes 2 "%d" place holder
    private static final String COUNT_CONDITON = "COUNT-CONDITON-%d-%d";

    // String includes 2 "%d" place holder
    private static final String CONDITION_COUNTER_WITHOUT_POINT = "%nADD 1 TO "
            + COUNT_CONDITON + "%n";

    // String includes 3 "%s" place holder
    private static final String CONDITION_COUNTER_IF = "IF %s THEN %n %s ELSE %s END-IF %n";

    // String includes 7 "%d" place holder
    private static final String CONDITION_COUNTER = "    10 " + COUNT_CONDITON
            + "-TXT PIC X(%d) VALUE \"C-%d-%d \".%n" + "    10 "
            + COUNT_CONDITON + " PIC 9(18) VALUE ZERO.%n"
            + NEW_LINE_IN_COVERAGE_FILE;

    // String includes 1 "%d" place holder
    private static final String TEST_CASE_NAME = "test case %d";

    /**
     * Constructor
     */
    public CounterProvider() {
        this.statementCounter = 0;
        this.branchCounter = 0;
        this.conditionCounter = 0;
        this.loopCounter = 0;
        this.testCaseCounter = 0;
    }

    /**
     * Returns an integer containing the binary code for a given line in the
     * truth table with n variables.
     * 
     * @param variables
     *            the number of variables
     * @param line
     *            the line in the truth table
     * @return binary code as integer
     */
    private Integer getTruthTableLine(int variables, int line) {
        String truthTableLine = Integer.toBinaryString(line);
        while (truthTableLine.length() < variables) {
            truthTableLine = ZERO + truthTableLine;
        }
        String truthTableLineAdapted = new String("");
        for (int position = 0; position < variables; position++) {
            truthTableLineAdapted = truthTableLineAdapted + ONE
                    + truthTableLine.charAt(position);
        }
        return Integer.valueOf(truthTableLineAdapted);
    }

    /**
     * Returns a new branch counter as formated string.
     * 
     * @return the branch counter
     */
    public String newBranchCounter() {
        this.branchCounter++;
        return String.format(BRANCH_COUNTER_WITHOUT_POINT, new Integer(
                this.branchCounter));
    }

    /**
     * Returns a new statement counter as formated string.
     * 
     * @return the statement counter
     */
    public String newStatementCounter() {
        this.statementCounter++;
        return String.format(STATEMENT_COUNTER_WITHOUT_POINT, new Integer(
                this.statementCounter));
    }

    /**
     * Returns a new test case name.
     * 
     * @return the test case name
     */
    public String newTestCaseCounter() {
        this.testCaseCounter++;
        return String.format(TEST_CASE_NAME, new Integer(this.testCaseCounter));
    }

    /**
     * Generates all branch counter that appears in the given program unit.
     * Writes results to the output writer.
     * 
     * @param programUnit
     *            the active program unit
     * @param printWriter
     *            the output writer
     */
    public void generateBranchCounter(ProgramUnit programUnit,
            PrintWriter printWriter) {
        int begin = nodeCounter.getBranchCounterBegin(programUnit);
        int end = nodeCounter.getBranchCounterEnd(programUnit);
        for (int i = begin + 1; i <= end; i++) {
            Integer outInteger = new Integer(i);
            // format: "B-" = 2; outInterger length; " " = 1;
            Integer outIntegerLentgh = new Integer(2 + outInteger.toString()
                    .length() + 1);
            printWriter.printf(BRANCH_COUNTER, outInteger, outIntegerLentgh,
                    outInteger, outInteger);
        }
    }

    public void generateConditionCoverageBlock(PrintWriter printWriter) {
        //
    }

    /**
     * Generates all branch counter that appears in the given program unit.
     * Writes results to the output writer.
     * 
     * @param programUnit
     *            the active program unit
     * @param printWriter
     *            the output writer
     */
    public void generateConditionCounter(ProgramUnit programUnit,
            PrintWriter printWriter) {
        int begin = nodeCounter.getConditionCounterBegin(programUnit);
        int end = nodeCounter.getConditionCounterEnd(programUnit);
        for (int i = begin + 1; i <= end; i++) {
            Integer outInteger = new Integer(i);
            Double basicBooleanTerms = new Double(nodeCounter
                    .getBasicBooleanCounter(i));
            double basicBooleanCounters = Math.pow(2, basicBooleanTerms
                    .doubleValue());
            // format: "C-" = 2; outInterger length; "-" = 1; basicBooleanCounter length; " " = 1;
            Integer outIntegerLentgh = new Integer(2 + outInteger.toString()
                    .length()
                    + 1 + 2 * basicBooleanTerms.intValue() + 1);
            for (int j = 0; j < basicBooleanCounters; j++) {
                Integer basicBooleanCounter = this.getTruthTableLine(
                        basicBooleanTerms.intValue(), j);
                printWriter.printf(CONDITION_COUNTER, outInteger,
                        basicBooleanCounter, outIntegerLentgh, outInteger,
                        basicBooleanCounter, outInteger, basicBooleanCounter);
            }
        }
    }

    /**
     * Generates all statement counter that appears in the given program unit.
     * Writes results to the output writer.
     * 
     * @param programUnit
     *            the active program unit
     * @param printWriter
     *            the output writer
     */
    public void generateStatementCounter(ProgramUnit programUnit,
            PrintWriter printWriter) {
        int begin = nodeCounter.getStatementCounterBegin(programUnit);
        int end = nodeCounter.getStatementCounterEnd(programUnit);
        for (int i = begin + 1; i <= end; i++) {
            Integer outInteger = new Integer(i);
            // format: "S-" = 2; outInterger length; " " = 1;
            Integer outIntegerLentgh = new Integer(2 + outInteger.toString()
                    .length() + 1);
            printWriter.printf(STATEMENT_COUNTER, outInteger, outIntegerLentgh,
                    outInteger, outInteger);
        }
    }

}
