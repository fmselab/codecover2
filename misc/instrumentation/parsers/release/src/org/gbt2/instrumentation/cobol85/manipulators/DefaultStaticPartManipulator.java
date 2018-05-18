///////////////////////////////////////////////////////////////////////////////
//
// $Id: DefaultStaticPartManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 28.03.2007 18:44:17
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;

import org.gbt2.instrumentation.cobol85.CounterProvider;
import org.gbt2.instrumentation.cobol85.NodeCounter;
import org.gbt2.instrumentation.cobol85.syntaxtree.NodeChoice;
import org.gbt2.instrumentation.cobol85.syntaxtree.NodeToken;
import org.gbt2.instrumentation.cobol85.syntaxtree.ProgramUnit;

/**
 * This is a default implementation of the static part manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 - 28.03.2007
 * 
 */
public class DefaultStaticPartManipulator implements StaticPartManipulator {

    private static final String FILE_CONTROL = "FILE-CONTROL.%n"
            + "SELECT COVERAGE-LOG-FILE ASSIGN TO \"%s\"%n"
            + "  ORGANIZATION IS LINE SEQUENTIAL.%n";

    private static final String INPUT_OUTPUT_SECTION = "INPUT-OUTPUT SECTION.%n"
            + FILE_CONTROL;

    private static final String ENVIRONMENT_DIVISION = "ENVIRONMENT DIVISION.%n"
            + INPUT_OUTPUT_SECTION;

    private static final String NEW_LINE_IN_COVERAGE_FILE = "    10 COVERAGE-TXT-NEW-LINE PIC X VALUE X\"0A\".%n";

    private static final String HORIZONTAL_LINE_IN_COVERAGE_FILE = "    10 COUNT-TXT-HORIZONTAL-LINE PIC X(66) VALUE ALL \"-\".%n";

    private static final String FILE_DESCRIPTION = "FD COVERAGE-LOG-FILE.%n"
            + "01 COVERAGE-LOG-FILE-DATA.%n"
            + "  05 COVERAGE-LOG-FILE-HEADER.%n"
            + "    10 COVERAGE-SOURCE-FILE-TXT PIC X(18) VALUE \"Program name: \".%n"
            + "    10 COVERAGE-SOURCE-FILE PIC X(11) VALUE \"%s\".%n"
            + NEW_LINE_IN_COVERAGE_FILE
            + "  05 COVERAGE-LOG-FILE-COUNTER.%n"
            + "    10 COUNT-TEST-CASE-HEADER-TXT PIC X(16) VALUE \"Test case name: \".%n"
            + "    10 COUNT-TEST-CASE-HEADER PIC X(50) VALUE \"test case 1\".%n"
            + NEW_LINE_IN_COVERAGE_FILE + HORIZONTAL_LINE_IN_COVERAGE_FILE
            + NEW_LINE_IN_COVERAGE_FILE;

    private static final String FILE_SECTION = "FILE SECTION.%n"
            + FILE_DESCRIPTION;

    private static final String DATA_DIVISION = "DATA DIVISION.%n"
            + FILE_SECTION;

    private static final String PERFORM_PROGRAM_END = "GOTO ENDTHEPROGRAMNOW.%n";

    private static final String START_TEST_CASE = "STARTTESTCASE.%n"
            + "IF START-TEST-CASE-TRUE THEN%n"
            + "  PERFORM WRITEALLCOVERAGECOUNTERTOFILE%n" + "END-IF.%n"
            + "PERFORM SETALLCOVERAGECOUNTERTOZERO.%n"
            + "MOVE \"t\" TO START-TEST-CASE-BOOLEAN.%n";

    private static final String END_TEST_CASE = "ENDTESTCASE.%n"
            + "PERFORM WRITEALLCOVERAGECOUNTERTOFILE.%n"
            + "PERFORM SETALLCOVERAGECOUNTERTOZERO.%n"
            + "MOVE \"f\" TO START-TEST-CASE-BOOLEAN.%n";

    private static final String SET_ALL_COVERAGE_COUNTER_TO_ZERO = "SETALLCOVERAGECOUNTERTOZERO.%n"
            + "INITIALIZE COVERAGE-COUNTER REPLACING NUMERIC BY ZERO.%n";

    private static final String WRITE_COVERAGE_FILE_TO_DISK = "WRITECOVERAGEFILETODISK.%n"
            + "OPEN OUTPUT COVERAGE-LOG-FILE.%n";

    private static final String WRITE_ALL_COVERAGE_COUNTER_TO_FILE = "WRITEALLCOVERAGECOUNTERTOFILE.%n"
            + "WRITE COVERAGE-LOG-FILE-DATA.%n";

    private static final String STOP_THE_PROGRAM_NOW = "STOPTHEPROGRAMNOW.%n"
            + PERFORM_PROGRAM_END + "STOP RUN.%n";

    private static final String END_THE_PRGRAM_NOW = "ENDTHEPROGRAMNOW.%n"
            + "IF START-TEST-CASE-TRUE THEN%n"
            + "  PERFORM WRITEALLCOVERAGECOUNTERTOFILE%n" + "END-IF.%n"
            + "CLOSE COVERAGE-LOG-FILE.%n";

    private static final String END_OF_PROGRAM_SECTION = PERFORM_PROGRAM_END
            + "%n" + START_TEST_CASE + "%n" + END_TEST_CASE + "%n"
            + SET_ALL_COVERAGE_COUNTER_TO_ZERO + "%n"
            + WRITE_COVERAGE_FILE_TO_DISK + "%n"
            + WRITE_ALL_COVERAGE_COUNTER_TO_FILE + "%n" + STOP_THE_PROGRAM_NOW
            + "%n" + END_THE_PRGRAM_NOW + "%n";

    private static final String PERFORM_STOP_THE_PROGRAM_NOW = "PERFORM STOPTHEPROGRAMNOW%n";

    private static final String WORKING_STORAGE_SECTION = "WORKING-STORAGE SECTION.%n"
            + "01 COVERAGE-DATA-FIELDS.%n"
            + "  05 START-TEST-CASE-BOOLEAN PIC X VALUE \"%s\".%n"
            + "    88 START-TEST-CASE-TRUE VALUE \"t\".%n";

    private static final String PERFORM_START_TEST_CASE = "PERFORM STARTTESTCASE%n";

    private static final String MOVE_TEST_CASE_NAME = "MOVE \"%s\" TO COUNT-TEST-CASE-HEADER%n";

    private static final String PERFORM_END_TEST_CASE = "PERFORM ENDTESTCASE%n";

    private static NodeCounter nodeCounter = NodeCounter.getInstance();

    private CounterProvider counterProvider;

    /**
     * Constructor
     * 
     * @param counterProvider
     *            the counter provider
     */
    public DefaultStaticPartManipulator(CounterProvider counterProvider) {
        this.counterProvider = counterProvider;
    }

    public void generateDataDivision(String programName, PrintWriter printWriter) {
        printWriter.printf(DATA_DIVISION, programName);
    }

    public void generateEnvironmentDivision(String fileName,
            PrintWriter printWriter) {
        printWriter.printf(ENVIRONMENT_DIVISION, fileName);
    }

    public void generateFileDescription(String programName,
            PrintWriter printWriter) {
        printWriter.printf(FILE_SECTION, programName);
    }

    public void generateInputOutputSection(String fileName,
            PrintWriter printWriter) {
        printWriter.printf(INPUT_OUTPUT_SECTION, fileName);
    }

    public void generateWorkingStorageSection(ProgramUnit programUnit,
            PrintWriter printWriter) {
        boolean startTestCase = nodeCounter.getStartTestCase(programUnit);
        printWriter.printf(WORKING_STORAGE_SECTION, Boolean.toString(
                startTestCase).charAt(0));
    }

    public void generateHorizontalLineInCoverageFile(PrintWriter printWriter) {
        printWriter.printf(HORIZONTAL_LINE_IN_COVERAGE_FILE);
    }

    public void generateWriteLogic(PrintWriter printWriter) {
        printWriter.printf(END_OF_PROGRAM_SECTION);
    }

    public void replaceStopRun(NodeChoice nodeChoice, PrintWriter printWriter) {
        printWriter.printf(PERFORM_STOP_THE_PROGRAM_NOW);
    }

    public void replaceEndTestCase(PrintWriter printWriter) {
        printWriter.printf(PERFORM_END_TEST_CASE);
    }

    public void replaceStartTestCase(NodeToken nodeToken,
            PrintWriter printWriter) {
        printWriter.printf(PERFORM_START_TEST_CASE);
        String testCaseName = nodeToken.tokenImage.substring(15);
        int index = testCaseName.indexOf('"');
        if (index != -1) {
            testCaseName = testCaseName.substring(index + 1);
            index = testCaseName.indexOf('"');
            if (index != -1) {
                testCaseName = testCaseName.substring(0, index);
                printWriter.printf(MOVE_TEST_CASE_NAME, testCaseName);
            } else {
                testCaseName = this.counterProvider.newTestCaseCounter();
                printWriter.printf(MOVE_TEST_CASE_NAME, testCaseName);
            }
        } else {
            testCaseName = this.counterProvider.newTestCaseCounter();
            printWriter.printf(MOVE_TEST_CASE_NAME, testCaseName);
        }
    }

}
