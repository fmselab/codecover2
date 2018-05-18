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

package org.codecover.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.codecover.instrumentation.cobol85.NodeCounter;
import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectivesManipulator;
import org.codecover.instrumentation.cobol85.syntaxtree.NodeToken;
import org.codecover.instrumentation.cobol85.syntaxtree.Statement;
import org.codecover.instrumentation.cobol85.visitor.Visitor;

/**
 * This is a default implementation of the static part manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: DefaultStaticPartManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class DefaultStaticPartManipulator implements StaticPartManipulator {

    private static final String NAME_REG_EXP = "(([^\"\\\\\n\r\t\b\f])|(\\\\[ntbrf\\\\'\"]))*";

    private static final String REG_EXP = "\\*\\>STARTTESTCASE \"("
            + NAME_REG_EXP + ")\"( \"(" + NAME_REG_EXP + ")\")?";
    
    private static final String SELECT_FILE = "%n%1$sSELECT COVERAGE-LOG-FILE ASSIGN TO \"%2$s\""
            + "%n%1$sORGANIZATION IS LINE SEQUENTIAL.%n";

    private static final String FILE_CONTROL = "%n%1$sFILE-CONTROL."
            + SELECT_FILE;

    private static final String INPUT_OUTPUT_SECTION = "%n%1$sINPUT-OUTPUT SECTION."
            + FILE_CONTROL;

    private static final String ENVIRONMENT_DIVISION = "%n%1$sENVIRONMENT DIVISION."
            + INPUT_OUTPUT_SECTION;

    private static final String NEW_LINE_IN_COVERAGE_FILE = "%n%1$s10 CC-TXT-NEW-LINE PIC X VALUE X\"0A\".";

    private static final String QUOTE_IN_COVERAGE_FILE = "%n%1$s10 CC-TXT-QUOTE PIC X VALUE \"\"\"\".";

    private static final String FILE_DESCRIPTION = "%n%1$sFD COVERAGE-LOG-FILE GLOBAL."
            + "%n%1$s01 COVERAGE-LOG-FILE-DATA."
            + "%n%1$s05 COVERAGE-LOG-FILE-HEADER."
            + "%n%1$s10 CC-CB-TXT PIC X(23) VALUE \"TEST_SESSION_CONTAINER \"."
            + QUOTE_IN_COVERAGE_FILE
            + "%n%1$s10 CC-CB PIC X(50) VALUE \"%2$s\"."
            + QUOTE_IN_COVERAGE_FILE
            + NEW_LINE_IN_COVERAGE_FILE
            + "%n%1$s05 COVERAGE-LOG-FILE-COUNTER."
            + "%n%1$s10 CC-START-TEST-CASE-TXT PIC X(16) VALUE \"START_TEST_CASE \"."
            + QUOTE_IN_COVERAGE_FILE
            + "%n%1$s10 CC-START-TEST-CASE-NAME PIC X(50) VALUE \"test case 1\"."
            + QUOTE_IN_COVERAGE_FILE 
            + "%n%1$s10 CC-TXT-SPACE PIC X VALUE \" \"."
            + QUOTE_IN_COVERAGE_FILE
            + "%n%1$s10 CC-START-TEST-CASE-COMMENT PIC X(150) VALUE SPACES."
            + QUOTE_IN_COVERAGE_FILE
            + NEW_LINE_IN_COVERAGE_FILE + "%n";

    private static final String FILE_SECTION = "%n%1$sFILE SECTION."
            + FILE_DESCRIPTION;

    private static final String DATA_DIVISION = "%n%1$sDATA DIVISION."
            + FILE_SECTION;

    private static final String FILE_DESCRIPTION_END_TEST_CASE = "%n%1$s10 CC-END-TEST-CASE-TXT PIC X(14) VALUE \"END_TEST_CASE \"."
            + QUOTE_IN_COVERAGE_FILE
            + "%n%1$s10 CC-END-TEST-CASE-NAME PIC X(50) VALUE \"test case 1\"."
            + QUOTE_IN_COVERAGE_FILE
            + NEW_LINE_IN_COVERAGE_FILE + "%n";

    private static final String PERFORM_PROGRAM_END = "%n%1$sGO TO ENDTHEPROGRAMNOW.%n";

    private static final String START_TEST_CASE = "%n%1$sSTARTTESTCASE."
            + "%n%1$sIF START-TEST-CASE-TRUE THEN"
            + "%n%1$s  PERFORM WRITEALLCOVERAGECOUNTERTOFILE"
            + "%n%1$sEND-IF."
            + "%n%1$sPERFORM SETALLCOVERAGECOUNTERTOZERO."
            + "%n%1$sMOVE \"t\" TO START-TEST-CASE-BOOLEAN.%n";

    private static final String END_TEST_CASE = "%n%1$sENDTESTCASE."
            + "%n%1$sPERFORM WRITEALLCOVERAGECOUNTERTOFILE."
            + "%n%1$sPERFORM SETALLCOVERAGECOUNTERTOZERO."
            + "%n%1$sMOVE \"f\" TO START-TEST-CASE-BOOLEAN.%n";

    private static final String SET_ALL_COVERAGE_COUNTER_TO_ZERO = "%n%1$sSETALLCOVERAGECOUNTERTOZERO."
            + "%n%1$sINITIALIZE COVERAGE-COUNTER REPLACING NUMERIC BY ZERO.%n";

    private static final String PERFORM_WRITE_COVERAGE_FILE_TO_DISK = "%n%1$sPERFORM WRITECOVERAGEFILETODISK.%n";

    private static final String WRITE_COVERAGE_FILE_TO_DISK = "%n%1$sWRITECOVERAGEFILETODISK."
            + "%n%1$sOPEN OUTPUT COVERAGE-LOG-FILE.%n";

    private static final String WRITE_ALL_COVERAGE_COUNTER_TO_FILE = "%n%1$sWRITEALLCOVERAGECOUNTERTOFILE."
            + "%n%1$sWRITE COVERAGE-LOG-FILE-DATA.%n";

    private static final String STOP_THE_PROGRAM_NOW = "%n%1$sSTOPTHEPROGRAMNOW."
            + PERFORM_PROGRAM_END + "%n%1$sSTOP RUN.%n";

    private static final String END_THE_PRGRAM_NOW = "%n%1$sENDTHEPROGRAMNOW."
            + "%n%1$sIF START-TEST-CASE-TRUE THEN"
            + "%n%1$s  PERFORM WRITEALLCOVERAGECOUNTERTOFILE"
            + "%n%1$sEND-IF."
            + "%n%1$sCLOSE COVERAGE-LOG-FILE.%n";

    private static final String END_OF_PROGRAM_SECTION = PERFORM_PROGRAM_END
            + START_TEST_CASE
            + END_TEST_CASE
            + SET_ALL_COVERAGE_COUNTER_TO_ZERO
            + WRITE_COVERAGE_FILE_TO_DISK
            + WRITE_ALL_COVERAGE_COUNTER_TO_FILE
            + STOP_THE_PROGRAM_NOW
            + END_THE_PRGRAM_NOW;

    private static final String END_OF_NESTED_PROGRAM_SECTION = PERFORM_PROGRAM_END
            + START_TEST_CASE
            + END_TEST_CASE
            + SET_ALL_COVERAGE_COUNTER_TO_ZERO
            + WRITE_COVERAGE_FILE_TO_DISK
            + WRITE_ALL_COVERAGE_COUNTER_TO_FILE
            + "%n%1$sSTOPTHEPROGRAMNOW."
            + "%n%1$sSTOP RUN.%n"
            + "%n%1$sENDTHEPROGRAMNOW.%n";

    private static final String PERFORM_STOP_THE_PROGRAM_NOW = "%n%1$sPERFORM STOPTHEPROGRAMNOW%n";
    
    private static final String DATA_ENTRIES = "%n%1$s01 COVERAGE-DATA-FIELDS."
            + "%n%1$s05 START-TEST-CASE-BOOLEAN PIC X VALUE \"%2$s\" GLOBAL."
            + "%n%1$s88 START-TEST-CASE-TRUE VALUE \"t\".%n";

    private static final String WORKING_STORAGE_SECTION = "%n%1$sWORKING-STORAGE SECTION."
            + DATA_ENTRIES;

    private static final String PERFORM_START_TEST_CASE = "%n%1$sPERFORM STARTTESTCASE%n";

    private static final String MOVE_TEST_CASE_NAME = "%n%1$sMOVE \"%2$s\" TO CC-START-TEST-CASE-NAME" +
                "%n%1$sMOVE \"%2$s\" TO CC-END-TEST-CASE-NAME" +
                "%n%1$sMOVE \"%3$s\" TO CC-START-TEST-CASE-COMMENT%n";

    private static final String PERFORM_END_TEST_CASE = "%n%1$sPERFORM ENDTESTCASE%n";

    private static NodeCounter nodeCounter = NodeCounter.getInstance();

    private static Pattern pattern = Pattern.compile(REG_EXP);

    private String whiteSpace;

    private String whiteSpaceDeclaration;

    /**
     * Constructor
     *
     * @param compilerDirectivesManipulator
     */
    public DefaultStaticPartManipulator(
            CompilerDirectivesManipulator compilerDirectivesManipulator) {
        this.whiteSpace = compilerDirectivesManipulator.insertWhiteSpace();
        this.whiteSpaceDeclaration = compilerDirectivesManipulator.insertWhiteSpaceDeclaration();
    }

    public void generateOpenCoverageFile(PrintWriter printWriter) {
        printWriter.printf(PERFORM_WRITE_COVERAGE_FILE_TO_DISK, this.whiteSpace);
    }

    public void generateDataDivision(String testSessionContainerUID, PrintWriter printWriter) {
        printWriter.printf(DATA_DIVISION, this.whiteSpaceDeclaration, testSessionContainerUID);
    }

    public void generateEndTestCaseDeclaration(PrintWriter printWriter) {
        printWriter.printf(FILE_DESCRIPTION_END_TEST_CASE, this.whiteSpaceDeclaration);
    }

    public void generateEnvironmentDivision(String fileName,
            PrintWriter printWriter) {
        printWriter.printf(ENVIRONMENT_DIVISION, this.whiteSpaceDeclaration, fileName);
    }

    public void generateFileControl(String fileName,
            PrintWriter printWriter) {
        printWriter.printf(FILE_CONTROL, this.whiteSpaceDeclaration, fileName);
    }

    public void generateFileDescription(String testSessionContainerUID,
            PrintWriter printWriter) {
        printWriter.printf(FILE_DESCRIPTION, this.whiteSpaceDeclaration, testSessionContainerUID);
    }

    public void generateFileSection(String testSessionContainerUID,
            PrintWriter printWriter) {
        printWriter.printf(FILE_SECTION, this.whiteSpaceDeclaration, testSessionContainerUID);
    }

    public void generateInputOutputSection(String fileName,
            PrintWriter printWriter) {
        printWriter.printf(INPUT_OUTPUT_SECTION, this.whiteSpaceDeclaration, fileName);
    }

    public void generateSelectFile(String fileName,
            PrintWriter printWriter) {
        printWriter.printf(SELECT_FILE, this.whiteSpaceDeclaration, fileName);
    }

    public void generateWorkingStorageSection(int programUnit,
            PrintWriter printWriter) {
        boolean startTestCase = nodeCounter.getStartTestCase(programUnit);
        printWriter.printf(WORKING_STORAGE_SECTION, this.whiteSpaceDeclaration, new Character(Boolean.toString(
                startTestCase).charAt(0)));
    }

    public void generateDataEntries(int programUnit,
            PrintWriter printWriter) {
        boolean startTestCase = nodeCounter.getStartTestCase(programUnit);
        printWriter.printf(DATA_ENTRIES, this.whiteSpaceDeclaration, new Character(Boolean.toString(
                startTestCase).charAt(0)));
    }

    public void generateWriteLogic(PrintWriter printWriter) {
        printWriter.printf(END_OF_PROGRAM_SECTION, this.whiteSpace);
    }

    public void generateWriteLogicForNestedPrograms(PrintWriter printWriter) {
        printWriter.printf(END_OF_NESTED_PROGRAM_SECTION, this.whiteSpace);
    }

    public void replaceStopRun(Visitor visitor, Statement statement, PrintWriter printWriter) {
        printWriter.printf(PERFORM_STOP_THE_PROGRAM_NOW, this.whiteSpace);
    }

    public void replaceEndTestCase(PrintWriter printWriter) {
        printWriter.printf(PERFORM_END_TEST_CASE, this.whiteSpace);
    }

    public void replaceStartTestCase(NodeToken nodeToken,
            PrintWriter printWriter) {
        String testCaseName;
        String testCaseComment;
        Matcher matcher = pattern.matcher(nodeToken.tokenImage);
        if (matcher.matches()) {
            printWriter.printf(PERFORM_START_TEST_CASE);
            testCaseName = matcher.group(1).replaceAll("\"", "\"\"");
            testCaseComment = matcher.group(6);
            if (testCaseComment == null) {
                printWriter.printf(MOVE_TEST_CASE_NAME, this.whiteSpace, testCaseName, testCaseName, " ");
            } else {
                testCaseComment = testCaseComment.replaceAll("\"", "\"\"");
                printWriter.printf(MOVE_TEST_CASE_NAME, this.whiteSpace, testCaseName, testCaseName, testCaseComment);
            }
        } //else {
            //throw new ParseException("Start test case parse error.", 0);
        //}

    }

}
