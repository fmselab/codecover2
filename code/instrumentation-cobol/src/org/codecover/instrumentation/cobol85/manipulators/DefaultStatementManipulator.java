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

import org.codecover.instrumentation.cobol85.CounterProvider;
import org.codecover.instrumentation.cobol85.NodeCounter;
import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectivesManipulator;

/**
 * This is a default implementation of the statement manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: DefaultStatementManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public class DefaultStatementManipulator implements StatementManipulator {

    // String includes 1 "%d" place holder
    private static final String COUNT_STATEMENT = "COUNT-STATEMENT-%2$d";

    private static final String END_ADD = "%n%1$sEND-ADD%n";

    private static final String END_CALL = "%n%1$sEND-CALL%n";

    private static final String END_COMPUTE = "%n%1$sEND-COMPUTE%n";

    private static final String END_DELETE = "%n%1$sEND-DELETE%n";

    private static final String END_DIVIDE = "%n%1$sEND-DIVIDE%n";

    private static final String END_EVALUATE = "%n%1$sEND-EVALUATE%n";

    private static final String END_IF = "%n%1$sEND-IF%n";

    private static final String END_MULTIPLY = "%n%1$sEND-MULTIPLY%n";

    private static final String END_READ = "%n%1$sEND-READ%n";

    private static final String END_REWRITE = "%n%1$sEND-REWRITE%n";

    private static final String END_SEARCH = "%n%1$sEND-SEARCH%n";

    private static final String END_START = "%n%1$sEND-START%n";

    private static final String END_STRING = "%n%1$sEND-STRING%n";

    private static final String END_SUBTRACT = "%n%1$sEND-SUBTRACT%n";

    private static final String END_UNSTRING = "%n%1$sEND-UNSTRING%n";

    private static final String END_WRITE = "%n%1$sEND-WRITE%n";

    private static final String NEW_LINE_IN_COVERAGE_FILE = "10 COVERAGE-TXT-NEW-LINE PIC X VALUE X\"0A\".%n";

    private static NodeCounter nodeCounter = NodeCounter.getInstance();

    // String includes 4 "%d" place holder
    private static final String STATEMENT_COUNTER = "%n%1$s10 " + COUNT_STATEMENT
            + "-TXT PIC X(%3$d) VALUE \"S%2$d\".%n%1$s10 "
            + COUNT_STATEMENT + " PIC 9(18) VALUE ZERO.%n%1$s"
            + NEW_LINE_IN_COVERAGE_FILE;

    // String includes 1 "%d" place holder
    private static final String STATEMENT_COUNTER_WITHOUT_POINT = "%n%1$sADD 1 TO "
            + COUNT_STATEMENT + "%n%1$s";

    private CounterProvider counterProvider;

    private String whiteSpace;

    private String whiteSpaceDeclaration;

    /**
     * Constructor
     *
     * @param counterProvider the counter provider
     * @param compilerDirectivesManipulator 
     */
    public DefaultStatementManipulator(CounterProvider counterProvider, CompilerDirectivesManipulator compilerDirectivesManipulator) {
        this.counterProvider = counterProvider;
        this.whiteSpace = compilerDirectivesManipulator.insertWhiteSpace();
        this.whiteSpaceDeclaration = compilerDirectivesManipulator.insertWhiteSpaceDeclaration();
    }

    public void generateEndAdd(PrintWriter printWriter) {
        printWriter.printf(END_ADD, this.whiteSpace);
    }

    public void generateEndCall(PrintWriter printWriter) {
        printWriter.printf(END_CALL, this.whiteSpace);
    }

    public void generateEndCompute(PrintWriter printWriter) {
        printWriter.printf(END_COMPUTE, this.whiteSpace);
    }

    public void generateEndDelete(PrintWriter printWriter) {
        printWriter.printf(END_DELETE, this.whiteSpace);
    }

    public void generateEndDivide(PrintWriter printWriter) {
        printWriter.printf(END_DIVIDE, this.whiteSpace);
    }

    public void generateEndEvaluate(PrintWriter printWriter) {
        printWriter.printf(END_EVALUATE, this.whiteSpace);
    }

    public void generateEndIf(PrintWriter printWriter) {
        printWriter.printf(END_IF, this.whiteSpace);
    }

    public void generateEndMultiply(PrintWriter printWriter) {
        printWriter.printf(END_MULTIPLY, this.whiteSpace);
    }

    public void generateEndRead(PrintWriter printWriter) {
        printWriter.printf(END_READ, this.whiteSpace);
    }

    public void generateEndRewrite(PrintWriter printWriter) {
        printWriter.printf(END_REWRITE, this.whiteSpace);
    }

    public void generateEndSearch(PrintWriter printWriter) {
        printWriter.printf(END_SEARCH, this.whiteSpace);
    }

    public void generateEndStart(PrintWriter printWriter) {
        printWriter.printf(END_START, this.whiteSpace);
    }

    public void generateEndString(PrintWriter printWriter) {
        printWriter.printf(END_STRING, this.whiteSpace);
    }

    public void generateEndSubtract(PrintWriter printWriter) {
        printWriter.printf(END_SUBTRACT, this.whiteSpace);
    }

    public void generateEndUnstring(PrintWriter printWriter) {
        printWriter.printf(END_UNSTRING, this.whiteSpace);
    }

    public void generateEndWrite(PrintWriter printWriter) {
        printWriter.printf(END_WRITE, this.whiteSpace);
    }

    public void generateStatementCounter(int programUnit, PrintWriter printWriter) {
        int begin = nodeCounter.getStatementCounterBegin(programUnit);
        int end = nodeCounter.getStatementCounterEnd(programUnit);
        for (int i = begin + 1; i <= end; i++) {
            Integer outInteger = new Integer(i);
            // format: "S-" = 2; outInterger length; " " = 1;
            Integer outIntegerLentgh = new Integer(2 + outInteger.toString()
                    .length() + 1);
            printWriter.printf(STATEMENT_COUNTER, this.whiteSpaceDeclaration, outInteger, outIntegerLentgh);
        }
    }

    public void manipulate(PrintWriter printWriter) {
        Integer outInteger = new Integer(this.counterProvider.getStatementCounter());
        printWriter.printf(STATEMENT_COUNTER_WITHOUT_POINT, this.whiteSpace, outInteger);
    }

}
