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
import java.util.List;

import org.codecover.instrumentation.booleanterms.InstrBasicBooleanTerm;
import org.codecover.instrumentation.cobol85.CounterProvider;
import org.codecover.instrumentation.cobol85.NodeCounter;
import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectivesManipulator;

/**
 * This is a default implementation of the condition manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: DefaultConditionManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public class DefaultConditionManipulator implements ConditionManipulator {

    private static final String END_IF = "%n%1$sEND-IF %n";

    private static final String ELSE = "%n%1$sELSE %n";

    private static final String IF_NOT = "%n%1$sIF NOT(%2$s) THEN %n";

    private static final String ONE = "1";

    private static final String ZERO = "0";

    private static final String NEW_LINE_IN_COVERAGE_FILE = "10 COVERAGE-TXT-NEW-LINE PIC X VALUE X\"0A\".%n";

    // String includes 2 "%d" place holder
    private static final String COUNT_CONDITON = "COUNT-CONDITON-%2$d-%3$s";

    // String includes 3 "%d" place holder
    private static final String CONDITION_COUNTER_WITHOUT_POINT = "%n%1$sADD 1 TO "
            + COUNT_CONDITON;

    // String includes 7 "%d" place holder
    private static final String CONDITION_COUNTER = "%n%1$s10 " + COUNT_CONDITON
            + "-TXT PIC X(%4$d) VALUE \"C%2$d-%3$s\".%n%1$s10 "
            + COUNT_CONDITON + " PIC 9(18) VALUE ZERO.%n"
            + NEW_LINE_IN_COVERAGE_FILE;

    private static NodeCounter nodeCounter = NodeCounter.getInstance();

    private int truthTableLine;

    private List<InstrBasicBooleanTerm> basicBooleanTerms;

    private CounterProvider counterProvider;

    private String whiteSpace;

    private String whiteSpaceDeclaration;

    /**
     * Constructor
     *
     * @param counterProvider the counter provider
     * @param compilerDirectivesManipulator 
     */
    public DefaultConditionManipulator(CounterProvider counterProvider, CompilerDirectivesManipulator compilerDirectivesManipulator) {
        this.counterProvider = counterProvider;
        this.whiteSpace = compilerDirectivesManipulator.insertWhiteSpace();
        this.whiteSpaceDeclaration = compilerDirectivesManipulator.insertWhiteSpaceDeclaration();
    }

    public void generateConditionCounter(int programUnit, PrintWriter printWriter) {
        int begin = nodeCounter.getConditionCounterBegin(programUnit);
        int end = nodeCounter.getConditionCounterEnd(programUnit);
        for (int i = begin + 1; i <= end; i++) {
            Integer outInteger = new Integer(i);
            Double bBooleanTerms = new Double(nodeCounter
                    .getBasicBooleanCounter(i));
            double basicBooleanCounters = Math.pow(2, bBooleanTerms
                    .doubleValue());
            // format: "C-" = 2; outInterger length; "-" = 1; basicBooleanCounter length; " " = 1;
            Integer outIntegerLentgh = new Integer(2 + outInteger.toString()
                    .length()
                    + 1 + 2 * bBooleanTerms.intValue() + 1);
            for (int j = 0; j < basicBooleanCounters; j++) {
                String truthTableLineString = this.getTruthTableLine(
                        bBooleanTerms.intValue(), j);
                printWriter.printf(CONDITION_COUNTER, this.whiteSpaceDeclaration, outInteger,
                        truthTableLineString, outIntegerLentgh);
            }
        }
    }

    public void manipulate(PrintWriter printWriter, List<InstrBasicBooleanTerm> bBooleanTerms) {
        this.truthTableLine = 0;
        this.basicBooleanTerms = bBooleanTerms;
        this.generateNestedIfBlock(printWriter, 0, this.counterProvider.getConditionCounter());
    }

    public void manipulate(PrintWriter printWriter, List<InstrBasicBooleanTerm> bBooleanTerms, int conditionCounter) {
        this.truthTableLine = 0;
        this.basicBooleanTerms = bBooleanTerms;
        this.generateNestedIfBlock(printWriter, 0, conditionCounter);
    }

    private void generateNestedIfBlock(PrintWriter printWriter,
            int basicBooleanTerm, int conditionNumber) {
        String basicBooleanTermString = this.basicBooleanTerms.get(basicBooleanTerm).termToString();
        printWriter.printf(IF_NOT, this.whiteSpace, basicBooleanTermString);
        if (basicBooleanTerm < this.basicBooleanTerms.size() - 1) {
            this.generateNestedIfBlock(printWriter, basicBooleanTerm + 1, conditionNumber);
        } else {
            Integer condition = new Integer(conditionNumber);
            String truthTableLineString = this.getTruthTableLine(
                    this.basicBooleanTerms.size(), this.truthTableLine);
            printWriter.printf(CONDITION_COUNTER_WITHOUT_POINT, this.whiteSpace, condition, truthTableLineString);
            this.truthTableLine++;
        }
        printWriter.printf(ELSE, this.whiteSpace);
        if (basicBooleanTerm < this.basicBooleanTerms.size() - 1) {
            this.generateNestedIfBlock(printWriter, basicBooleanTerm + 1, conditionNumber);
        } else {
            Integer condition = new Integer(conditionNumber);
            String truthTableLineString = this.getTruthTableLine(
                    this.basicBooleanTerms.size(), this.truthTableLine);
            printWriter.printf(CONDITION_COUNTER_WITHOUT_POINT, this.whiteSpace, condition, truthTableLineString);
            this.truthTableLine++;
        }
        printWriter.printf(END_IF, this.whiteSpace);
    }

    /**
     * Returns an string containing the binary code for a given line in the
     * truth table with n variables.
     * 
     * @param variables
     *            the number of variables
     * @param line
     *            the line in the truth table
     * @return binary code
     */
    private String getTruthTableLine(int variables, int line) {
        String tTableLine = Integer.toBinaryString(line);
        while (tTableLine.length() < variables) {
            tTableLine = ZERO + tTableLine;
        }
        String truthTableLineAdapted = new String("");
        for (int position = 0; position < variables; position++) {
            truthTableLineAdapted = truthTableLineAdapted + ONE
                    + tTableLine.charAt(position);
        }
        return truthTableLineAdapted;
    }

}
