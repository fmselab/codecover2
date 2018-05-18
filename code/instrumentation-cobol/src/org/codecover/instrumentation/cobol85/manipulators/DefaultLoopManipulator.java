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
 * This is a default implementation of the loop manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: DefaultLoopManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public class DefaultLoopManipulator implements LoopManipulator {

    private static final String NEW_LINE_IN_COVERAGE_FILE = "10 COVERAGE-TXT-NEW-LINE PIC X VALUE X\"0A\".%n";

    // String includes 2 "%d" place holder
    private static final String COUNT_LOOP = "COUNT-LOOP-%2$d-%3$d";

    // String includes 1 "%d" place holder
    private static final String COUNT_LOOP_AUX = "COUNT-LOOP-A-%2$d";

    // String includes 1 "%d" place holder
    private static final String LOOP_COUNTER_SET_ZERO = "%n%1$sMOVE 0 TO "
            + COUNT_LOOP_AUX + "%n";

    // String includes 1 "%d" place holder
    private static final String LOOP_COUNTER_INCREMENT = "%n%1$sADD 1 TO "
            + COUNT_LOOP_AUX + "%n";

    // String includes 7 "%d" place holder
    private static final String LOOP_COUNTER_EVALUATE = "%n%1$sEVALUATE "
            + COUNT_LOOP_AUX
            + "%n%1$sWHEN 0 ADD 1 TO COUNT-LOOP-%2$d-0"
            + "%n%1$sWHEN 1 ADD 1 TO COUNT-LOOP-%2$d-1"
            + "%n%1$sWHEN OTHER ADD 1 TO COUNT-LOOP-%2$d-2"
            + "%n%1$sEND-EVALUATE";

    // String includes 7 "%d" place holder
    private static final String LOOP_COUNTER = "%n%1$s10 " + COUNT_LOOP
            + "-TXT PIC X(%4$d) VALUE \"L%2$d-%3$d\".%n%1$s10 "
            + COUNT_LOOP + " PIC 9(18) VALUE ZERO.%n%1$s"
            + NEW_LINE_IN_COVERAGE_FILE;

    // String includes 1 "%d" place holder
    private static final String LOOP_COUNTER_AUX = "%n%1$s10 " + COUNT_LOOP_AUX
            + " PIC 9(18) VALUE ZERO.%n";

    private static final String LOOPING_COVERAGE_COUNTER = "%n%1$s05 LOOPING-COVERAGE-COUNTER.";

    private static NodeCounter nodeCounter = NodeCounter.getInstance();

    // at this stage the counter provider is not used 
    @SuppressWarnings("unused")
    private CounterProvider counterProvider;

    private String whiteSpace;

    private String whiteSpaceDeclaration;

    /**
     * Constructor
     *
     * @param counterProvider the counter provider
     * @param compilerDirectivesManipulator 
     */
    public DefaultLoopManipulator(CounterProvider counterProvider, CompilerDirectivesManipulator compilerDirectivesManipulator) {
        this.counterProvider = counterProvider;
        this.whiteSpace = compilerDirectivesManipulator.insertWhiteSpace();
        this.whiteSpaceDeclaration = compilerDirectivesManipulator.insertWhiteSpaceDeclaration();
    }

    public void evaluate(PrintWriter printWriter, int loopCounter) {
        Integer loCounter = new Integer(loopCounter);
        printWriter.printf(LOOP_COUNTER_EVALUATE, this.whiteSpace, loCounter);
    }

    public void generateLoopCounter(int programUnit, PrintWriter printWriter) {
        int begin = nodeCounter.getLoopCounterBegin(programUnit);
        int end = nodeCounter.getLoopCounterEnd(programUnit);
        for (int i = begin + 1; i <= end; i++) {
            Integer outInteger = new Integer(i);
            // format: "L-" = 2; outInterger length; "-" = 1; "0" | "1" | "2" = 1; " " = 1;
            Integer outIntegerLentgh = new Integer(2 + outInteger.toString()
                    .length() + 1 + 1 + 1);
            for (int j = 0; j <= 2; j++) {
                Integer lcounter = new Integer(j);
                printWriter.printf(LOOP_COUNTER, this.whiteSpaceDeclaration, outInteger, lcounter, outIntegerLentgh);
            }
        }
    }

    public void generateAuxiliaryLoopCounterHeader(PrintWriter printWriter) {
        printWriter.printf(LOOPING_COVERAGE_COUNTER, this.whiteSpaceDeclaration);
    }

    public void generateAuxiliaryLoopCounter(int programUnit, PrintWriter printWriter) {
        int begin = nodeCounter.getLoopCounterBegin(programUnit);
        int end = nodeCounter.getLoopCounterEnd(programUnit);
        for (int i = begin + 1; i <= end; i++) {
            Integer outInteger = new Integer(i);
            printWriter.printf(LOOP_COUNTER_AUX, this.whiteSpaceDeclaration, outInteger);
        }
    }

    public void increment(PrintWriter printWriter, int loopCounter) {
        printWriter.printf(LOOP_COUNTER_INCREMENT, this.whiteSpace, new Integer(loopCounter));
    }

    public void manipulate(PrintWriter printWriter, int loopCounter) {
        printWriter.printf(LOOP_COUNTER_SET_ZERO, this.whiteSpace, new Integer(loopCounter));
    }

}
