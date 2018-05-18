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
import org.codecover.instrumentation.cobol85.syntaxtree.NodeSequence;
import org.codecover.instrumentation.cobol85.visitor.Visitor;

/**
 * This is a default implementation of the branch manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: DefaultBranchManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public class DefaultBranchManipulator implements BranchManipulator {

    private static final String NEW_LINE_IN_COVERAGE_FILE = "10 COVERAGE-TXT-NEW-LINE PIC X VALUE X\"0A\".%n";

    // String includes 1 "%d" place holder
    private static final String COUNT_BRANCH = "COUNT-BRANCH-%2$d";

    // String includes 1 "%d" place holder
    private static final String BRANCH_COUNTER_WITHOUT_POINT = "%n%1$sADD 1 TO "
            + COUNT_BRANCH + "%n";

    // String includes 4 "%d" place holder
    private static final String BRANCH_COUNTER = "%n%1$s10 " + COUNT_BRANCH
            + "-TXT PIC X(%3$d) VALUE \"B%2$d\".%n%1$s10 " + COUNT_BRANCH
            + " PIC 9(18) VALUE ZERO.%n%1$s" + NEW_LINE_IN_COVERAGE_FILE;

    private static final String ELSE = "%n%1$sELSE";

    private static final String WHEN_OTHER = "%n%1$sWHEN OTHER";

    private static final String END_IF = "%n%1$sEND-IF";

    private static final String END_EVALUATE = "%n%1$sEND-EVALUATE";

    private static final String NEXT_SENTENCE = "%n%1$sCCNEXTSENTENCEMARK%2$d.%n";

    private static final String GOTO_NEXT_SENTENCE = "%n%1$sGO TO CCNEXTSENTENCEMARK%2$d%n";

    private static NodeCounter nodeCounter = NodeCounter.getInstance();

    private CounterProvider counterProvider;

    private boolean nextSentence = false;

    private int nextSentenceCounter = 0;

    private String whiteSpace;

    private String whiteSpaceDeclaration;

    /**
     * Constructor
     *
     * @param counterProvider the counter provider
     * @param compilerDirectivesManipulator 
     */
    public DefaultBranchManipulator(CounterProvider counterProvider, CompilerDirectivesManipulator compilerDirectivesManipulator) {
        this.counterProvider = counterProvider;
        this.whiteSpace = compilerDirectivesManipulator.insertWhiteSpace();
        this.whiteSpaceDeclaration = compilerDirectivesManipulator.insertWhiteSpaceDeclaration();
    }

    public void generateNextSentenceMark(PrintWriter printWriter) {
        if (this.nextSentence) {
            Integer outInteger = new Integer(this.nextSentenceCounter);
            printWriter.printf(NEXT_SENTENCE, this.whiteSpace, outInteger);
        }
    }

    public void generateNextSentenceGoTo(Visitor visitor, NodeSequence nodeSequence, PrintWriter printWriter) {
        if (!this.nextSentence) {
            this.nextSentence = true;
            this.nextSentenceCounter++;
        }
        Integer outInteger = new Integer(this.nextSentenceCounter);
        printWriter.printf(GOTO_NEXT_SENTENCE, this.whiteSpace, outInteger);
    }

    public void resetNextSentenceMark() {
        this.nextSentence = false;
    }

    public void generateBranchCounter(int programUnit, PrintWriter printWriter) {
        int begin = nodeCounter.getBranchCounterBegin(programUnit);
        int end = nodeCounter.getBranchCounterEnd(programUnit);
        for (int i = begin + 1; i <= end; i++) {
            Integer outInteger = new Integer(i);
            // format: "B-" = 2; outInterger length; " " = 1;
            Integer outIntegerLentgh = new Integer(2 + outInteger.toString()
                    .length() + 1);
            printWriter.printf(BRANCH_COUNTER, this.whiteSpaceDeclaration, outInteger, outIntegerLentgh);
        }
    }

    public void generateElseBranch(PrintWriter printWriter) {
        printWriter.printf(ELSE, this.whiteSpace);
        this.manipulate(printWriter);
    }

    public void generateEndEvaluate(PrintWriter printWriter) {
        printWriter.printf(END_EVALUATE, this.whiteSpace);
    }

    public void generateEndIf(PrintWriter printWriter) {
        printWriter.printf(END_IF, this.whiteSpace);
    }

    public void generateWhenOtherBranch(PrintWriter printWriter) {
        printWriter.printf(WHEN_OTHER, this.whiteSpace);
        this.manipulate(printWriter);
    }

    public void manipulate(PrintWriter printWriter) {
        Integer outInteger = new Integer(this.counterProvider.getBranchCounter());
        printWriter.printf(BRANCH_COUNTER_WITHOUT_POINT, this.whiteSpace, outInteger);
    }

}
