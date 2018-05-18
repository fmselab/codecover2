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

package org.codecover.instrumentation.xampil.manipulator;

import static org.codecover.instrumentation.xampil.visitor.InstrumentationVisitor.writeFileStatement;

import java.io.PrintWriter;
import java.util.List;

import org.codecover.instrumentation.booleanterms.InstrBasicBooleanTerm;
import org.codecover.instrumentation.xampil.CounterIDProvider;
import org.codecover.instrumentation.xampil.parser.InstrumentableItemCounter;

/**
 * This is a {@link ConditionManipulator}.
 * 
 * @see ConditionManipulator
 * 
 * @author Stefan Franke
 */
public class DefaultConditionManipulator extends AbstractDefaultManipulator
        implements ConditionManipulator {

    private static final String END_IF = "ENDIF %n    ";

    private static final String ELSE = "ELSE %n    ";

    private static final String IF_NOT = "IF NOT(%s) THEN %n    ";

    private static final String ONE = "1";

    private static final String ZERO = "0";

    private static final String CONDITION_COUNTER = "%1$s%2$s_%3$s := %1$s%2$s_%3$s + 1%n    ";

    private int truthTableLine;

    private List<InstrBasicBooleanTerm> basicBooleanTerms;

    public void writeDeclarations(InstrumentableItemCounter counter) {
        PrintWriter writer = super.getWriter();
        OUTER_LOOP : for (int i = 0; i < counter.getConditionCount(); i++) {
            int booleanTerms = counter.getBasicBooleanCount(i);
            if (booleanTerms == 0) {
                continue OUTER_LOOP;
            }
            int basicBooleanCounters = 1 << booleanTerms;
            String conditionPrimaryID = CounterIDProvider.generateConditionPrimaryID(i);
            for (int j = 0; j < basicBooleanCounters; j++) {
                writer.printf("    INTEGER %s%s_%s := 0%n",
                        CounterIDProvider.VARIABLE_PREFIX,
                        conditionPrimaryID,
                        getTruthTableLine(booleanTerms, j));
            }
        }
        writer.printf("%n");
    }
    
    public void writeCoverageLogFileOutput(InstrumentableItemCounter counter) {
        PrintWriter writer = super.getWriter();
        OUTER_LOOP : for (int i = 0; i < counter.getConditionCount(); i++) {
            int booleanTerms = counter.getBasicBooleanCount(i);
            if (booleanTerms == 0) {
                continue OUTER_LOOP;
            }
            int basicBooleanCounters = 1 << booleanTerms;
            String conditionPrimaryID = CounterIDProvider.generateConditionPrimaryID(i);
            for (int j = 0; j < basicBooleanCounters; j++) {
                writeFileStatement(writer, false,
                    String.format("\"%2$s-%3$s \" + %1$s%2$s_%3$s",
                                  CounterIDProvider.VARIABLE_PREFIX,
                                  conditionPrimaryID,
                                  getTruthTableLine(booleanTerms, j)));
            }
        }
    }

    public void manipulate(String conditionID, List<InstrBasicBooleanTerm> termList) {
        if (termList.isEmpty()) {
            return;
        }
        this.truthTableLine = 0;
        this.basicBooleanTerms = termList;
        this.generateNestedIfBlock(0, conditionID);
    }

    private void generateNestedIfBlock(int basicBooleanTerm, String conditionID) {
        String basicBooleanTermString = this.basicBooleanTerms.get(basicBooleanTerm).termToString();
        super.getWriter().printf(IF_NOT, basicBooleanTermString);
        if (basicBooleanTerm < this.basicBooleanTerms.size() - 1) {
            this.generateNestedIfBlock(basicBooleanTerm + 1, conditionID);
        } else {
            String truthTableLineString = this.getTruthTableLine(
                    this.basicBooleanTerms.size(), this.truthTableLine);
            super.getWriter().printf(CONDITION_COUNTER, 
                    CounterIDProvider.VARIABLE_PREFIX, 
                    conditionID, 
                    truthTableLineString);
            this.truthTableLine++;
        }
        super.getWriter().printf(ELSE);
        if (basicBooleanTerm < this.basicBooleanTerms.size() - 1) {
            this.generateNestedIfBlock(basicBooleanTerm + 1, conditionID);
        } else {
            String truthTableLineString = this.getTruthTableLine(
                    this.basicBooleanTerms.size(), this.truthTableLine);
            super.getWriter().printf(CONDITION_COUNTER, 
                    CounterIDProvider.VARIABLE_PREFIX, 
                    conditionID, 
                    truthTableLineString);
            this.truthTableLine++;
        }
        super.getWriter().printf(END_IF);
    }

    /**
     * Returns a string containing the binary code for a given line in the
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
