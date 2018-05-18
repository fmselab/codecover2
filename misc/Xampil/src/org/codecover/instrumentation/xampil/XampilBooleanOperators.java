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

package org.codecover.instrumentation.xampil;

import static org.codecover.model.mast.BooleanResult.FALSE;
import static org.codecover.model.mast.BooleanResult.TRUE;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.codecover.instrumentation.booleanterms.InstrBooleanOperator;
import org.codecover.model.mast.BooleanAssignment;

public class XampilBooleanOperators {

    /**
     * {@value #OR}  
     */
    public static final String OR = "OR";
    
    /**
     * {@value #NOT}  
     */
    public static final String NOT = "NOT";

    /**
     * {@value #AND}  
     */
    public static final String AND = "AND";

    /**
     * {@value #TRUE_DESCRIPTION}
     */
    public static final String TRUE_DESCRIPTION = "TRUE";

    /**
     * {@value #FALSE_DESCRIPTION}
     */
    public static final String FALSE_DESCRIPTION = "FALSE";

    /**
     * An OR operator <b>OR</b> with arity 2.
     */
    private static InstrBooleanOperator orOperator = null;

    /**
     * An AND operator <b>AND</b> with arity 2.
     */
    private static InstrBooleanOperator andOperator = null;

    /**
     * A NOT operator <b>NOT</b> with arity 1.
     */
    private static InstrBooleanOperator notOperator = null;
    
    /**
     * A TRUE operator <b>TRUE</b> with arity 0.
     */
    private static InstrBooleanOperator trueOperator = null;

    /**
     * A FALSE operator <b>FALSE</b> with arity 0.
     */
    private static InstrBooleanOperator falseOperator = null;

    /**
     * @return An OR operator with arity 2.
     */
    public static InstrBooleanOperator getOrOperator() {
        if (orOperator == null) {
            Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
            BooleanAssignment assignment;

            assignment = new BooleanAssignment(FALSE, FALSE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(FALSE, TRUE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            assignment = new BooleanAssignment(TRUE, FALSE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            assignment = new BooleanAssignment(TRUE, TRUE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            orOperator = InstrBooleanOperator.getTwoArgumentOperator(
                    XampilBooleanOperators.OR, "OR", possibleAssignments);
        }

        return orOperator;
    }

    /**
     * @return An AND operator with arity 2.
     */
    public static InstrBooleanOperator getAndOperator() {
        if (andOperator == null) {
            Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
            BooleanAssignment assignment;

            assignment = new BooleanAssignment(FALSE, FALSE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(FALSE, TRUE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(TRUE, FALSE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(TRUE, TRUE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            andOperator = InstrBooleanOperator.getTwoArgumentOperator(
                    XampilBooleanOperators.AND, "AND", possibleAssignments);
        }

        return andOperator;
    }

    /**
     * @return A NOT operator with arity 1.
     */
    public static InstrBooleanOperator getNotOperator() {
        if (notOperator == null) {
            Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
            BooleanAssignment assignment;

            assignment = new BooleanAssignment(FALSE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            assignment = new BooleanAssignment(TRUE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            notOperator = InstrBooleanOperator.getOneArgumentOperator(
                    XampilBooleanOperators.NOT, "NOT", false, possibleAssignments);
        }

        return notOperator;
    }

    /**
     * @return A TRUE operator with arity 0.
     */
    public static InstrBooleanOperator getTrueOperator() {
        if (trueOperator == null) {
            BooleanAssignment assignment = new BooleanAssignment();

            trueOperator = InstrBooleanOperator.getConstantOperator(
                    XampilBooleanOperators.TRUE_DESCRIPTION, "TRUE",
                    Collections.singletonMap(assignment, Boolean.TRUE));
        }

        return trueOperator;
    }

    /**
     * @return A FALSE operator with arity 0.
     */
    public static InstrBooleanOperator getFalseOperator() {
        if (falseOperator == null) {
            BooleanAssignment assignment = new BooleanAssignment();

            falseOperator = InstrBooleanOperator.getConstantOperator(
                    XampilBooleanOperators.FALSE_DESCRIPTION, "FALSE",
                    Collections.singletonMap(assignment, Boolean.FALSE));
        }

        return falseOperator;
    }
}
