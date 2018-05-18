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

package org.codecover.instrumentation.java15;

import static org.codecover.model.mast.BooleanResult.FALSE;
import static org.codecover.model.mast.BooleanResult.NOT_EVALUATED;
import static org.codecover.model.mast.BooleanResult.TRUE;

import java.util.HashMap;
import java.util.Map;

import org.codecover.instrumentation.booleanterms.InstrBasicBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrBooleanOperator;
import org.codecover.instrumentation.booleanterms.InstrBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrOperatorTerm;
import org.codecover.model.mast.BooleanAssignment;
import org.codecover.model.mast.BooleanResult;
import org.codecover.model.mast.Location;

/**
 * This class contains constants and constant methods to provide boolean
 * operators for Java 1.5.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: JavaBooleanOperators.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class JavaBooleanOperators {

    /**
     * {@value #ASSIGNMENT_EXCLUSIVE_OR}  
     */
    public static final String ASSIGNMENT_EXCLUSIVE_OR = "assignment xor (^=)";

    /**
     * {@value #ASSIGNMENT_AND}  
     */
    public static final String ASSIGNMENT_AND = "assignment and (&=)";

    /**
     * {@value #ASSIGNMENT_OR}  
     */
    public static final String ASSIGNMENT_OR = "assignment or (|=)";

    /**
     * {@value #ASSIGNMENT}  
     */
    public static final String ASSIGNMENT = "assignment (=)";

    /**
     * {@value #ASSIGNMENT_EXCLUSIVE_OR_REPLACEMENT}  
     */
    public static final String ASSIGNMENT_EXCLUSIVE_OR_REPLACEMENT =
        ASSIGNMENT_EXCLUSIVE_OR + " replacement";

    /**
     * {@value #ASSIGNMENT_AND_REPLACEMENT}  
     */
    public static final String ASSIGNMENT_AND_REPLACEMENT =
        ASSIGNMENT_AND + " replacement";

    /**
     * {@value #ASSIGNMENT_OR_REPLACEMENT}  
     */
    public static final String ASSIGNMENT_OR_REPLACEMENT =
        ASSIGNMENT_OR + " replacement";

    /**
     * {@value #CONDITIONAL_OPERATOR}  
     */
    public static final String CONDITIONAL_OPERATOR = "conditional operator ( ? : )";

    /**
     * {@value #NOT}  
     */
    public static final String NOT = "not (!)";

    /**
     * {@value #AND}  
     */
    public static final String AND = "and (&)";

    /**
     * {@value #EXCLUSIVE_OR}  
     */
    public static final String EXCLUSIVE_OR = "exclusive or (^)";

    /**
     * {@value #OR}  
     */
    public static final String OR = "or (|)";

    /**
     * {@value #CONDITIONAL_AND}  
     */
    public static final String CONDITIONAL_AND = "conditional and (&&)";

    /**
     * {@value #CONDITIONAL_OR}  
     */
    public static final String CONDITIONAL_OR = "conditional or (||)";

    /**
     * An or operator <b>|</b> with arity 2.
     */
    private static InstrBooleanOperator orOperator = null;

    /**
     * A conditional or operator <b>||</b> with arity 2.
     */
    private static InstrBooleanOperator conditionalOrOperator = null;

    /**
     * An inclusive or operator <b>^</b> with arity 2.
     */
    private static InstrBooleanOperator exclusiveOrOperator = null;

    /**
     * An and operator <b>&</b> with arity 2.
     */
    private static InstrBooleanOperator andOperator = null;

    /**
     * A conditional and operator <b>&&</b> with arity 2.
     */
    private static InstrBooleanOperator conditionalAndOperator = null;

    /**
     * A not operator <b>!</b> with arity 1.
     */
    private static InstrBooleanOperator notOperator = null;

    /**
     * A conditional operator <b>a ? b : c</b> with arity 3.
     */
    private static InstrBooleanOperator conditionalOperator = null;
    
    /**
     * An assignment operator <b>=</b> with arity 2.
     */
    private static InstrBooleanOperator assignmentOperator = null;

    /**
     * An assignment or operator <b>|=</b> with arity 2.
     */
    private static InstrBooleanOperator assignmentOrOperator = null;

    /**
     * An assignment and operator <b>&=</b> with arity 2.
     */
    private static InstrBooleanOperator assignmentAndOperator = null;

    /**
     * An assignment exclusive or operator <b>^=</b> with arity 2.
     */
    private static InstrBooleanOperator assignmentExclusiveOrOperator = null;

    /**
     * A constant operator with semantic <b>true</b>.
     */
    private static InstrBooleanOperator trueOperator = null;

    /**
     * A constant operator with semantic <b>false</b>.
     */
    private static InstrBooleanOperator falseOperator = null;

    /**
     * A {@link InstrOperatorTerm} with semantic <b>true</b>.
     */
    private static InstrOperatorTerm trueTerm = null;

    /**
     * A {@link InstrOperatorTerm} with semantic <b>false</b>.
     */
    private static InstrOperatorTerm falseTerm = null;

    /**
     * @return An or operator <b>|</b> with arity 2.
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
                    JavaBooleanOperators.OR, "|", possibleAssignments);
        }

        return orOperator;
    }

    /**
     * @return A conditional or operator <b>||</b> with arity 2.
     */
    public static InstrBooleanOperator getConditionalOrOperator() {
        if (conditionalOrOperator == null) {
            Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
            BooleanAssignment assignment;

            assignment = new BooleanAssignment(FALSE, FALSE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(FALSE, TRUE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            assignment = new BooleanAssignment(TRUE, NOT_EVALUATED);
            possibleAssignments.put(assignment, Boolean.TRUE);

            conditionalOrOperator = InstrBooleanOperator.getTwoArgumentOperator(
                    JavaBooleanOperators.CONDITIONAL_OR, "||", possibleAssignments);
        }

        return conditionalOrOperator;
    }

    /**
     * @return An exclusive or operator <b>^</b> with arity 2.
     */
    public static InstrBooleanOperator getExclusiveOrOperator() {
        if (exclusiveOrOperator == null) {
            Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
            BooleanAssignment assignment;

            assignment = new BooleanAssignment(FALSE, FALSE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(FALSE, TRUE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            assignment = new BooleanAssignment(TRUE, FALSE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            assignment = new BooleanAssignment(TRUE, TRUE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            exclusiveOrOperator = InstrBooleanOperator.getTwoArgumentOperator(
                    JavaBooleanOperators.EXCLUSIVE_OR, "^", possibleAssignments);
        }

        return exclusiveOrOperator;
    }

    /**
     * @return An and operator <b>&</b> with arity 2.
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
                    JavaBooleanOperators.AND, "&", possibleAssignments);
        }

        return andOperator;
    }

    /**
     * @return A conditional and operator <b>&&</b> with arity 2.
     */
    public static InstrBooleanOperator getConditionalAndOperator() {
        if (conditionalAndOperator == null) {
            Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
            BooleanAssignment assignment;

            assignment = new BooleanAssignment(FALSE, NOT_EVALUATED);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(TRUE, FALSE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(TRUE, TRUE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            conditionalAndOperator = InstrBooleanOperator.getTwoArgumentOperator(
                    JavaBooleanOperators.CONDITIONAL_AND, "&&", possibleAssignments);
        }

        return conditionalAndOperator;
    }

    /**
     * @return A not operator <b>!</b> with arity 1.
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
                    JavaBooleanOperators.NOT, "!", false, possibleAssignments);
        }

        return notOperator;
    }

    /**
     * @return A conditional operator <b>a ? b : c</b> with arity 3.
     */
    public static InstrBooleanOperator getConditionalOperator() {
        if (conditionalOperator == null) {
            String[] parts = new String[]{ "", " ? ", " : ", "" };
            Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
            BooleanAssignment assignment;

            assignment = new BooleanAssignment(FALSE, NOT_EVALUATED, FALSE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(FALSE, NOT_EVALUATED, TRUE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            assignment = new BooleanAssignment(TRUE, FALSE, NOT_EVALUATED);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(TRUE, TRUE, NOT_EVALUATED);
            possibleAssignments.put(assignment, Boolean.TRUE);

            conditionalOperator = new InstrBooleanOperator(
                    JavaBooleanOperators.CONDITIONAL_OPERATOR, parts, possibleAssignments);
        }

        return conditionalOperator;
    }

    /**
     * @return An assignment operator <b>=</b> with arity 2.
     */
    public static InstrBooleanOperator getAssignmentOperator() {
        if (assignmentOperator == null) {
            Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
            BooleanAssignment assignment;

            assignment = new BooleanAssignment(NOT_EVALUATED, FALSE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(NOT_EVALUATED, TRUE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            assignmentOperator = InstrBooleanOperator.getTwoArgumentOperator(
                    JavaBooleanOperators.ASSIGNMENT, "=", possibleAssignments);
        }

        return assignmentOperator;
    }

    /**
     * @return An assignment or operator <b>|=</b> with arity 2.
     */
    public static InstrBooleanOperator getAssignmentOrOperator() {
        if (assignmentOrOperator == null) {
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

            assignmentOrOperator = InstrBooleanOperator.getTwoArgumentOperator(
                    JavaBooleanOperators.ASSIGNMENT_OR, "|=", possibleAssignments);
        }

        return assignmentOrOperator;
    }

    /**
     * @return An assignment and operator <b>&=</b> with arity 2.
     */
    public static InstrBooleanOperator getAssignmentAndOperator() {
        if (assignmentAndOperator == null) {
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

            assignmentAndOperator = InstrBooleanOperator.getTwoArgumentOperator(
                    JavaBooleanOperators.ASSIGNMENT_AND, "&=", possibleAssignments);
        }

        return assignmentAndOperator;
    }

    /**
     * @return An assignment exclusive operator <b>^=</b> with arity 2.
     */
    public static InstrBooleanOperator getAssignmentExclusiveOrOperator() {
        if (assignmentExclusiveOrOperator == null) {
            Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
            BooleanAssignment assignment;

            assignment = new BooleanAssignment(FALSE, FALSE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(FALSE, TRUE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            assignment = new BooleanAssignment(TRUE, FALSE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            assignment = new BooleanAssignment(TRUE, TRUE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignmentExclusiveOrOperator = InstrBooleanOperator.getTwoArgumentOperator(
                    JavaBooleanOperators.ASSIGNMENT_EXCLUSIVE_OR, "^=", possibleAssignments);
        }

        return assignmentExclusiveOrOperator;
    }

    /**
     * @return A constant operator with semantic <b>true</b>.
     */
    public static InstrBooleanOperator getTrueOperator() {
        if (trueOperator == null) {
            Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
            BooleanAssignment assignment = new BooleanAssignment(new BooleanResult[0]);
            possibleAssignments.put(assignment, Boolean.TRUE);

            trueOperator = InstrBooleanOperator.getConstantOperator(
                    "true", "true", possibleAssignments);
        }

        return trueOperator;
    }

    /**
     * @return A constant operator with semantic <b>false</b>.
     */
    public static InstrBooleanOperator getFalseOperator() {
        if (falseOperator == null) {
            Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
            BooleanAssignment assignment = new BooleanAssignment(new BooleanResult[0]);
            possibleAssignments.put(assignment, Boolean.FALSE);

            falseOperator = InstrBooleanOperator.getConstantOperator(
                    "false", "false", possibleAssignments);
        }

        return falseOperator;
    }

    /**
     * A {@link InstrBooleanTerm} with semantic <b>true</b>.<br>
     * This {@link InstrOperatorTerm} has no {@link Location}! Prefer:
     * <pre>
     * new InstrOperatorTerm(getTrueOperator(), startOffset, endOffset);
     * </pre>
     * 
     * @return A {@link InstrBooleanTerm} with semantic <b>true</b>.
     */
    public static InstrOperatorTerm getTrueTerm() {
        if (trueTerm == null) {
            trueTerm = new InstrOperatorTerm(getTrueOperator(), -1, -1);
        }

        return trueTerm;
    }

    /**
     * A {@link InstrBooleanTerm} with semantic <b>false</b>.<br>
     * This {@link InstrOperatorTerm} has no {@link Location}! Prefer:
     * <pre>
     * new InstrOperatorTerm(getFalseOperator(), startOffset, endOffset);
     * </pre>
     * 
     * @return A {@link InstrBooleanTerm} with semantic <b>false</b>.
     */
    public static InstrOperatorTerm getFalseTerm() {
        if (falseTerm == null) {
            falseTerm = new InstrOperatorTerm(getFalseOperator(), -1, -1);
        }

        return falseTerm;
    }

    /**
     * Used for replacement of a {@link #ASSIGNMENT_OR} operator.<br>
     * <br>
     * Term <code>A |= B</code> is replaced by <code>(A = (A | B))</code>,
     * where <code>A</code> and <code>B</code> are the only operands. This
     * operator is just a delegation to the new inner term <code>(A | B)</code>
     * 
     * @param leftTerm
     *            The image of <code>A</code>.
     * 
     * @return The new term with name {@value #ASSIGNMENT_OR_REPLACEMENT}.
     */
    public static InstrBooleanOperator getAssignmentOrReplacement(InstrBasicBooleanTerm leftTerm) {
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
        BooleanAssignment assignment;

        assignment = new BooleanAssignment(FALSE);
        possibleAssignments.put(assignment, Boolean.FALSE);

        assignment = new BooleanAssignment(TRUE);
        possibleAssignments.put(assignment, Boolean.TRUE);

        // replaces the term A = B by (A = (B)), where B is the only operand 
        String[] opParts = new String[] {
                "(" + leftTerm.termToString() + " = (",
                "))"
        };

        return new InstrBooleanOperator(ASSIGNMENT_OR_REPLACEMENT,
                opParts,
                possibleAssignments);
    }

    /**
     * Used for replacement of a {@link #ASSIGNMENT_AND} operator.<br>
     * <br>
     * Term <code>A &= B</code> is replaced by <code>(A = (A & B))</code>,
     * where <code>A</code> and <code>B</code> are the only operands. This
     * operator is just a delegation to the new inner term <code>(A & B)</code>
     * 
     * @param leftTerm
     *            The image of <code>A</code>.
     * 
     * @return The new term with name {@value #ASSIGNMENT_AND_REPLACEMENT}.
     */
    public static InstrBooleanOperator getAssignmentAndReplacement(InstrBasicBooleanTerm leftTerm) {
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
        BooleanAssignment assignment;

        assignment = new BooleanAssignment(FALSE);
        possibleAssignments.put(assignment, Boolean.FALSE);

        assignment = new BooleanAssignment(TRUE);
        possibleAssignments.put(assignment, Boolean.TRUE);

        // replaces the term A = B by (A = (B)), where B is the only operand 
        String[] opParts = new String[] {
                "(" + leftTerm.termToString() + " = (",
                "))"
        };

        return new InstrBooleanOperator(ASSIGNMENT_AND_REPLACEMENT,
                opParts,
                possibleAssignments);
    }

    /**
     * Used for replacement of a {@link #ASSIGNMENT_EXCLUSIVE_OR} operator.<br>
     * <br>
     * Term <code>A ^= B</code> is replaced by <code>(A = (A ^ B))</code>,
     * where <code>A</code> and <code>B</code> are the only operands. This
     * operator is just a delegation to the new inner term <code>(A ^ B)</code>
     * 
     * @param leftTerm
     *            The image of <code>A</code>.
     * 
     * @return The new term with name {@value #ASSIGNMENT_EXCLUSIVE_OR_REPLACEMENT}.
     */
    public static InstrBooleanOperator getAssignmentExclusiveOrReplacement(InstrBasicBooleanTerm leftTerm) {
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
        BooleanAssignment assignment;

        assignment = new BooleanAssignment(FALSE);
        possibleAssignments.put(assignment, Boolean.FALSE);

        assignment = new BooleanAssignment(TRUE);
        possibleAssignments.put(assignment, Boolean.TRUE);

        // replaces the term A = B by (A = (B)), where B is the only operand 
        String[] opParts = new String[] {
                "(" + leftTerm.termToString() + " = (",
                "))"
        };

        return new InstrBooleanOperator(ASSIGNMENT_EXCLUSIVE_OR_REPLACEMENT,
                opParts,
                possibleAssignments);
    }
}
