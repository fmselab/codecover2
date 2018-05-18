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

import static org.codecover.instrumentation.java15.JavaBooleanOperators.AND;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.ASSIGNMENT;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.ASSIGNMENT_AND;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.ASSIGNMENT_EXCLUSIVE_OR;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.ASSIGNMENT_OR;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.CONDITIONAL_AND;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.CONDITIONAL_OPERATOR;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.CONDITIONAL_OR;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.EXCLUSIVE_OR;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.NOT;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.OR;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getAndOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getAssignmentAndOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getAssignmentExclusiveOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getAssignmentOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getAssignmentOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getConditionalAndOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getConditionalOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getConditionalOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getExclusiveOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getFalseOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getFalseTerm;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getNotOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getTrueOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getTrueTerm;
import static org.codecover.model.mast.BooleanResult.FALSE;
import static org.codecover.model.mast.BooleanResult.NOT_EVALUATED;
import static org.codecover.model.mast.BooleanResult.TRUE;

import java.util.List;
import java.util.Vector;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJava;
import org.codecover.instrumentation.booleanterms.InstrBooleanOperator;
import org.codecover.instrumentation.booleanterms.InstrOperatorTerm;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.BasicBooleanTerm;
import org.codecover.model.mast.BooleanAssignment;
import org.codecover.model.mast.BooleanOperator;
import org.codecover.model.mast.BooleanResult;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.RootTerm;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: JavaBooleanOperatorsTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class JavaBooleanOperatorsTest extends TestCase {

    MASTBuilder builder;

    @Override
    protected void setUp() throws Exception {
        this.builder = UtilsForTestingJava.newMASTBuilder(); 
    }

    @Override
    protected void tearDown() {
        this.builder = null;
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.JavaBooleanOperators#getOrOperator()}.
     */
    public void testGetOrOperator() {
        InstrBooleanOperator iOp = getOrOperator();
        Assert.assertNotNull(iOp);
        Assert.assertEquals(2, iOp.getArity());
        Assert.assertEquals(OR, iOp.getName());

        BooleanOperator op = iOp.toBooleanOperator(this.builder);
        Assert.assertEquals(2, op.getArity());
        Assert.assertEquals(OR, op.getName());

        BooleanTerm term = createTerm(op, op.getArity());
        Assert.assertEquals(Boolean.FALSE, assign(term, FALSE, FALSE));
        Assert.assertEquals(Boolean.TRUE, assign(term, TRUE, FALSE));
        Assert.assertEquals(Boolean.TRUE, assign(term, FALSE, TRUE));
        Assert.assertEquals(Boolean.TRUE, assign(term, TRUE, TRUE));

        Assert.assertNull(assign(term, TRUE, NOT_EVALUATED));
        Assert.assertNull(assign(term, FALSE, NOT_EVALUATED));
        Assert.assertNull(assign(term, NOT_EVALUATED, TRUE));
        Assert.assertNull(assign(term, NOT_EVALUATED, FALSE));
        Assert.assertNull(assign(term, NOT_EVALUATED, NOT_EVALUATED));

        Assert.assertSame(getOrOperator(), getOrOperator());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.JavaBooleanOperators#getConditionalOrOperator()}.
     */
    public void testGetConditionalOrOperator() {
        InstrBooleanOperator iOp = getConditionalOrOperator();
        Assert.assertNotNull(iOp);
        Assert.assertEquals(2, iOp.getArity());
        Assert.assertEquals(CONDITIONAL_OR, iOp.getName());

        BooleanOperator op = iOp.toBooleanOperator(this.builder);
        Assert.assertEquals(2, op.getArity());
        Assert.assertEquals(CONDITIONAL_OR, op.getName());

        BooleanTerm term = createTerm(op, op.getArity());
        Assert.assertEquals(Boolean.FALSE, assign(term, FALSE, FALSE));
        Assert.assertEquals(Boolean.TRUE, assign(term, TRUE, NOT_EVALUATED));
        Assert.assertEquals(Boolean.TRUE, assign(term, FALSE, TRUE));

        Assert.assertNull(assign(term, TRUE, TRUE));
        Assert.assertNull(assign(term, TRUE, FALSE));
        Assert.assertNull(assign(term, NOT_EVALUATED, NOT_EVALUATED));

        Assert.assertSame(getConditionalOrOperator(), getConditionalOrOperator());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.JavaBooleanOperators#getExclusiveOrOperator()}.
     */
    public void testGetExclusiveOrOperator() {
        InstrBooleanOperator iOp = getExclusiveOrOperator();
        Assert.assertNotNull(iOp);
        Assert.assertEquals(2, iOp.getArity());
        Assert.assertEquals(EXCLUSIVE_OR, iOp.getName());

        BooleanOperator op = iOp.toBooleanOperator(this.builder);
        Assert.assertEquals(2, op.getArity());
        Assert.assertEquals(EXCLUSIVE_OR, op.getName());

        BooleanTerm term = createTerm(op, op.getArity());
        Assert.assertEquals(Boolean.FALSE, assign(term, FALSE, FALSE));
        Assert.assertEquals(Boolean.TRUE, assign(term, TRUE, FALSE));
        Assert.assertEquals(Boolean.TRUE, assign(term, FALSE, TRUE));
        Assert.assertEquals(Boolean.FALSE, assign(term, TRUE, TRUE));

        Assert.assertNull(assign(term, TRUE, NOT_EVALUATED));
        Assert.assertNull(assign(term, FALSE, NOT_EVALUATED));
        Assert.assertNull(assign(term, NOT_EVALUATED, TRUE));
        Assert.assertNull(assign(term, NOT_EVALUATED, FALSE));
        Assert.assertNull(assign(term, NOT_EVALUATED, NOT_EVALUATED));

        Assert.assertSame(getExclusiveOrOperator(), getExclusiveOrOperator());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.JavaBooleanOperators#getAndOperator()}.
     */
    public void testGetAndOperator() {
        InstrBooleanOperator iOp = getAndOperator();
        Assert.assertNotNull(iOp);
        Assert.assertEquals(2, iOp.getArity());
        Assert.assertEquals(AND, iOp.getName());

        BooleanOperator op = iOp.toBooleanOperator(this.builder);
        Assert.assertEquals(2, op.getArity());
        Assert.assertEquals(AND, op.getName());

        BooleanTerm term = createTerm(op, op.getArity());
        Assert.assertEquals(Boolean.FALSE, assign(term, FALSE, FALSE));
        Assert.assertEquals(Boolean.FALSE, assign(term, TRUE, FALSE));
        Assert.assertEquals(Boolean.FALSE, assign(term, FALSE, TRUE));
        Assert.assertEquals(Boolean.TRUE, assign(term, TRUE, TRUE));

        Assert.assertNull(assign(term, TRUE, NOT_EVALUATED));
        Assert.assertNull(assign(term, FALSE, NOT_EVALUATED));
        Assert.assertNull(assign(term, NOT_EVALUATED, TRUE));
        Assert.assertNull(assign(term, NOT_EVALUATED, FALSE));
        Assert.assertNull(assign(term, NOT_EVALUATED, NOT_EVALUATED));

        Assert.assertSame(getAndOperator(), getAndOperator());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.JavaBooleanOperators#getConditionalAndOperator()}.
     */
    public void testGetConditionalAndOperator() {
        InstrBooleanOperator iOp = getConditionalAndOperator();
        Assert.assertNotNull(iOp);
        Assert.assertEquals(2, iOp.getArity());
        Assert.assertEquals(CONDITIONAL_AND, iOp.getName());

        BooleanOperator op = iOp.toBooleanOperator(this.builder);
        Assert.assertEquals(2, op.getArity());
        Assert.assertEquals(CONDITIONAL_AND, op.getName());

        BooleanTerm term = createTerm(op, op.getArity());
        Assert.assertEquals(Boolean.FALSE, assign(term, FALSE, NOT_EVALUATED));
        Assert.assertEquals(Boolean.FALSE, assign(term, TRUE, FALSE));
        Assert.assertEquals(Boolean.TRUE, assign(term, TRUE, TRUE));

        Assert.assertNull(assign(term, FALSE, TRUE));
        Assert.assertNull(assign(term, FALSE, FALSE));
        Assert.assertNull(assign(term, NOT_EVALUATED, NOT_EVALUATED));

        Assert.assertSame(getConditionalAndOperator(), getConditionalAndOperator());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.JavaBooleanOperators#getNotOperator()}.
     */
    public void testGetNotOperator() {
        InstrBooleanOperator iOp = getNotOperator();
        Assert.assertNotNull(iOp);
        Assert.assertEquals(1, iOp.getArity());
        Assert.assertEquals(NOT, iOp.getName());

        BooleanOperator op = iOp.toBooleanOperator(this.builder);
        Assert.assertEquals(1, op.getArity());
        Assert.assertEquals(NOT, op.getName());

        BooleanTerm term = createTerm(op, op.getArity());
        Assert.assertEquals(Boolean.FALSE, assign(term, TRUE));
        Assert.assertEquals(Boolean.TRUE, assign(term, FALSE));

        Assert.assertNull(assign(term, NOT_EVALUATED));

        Assert.assertSame(getNotOperator(), getNotOperator());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.JavaBooleanOperators#getConditionalOperator()}.
     */
    public void testGetConditonalOperator() {
        InstrBooleanOperator iOp = getConditionalOperator();
        Assert.assertNotNull(iOp);
        Assert.assertEquals(3, iOp.getArity());
        Assert.assertEquals(CONDITIONAL_OPERATOR, iOp.getName());

        BooleanOperator op = iOp.toBooleanOperator(this.builder);
        Assert.assertEquals(3, op.getArity());
        Assert.assertEquals(CONDITIONAL_OPERATOR, op.getName());

        BooleanTerm term = createTerm(op, op.getArity());
        Assert.assertEquals(Boolean.FALSE, assign(term, FALSE, NOT_EVALUATED, FALSE));
        Assert.assertEquals(Boolean.TRUE, assign(term, FALSE, NOT_EVALUATED, TRUE));
        Assert.assertEquals(Boolean.FALSE, assign(term, TRUE, FALSE, NOT_EVALUATED));
        Assert.assertEquals(Boolean.TRUE, assign(term, TRUE, TRUE, NOT_EVALUATED));

        Assert.assertNull(assign(term, NOT_EVALUATED, NOT_EVALUATED, NOT_EVALUATED));
        Assert.assertNull(assign(term, NOT_EVALUATED, FALSE, FALSE));
        Assert.assertNull(assign(term, NOT_EVALUATED, FALSE, TRUE));
        Assert.assertNull(assign(term, NOT_EVALUATED, TRUE, FALSE));
        Assert.assertNull(assign(term, NOT_EVALUATED, TRUE, TRUE));
        Assert.assertNull(assign(term, FALSE, NOT_EVALUATED, NOT_EVALUATED));
        Assert.assertNull(assign(term, FALSE, FALSE, NOT_EVALUATED));
        Assert.assertNull(assign(term, FALSE, TRUE, NOT_EVALUATED));
        Assert.assertNull(assign(term, TRUE, NOT_EVALUATED, NOT_EVALUATED));
        Assert.assertNull(assign(term, TRUE, NOT_EVALUATED, FALSE));
        Assert.assertNull(assign(term, TRUE, NOT_EVALUATED, TRUE));

        Assert.assertSame(getConditionalOperator(), getConditionalOperator());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.JavaBooleanOperators#getAssignmentOperator()}.
     */
    public void testGetAssignmentOperator() {
        InstrBooleanOperator iOp = getAssignmentOperator();
        Assert.assertNotNull(iOp);
        Assert.assertEquals(2, iOp.getArity());
        Assert.assertEquals(ASSIGNMENT, iOp.getName());

        BooleanOperator op = iOp.toBooleanOperator(this.builder);
        Assert.assertEquals(2, op.getArity());
        Assert.assertEquals(ASSIGNMENT, op.getName());

        BooleanTerm term = createTerm(op, op.getArity());
        Assert.assertEquals(Boolean.FALSE, assign(term, NOT_EVALUATED, FALSE));
        Assert.assertEquals(Boolean.TRUE, assign(term, NOT_EVALUATED, TRUE));

        Assert.assertNull(assign(term, TRUE, NOT_EVALUATED));
        Assert.assertNull(assign(term, FALSE, NOT_EVALUATED));
        Assert.assertNull(assign(term, NOT_EVALUATED, NOT_EVALUATED));
        Assert.assertNull(assign(term, TRUE, TRUE));
        Assert.assertNull(assign(term, TRUE, FALSE));
        Assert.assertNull(assign(term, FALSE, TRUE));
        Assert.assertNull(assign(term, FALSE, FALSE));

        Assert.assertSame(getAssignmentOrOperator(), getAssignmentOrOperator());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.JavaBooleanOperators#getAssignmentOrOperator()}.
     */
    public void testGetAssignmentOrOperator() {
        InstrBooleanOperator iOp = getAssignmentOrOperator();
        Assert.assertNotNull(iOp);
        Assert.assertEquals(2, iOp.getArity());
        Assert.assertEquals(ASSIGNMENT_OR, iOp.getName());

        BooleanOperator op = iOp.toBooleanOperator(this.builder);
        Assert.assertEquals(2, op.getArity());
        Assert.assertEquals(ASSIGNMENT_OR, op.getName());

        BooleanTerm term = createTerm(op, op.getArity());
        Assert.assertEquals(Boolean.FALSE, assign(term, FALSE, FALSE));
        Assert.assertEquals(Boolean.TRUE, assign(term, TRUE, FALSE));
        Assert.assertEquals(Boolean.TRUE, assign(term, FALSE, TRUE));
        Assert.assertEquals(Boolean.TRUE, assign(term, TRUE, TRUE));

        Assert.assertNull(assign(term, TRUE, NOT_EVALUATED));
        Assert.assertNull(assign(term, FALSE, NOT_EVALUATED));
        Assert.assertNull(assign(term, NOT_EVALUATED, TRUE));
        Assert.assertNull(assign(term, NOT_EVALUATED, FALSE));
        Assert.assertNull(assign(term, NOT_EVALUATED, NOT_EVALUATED));

        Assert.assertSame(getAssignmentOrOperator(), getAssignmentOrOperator());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.JavaBooleanOperators#getAssignmentAndOperator()}.
     */
    public void testGetAssignmentAndOperator() {
        InstrBooleanOperator iOp = getAssignmentAndOperator();
        Assert.assertNotNull(iOp);
        Assert.assertEquals(2, iOp.getArity());
        Assert.assertEquals(ASSIGNMENT_AND, iOp.getName());

        BooleanOperator op = iOp.toBooleanOperator(this.builder);
        Assert.assertEquals(2, op.getArity());
        Assert.assertEquals(ASSIGNMENT_AND, op.getName());

        BooleanTerm term = createTerm(op, op.getArity());
        Assert.assertEquals(Boolean.FALSE, assign(term, FALSE, FALSE));
        Assert.assertEquals(Boolean.FALSE, assign(term, TRUE, FALSE));
        Assert.assertEquals(Boolean.FALSE, assign(term, FALSE, TRUE));
        Assert.assertEquals(Boolean.TRUE, assign(term, TRUE, TRUE));

        Assert.assertNull(assign(term, TRUE, NOT_EVALUATED));
        Assert.assertNull(assign(term, FALSE, NOT_EVALUATED));
        Assert.assertNull(assign(term, NOT_EVALUATED, TRUE));
        Assert.assertNull(assign(term, NOT_EVALUATED, FALSE));
        Assert.assertNull(assign(term, NOT_EVALUATED, NOT_EVALUATED));

        Assert.assertSame(getAssignmentAndOperator(), getAssignmentAndOperator());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.JavaBooleanOperators#getAssignmentExclusiveOrOperator()}.
     */
    public void testGetAssignmentExclusiveOrOperator() {
        InstrBooleanOperator iOp = getAssignmentExclusiveOrOperator();
        Assert.assertNotNull(iOp);
        Assert.assertEquals(2, iOp.getArity());
        Assert.assertEquals(ASSIGNMENT_EXCLUSIVE_OR, iOp.getName());

        BooleanOperator op = iOp.toBooleanOperator(this.builder);
        Assert.assertEquals(2, op.getArity());
        Assert.assertEquals(ASSIGNMENT_EXCLUSIVE_OR, op.getName());

        BooleanTerm term = createTerm(op, op.getArity());
        Assert.assertEquals(Boolean.FALSE, assign(term, FALSE, FALSE));
        Assert.assertEquals(Boolean.TRUE, assign(term, TRUE, FALSE));
        Assert.assertEquals(Boolean.TRUE, assign(term, FALSE, TRUE));
        Assert.assertEquals(Boolean.FALSE, assign(term, TRUE, TRUE));

        Assert.assertNull(assign(term, TRUE, NOT_EVALUATED));
        Assert.assertNull(assign(term, FALSE, NOT_EVALUATED));
        Assert.assertNull(assign(term, NOT_EVALUATED, TRUE));
        Assert.assertNull(assign(term, NOT_EVALUATED, FALSE));
        Assert.assertNull(assign(term, NOT_EVALUATED, NOT_EVALUATED));

        Assert.assertSame(getAssignmentExclusiveOrOperator(), getAssignmentExclusiveOrOperator());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.JavaBooleanOperators#getTrueOperator()}.
     */
    public void testGetTrueOperator() {
        InstrBooleanOperator iOp = getTrueOperator();
        Assert.assertNotNull(iOp);
        Assert.assertEquals(0, iOp.getArity());
        Assert.assertEquals("true", iOp.getName());

        BooleanOperator op = iOp.toBooleanOperator(this.builder);
        Assert.assertEquals(0, op.getArity());
        Assert.assertEquals("true", op.getName());

        BooleanTerm term = createTerm(op, op.getArity());
        Assert.assertEquals(Boolean.TRUE, assign(term));

        Assert.assertSame(getTrueOperator(), getTrueOperator());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.JavaBooleanOperators#getFalseOperator()}.
     */
    public void testGetFalseOperator() {
        InstrBooleanOperator iOp = getFalseOperator();
        Assert.assertNotNull(iOp);
        Assert.assertEquals(0, iOp.getArity());
        Assert.assertEquals("false", iOp.getName());

        BooleanOperator op = iOp.toBooleanOperator(this.builder);
        Assert.assertEquals(0, op.getArity());
        Assert.assertEquals("false", op.getName());

        BooleanTerm term = createTerm(op, op.getArity());
        Assert.assertEquals(Boolean.FALSE, assign(term));

        Assert.assertSame(getFalseOperator(), getFalseOperator());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.JavaBooleanOperators#getTrueTerm()}.
     */
    public void testGetTrueTerm() {
        InstrOperatorTerm term = getTrueTerm();
        Assert.assertSame(getTrueOperator(), term.getOperator());
        Assert.assertEquals(0, term.getArity());

        Assert.assertSame(getTrueTerm(), getTrueTerm());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.JavaBooleanOperators#getFalseTerm()}.
     */
    public void testGetFalseTerm() {
        InstrOperatorTerm term = getFalseTerm();
        Assert.assertSame(getFalseOperator(), term.getOperator());
        Assert.assertEquals(0, term.getArity());

        Assert.assertSame(getFalseTerm(), getFalseTerm());
    }

    private BooleanTerm createTerm(BooleanOperator op, int arity) {
        List<BooleanTerm> booleanTerms = new Vector<BooleanTerm>(arity);

        for (int i = 1; i <= arity; i++) {
            BasicBooleanTerm basicTerm = this.builder.createBasicBooleanTerm(
                    this.builder.createEmptyLocationList());
            booleanTerms.add(basicTerm);
        }

        return this.builder.createOperatorTerm(
                this.builder.createEmptyLocationList(),
                op,
                booleanTerms);
    }

    private Boolean assign(BooleanTerm term, BooleanResult ... bools) {
        List<BooleanResult> results = new Vector<BooleanResult>(bools.length);

        for (BooleanResult thisBool : bools) {
            results.add(thisBool);
        }

        return RootTerm.getAssignmentResult(new BooleanAssignment(results),
                term);
    }
}
