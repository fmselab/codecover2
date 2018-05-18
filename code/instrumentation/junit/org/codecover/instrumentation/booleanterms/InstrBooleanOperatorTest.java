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

package org.codecover.instrumentation.booleanterms;

import static org.codecover.UtilsForTestingInstr.handleException;

import java.io.IOException;
import java.io.StringWriter;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingInstr;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.BooleanAssignment;
import org.codecover.model.mast.BooleanOperator;
import org.codecover.model.mast.BooleanResult;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: InstrBooleanOperatorTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public class InstrBooleanOperatorTest extends TestCase {

    private MASTBuilder builder;

    @Override
    protected void setUp() throws Exception {
        this.builder = UtilsForTestingInstr.newMASTBuilder(); 
    }

    @Override
    protected void tearDown() throws Exception {
        this.builder = null;
    }

    public void testInstrBooleanOperator0() {
        List<InstrBooleanTerm> terms = new LinkedList<InstrBooleanTerm>();

        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
        BooleanAssignment assignment = new BooleanAssignment(Collections.<BooleanResult>emptyList());
        possibleAssignments.put(assignment, Boolean.TRUE);

        String name = "testname";
        String[] parts = new String[]{"?1"};
        InstrBooleanOperator op = new InstrBooleanOperator(name, parts, possibleAssignments);

        Assert.assertEquals(name, op.getName());
        Assert.assertEquals(0, op.getArity());
        Assert.assertEquals("?1", op.termToString(terms));

        BooleanOperator bOp = op.toBooleanOperator(this.builder);
        Assert.assertEquals(op.getArity(), bOp.getArity());
        Assert.assertEquals(op.getName(), bOp.getName());
        Assert.assertSame(bOp, op.toBooleanOperator(this.builder));

        StringWriter target = new StringWriter();
        try {
            op.writeToTarget(target, terms);
            Assert.assertEquals("?1", target.toString());
        } catch (IOException e) {
            handleException(e);
        }

        terms.add(new InstrBasicBooleanTerm("JUHU", -1, -1));
        try {
            op.termToString(terms);
            Assert.fail("IllegalArgumentException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
            Assert.assertEquals("operandQueue.size() != this.getArity()", e.getMessage());
        }
    }

    public void testInstrBooleanOperator1() {
        List<InstrBooleanTerm> terms = new LinkedList<InstrBooleanTerm>();
        terms.add(new InstrBasicBooleanTerm("term1", -1, -1));

        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
        List<BooleanResult> parameters;
        BooleanAssignment assignment;
        parameters = new Vector<BooleanResult>(3);
        parameters.add(BooleanResult.NOT_EVALUATED);
        assignment = new BooleanAssignment(parameters);
        possibleAssignments.put(assignment, Boolean.TRUE);

        String name = "testname";
        String[] parts = new String[]{"?1 ", " ?2"};
        InstrBooleanOperator op = new InstrBooleanOperator(name, parts, possibleAssignments);

        Assert.assertEquals(name, op.getName());
        Assert.assertEquals(1, op.getArity());
        Assert.assertEquals("?1 term1 ?2", op.termToString(terms));

        BooleanOperator bOp = op.toBooleanOperator(this.builder);
        Assert.assertEquals(op.getArity(), bOp.getArity());
        Assert.assertEquals(op.getName(), bOp.getName());
        Assert.assertSame(bOp, op.toBooleanOperator(this.builder));

        StringWriter target = new StringWriter();
        try {
            op.writeToTarget(target, terms);
            Assert.assertEquals("?1 term1 ?2", target.toString());
        } catch (IOException e) {
            handleException(e);
        }

        terms.add(new InstrBasicBooleanTerm("JUHU", -1, -1));
        try {
            op.termToString(terms);
            Assert.fail("IllegalArgumentException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
            Assert.assertEquals("operandQueue.size() != this.getArity()", e.getMessage());
        }

        terms.remove(0);
        try {
            Assert.assertEquals("?1 JUHU ?2", op.termToString(terms));
        } catch (RuntimeException e) {
            Assert.fail("no IllegalArgumentException expected");
        }

        terms.remove(0);
        try {
            op.termToString(terms);
            Assert.fail("IllegalArgumentException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
            Assert.assertEquals("operandQueue.size() != this.getArity()", e.getMessage());
        }
    }

    public void testInstrBooleanOperator2() {
        List<InstrBooleanTerm> terms = new LinkedList<InstrBooleanTerm>();
        terms.add(new InstrBasicBooleanTerm("term1", -1, -1));
        terms.add(new InstrBasicBooleanTerm("term2", -1, -1));

        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
        List<BooleanResult> parameters;
        BooleanAssignment assignment;
        parameters = new Vector<BooleanResult>(3);
        parameters.add(BooleanResult.NOT_EVALUATED);
        parameters.add(BooleanResult.NOT_EVALUATED);
        assignment = new BooleanAssignment(parameters);
        possibleAssignments.put(assignment, Boolean.TRUE);

        String name = "testname";
        String[] parts = new String[]{"?1 ", " ?2 ", " ?3"};
        InstrBooleanOperator op = new InstrBooleanOperator(name, parts, possibleAssignments);

        Assert.assertEquals(name, op.getName());
        Assert.assertEquals(2, op.getArity());
        Assert.assertEquals("?1 term1 ?2 term2 ?3", op.termToString(terms));

        BooleanOperator bOp = op.toBooleanOperator(this.builder);
        Assert.assertEquals(op.getArity(), bOp.getArity());
        Assert.assertEquals(op.getName(), bOp.getName());
        Assert.assertSame(bOp, op.toBooleanOperator(this.builder));

        StringWriter target = new StringWriter();
        try {
            op.writeToTarget(target, terms);
            Assert.assertEquals("?1 term1 ?2 term2 ?3", target.toString());
        } catch (IOException e) {
            handleException(e);
        }

        terms.add(new InstrBasicBooleanTerm("JUHU", -1, -1));
        try {
            op.termToString(terms);
            Assert.fail("IllegalArgumentException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
            Assert.assertEquals("operandQueue.size() != this.getArity()", e.getMessage());
        }

        terms.remove(0);
        try {
            Assert.assertEquals("?1 term2 ?2 JUHU ?3", op.termToString(terms));
        } catch (RuntimeException e) {
            Assert.fail("no IllegalArgumentException expected");
        }

        terms.remove(0);
        try {
            op.termToString(terms);
            Assert.fail("IllegalArgumentException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
            Assert.assertEquals("operandQueue.size() != this.getArity()", e.getMessage());
        }

        terms.remove(0);
        try {
            op.termToString(terms);
            Assert.fail("IllegalArgumentException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
            Assert.assertEquals("operandQueue.size() != this.getArity()", e.getMessage());
        }
    }

    public void testInstrBooleanOperator3() {
        List<InstrBooleanTerm> terms = new LinkedList<InstrBooleanTerm>();
        terms.add(new InstrBasicBooleanTerm("term1", -1, -1));
        terms.add(new InstrBasicBooleanTerm("term2", -1, -1));

        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
        List<BooleanResult> parameters;
        BooleanAssignment assignment;
        parameters = new Vector<BooleanResult>(3);
        parameters.add(BooleanResult.NOT_EVALUATED);
        parameters.add(BooleanResult.NOT_EVALUATED);
        assignment = new BooleanAssignment(parameters);
        possibleAssignments.put(assignment, Boolean.TRUE);

        String name = "testname";
        String[] parts = new String[]{"?1 ", " %s ", " ?2"};
        InstrBooleanOperator op = new InstrBooleanOperator(name, parts, possibleAssignments);

        Assert.assertEquals(name, op.getName());
        Assert.assertEquals(2, op.getArity());
        Assert.assertEquals("?1 term1 %s term2 ?2", op.termToString(terms));

        BooleanOperator bOp = op.toBooleanOperator(this.builder);
        Assert.assertEquals(op.getArity(), bOp.getArity());
        Assert.assertEquals(op.getName(), bOp.getName());
        Assert.assertSame(bOp, op.toBooleanOperator(this.builder));

        StringWriter target = new StringWriter();
        try {
            op.writeToTarget(target, terms);
            Assert.assertEquals("?1 term1 %s term2 ?2", target.toString());
        } catch (IOException e) {
            handleException(e);
        }

        terms.add(new InstrBasicBooleanTerm("JUHU", -1, -1));
        try {
            op.termToString(terms);
            Assert.fail("IllegalArgumentException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
            Assert.assertEquals("operandQueue.size() != this.getArity()", e.getMessage());
        }

        terms.remove(0);
        try {
            Assert.assertEquals("?1 term2 %s JUHU ?2", op.termToString(terms));
        } catch (RuntimeException e) {
            handleException(e);
        }

        terms.remove(0);
        try {
            op.termToString(terms);
            Assert.fail("IllegalArgumentException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
            Assert.assertEquals("operandQueue.size() != this.getArity()", e.getMessage());
        }

        terms.remove(0);
        try {
            op.termToString(terms);
            Assert.fail("IllegalArgumentException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
            Assert.assertEquals("operandQueue.size() != this.getArity()", e.getMessage());
        }
    }

    public void testGetConstantOperator() {
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
        List<BooleanResult> parameters;
        BooleanAssignment assignment;
        parameters = new Vector<BooleanResult>(3);
        assignment = new BooleanAssignment(parameters);
        possibleAssignments.put(assignment, Boolean.TRUE);

        String name = "testname for the operator";
        String image = "FalseAndTrue";

        InstrBooleanOperator op = InstrBooleanOperator
                .getConstantOperator(name, image, possibleAssignments);

        Assert.assertEquals(0, op.getArity());
        Assert.assertEquals(name, op.getName());

        List<InstrBooleanTerm> terms = new LinkedList<InstrBooleanTerm>();

        Assert.assertEquals(image, op.termToString(terms));

        BooleanOperator bOp = op.toBooleanOperator(this.builder);
        Assert.assertEquals(op.getArity(), bOp.getArity());
        Assert.assertEquals(op.getName(), bOp.getName());
        Assert.assertSame(bOp, op.toBooleanOperator(this.builder));
    }

    public void testGetOneArgumentOperator1() {
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
        List<BooleanResult> parameters;
        BooleanAssignment assignment;
        parameters = new Vector<BooleanResult>(3);
        parameters.add(BooleanResult.NOT_EVALUATED);
        assignment = new BooleanAssignment(parameters);
        possibleAssignments.put(assignment, Boolean.TRUE);

        String name = "testname for the operator";
        String image = "isNull";

        InstrBooleanOperator op = InstrBooleanOperator
        .getOneArgumentOperator(name, image, false, possibleAssignments);

        Assert.assertEquals(1, op.getArity());
        Assert.assertEquals(name, op.getName());

        List<InstrBooleanTerm> terms = new LinkedList<InstrBooleanTerm>();
        terms.add(new InstrBasicBooleanTerm("argument1", -1, -1));

        Assert.assertEquals(image + "argument1", op.termToString(terms));

        BooleanOperator bOp = op.toBooleanOperator(this.builder);
        Assert.assertEquals(op.getArity(), bOp.getArity());
        Assert.assertEquals(op.getName(), bOp.getName());
        Assert.assertSame(bOp, op.toBooleanOperator(this.builder));
    }

    public void testGetOneArgumentOperator2() {
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
        List<BooleanResult> parameters;
        BooleanAssignment assignment;
        parameters = new Vector<BooleanResult>(3);
        parameters.add(BooleanResult.NOT_EVALUATED);
        assignment = new BooleanAssignment(parameters);
        possibleAssignments.put(assignment, Boolean.TRUE);

        String name = "testname for the operator";
        String image = "isNull";

        InstrBooleanOperator op = InstrBooleanOperator
        .getOneArgumentOperator(name, image, true, possibleAssignments);

        Assert.assertEquals(1, op.getArity());
        Assert.assertEquals(name, op.getName());

        List<InstrBooleanTerm> terms = new LinkedList<InstrBooleanTerm>();
        terms.add(new InstrBasicBooleanTerm("argument2", -1, -1));

        Assert.assertEquals(image + " argument2", op.termToString(terms));

        BooleanOperator bOp = op.toBooleanOperator(this.builder);
        Assert.assertEquals(op.getArity(), bOp.getArity());
        Assert.assertEquals(op.getName(), bOp.getName());
        Assert.assertSame(bOp, op.toBooleanOperator(this.builder));
    }

    public void testGetTwoArgumentOperator() {
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
        List<BooleanResult> parameters;
        BooleanAssignment assignment;
        parameters = new Vector<BooleanResult>(3);
        parameters.add(BooleanResult.NOT_EVALUATED);
        parameters.add(BooleanResult.NOT_EVALUATED);
        assignment = new BooleanAssignment(parameters);
        possibleAssignments.put(assignment, Boolean.TRUE);

        String name = "testname for the operator";
        String image = "<>";

        InstrBooleanOperator op = InstrBooleanOperator
        .getTwoArgumentOperator(name, image, possibleAssignments);

        Assert.assertEquals(2, op.getArity());
        Assert.assertEquals(name, op.getName());

        List<InstrBooleanTerm> terms = new LinkedList<InstrBooleanTerm>();
        terms.add(new InstrBasicBooleanTerm("arg1", -1, -1));
        terms.add(new InstrBasicBooleanTerm("arg2", -1, -1));

        Assert.assertEquals("arg1 " + image + " arg2", op.termToString(terms));

        BooleanOperator bOp = op.toBooleanOperator(this.builder);
        Assert.assertEquals(op.getArity(), bOp.getArity());
        Assert.assertEquals(op.getName(), bOp.getName());
        Assert.assertSame(bOp, op.toBooleanOperator(this.builder));
    }

    public void testNull1() {
        try {
            new InstrBooleanOperator(null, new String[]{"!"}, new HashMap<BooleanAssignment, Boolean>());
            Assert.fail("NullpointerException expected");
        } catch (Throwable e) {
            Assert.assertTrue(e instanceof NullPointerException);
        }
    }

    public void testNull2() {
        try {
            new InstrBooleanOperator("opname", null, new HashMap<BooleanAssignment, Boolean>());
            Assert.fail("NullpointerException expected");
        } catch (Throwable e) {
            Assert.assertTrue(e instanceof NullPointerException);
        }
    }

    public void testNull3() {
        try {
            new InstrBooleanOperator(null, null, new HashMap<BooleanAssignment, Boolean>());
            Assert.fail("NullpointerException expected");
        } catch (Throwable e) {
            Assert.assertTrue(e instanceof NullPointerException);
        }
    }

    public void testNull4() {
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
        List<BooleanResult> parameters;
        BooleanAssignment assignment;
        parameters = new Vector<BooleanResult>(3);
        parameters.add(BooleanResult.NOT_EVALUATED);
        parameters.add(BooleanResult.NOT_EVALUATED);
        assignment = new BooleanAssignment(parameters);
        possibleAssignments.put(assignment, Boolean.TRUE);

        try {
            new InstrBooleanOperator("opname", new String[]{null}, possibleAssignments);
            Assert.fail("NullpointerException expected");
        } catch (Throwable e) {
            Assert.assertTrue(e instanceof NullPointerException);
        }
    }

    public void testNull5() {
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
        List<BooleanResult> parameters;
        BooleanAssignment assignment;
        parameters = new Vector<BooleanResult>(3);
        parameters.add(BooleanResult.NOT_EVALUATED);
        parameters.add(BooleanResult.NOT_EVALUATED);
        assignment = new BooleanAssignment(parameters);
        possibleAssignments.put(assignment, Boolean.TRUE);

        try {
            new InstrBooleanOperator("opname", new String[]{"", null, "<>"}, possibleAssignments);
            Assert.fail("NullPointerException expected");
        } catch (Throwable e) {
            Assert.assertTrue(e instanceof NullPointerException);
        }
    }

    public void testNull6() {
        try {
            new InstrBooleanOperator("opname", new String[]{"Not(", ")"}, null);
            Assert.fail("NullpointerException expected");
        } catch (Throwable e) {
            Assert.assertTrue(e instanceof NullPointerException);
        }
    }

    public void testNull7() {
        try {
            new InstrBooleanOperator("opname", new String[]{"", null, "<>"}, null);
            Assert.fail("NullpointerException expected");
        } catch (Throwable e) {
            Assert.assertTrue(e instanceof NullPointerException);
        }
    }

    public void testWrongArray() {
        try {
            new InstrBooleanOperator("opname", new String[0], new HashMap<BooleanAssignment, Boolean>());
            Assert.fail("RuntimeException expected");
        } catch (Throwable e) {
            Assert.assertTrue(e instanceof RuntimeException);
        }
    }

    public void testWrongBooleanAssignment1() {
        try {
            new InstrBooleanOperator("opname", new String[]{"Not(", ")"}, new HashMap<BooleanAssignment, Boolean>());
            Assert.fail("RuntimeException expected");
        } catch (Throwable e) {
            Assert.assertTrue(e instanceof RuntimeException);
        }
    }

    public void testWrongBooleanAssignment2() {
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
        List<BooleanResult> parameters;
        BooleanAssignment assignment;
        parameters = new Vector<BooleanResult>(3);
        assignment = new BooleanAssignment(parameters);
        possibleAssignments.put(assignment, Boolean.TRUE);

        try {
            new InstrBooleanOperator("opname", new String[]{"Not(", ")"}, possibleAssignments);
            Assert.fail("RuntimeException expected");
        } catch (Throwable e) {
            Assert.assertTrue(e instanceof RuntimeException);
        }
    }

    public void testWrongBooleanAssignment3() {
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();
        List<BooleanResult> parameters;
        BooleanAssignment assignment;
        parameters = new Vector<BooleanResult>(3);
        parameters.add(BooleanResult.NOT_EVALUATED);
        parameters.add(BooleanResult.NOT_EVALUATED);
        assignment = new BooleanAssignment(parameters);
        possibleAssignments.put(assignment, Boolean.TRUE);

        try {
            new InstrBooleanOperator("opname", new String[]{"Not(", ")"}, possibleAssignments);
            Assert.fail("RuntimeException expected");
        } catch (Throwable e) {
            Assert.assertTrue(e instanceof RuntimeException);
        }
    }
}
