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

package org.codecover.instrumentation;

import static org.codecover.UtilsForTestingJava.handleException;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getAndOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getConditionalAndOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getConditionalOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getConditionalOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getExclusiveOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getFalseOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getNotOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getTrueOperator;

import java.io.IOException;
import java.io.StringWriter;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJava;
import org.codecover.instrumentation.booleanterms.InstrBasicBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrBooleanOperator;
import org.codecover.instrumentation.booleanterms.InstrBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrOperatorTerm;
import org.codecover.instrumentation.java15.JavaBooleanOperators;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.SourceFile;

/**
 * @author Christoph Müller
 * 
 * Tests {@link InstrOperatorTerm}!
 * 
 * @version 1.0 ($Id: InstrOperatorTermTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
@SuppressWarnings("deprecation")
public class InstrOperatorTermTest extends TestCase {

    private MASTBuilder builder;

    private SourceFile sourceFile;

    @Override
    protected void setUp() {
        this.builder = UtilsForTestingJava.newMASTBuilder();
        this.sourceFile = this.builder.createSourceFile("test source file", "abcdefghijklmnopqrstuvwxyz" +
                                                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" + 
                                                                            "abcdefghijklmnopqrstuvwxyz" +
                                                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +  
                                                                            "abcdefghijklmnopqrstuvwxyz" +
                                                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +  
                                                                            "abcdefghijklmnopqrstuvwxyz" +
                                                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +  
                                                                            "abcdefghijklmnopqrstuvwxyz" +
                                                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +  
                                                                            "abcdefghijklmnopqrstuvwxyz" +
                                                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +  
                                                                            "abcdefghijklmnopqrstuvwxyz" +
                                                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +  
                                                                            "abcdefghijklmnopqrstuvwxyz" +
                                                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +  
                                                                            "abcdefghijklmnopqrstuvwxyz" +
                                                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +  
                                                                            "abcdefghijklmnopqrstuvwxyz" +
                                                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +  
                                                                            "abcdefghijklmnopqrstuvwxyz" +
                                                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +  
                                                                            "abcdefghijklmnopqrstuvwxyz" +
                                                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +  
                                                                            "abcdefghijklmnopqrstuvwxyz" +
                                                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
    }

    public void testInstrOperatorTerm0GoodArity() {
        String trueString = "true";
        InstrBooleanOperator op = getTrueOperator();
        Assert.assertEquals(0, op.getArity());

        InstrOperatorTerm term = new InstrOperatorTerm(op);
        Assert.assertEquals(0, term.getArity());
        Assert.assertSame(op, term.getOperator());
        Assert.assertEquals(0, term.getOperands().size());
        Assert.assertTrue(term.getOperands().isEmpty());
        Assert.assertEquals(trueString, term.termToString());
        Assert.assertEquals(trueString, term.toString());
        Assert.assertSame(InstrOperatorTerm.EMPTY_START_END_LOCATION, term.getOperatorLocations());

        StringWriter target = new StringWriter();
        try {
            term.writeToTarget(target);
            Assert.assertEquals(trueString, target.toString());
        } catch (IOException e) {
            handleException(e);
        }

        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        term.getAllBasicBooleanTerms(basicTerms);
        Assert.assertEquals(0, basicTerms.size());
        Assert.assertTrue(basicTerms.isEmpty());

        BooleanTerm bTerm = term.toBooleanTerm(this.builder, this.sourceFile);
        Assert.assertEquals(0, bTerm.getBasicBooleanTerms());
        Assert.assertEquals(0, bTerm.getLocation().getLocations().size());
    }

    public void testInstrOperatorTerm0WrongArity() {
        try {
            new InstrOperatorTerm(getNotOperator());
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }

        try {
            new InstrOperatorTerm(getOrOperator());
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }
    }

    public void testInstrOperatorTerm0Null() {
        try {
            new InstrOperatorTerm(null);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operator == null", e.getMessage());
        }
    }

    public void testGetOperantsModify0() {
        InstrBooleanOperator op = getTrueOperator();
        Assert.assertEquals(0, op.getArity());

        InstrOperatorTerm term = new InstrOperatorTerm(op);

        try {
            term.getOperands().remove(0);
            Assert.fail("modifications not allowed!");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof UnsupportedOperationException);
        }
    }

    public void testInstrOperatorTerm0LocationGoodArity() {
        String trueString = "true";
        InstrBooleanOperator op = getTrueOperator();
        Assert.assertEquals(0, op.getArity());

        InstrOperatorTerm term = new InstrOperatorTerm(op, 6, 8);
        Assert.assertEquals(0, term.getArity());
        Assert.assertSame(op, term.getOperator());
        Assert.assertEquals(0, term.getOperands().size());
        Assert.assertTrue(term.getOperands().isEmpty());
        Assert.assertEquals(trueString, term.termToString());
        Assert.assertEquals(trueString, term.toString());
        Assert.assertEquals(6, term.getOperatorLocations()[0]);
        Assert.assertEquals(8, term.getOperatorLocations()[1]);

        StringWriter target = new StringWriter();
        try {
            term.writeToTarget(target);
            Assert.assertEquals(trueString, target.toString());
        } catch (IOException e) {
            handleException(e);
        }
    
        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        term.getAllBasicBooleanTerms(basicTerms);
        Assert.assertEquals(0, basicTerms.size());
        Assert.assertTrue(basicTerms.isEmpty());

        BooleanTerm bTerm = term.toBooleanTerm(this.builder, this.sourceFile);
        Assert.assertEquals(0, bTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, bTerm.getLocation().getLocations().size());
        Assert.assertEquals(6, bTerm.getLocation().getLocations().get(0).getStartOffset());
        Assert.assertEquals(8, bTerm.getLocation().getLocations().get(0).getEndOffset());
    }

    public void testInstrOperatorTerm0LocationWrongArity() {
        try {
            new InstrOperatorTerm(getNotOperator(), 6, 8);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }
    
        try {
            new InstrOperatorTerm(getOrOperator(), 6, 8);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }
    }

    public void testInstrOperatorTerm0LocationNull() {
        try {
            new InstrOperatorTerm(null, 6, 8);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operator == null", e.getMessage());
        }
    }

    public void testGetOperantsModify0Location() {
        InstrBooleanOperator op = getTrueOperator();
        Assert.assertEquals(0, op.getArity());
    
        InstrOperatorTerm term = new InstrOperatorTerm(op, 6, 8);
    
        try {
            term.getOperands().remove(0);
            Assert.fail("modifications not allowed!");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof UnsupportedOperationException);
        }
    }

    public void testInstrOperatorTerm1GoodArity() {
        String image = "v.isEmpty()";
        InstrBasicBooleanTerm innerTerm = new InstrBasicBooleanTerm(image, -1, -1);
        Assert.assertEquals(image, innerTerm.termToString());

        InstrBooleanOperator op = getNotOperator();
        Assert.assertEquals(1, op.getArity());

        InstrOperatorTerm term = new InstrOperatorTerm(op, innerTerm);
        Assert.assertEquals(1, term.getArity());
        Assert.assertSame(op, term.getOperator());
        Assert.assertEquals(1, term.getOperands().size());
        Assert.assertFalse(term.getOperands().isEmpty());
        Assert.assertSame(innerTerm, term.getOperands().get(0));
        Assert.assertEquals("!" + image, term.termToString());
        Assert.assertEquals("!" + image, term.toString());
        Assert.assertSame(InstrOperatorTerm.EMPTY_START_END_LOCATION, term.getOperatorLocations());

        StringWriter target = new StringWriter();
        try {
            term.writeToTarget(target);
            Assert.assertEquals("!" + image, target.toString());
        } catch (IOException e) {
            handleException(e);
        }

        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        term.getAllBasicBooleanTerms(basicTerms);
        Assert.assertEquals(1, basicTerms.size());
        Assert.assertSame(innerTerm, basicTerms.get(0));
        
        BooleanTerm bTerm = term.toBooleanTerm(this.builder, this.sourceFile);
        Assert.assertEquals(1, bTerm.getBasicBooleanTerms());
        Assert.assertEquals(0, bTerm.getLocation().getLocations().size());
    }

    public void testInstrOperatorTerm1WrongArity() {
        try {
            String image = "v.isEmpty()";
            InstrBasicBooleanTerm innerTerm = new InstrBasicBooleanTerm(image, -1, -1);
            Assert.assertEquals(image, innerTerm.termToString());

            new InstrOperatorTerm(getTrueOperator(), innerTerm);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }

        try {
            String image = "v.isEmpty()";
            InstrBasicBooleanTerm innerTerm = new InstrBasicBooleanTerm(image, -1, -1);
            Assert.assertEquals(image, innerTerm.termToString());

            new InstrOperatorTerm(getOrOperator(), innerTerm);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }
    }

    public void testInstrOperatorTerm1Null() {
        try {
            String image = "v.isEmpty()";
            InstrBasicBooleanTerm innerTerm = new InstrBasicBooleanTerm(image, -1, -1);
            Assert.assertEquals(image, innerTerm.termToString());

            new InstrOperatorTerm(null, innerTerm);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operator == null", e.getMessage());
        }

        try {
            InstrBasicBooleanTerm innerTerm = null;

            new InstrOperatorTerm(getNotOperator(), innerTerm);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("singleTerm == null", e.getMessage());
        }

        try {
            InstrBasicBooleanTerm innerTerm = null;

            new InstrOperatorTerm(null, innerTerm);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operator == null", e.getMessage());
        }
    }

    public void testGetOperantsModify1() {
        String image = "v.isEmpty()";
        InstrBasicBooleanTerm innerTerm = new InstrBasicBooleanTerm(image, -1, -1);
    
        InstrOperatorTerm term = new InstrOperatorTerm(getNotOperator(), innerTerm);
    
        try {
            term.getOperands().remove(0);
            Assert.fail("modifications not allowed!");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof UnsupportedOperationException);
        }
    }

    public void testInstrOperatorTerm1LocationGoodArity() {
        String image = "v.isEmpty()";
        InstrBasicBooleanTerm innerTerm = new InstrBasicBooleanTerm(image, -1, -1);
        Assert.assertEquals(image, innerTerm.termToString());
    
        InstrBooleanOperator op = getNotOperator();
        Assert.assertEquals(1, op.getArity());
    
        InstrOperatorTerm term = new InstrOperatorTerm(op, innerTerm, 0, 3);
        Assert.assertEquals(1, term.getArity());
        Assert.assertSame(op, term.getOperator());
        Assert.assertEquals(1, term.getOperands().size());
        Assert.assertFalse(term.getOperands().isEmpty());
        Assert.assertSame(innerTerm, term.getOperands().get(0));
        Assert.assertEquals("!" + image, term.termToString());
        Assert.assertEquals("!" + image, term.toString());
        Assert.assertEquals(0, term.getOperatorLocations()[0]);
        Assert.assertEquals(3, term.getOperatorLocations()[1]);
    
        StringWriter target = new StringWriter();
        try {
            term.writeToTarget(target);
            Assert.assertEquals("!" + image, target.toString());
        } catch (IOException e) {
            handleException(e);
        }
    
        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        term.getAllBasicBooleanTerms(basicTerms);
        Assert.assertEquals(1, basicTerms.size());
        Assert.assertSame(innerTerm, basicTerms.get(0));
        
        BooleanTerm bTerm = term.toBooleanTerm(this.builder, this.sourceFile);
        Assert.assertEquals(1, bTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, bTerm.getLocation().getLocations().size());
        Assert.assertEquals(0, bTerm.getLocation().getLocations().get(0).getStartOffset());
        Assert.assertEquals(3, bTerm.getLocation().getLocations().get(0).getEndOffset());
    }

    public void testInstrOperatorTerm1LocationWrongArity() {
        try {
            String image = "v.isEmpty()";
            InstrBasicBooleanTerm innerTerm = new InstrBasicBooleanTerm(image, -1, -1);
            Assert.assertEquals(image, innerTerm.termToString());
    
            new InstrOperatorTerm(getTrueOperator(), innerTerm, Integer.MIN_VALUE, Integer.MIN_VALUE + 3);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }
    
        try {
            String image = "v.isEmpty()";
            InstrBasicBooleanTerm innerTerm = new InstrBasicBooleanTerm(image, -1, -1);
            Assert.assertEquals(image, innerTerm.termToString());
    
            new InstrOperatorTerm(getOrOperator(), innerTerm, Integer.MIN_VALUE, Integer.MIN_VALUE + 3);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }
    }

    public void testInstrOperatorTerm1LocationNull() {
        try {
            String image = "v.isEmpty()";
            InstrBasicBooleanTerm innerTerm = new InstrBasicBooleanTerm(image, -1, -1);
            Assert.assertEquals(image, innerTerm.termToString());
    
            new InstrOperatorTerm(null, innerTerm, Integer.MIN_VALUE, Integer.MIN_VALUE + 3);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operator == null", e.getMessage());
        }
    
        try {
            InstrBasicBooleanTerm innerTerm = null;
    
            new InstrOperatorTerm(getNotOperator(), innerTerm, Integer.MIN_VALUE, Integer.MIN_VALUE + 3);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("singleTerm == null", e.getMessage());
        }
    
        try {
            InstrBasicBooleanTerm innerTerm = null;
    
            new InstrOperatorTerm(null, innerTerm, Integer.MIN_VALUE, Integer.MIN_VALUE + 3);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operator == null", e.getMessage());
        }
    }

    public void testGetOperantsModify1Location() {
        String image = "v.isEmpty()";
        InstrBasicBooleanTerm innerTerm = new InstrBasicBooleanTerm(image, -1, -1);
    
        InstrOperatorTerm term = new InstrOperatorTerm(getNotOperator(), innerTerm, Integer.MIN_VALUE, Integer.MIN_VALUE + 3);
    
        try {
            term.getOperands().remove(0);
            Assert.fail("modifications not allowed!");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof UnsupportedOperationException);
        }
    }

    public void testInstrOperatorTerm2GoodArity() {
        String image1 = "v.isEmpty()";
        InstrBasicBooleanTerm leftTerm = new InstrBasicBooleanTerm(image1, -1, -1);
        Assert.assertEquals(image1, leftTerm.termToString());

        String image2 = "a instance of Object";
        InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);
        Assert.assertEquals(image1, leftTerm.termToString());

        InstrBooleanOperator op = getAndOperator();
        Assert.assertEquals(2, op.getArity());

        InstrOperatorTerm term = new InstrOperatorTerm(leftTerm, op, rightTerm);
        Assert.assertEquals(2, term.getArity());
        Assert.assertSame(op, term.getOperator());
        Assert.assertEquals(2, term.getOperands().size());
        Assert.assertFalse(term.getOperands().isEmpty());
        Assert.assertSame(leftTerm, term.getOperands().toArray()[0]);
        Assert.assertSame(rightTerm, term.getOperands().toArray()[1]);
        Assert.assertEquals(image1 + " & " + image2, term.termToString());
        Assert.assertEquals(image1 + " & " + image2, term.toString());
        Assert.assertSame(InstrOperatorTerm.EMPTY_START_END_LOCATION, term.getOperatorLocations());

        StringWriter target = new StringWriter();
        try {
            term.writeToTarget(target);
            Assert.assertEquals(image1 + " & " + image2, target.toString());
        } catch (IOException e) {
            handleException(e);
        }

        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        term.getAllBasicBooleanTerms(basicTerms);
        Assert.assertEquals(2, basicTerms.size());
        Assert.assertSame(leftTerm, basicTerms.toArray()[0]);
        Assert.assertSame(rightTerm, basicTerms.toArray()[1]);

        BooleanTerm bTerm = term.toBooleanTerm(this.builder, this.sourceFile);
        Assert.assertEquals(2, bTerm.getBasicBooleanTerms());
        Assert.assertEquals(0, bTerm.getLocation().getLocations().size());
    }

    public void testInstrOperatorTerm2WrongArity() {
        try {
            String image1 = "v.isEmpty()";
            InstrBasicBooleanTerm leftTerm = new InstrBasicBooleanTerm(image1, -1, -1);

            String image2 = "a instance of Object";
            InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);

            new InstrOperatorTerm(leftTerm, getTrueOperator(), rightTerm);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }

        try {
            String image1 = "v.isEmpty()";
            InstrBasicBooleanTerm leftTerm = new InstrBasicBooleanTerm(image1, -1, -1);

            String image2 = "a instance of Object";
            InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);

            new InstrOperatorTerm(leftTerm, getNotOperator(), rightTerm);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }

        try {
            String image1 = "v.isEmpty()";
            InstrBasicBooleanTerm leftTerm = new InstrBasicBooleanTerm(image1, -1, -1);

            String image2 = "a instance of Object";
            InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);

            new InstrOperatorTerm(leftTerm, getConditionalOperator(), rightTerm);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }
    }

    public void testInstrOperatorTerm2Null() {
        try {
            String image1 = "v.isEmpty()";
            InstrBasicBooleanTerm leftTerm = new InstrBasicBooleanTerm(image1, -1, -1);

            String image2 = "a instance of Object";
            InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);

            new InstrOperatorTerm(leftTerm, null, rightTerm);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operator == null", e.getMessage());
        }

        try {
            String image2 = "a instance of Object";
            InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);

            new InstrOperatorTerm(null, null, rightTerm);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operator == null", e.getMessage());
        }

        try {
            String image2 = "a instance of Object";
            InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);

            new InstrOperatorTerm(null, getOrOperator(), rightTerm);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("leftTerm == null", e.getMessage());
        }

        try {
            String image1 = "v.isEmpty()";
            InstrBasicBooleanTerm leftTerm = new InstrBasicBooleanTerm(image1, -1, -1);

            new InstrOperatorTerm(leftTerm, getOrOperator(), null);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("rightTerm == null", e.getMessage());
        }
    }

    public void testGetOperantsModify2() {
        String image1 = "v.isEmpty()";
        InstrBasicBooleanTerm leftTerm = new InstrBasicBooleanTerm(image1, -1, -1);

        String image2 = "a instance of Object";
        InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);

        InstrBooleanOperator op = getAndOperator();

        InstrOperatorTerm term = new InstrOperatorTerm(leftTerm, op, rightTerm);

        try {
            term.getOperands().remove(0);
            Assert.fail("modifications not allowed!");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof UnsupportedOperationException);
        }
    }

    public void testInstrOperatorTerm2LocationGoodArity() {
        String image1 = "v.isEmpty()";
        InstrBasicBooleanTerm leftTerm = new InstrBasicBooleanTerm(image1, -1, -1);
        Assert.assertEquals(image1, leftTerm.termToString());
    
        String image2 = "a instance of Object";
        InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);
        Assert.assertEquals(image1, leftTerm.termToString());
    
        InstrBooleanOperator op = getAndOperator();
        Assert.assertEquals(2, op.getArity());

        InstrOperatorTerm term = new InstrOperatorTerm(leftTerm, op, rightTerm,
                97, 100);
        Assert.assertEquals(2, term.getArity());
        Assert.assertSame(op, term.getOperator());
        Assert.assertEquals(2, term.getOperands().size());
        Assert.assertFalse(term.getOperands().isEmpty());
        Assert.assertSame(leftTerm, term.getOperands().toArray()[0]);
        Assert.assertSame(rightTerm, term.getOperands().toArray()[1]);
        Assert.assertEquals(image1 + " & " + image2, term.termToString());
        Assert.assertEquals(image1 + " & " + image2, term.toString());
        Assert.assertEquals(97, term.getOperatorLocations()[0]);
        Assert.assertEquals(100, term.getOperatorLocations()[1]);
    
        StringWriter target = new StringWriter();
        try {
            term.writeToTarget(target);
            Assert.assertEquals(image1 + " & " + image2, target.toString());
        } catch (IOException e) {
            handleException(e);
        }
    
        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        term.getAllBasicBooleanTerms(basicTerms);
        Assert.assertEquals(2, basicTerms.size());
        Assert.assertSame(leftTerm, basicTerms.toArray()[0]);
        Assert.assertSame(rightTerm, basicTerms.toArray()[1]);
        
        BooleanTerm bTerm = term.toBooleanTerm(this.builder, this.sourceFile);
        Assert.assertEquals(2, bTerm.getBasicBooleanTerms());
        Assert.assertEquals(97, bTerm.getLocation().getLocations().get(0).getStartOffset());
        Assert.assertEquals(100, bTerm.getLocation().getLocations().get(0).getEndOffset());
    }

    public void testInstrOperatorTerm2LocationWrongArity() {
        try {
            String image1 = "v.isEmpty()";
            InstrBasicBooleanTerm leftTerm = new InstrBasicBooleanTerm(image1, -1, -1);
    
            String image2 = "a instance of Object";
            InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);
    
            new InstrOperatorTerm(leftTerm, getTrueOperator(), rightTerm,
                    Integer.MAX_VALUE - 3, Integer.MAX_VALUE);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }
    
        try {
            String image1 = "v.isEmpty()";
            InstrBasicBooleanTerm leftTerm = new InstrBasicBooleanTerm(image1, -1, -1);
    
            String image2 = "a instance of Object";
            InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);
    
            new InstrOperatorTerm(leftTerm, getNotOperator(), rightTerm,
                    Integer.MAX_VALUE - 3, Integer.MAX_VALUE);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }
    
        try {
            String image1 = "v.isEmpty()";
            InstrBasicBooleanTerm leftTerm = new InstrBasicBooleanTerm(image1, -1, -1);
    
            String image2 = "a instance of Object";
            InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);
    
            new InstrOperatorTerm(leftTerm, getConditionalOperator(), rightTerm,
                    Integer.MAX_VALUE - 3, Integer.MAX_VALUE);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }
    }

    public void testInstrOperatorTerm2LocationNull() {
        try {
            String image1 = "v.isEmpty()";
            InstrBasicBooleanTerm leftTerm = new InstrBasicBooleanTerm(image1, -1, -1);
    
            String image2 = "a instance of Object";
            InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);
    
            new InstrOperatorTerm(leftTerm, null, rightTerm,
                    Integer.MAX_VALUE - 3, Integer.MAX_VALUE);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operator == null", e.getMessage());
        }
    
        try {
            String image2 = "a instance of Object";
            InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);
    
            new InstrOperatorTerm(null, null, rightTerm,
                    Integer.MAX_VALUE - 3, Integer.MAX_VALUE);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operator == null", e.getMessage());
        }
    
        try {
            String image2 = "a instance of Object";
            InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);
    
            new InstrOperatorTerm(null, getOrOperator(), rightTerm,
                    Integer.MAX_VALUE - 3, Integer.MAX_VALUE);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("leftTerm == null", e.getMessage());
        }
    
        try {
            String image1 = "v.isEmpty()";
            InstrBasicBooleanTerm leftTerm = new InstrBasicBooleanTerm(image1, -1, -1);
    
            new InstrOperatorTerm(leftTerm, getOrOperator(), null,
                    Integer.MAX_VALUE - 3, Integer.MAX_VALUE);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("rightTerm == null", e.getMessage());
        }
    }

    public void testGetOperantsModify2Location() {
        String image1 = "v.isEmpty()";
        InstrBasicBooleanTerm leftTerm = new InstrBasicBooleanTerm(image1, -1, -1);
    
        String image2 = "a instance of Object";
        InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm(image2, -1, -1);
    
        InstrBooleanOperator op = getAndOperator();
    
        InstrOperatorTerm term = new InstrOperatorTerm(leftTerm, op, rightTerm,
                Integer.MAX_VALUE - 3, Integer.MAX_VALUE);
    
        try {
            term.getOperands().remove(0);
            Assert.fail("modifications not allowed!");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof UnsupportedOperationException);
        }
    }

    public void testInstrOperatorTerm3GoodArity0() {
        List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();

        InstrBooleanOperator op = getTrueOperator();
        Assert.assertEquals(0, op.getArity());

        InstrOperatorTerm term = new InstrOperatorTerm(op, termList);
        Assert.assertEquals(0, term.getArity());
        Assert.assertSame(op, term.getOperator());
        Assert.assertEquals(0, term.getOperands().size());
        Assert.assertTrue(term.getOperands().isEmpty());
        Assert.assertEquals("true", term.termToString());
        Assert.assertEquals("true", term.toString());
        Assert.assertSame(InstrOperatorTerm.EMPTY_START_END_LOCATION, term.getOperatorLocations());

        StringWriter target = new StringWriter();
        try {
            term.writeToTarget(target);
            Assert.assertEquals("true", target.toString());
        } catch (IOException e) {
            handleException(e);
        }

        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        term.getAllBasicBooleanTerms(basicTerms);
        Assert.assertEquals(0, basicTerms.size());
        Assert.assertEquals(termList, basicTerms);
        
        BooleanTerm bTerm = term.toBooleanTerm(this.builder, this.sourceFile);
        Assert.assertEquals(0, bTerm.getBasicBooleanTerms());
        Assert.assertEquals(0, bTerm.getLocation().getLocations().size());
    }

    public void testInstrOperatorTerm3GoodArity1() {
        List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();

        String image1 = "v.isEmpty()";
        termList.add(new InstrBasicBooleanTerm(image1, -1, -1));
        Assert.assertEquals(image1, termList.toArray()[0].toString());

        InstrBooleanOperator op = getNotOperator();
        Assert.assertEquals(1, op.getArity());

        InstrOperatorTerm term = new InstrOperatorTerm(op, termList);
        Assert.assertEquals(1, term.getArity());
        Assert.assertSame(op, term.getOperator());
        Assert.assertEquals(1, term.getOperands().size());
        Assert.assertFalse(term.getOperands().isEmpty());
        Assert.assertSame(termList.toArray()[0], term.getOperands().toArray()[0]);
        Assert.assertEquals("!" + image1, term.termToString());
        Assert.assertEquals("!" + image1, term.toString());
        Assert.assertSame(InstrOperatorTerm.EMPTY_START_END_LOCATION, term.getOperatorLocations());

        StringWriter target = new StringWriter();
        try {
            term.writeToTarget(target);
            Assert.assertEquals("!" + image1, target.toString());
        } catch (IOException e) {
            handleException(e);
        }

        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        term.getAllBasicBooleanTerms(basicTerms);
        Assert.assertEquals(1, basicTerms.size());
        Assert.assertSame(termList.toArray()[0], basicTerms.toArray()[0]);
        Assert.assertEquals(termList, basicTerms);
        
        BooleanTerm bTerm = term.toBooleanTerm(this.builder, this.sourceFile);
        Assert.assertEquals(1, bTerm.getBasicBooleanTerms());
        Assert.assertEquals(0, bTerm.getLocation().getLocations().size());
    }

    public void testInstrOperatorTerm3GoodArity2() {
        List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();

        String image1 = "v.isEmpty()";
        termList.add(new InstrBasicBooleanTerm(image1, -1, -1));
        Assert.assertEquals(image1, termList.toArray()[0].toString());

        String image2 = "a instance of Object";
        termList.add(new InstrBasicBooleanTerm(image2, -1, -1));
        Assert.assertEquals(image2, termList.toArray()[1].toString());

        InstrBooleanOperator op = getConditionalAndOperator();
        Assert.assertEquals(2, op.getArity());

        InstrOperatorTerm term = new InstrOperatorTerm(op, termList);
        Assert.assertEquals(2, term.getArity());
        Assert.assertSame(op, term.getOperator());
        Assert.assertEquals(2, term.getOperands().size());
        Assert.assertFalse(term.getOperands().isEmpty());
        Assert.assertSame(termList.toArray()[0], term.getOperands().toArray()[0]);
        Assert.assertSame(termList.toArray()[1], term.getOperands().toArray()[1]);
        Assert.assertEquals(image1 + " && " + image2, term.termToString());
        Assert.assertEquals(image1 + " && " + image2, term.toString());
        Assert.assertSame(InstrOperatorTerm.EMPTY_START_END_LOCATION, term.getOperatorLocations());

        StringWriter target = new StringWriter();
        try {
            term.writeToTarget(target);
            Assert.assertEquals(image1 + " && " + image2, target.toString());
        } catch (IOException e) {
            handleException(e);
        }

        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        term.getAllBasicBooleanTerms(basicTerms);
        Assert.assertEquals(2, basicTerms.size());
        Assert.assertSame(termList.toArray()[0], basicTerms.toArray()[0]);
        Assert.assertSame(termList.toArray()[1], basicTerms.toArray()[1]);
        Assert.assertEquals(termList, basicTerms);
        
        BooleanTerm bTerm = term.toBooleanTerm(this.builder, this.sourceFile);
        Assert.assertEquals(2, bTerm.getBasicBooleanTerms());
        Assert.assertEquals(0, bTerm.getLocation().getLocations().size());
    }

    public void testInstrOperatorTerm3GoodArity3() {
        List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();

        String image1 = "v.isEmpty()";
        termList.add(new InstrBasicBooleanTerm(image1, -1, -1));
        Assert.assertEquals(image1, termList.toArray()[0].toString());

        String image2 = "a instance of Object";
        termList.add(new InstrBasicBooleanTerm(image2, -1, -1));
        Assert.assertEquals(image2, termList.toArray()[1].toString());

        String image3 = "v.contains(a)";
        termList.add(new InstrBasicBooleanTerm(image3, -1, -1));
        Assert.assertEquals(image3, termList.toArray()[2].toString());

        InstrBooleanOperator op = getConditionalOperator();
        Assert.assertEquals(3, op.getArity());

        InstrOperatorTerm term = new InstrOperatorTerm(op, termList);
        Assert.assertEquals(3, term.getArity());
        Assert.assertSame(op, term.getOperator());
        Assert.assertEquals(3, term.getOperands().size());
        Assert.assertFalse(term.getOperands().isEmpty());
        Assert.assertSame(termList.toArray()[0], term.getOperands().toArray()[0]);
        Assert.assertSame(termList.toArray()[1], term.getOperands().toArray()[1]);
        Assert.assertSame(termList.toArray()[2], term.getOperands().toArray()[2]);
        Assert.assertEquals(image1 + " ? " + image2 + " : " + image3, term.termToString());
        Assert.assertEquals(image1 + " ? " + image2 + " : " + image3, term.toString());
        Assert.assertSame(InstrOperatorTerm.EMPTY_START_END_LOCATION, term.getOperatorLocations());

        StringWriter target = new StringWriter();
        try {
            term.writeToTarget(target);
            Assert.assertEquals(image1 + " ? " + image2 + " : " + image3, target.toString());
        } catch (IOException e) {
            handleException(e);
        }

        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        term.getAllBasicBooleanTerms(basicTerms);
        Assert.assertEquals(3, basicTerms.size());
        Assert.assertSame(termList.toArray()[0], basicTerms.toArray()[0]);
        Assert.assertSame(termList.toArray()[1], basicTerms.toArray()[1]);
        Assert.assertSame(termList.toArray()[2], basicTerms.toArray()[2]);
        Assert.assertEquals(termList, basicTerms);

        BooleanTerm bTerm = term.toBooleanTerm(this.builder, this.sourceFile);
        Assert.assertEquals(3, bTerm.getBasicBooleanTerms());
        Assert.assertEquals(0, bTerm.getLocation().getLocations().size());
    }

    public void testInstrOperatorTerm3WrongArity() {
        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();

            String image1 = "v.isEmpty()";
            termList.add(new InstrBasicBooleanTerm(image1, -1, -1));

            String image2 = "a instance of Object";
            termList.add(new InstrBasicBooleanTerm(image2, -1, -1));

            String image3 = "v.contains(a)";
            termList.add(new InstrBasicBooleanTerm(image3, -1, -1));

            new InstrOperatorTerm(getFalseOperator(), termList);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof RuntimeException);
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }

        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();

            String image1 = "v.isEmpty()";
            termList.add(new InstrBasicBooleanTerm(image1, -1, -1));

            String image2 = "a instance of Object";
            termList.add(new InstrBasicBooleanTerm(image2, -1, -1));

            String image3 = "v.contains(a)";
            termList.add(new InstrBasicBooleanTerm(image3, -1, -1));

            new InstrOperatorTerm(getNotOperator(), termList);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof RuntimeException);
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }

        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();

            String image1 = "v.isEmpty()";
            termList.add(new InstrBasicBooleanTerm(image1, -1, -1));

            String image2 = "a instance of Object";
            termList.add(new InstrBasicBooleanTerm(image2, -1, -1));

            String image3 = "v.contains(a)";
            termList.add(new InstrBasicBooleanTerm(image3, -1, -1));

            new InstrOperatorTerm(getExclusiveOrOperator(), termList);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof RuntimeException);
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }

        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();

            termList.add(new InstrBasicBooleanTerm("v.isEmpty()", -1, -1));
            termList.add(new InstrBasicBooleanTerm("a instance of Object", -1, -1));
            termList.add(new InstrBasicBooleanTerm("v.contains(a)", -1, -1));
            termList.add(new InstrBasicBooleanTerm("v.contains(b)", -1, -1));

            new InstrOperatorTerm(getConditionalOperator(), termList);
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof RuntimeException);
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }
    }

    public void testInstrOperatorTerm3Null() {
        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();

            new InstrOperatorTerm(null, termList);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operator == null", e.getMessage());
        }

        try {
            List<InstrBooleanTerm> termList = null;

            new InstrOperatorTerm(null, termList);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operator == null", e.getMessage());
        }

        try {
            List<InstrBooleanTerm> termList = null;

            new InstrOperatorTerm(getConditionalOrOperator(), termList);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operands == null", e.getMessage());
        }
    }

    public void testGetOperantsModify3() {
        List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();

        String image1 = "v.isEmpty()";
        termList.add(new InstrBasicBooleanTerm(image1, -1, -1));

        String image2 = "a instance of Object";
        termList.add(new InstrBasicBooleanTerm(image2, -1, -1));

        String image3 = "v.contains(a)";
        termList.add(new InstrBasicBooleanTerm(image3, -1, -1));

        InstrBooleanOperator op = getConditionalOperator();

        InstrOperatorTerm term = new InstrOperatorTerm(op, termList);
        try {
            term.getOperands().remove(0);
            Assert.fail("modifications not allowed!");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof UnsupportedOperationException);
        }
    }

    public void testInstrOperatorTerm3LocationGoodArity0() {
        List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
    
        InstrBooleanOperator op = getTrueOperator();
        Assert.assertEquals(0, op.getArity());

        InstrOperatorTerm term = new InstrOperatorTerm(op, termList, new int[]{10, 20});
        Assert.assertEquals(0, term.getArity());
        Assert.assertSame(op, term.getOperator());
        Assert.assertEquals(0, term.getOperands().size());
        Assert.assertTrue(term.getOperands().isEmpty());
        Assert.assertEquals("true", term.termToString());
        Assert.assertEquals("true", term.toString());
        Assert.assertEquals(10, term.getOperatorLocations()[0]);
        Assert.assertEquals(20, term.getOperatorLocations()[1]);

        StringWriter target = new StringWriter();
        try {
            term.writeToTarget(target);
            Assert.assertEquals("true", target.toString());
        } catch (IOException e) {
            handleException(e);
        }
    
        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        term.getAllBasicBooleanTerms(basicTerms);
        Assert.assertEquals(0, basicTerms.size());
        Assert.assertEquals(termList, basicTerms);
        
        BooleanTerm bTerm = term.toBooleanTerm(this.builder, this.sourceFile);
        Assert.assertEquals(0, bTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, bTerm.getLocation().getLocations().size());
        Assert.assertEquals(10, bTerm.getLocation().getLocations().get(0).getStartOffset());
        Assert.assertEquals(20, bTerm.getLocation().getLocations().get(0).getEndOffset());
    }

    public void testInstrOperatorTerm3LocationGoodArity1() {
        List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
    
        String image1 = "v.isEmpty()";
        termList.add(new InstrBasicBooleanTerm(image1, -1, -1));
        Assert.assertEquals(image1, termList.toArray()[0].toString());
    
        InstrBooleanOperator op = getNotOperator();
        Assert.assertEquals(1, op.getArity());
    
        InstrOperatorTerm term = new InstrOperatorTerm(op, termList, new int[]{10, 20});
        Assert.assertEquals(1, term.getArity());
        Assert.assertSame(op, term.getOperator());
        Assert.assertEquals(1, term.getOperands().size());
        Assert.assertFalse(term.getOperands().isEmpty());
        Assert.assertSame(termList.toArray()[0], term.getOperands().toArray()[0]);
        Assert.assertEquals("!" + image1, term.termToString());
        Assert.assertEquals("!" + image1, term.toString());
        Assert.assertEquals(10, term.getOperatorLocations()[0]);
        Assert.assertEquals(20, term.getOperatorLocations()[1]);
    
        StringWriter target = new StringWriter();
        try {
            term.writeToTarget(target);
            Assert.assertEquals("!" + image1, target.toString());
        } catch (IOException e) {
            handleException(e);
        }
    
        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        term.getAllBasicBooleanTerms(basicTerms);
        Assert.assertEquals(1, basicTerms.size());
        Assert.assertSame(termList.toArray()[0], basicTerms.toArray()[0]);
        Assert.assertEquals(termList, basicTerms);
        
        BooleanTerm bTerm = term.toBooleanTerm(this.builder, this.sourceFile);
        Assert.assertEquals(1, bTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, bTerm.getLocation().getLocations().size());
        Assert.assertEquals(10, bTerm.getLocation().getLocations().get(0).getStartOffset());
        Assert.assertEquals(20, bTerm.getLocation().getLocations().get(0).getEndOffset());
    }

    public void testInstrOperatorTerm3LocationGoodArity2() {
        List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
    
        String image1 = "v.isEmpty()";
        termList.add(new InstrBasicBooleanTerm(image1, -1, -1));
        Assert.assertEquals(image1, termList.toArray()[0].toString());
    
        String image2 = "a instance of Object";
        termList.add(new InstrBasicBooleanTerm(image2, -1, -1));
        Assert.assertEquals(image2, termList.toArray()[1].toString());
    
        InstrBooleanOperator op = getConditionalAndOperator();
        Assert.assertEquals(2, op.getArity());
    
        InstrOperatorTerm term = new InstrOperatorTerm(op, termList, new int[]{10, 20});
        Assert.assertEquals(2, term.getArity());
        Assert.assertSame(op, term.getOperator());
        Assert.assertEquals(2, term.getOperands().size());
        Assert.assertFalse(term.getOperands().isEmpty());
        Assert.assertSame(termList.toArray()[0], term.getOperands().toArray()[0]);
        Assert.assertSame(termList.toArray()[1], term.getOperands().toArray()[1]);
        Assert.assertEquals(image1 + " && " + image2, term.termToString());
        Assert.assertEquals(image1 + " && " + image2, term.toString());
        Assert.assertEquals(10, term.getOperatorLocations()[0]);
        Assert.assertEquals(20, term.getOperatorLocations()[1]);
    
        StringWriter target = new StringWriter();
        try {
            term.writeToTarget(target);
            Assert.assertEquals(image1 + " && " + image2, target.toString());
        } catch (IOException e) {
            handleException(e);
        }
    
        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        term.getAllBasicBooleanTerms(basicTerms);
        Assert.assertEquals(2, basicTerms.size());
        Assert.assertSame(termList.toArray()[0], basicTerms.toArray()[0]);
        Assert.assertSame(termList.toArray()[1], basicTerms.toArray()[1]);
        Assert.assertEquals(termList, basicTerms);
        
        BooleanTerm bTerm = term.toBooleanTerm(this.builder, this.sourceFile);
        Assert.assertEquals(2, bTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, bTerm.getLocation().getLocations().size());
        Assert.assertEquals(10, bTerm.getLocation().getLocations().get(0).getStartOffset());
        Assert.assertEquals(20, bTerm.getLocation().getLocations().get(0).getEndOffset());
    }

    public void testInstrOperatorTerm3LocationGoodArity3() {
        List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();

        String image1 = "v.isEmpty()";
        termList.add(new InstrBasicBooleanTerm(image1, -1, -1));
        Assert.assertEquals(image1, termList.toArray()[0].toString());

        String image2 = "a instance of Object";
        termList.add(new InstrBasicBooleanTerm(image2, -1, -1));
        Assert.assertEquals(image2, termList.toArray()[1].toString());

        String image3 = "v.contains(a)";
        termList.add(new InstrBasicBooleanTerm(image3, -1, -1));
        Assert.assertEquals(image3, termList.toArray()[2].toString());

        InstrBooleanOperator op = getConditionalOperator();
        Assert.assertEquals(3, op.getArity());

        InstrOperatorTerm term = new InstrOperatorTerm(op, termList, new int[]{10, 20, 21, 31});
        Assert.assertEquals(3, term.getArity());
        Assert.assertSame(op, term.getOperator());
        Assert.assertEquals(3, term.getOperands().size());
        Assert.assertFalse(term.getOperands().isEmpty());
        Assert.assertSame(termList.toArray()[0], term.getOperands().toArray()[0]);
        Assert.assertSame(termList.toArray()[1], term.getOperands().toArray()[1]);
        Assert.assertSame(termList.toArray()[2], term.getOperands().toArray()[2]);
        Assert.assertEquals(image1 + " ? " + image2 + " : " + image3, term.termToString());
        Assert.assertEquals(image1 + " ? " + image2 + " : " + image3, term.toString());
        Assert.assertEquals(10, term.getOperatorLocations()[0]);
        Assert.assertEquals(20, term.getOperatorLocations()[1]);
        Assert.assertEquals(21, term.getOperatorLocations()[2]);
        Assert.assertEquals(31, term.getOperatorLocations()[3]);
    
        StringWriter target = new StringWriter();
        try {
            term.writeToTarget(target);
            Assert.assertEquals(image1 + " ? " + image2 + " : " + image3, target.toString());
        } catch (IOException e) {
            handleException(e);
        }
    
        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        term.getAllBasicBooleanTerms(basicTerms);
        Assert.assertEquals(3, basicTerms.size());
        Assert.assertSame(termList.toArray()[0], basicTerms.toArray()[0]);
        Assert.assertSame(termList.toArray()[1], basicTerms.toArray()[1]);
        Assert.assertSame(termList.toArray()[2], basicTerms.toArray()[2]);
        Assert.assertEquals(termList, basicTerms);

        BooleanTerm bTerm = term.toBooleanTerm(this.builder, this.sourceFile);
        Assert.assertEquals(3, bTerm.getBasicBooleanTerms());
        Assert.assertEquals(2, bTerm.getLocation().getLocations().size());
        Assert.assertEquals(10, bTerm.getLocation().getLocations().get(0).getStartOffset());
        Assert.assertEquals(20, bTerm.getLocation().getLocations().get(0).getEndOffset());
        Assert.assertEquals(21, bTerm.getLocation().getLocations().get(1).getStartOffset());
        Assert.assertEquals(31, bTerm.getLocation().getLocations().get(1).getEndOffset());
    }

    public void testInstrOperatorTerm3LocationWrongArity() {
        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
    
            String image1 = "v.isEmpty()";
            termList.add(new InstrBasicBooleanTerm(image1, -1, -1));
    
            String image2 = "a instance of Object";
            termList.add(new InstrBasicBooleanTerm(image2, -1, -1));
    
            String image3 = "v.contains(a)";
            termList.add(new InstrBasicBooleanTerm(image3, -1, -1));
    
            new InstrOperatorTerm(getFalseOperator(), termList, new int[]{10, 20});
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }
    
        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
    
            String image1 = "v.isEmpty()";
            termList.add(new InstrBasicBooleanTerm(image1, -1, -1));
    
            String image2 = "a instance of Object";
            termList.add(new InstrBasicBooleanTerm(image2, -1, -1));
    
            String image3 = "v.contains(a)";
            termList.add(new InstrBasicBooleanTerm(image3, -1, -1));
    
            new InstrOperatorTerm(getNotOperator(), termList, new int[]{10, 20});
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }

        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
    
            String image1 = "v.isEmpty()";
            termList.add(new InstrBasicBooleanTerm(image1, -1, -1));
    
            String image2 = "a instance of Object";
            termList.add(new InstrBasicBooleanTerm(image2, -1, -1));
    
            String image3 = "v.contains(a)";
            termList.add(new InstrBasicBooleanTerm(image3, -1, -1));

            new InstrOperatorTerm(getExclusiveOrOperator(), termList, new int[]{10, 20});
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }
    
        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
    
            termList.add(new InstrBasicBooleanTerm("v.isEmpty()", -1, -1));
            termList.add(new InstrBasicBooleanTerm("a instance of Object", -1, -1));
            termList.add(new InstrBasicBooleanTerm("v.contains(a)", -1, -1));
            termList.add(new InstrBasicBooleanTerm("v.contains(b)", -1, -1));

            new InstrOperatorTerm(getConditionalOperator(), termList, new int[]{10, 20});
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operands.size() != this.operator.getArity()", e.getMessage());
        }

        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
            
            termList.add(new InstrBasicBooleanTerm("v.isEmpty()", -1, -1));
            termList.add(new InstrBasicBooleanTerm("a instance of Object", -1, -1));

            new InstrOperatorTerm(getExclusiveOrOperator(), termList, new int[]{10});
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operandStartEndLocation.length must be even", e.getMessage());
        }
        
        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
            
            termList.add(new InstrBasicBooleanTerm("v.isEmpty()", -1, -1));
            termList.add(new InstrBasicBooleanTerm("a instance of Object", -1, -1));
            
            new InstrOperatorTerm(getExclusiveOrOperator(), termList, new int[]{10, 20, 30});
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("this.operandStartEndLocation.length must be even", e.getMessage());
        }

        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
            
            new InstrOperatorTerm(getTrueOperator(), termList, new int[]{10, 20, 30, 40});
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("wrong number of start ends for the operator(s)", e.getMessage());
        }
        
        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
            termList.add(new InstrBasicBooleanTerm("a instance of Object", -1, -1));
            
            new InstrOperatorTerm(getNotOperator(), termList, new int[]{10, 20, 30, 40});
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("wrong number of start ends for the operator(s)", e.getMessage());
        }

        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
            termList.add(new InstrBasicBooleanTerm("v.isEmpty()", -1, -1));
            termList.add(new InstrBasicBooleanTerm("a instance of Object", -1, -1));
            
            new InstrOperatorTerm(getExclusiveOrOperator(), termList, new int[]{10, 20, 30, 40});
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("wrong number of start ends for the operator(s)", e.getMessage());
        }

        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
            termList.add(new InstrBasicBooleanTerm("v.isEmpty()", -1, -1));
            termList.add(new InstrBasicBooleanTerm("a instance of Object", -1, -1));
            termList.add(new InstrBasicBooleanTerm("v.contains(a)", -1, -1));
            
            new InstrOperatorTerm(getConditionalOperator(), termList, new int[]{10, 20});
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("wrong number of start ends for the operator(s)", e.getMessage());
        }
        
        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
            termList.add(new InstrBasicBooleanTerm("v.isEmpty()", -1, -1));
            termList.add(new InstrBasicBooleanTerm("a instance of Object", -1, -1));
            termList.add(new InstrBasicBooleanTerm("v.contains(a)", -1, -1));
            
            new InstrOperatorTerm(getConditionalOperator(), termList, new int[]{10, 20, 30, 40, 50, 60});
            Assert.fail("RuntimeException expected");
        } catch (RuntimeException e) {
            Assert.assertEquals("wrong number of start ends for the operator(s)", e.getMessage());
        }
    }

    public void testInstrOperatorTerm3LocationNull() {
        try {
            List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
    
            new InstrOperatorTerm(null, termList, new int[]{10, 20});
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operator == null", e.getMessage());
        }
    
        try {
            List<InstrBooleanTerm> termList = null;
    
            new InstrOperatorTerm(null, termList, new int[]{10, 20});
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operator == null", e.getMessage());
        }

        try {
            List<InstrBooleanTerm> termList = null;
    
            new InstrOperatorTerm(getConditionalOrOperator(), termList, new int[]{10, 20});
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operands == null", e.getMessage());
        }

        try {
            new InstrOperatorTerm(getTrueOperator(),
                    Collections.<InstrBooleanTerm>emptyList(), null);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("operatorLocations == null", e.getMessage());
        }
    }

    public void testGetOperantsModify3Location() {
        List<InstrBooleanTerm> termList = new LinkedList<InstrBooleanTerm>();
    
        String image1 = "v.isEmpty()";
        termList.add(new InstrBasicBooleanTerm(image1, -1, -1));
    
        String image2 = "a instance of Object";
        termList.add(new InstrBasicBooleanTerm(image2, -1, -1));
    
        String image3 = "v.contains(a)";
        termList.add(new InstrBasicBooleanTerm(image3, -1, -1));
    
        InstrBooleanOperator op = getConditionalOperator();
    
        InstrOperatorTerm term = new InstrOperatorTerm(op, termList);
    
        try {
            term.getOperands().remove(0);
            Assert.fail("modifications not allowed!");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof UnsupportedOperationException);
        }
    }

    /**
     * tests {@link InstrOperatorTerm#resetOperandAndOperators(InstrBooleanOperator, List, int[])}
     *
     */
    public void testResetOperandAndOperators () {
        InstrBasicBooleanTerm leftTerm = new InstrBasicBooleanTerm("A", -1, -1);
        InstrBasicBooleanTerm rightTerm = new InstrBasicBooleanTerm("B", -1, -1);
        InstrOperatorTerm term = new InstrOperatorTerm(leftTerm,
                JavaBooleanOperators.getAndOperator(), rightTerm);

        Assert.assertEquals(2, term.getArity());
        Assert.assertSame(JavaBooleanOperators.getAndOperator(), term.getOperator());
        Assert.assertEquals(2, term.getOperands().size());
        Assert.assertFalse(term.getOperands().isEmpty());
        Assert.assertSame(leftTerm, term.getOperands().toArray()[0]);
        Assert.assertSame(rightTerm, term.getOperands().toArray()[1]);
        Assert.assertEquals("A & B", term.termToString());

        InstrBasicBooleanTerm newTerm = new InstrBasicBooleanTerm("C", -1, -1);
        List<InstrBooleanTerm> newInnerTerms = Collections.<InstrBooleanTerm>singletonList(newTerm);

        term.resetOperandAndOperators(JavaBooleanOperators.getNotOperator(),
                newInnerTerms, InstrOperatorTerm.EMPTY_START_END_LOCATION);
        Assert.assertEquals(1, term.getArity());
        Assert.assertSame(JavaBooleanOperators.getNotOperator(), term.getOperator());
        Assert.assertEquals(1, term.getOperands().size());
        Assert.assertFalse(term.getOperands().isEmpty());
        Assert.assertSame(newTerm, term.getOperands().toArray()[0]);
        Assert.assertEquals("!C", term.termToString());

        try {
            term.getOperands().remove(0);
            Assert.fail("modifications not allowed!");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof UnsupportedOperationException);
        }
    }
}
