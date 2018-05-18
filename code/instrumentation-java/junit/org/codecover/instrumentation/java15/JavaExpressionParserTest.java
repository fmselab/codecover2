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

import static org.codecover.UtilsForTestingJava.handleException;

import java.io.IOException;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.List;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJava;
import org.codecover.instrumentation.booleanterms.InstrBasicBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrOperatorTerm;
import org.codecover.instrumentation.java15.parser.JavaParser;
import org.codecover.instrumentation.java15.parser.ParseException;
import org.codecover.instrumentation.java15.syntaxtree.Expression;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.SourceFile;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: JavaExpressionParserTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class JavaExpressionParserTest extends TestCase {
    JavaParser javaParser = new JavaParser("");

    JavaExpressionParser javaExpressionParser = new JavaExpressionParser();

    private MASTBuilder builder;

    @Override
    protected void setUp() throws Exception {
        this.builder = UtilsForTestingJava.newMASTBuilder();
    }

    @Override
    protected void tearDown() throws Exception {
        this.builder = null;
    }

    public void testTerm001() {
        InstrBooleanTerm term = parseExpression("p instanceof String");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm002a() {
        InstrBooleanTerm term = parseExpression("a = b");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 2);
        Assert.assertEquals(opTerm.getOperator().getName(), "assignment (=)");

        List<InstrBooleanTerm> operands = opTerm.getOperands();
        Assert.assertEquals(operands.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operands.iterator();

        InstrBooleanTerm firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "a");

        InstrBooleanTerm secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(secondTerm.termToString(), "b");
    }

    public void testTerm002b() {
        InstrBooleanTerm term = parseExpression("a |= b");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 2);
        Assert.assertEquals(opTerm.getOperator().getName(),
                "assignment or (|=)");

        List<InstrBooleanTerm> operands = opTerm.getOperands();
        Assert.assertEquals(operands.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operands.iterator();

        InstrBooleanTerm firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "a");

        InstrBooleanTerm secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(secondTerm.termToString(), "b");
    }

    public void testTerm002c() {
        InstrBooleanTerm term = parseExpression("a &= b");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 2);
        Assert.assertEquals(opTerm.getOperator().getName(),
                "assignment and (&=)");

        List<InstrBooleanTerm> operands = opTerm.getOperands();
        Assert.assertEquals(operands.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operands.iterator();

        InstrBooleanTerm firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "a");

        InstrBooleanTerm secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(secondTerm.termToString(), "b");
    }

    public void testTerm002d() {
        InstrBooleanTerm term = parseExpression("a ^= b");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 2);
        Assert.assertEquals(opTerm.getOperator().getName(),
                "assignment xor (^=)");

        List<InstrBooleanTerm> operands = opTerm.getOperands();
        Assert.assertEquals(operands.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operands.iterator();

        InstrBooleanTerm firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "a");

        InstrBooleanTerm secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(secondTerm.termToString(), "b");
    }

    public void testTerm003() {
        InstrBooleanTerm term = parseExpression("a ? b : c");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 3);
        Assert.assertEquals(opTerm.getOperator().getName(),
                "conditional operator ( ? : )");

        List<InstrBooleanTerm> operands = opTerm.getOperands();
        Assert.assertEquals(operands.size(), 3);
        Iterator<InstrBooleanTerm> iterator = operands.iterator();

        InstrBooleanTerm firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "a");

        InstrBooleanTerm secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(secondTerm.termToString(), "b");

        InstrBooleanTerm thirdTerm = iterator.next();
        Assert.assertTrue(thirdTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(thirdTerm.termToString(), "c");
    }

    public void testTerm004a() {
        InstrBooleanTerm term = parseExpression("i == j");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm004b() {
        InstrBooleanTerm term = parseExpression("i != j");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm004c() {
        InstrBooleanTerm term = parseExpression("(i = j) == k");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm004d() {
        InstrBooleanTerm term = parseExpression("(i = j) != k");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm004e() {
        InstrBooleanTerm term = parseExpression("i = j == k");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 2);
        Assert.assertEquals(opTerm.getOperator().getName(), "assignment (=)");

        List<InstrBooleanTerm> operands = opTerm.getOperands();
        Assert.assertEquals(operands.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operands.iterator();

        InstrBooleanTerm firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "i");

        InstrBooleanTerm secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(secondTerm.termToString(), "j == k");
    }

    public void testTerm004f() {
        InstrBooleanTerm term = parseExpression("i = j != k");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 2);
        Assert.assertEquals(opTerm.getOperator().getName(), "assignment (=)");

        List<InstrBooleanTerm> operands = opTerm.getOperands();
        Assert.assertEquals(operands.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operands.iterator();

        InstrBooleanTerm firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "i");

        InstrBooleanTerm secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(secondTerm.termToString(), "j != k");
    }

    public void testTerm005a() {
        InstrBooleanTerm term = parseExpression("i > j");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm005b() {
        InstrBooleanTerm term = parseExpression("i >= j");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm005c() {
        InstrBooleanTerm term = parseExpression("i <= j");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm005d() {
        InstrBooleanTerm term = parseExpression("i < j");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm006a() {
        InstrBooleanTerm term = parseExpression("i << j < 0");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm006b() {
        InstrBooleanTerm term = parseExpression("i >> j < 0");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm006c() {
        InstrBooleanTerm term = parseExpression("i >>> j < 0");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm007a() {
        InstrBooleanTerm term = parseExpression("i + j < 0");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm007b() {
        InstrBooleanTerm term = parseExpression("i - j < 0");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm007c() {
        InstrBooleanTerm term = parseExpression("i * j < 0");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm007d() {
        InstrBooleanTerm term = parseExpression("i / j < 0");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm007e() {
        InstrBooleanTerm term = parseExpression("i % j < 0");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm008a() {
        InstrBooleanTerm term = parseExpression("i++ < 0");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm008b() {
        InstrBooleanTerm term = parseExpression("i-- < 0");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm008c() {
        InstrBooleanTerm term = parseExpression("++i < 0");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm008d() {
        InstrBooleanTerm term = parseExpression("--i < 0");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm009a() {
        InstrBooleanTerm term = parseExpression("this.d");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm009b() {
        InstrBooleanTerm term = parseExpression("this.equals(p)");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm009c() {
        InstrBooleanTerm term = parseExpression("super.equals(p)");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm009d() {
        InstrBooleanTerm term = parseExpression("new Vector<String>().remove(0).substring(0).getBytes().length > 0");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm009e() {
        InstrBooleanTerm term = parseExpression("ExpressionTestClass.class.getCanonicalName().contains(\"a < b\")");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm010() {
        InstrBooleanTerm term = parseExpression("aBoolean");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm101a() {
        InstrBooleanTerm term = parseExpression("true");

        Assert.assertTrue(term instanceof InstrOperatorTerm);

        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;
        Assert.assertTrue(opTerm.getOperands().isEmpty());
        Assert.assertEquals(opTerm.getOperator().getArity(), 0);
        Assert.assertEquals(opTerm.getOperator().getName(), "true");
    }

    public void testTerm102a() {
        InstrBooleanTerm term = parseExpression("false");

        Assert.assertTrue(term instanceof InstrOperatorTerm);

        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;
        Assert.assertTrue(opTerm.getOperands().isEmpty());
        Assert.assertEquals(opTerm.getOperator().getArity(), 0);
        Assert.assertEquals(opTerm.getOperator().getName(), "false");
    }

    public void testTerm201a() {
        InstrBooleanTerm term = parseExpression("a | b");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 2);
        Assert.assertEquals(opTerm.getOperator().getName(), "or (|)");

        List<InstrBooleanTerm> operands = opTerm.getOperands();
        Assert.assertEquals(operands.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operands.iterator();

        InstrBooleanTerm firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "a");

        InstrBooleanTerm secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(secondTerm.termToString(), "b");
    }

    public void testTerm202a() {
        InstrBooleanTerm term = parseExpression("a || b");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 2);
        Assert.assertEquals(opTerm.getOperator().getName(), "conditional or (||)");

        List<InstrBooleanTerm> operands = opTerm.getOperands();
        Assert.assertEquals(operands.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operands.iterator();

        InstrBooleanTerm firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "a");

        InstrBooleanTerm secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(secondTerm.termToString(), "b");
    }

    public void testTerm202b() {
        InstrBooleanTerm term = parseExpression("a || b || c");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 2);
        Assert.assertEquals(opTerm.getOperator().getName(), "conditional or (||)");

        List<InstrBooleanTerm> operands = opTerm.getOperands();
        Assert.assertEquals(operands.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operands.iterator();

        InstrBooleanTerm firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrOperatorTerm);
        Assert.assertEquals(firstTerm.termToString(), "a || b");

        InstrBooleanTerm secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(secondTerm.termToString(), "c");

        InstrOperatorTerm firstTermOp = (InstrOperatorTerm) firstTerm;
        Assert.assertEquals(firstTermOp.getOperator().getArity(), 2);
        Assert.assertEquals(firstTermOp.getOperator().getName(), "conditional or (||)");

        operands = firstTermOp.getOperands();
        Assert.assertEquals(operands.size(), 2);
        iterator = operands.iterator();

        firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "a");

        secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(secondTerm.termToString(), "b");
    }

    public void testTerm203a() {
        InstrBooleanTerm term = parseExpression("a & b");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 2);
        Assert.assertEquals(opTerm.getOperator().getName(), "and (&)");

        List<InstrBooleanTerm> operands = opTerm.getOperands();
        Assert.assertEquals(operands.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operands.iterator();

        InstrBooleanTerm firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "a");

        InstrBooleanTerm secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(secondTerm.termToString(), "b");
    }

    public void testTerm204a() {
        InstrBooleanTerm term = parseExpression("a && b");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 2);
        Assert.assertEquals(opTerm.getOperator().getName(), "conditional and (&&)");

        List<InstrBooleanTerm> operands = opTerm.getOperands();
        Assert.assertEquals(operands.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operands.iterator();

        InstrBooleanTerm firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "a");

        InstrBooleanTerm secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(secondTerm.termToString(), "b");
    }

    public void testTerm205a() {
        InstrBooleanTerm term = parseExpression("a ^ b");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 2);
        Assert.assertEquals(opTerm.getOperator().getName(), "exclusive or (^)");

        List<InstrBooleanTerm> operands = opTerm.getOperands();
        Assert.assertEquals(operands.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operands.iterator();

        InstrBooleanTerm firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "a");

        InstrBooleanTerm secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(secondTerm.termToString(), "b");
    }

    public void testTerm206b() {
        InstrBooleanTerm term = parseExpression("a || b & c");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 2);
        Assert.assertEquals(opTerm.getOperator().getName(), "conditional or (||)");

        List<InstrBooleanTerm> operands = opTerm.getOperands();
        Assert.assertEquals(operands.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operands.iterator();

        InstrBooleanTerm firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "a");

        InstrBooleanTerm secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrOperatorTerm);
        Assert.assertEquals(secondTerm.termToString(), "b & c");

        InstrOperatorTerm secondTermOp = (InstrOperatorTerm) secondTerm;
        Assert.assertEquals(secondTermOp.getOperator().getArity(), 2);
        Assert.assertEquals(secondTermOp.getOperator().getName(), "and (&)");

        operands = secondTermOp.getOperands();
        Assert.assertEquals(operands.size(), 2);
        iterator = operands.iterator();

        firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "b");

        secondTerm = iterator.next();
        Assert.assertTrue(secondTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(secondTerm.termToString(), "c");
    }

    public void testTerm300a() {
        InstrBooleanTerm term = parseExpression("!a");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 1);
        Assert.assertEquals(opTerm.getOperator().getName(), "not (!)");

        List<InstrBooleanTerm> operands = opTerm.getOperands();
        Assert.assertEquals(operands.size(), 1);
        Iterator<InstrBooleanTerm> iterator = operands.iterator();

        InstrBooleanTerm firstTerm = iterator.next();
        Assert.assertTrue(firstTerm instanceof InstrBasicBooleanTerm);
        Assert.assertEquals(firstTerm.termToString(), "a");
    }

    private InstrBooleanTerm parseExpression(String expressionString) {
        this.javaParser.ReInit(expressionString);
        try {
            Expression expression = this.javaParser.Expression();
            Assert.assertNotNull(expression);
            InstrBooleanTerm term = this.javaExpressionParser.parse(expression);
            Assert.assertNotNull(term);
            Assert.assertEquals(expressionString, term.termToString());

            StringWriter stringWriter = new StringWriter();
            term.writeToTarget(stringWriter);
            Assert.assertEquals(expressionString, stringWriter.toString());

            SourceFile sourceFile = this.builder.createSourceFile("testFile", expressionString);
            BooleanTerm booleanTerm = term.toBooleanTerm(this.builder, sourceFile);

            if (term instanceof InstrOperatorTerm) {
                InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

                int expectedOperatorLocationSize;
                if (opTerm.getArity() <= 1) {
                    expectedOperatorLocationSize = 1;
                } else {
                    expectedOperatorLocationSize = opTerm.getArity() - 1;
                }
                Assert.assertEquals(expectedOperatorLocationSize, booleanTerm.getLocation().getLocations().size());
            } else {
                Assert.assertEquals(term.toString(), booleanTerm.getLocation().getLocations().get(0).getContent());
            }

            return term;
        } catch (IOException e) {
            handleException(e);
        } catch (ParseException e) {
            handleException(e);
        }

        return null;
    }
}
