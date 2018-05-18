///////////////////////////////////////////////////////////////////////////////
//
// $Id: JavaExpressionParserTest.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 10.04.2007 22:04:31
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.booleanterms;

import java.io.Reader;
import java.io.StringReader;
import java.util.Iterator;
import java.util.Queue;

import org.gbt2.instrumentation.java15.booleanterms.InstrBasicBooleanTerm;
import org.gbt2.instrumentation.java15.booleanterms.InstrBooleanTerm;
import org.gbt2.instrumentation.java15.booleanterms.InstrExpressionParser;
import org.gbt2.instrumentation.java15.booleanterms.InstrOperatorTerm;
import org.gbt2.instrumentation.java15.parser.JavaParser;
import org.gbt2.instrumentation.java15.parser.ParseException;
import org.gbt2.instrumentation.java15.syntaxtree.Expression;

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * @author Christoph Müller
 */
public class JavaExpressionParserTest extends TestCase {
    JavaParser javaParser = new JavaParser(new StringReader(""));

    InstrExpressionParser instrExpressionParser = new InstrExpressionParser();

    public void testTerm001() {
        InstrBooleanTerm term = parseExpression("p instanceof String");

        Assert.assertTrue(term instanceof InstrBasicBooleanTerm);
    }

    public void testTerm002a() {
        InstrBooleanTerm term = parseExpression("a = b");

        Assert.assertTrue(term instanceof InstrOperatorTerm);
        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;

        Assert.assertEquals(opTerm.getOperator().getArity(), 2);
        Assert.assertEquals(opTerm.getOperator().getOperantFormatter(),
                "%s = %s");
        Assert.assertEquals(opTerm.getOperator().getName(), "assignment (=)");

        Queue<InstrBooleanTerm> operators = opTerm.getOperants();
        Assert.assertEquals(operators.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operators.iterator();

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
        Assert.assertEquals(opTerm.getOperator().getOperantFormatter(),
                "%s |= %s");
        Assert.assertEquals(opTerm.getOperator().getName(),
                "assignment or (|=)");

        Queue<InstrBooleanTerm> operators = opTerm.getOperants();
        Assert.assertEquals(operators.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operators.iterator();

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
        Assert.assertEquals(opTerm.getOperator().getOperantFormatter(),
                "%s &= %s");
        Assert.assertEquals(opTerm.getOperator().getName(),
                "assignment and (&=)");

        Queue<InstrBooleanTerm> operators = opTerm.getOperants();
        Assert.assertEquals(operators.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operators.iterator();

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
        Assert.assertEquals(opTerm.getOperator().getOperantFormatter(),
                "%s ^= %s");
        Assert.assertEquals(opTerm.getOperator().getName(),
                "assignment xor (^=)");

        Queue<InstrBooleanTerm> operators = opTerm.getOperants();
        Assert.assertEquals(operators.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operators.iterator();

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
        Assert.assertEquals(opTerm.getOperator().getOperantFormatter(),
                "%s ? %s : %s");
        Assert.assertEquals(opTerm.getOperator().getName(),
                "conditional operator ( ? : )");

        Queue<InstrBooleanTerm> operators = opTerm.getOperants();
        Assert.assertEquals(operators.size(), 3);
        Iterator<InstrBooleanTerm> iterator = operators.iterator();

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
        Assert.assertEquals(opTerm.getOperator().getOperantFormatter(),
                "%s = %s");
        Assert.assertEquals(opTerm.getOperator().getName(), "assignment (=)");

        Queue<InstrBooleanTerm> operators = opTerm.getOperants();
        Assert.assertEquals(operators.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operators.iterator();

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
        Assert.assertEquals(opTerm.getOperator().getOperantFormatter(),
                "%s = %s");
        Assert.assertEquals(opTerm.getOperator().getName(), "assignment (=)");

        Queue<InstrBooleanTerm> operators = opTerm.getOperants();
        Assert.assertEquals(operators.size(), 2);
        Iterator<InstrBooleanTerm> iterator = operators.iterator();

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

    public void testTerm100a() {
        InstrBooleanTerm term = parseExpression("true");

        Assert.assertTrue(term instanceof InstrOperatorTerm);

        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;
        Assert.assertTrue(opTerm.getOperants().isEmpty());
        Assert.assertEquals(opTerm.getOperator().getArity(), 0);
        Assert.assertEquals(opTerm.getOperator().getName(), "true");
    }

    public void testTerm100b() {
        InstrBooleanTerm term = parseExpression("false");

        Assert.assertTrue(term instanceof InstrOperatorTerm);

        InstrOperatorTerm opTerm = (InstrOperatorTerm) term;
        Assert.assertTrue(opTerm.getOperants().isEmpty());
        Assert.assertEquals(opTerm.getOperator().getArity(), 0);
        Assert.assertEquals(opTerm.getOperator().getName(), "false");
    }

    private InstrBooleanTerm parseExpression(String expressionString) {
        Reader reader = new StringReader(expressionString);

        this.javaParser.ReInit(reader);
        try {
            Expression expression = this.javaParser.Expression();
            Assert.assertNotNull(expression);
            InstrBooleanTerm term = this.instrExpressionParser
                    .parse(expression);
            Assert.assertNotNull(term);
            Assert.assertEquals(term.termToString(), expressionString);
            return term;

        } catch (ParseException e) {
            Assert.fail(e.getMessage());
        }

        return null;
    }
}
