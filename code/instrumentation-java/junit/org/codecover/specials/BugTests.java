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

package org.codecover.specials;

import static org.codecover.UtilsForTestingJava.TEST_SOURCE;
import static org.codecover.UtilsForTestingJava.TEST_TARGET;
import static org.codecover.UtilsForTestingJava.instrumentAndCompile;

import java.io.File;
import java.util.List;
import java.util.Set;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJava;
import org.codecover.instrumentation.exceptions.ParseException;
import org.codecover.instrumentation.java15.parser.JavaParser;
import org.codecover.model.mast.BasicBooleanTerm;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.ConditionalStatement;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.OperatorTerm;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.Statement;
import org.codecover.model.mast.StatementSequence;
import org.codecover.model.utils.criteria.Criterion;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.file.FileTool;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: AssignmentInstrumentationTest.java 2317 2007-11-04
 *          16:09:15Z muellecr $)
 * 
 */
public class BugTests extends TestCase {

    /**
     * For https://stupro.selfhost.eu/BugzillaStuPro/show_bug.cgi?id=221.
     * "Assignments"
     */
    public void testBug221() throws Exception {
        instrumentAndCompile("bugs", "Bug221_Assignment");
    }

    /**
     * For https://stupro.selfhost.eu/BugzillaStuPro/show_bug.cgi?id=229.
     * 
     * InstrumenterException when instrumenting conditional expressions with
     * casts.
     * 
     * class x {
     *  public void a(Object o) {
     *          if ((Boolean) aValue) { }
     *      }
     * }
     */
    public void testBug229() throws Exception {
        HierarchyLevel clazz = instrumentAndCompile("bugs", "Bug229_Cast");
        Assert.assertEquals(1, clazz.getChildren().size());

        HierarchyLevel mainMethod = clazz.getChildren().get(0);
        Assert.assertEquals("foo", mainMethod.getName());
        Assert.assertEquals("method", mainMethod.getType().getInternalName());
        Assert.assertEquals("method", mainMethod.getType().getEnglishName());
        Assert.assertTrue(mainMethod.getChildren().isEmpty());
        List<StatementSequence> statementSeq = mainMethod.getSequences();
        Assert.assertEquals(1, statementSeq.size());
        StatementSequence statementSeq0 = statementSeq.get(0);
        Assert.assertEquals(1, statementSeq0.getStatements().size());

        Statement statement = statementSeq0.getStatements().get(0);
        Assert.assertTrue(statement instanceof ConditionalStatement);
        Set<RootTerm> rTerms = statement.getTerms();
        Assert.assertEquals(1, rTerms.size());
        RootTerm rTerm = rTerms.iterator().next();
        BooleanTerm bTerm = rTerm.getTerm();
        Assert.assertEquals(1, bTerm.getBasicBooleanTerms());

        final int[] visiCount = {0};

        bTerm.accept(new BooleanTerm.DefaultVisitor() {
                    @Override
                    public void visit(BasicBooleanTerm term) {
                        visiCount[0]++;
                        Assert.assertEquals("(Boolean) o",
                                term.getLocation().getLocations().get(0).getContent());
                    }

                    @Override
                    public void visit(OperatorTerm term) {
                        visiCount[0]++;
                    }
                },
                     new BooleanTerm.DefaultVisitor());

        Assert.assertEquals(1, visiCount[0]);
    }

    public void testBug233() throws Exception {
        instrumentAndCompile("bugs", "Bug233_DefiniteAssigment");
    }

    public void testLoopInThen() throws Exception {
        instrumentAndCompile("bugs", "LoopInThen",
                new Criterion[]{LoopCoverage.getInstance()});
        String returnValue = UtilsForTestingJava.runJava("org.codecover.instrumentation.java15.test.bugs.LoopInThen");
        Assert.assertNotNull(returnValue);
        Assert.assertEquals("RETURN", returnValue);
    }

    public void testBug231() throws Exception {
        new JavaParser("this();").ExplicitConstructorInvocation();
        new JavaParser("<String>this();").ExplicitConstructorInvocation();
        new JavaParser("super();").ExplicitConstructorInvocation();
        new JavaParser("(new Outer()).super();").ExplicitConstructorInvocation();
        new JavaParser("new T8851q10().super();").ExplicitConstructorInvocation();
        new JavaParser("new T8851q2() {\n}.super();").ExplicitConstructorInvocation();
        new JavaParser("Middle.super.m.super();").ExplicitConstructorInvocation();
        new JavaParser("Middle.super.m.this.super.this.<String, Object>super();").ExplicitConstructorInvocation();
        new JavaParser("new Object(a = 6, vector.toString()).((List) null).((List) new Jungle()).<org.Vector, List>super();").ExplicitConstructorInvocation();

        FileTool.copy(new File(TEST_SOURCE + "bugs/Bug231_ParseErrors.java"),
                      new File(TEST_TARGET + "bugs/Bug231_ParseErrors.java"));
        UtilsForTestingJava.runJavac(new File(TEST_TARGET + "bugs/Bug231_ParseErrors.java"));
        instrumentAndCompile("bugs", "Bug231_ParseErrors");
    }

    public void testBug232() throws Exception {
        new JavaParser("(int.class)").PostfixExpression();
        new JavaParser("(int.class)").PrimaryPrefix();
        new JavaParser("int.class").Expression();
        try {
            new JavaParser("(int.class)").CastExpression();
            Assert.fail("ParseException expected");
        } catch (ParseException e) { /* expected */ }
        try {
            new JavaParser("(int.class)").CastLookahead();
            Assert.fail("ParseException expected");
        } catch (ParseException e) { /* expected */ }
        new JavaParser("(int[]) a").Expression();
        new JavaParser("(int.class)").Expression();
        new JavaParser("i = (int) ++l;").Statement();

        instrumentAndCompile("bugs", "Bug232_ParseError");
    }

    public void testBug234_1() throws Exception {
        instrumentAndCompile("bugs", "Bug234_Ignore001a_1");
    }

    public void testBug234_2() throws Exception {
        instrumentAndCompile("bugs", "Bug234_Ignore001a_2");
    }
    
    public void testBug253() throws Exception {
        instrumentAndCompile("bugs", "Bug253_TwoExtends");
    }

    public void testBug256() throws Exception {
        try {
            new JavaParser("( len ) +1").CastExpression();
            Assert.fail("ParseException expected");
        } catch (ParseException e) { /* expected */ }
        try {
            new JavaParser("( len ) +1").CastLookahead();
            Assert.fail("ParseException expected");
        } catch (ParseException e) { /* expected */ }
        new JavaParser("!var").UnaryExpressionNotPlusMinus();
        new JavaParser("~var").UnaryExpressionNotPlusMinus();
        new JavaParser("(int) +1").CastExpression();

        new JavaParser("( len ) +1").PostfixExpression();

        new JavaParser("( len ) +1").UnaryExpressionNotPlusMinus();
        new JavaParser("( len ) +1").Expression();
        new JavaParser("((new int[4]).length) +1").Expression();
        instrumentAndCompile("bugs", "Bug256_Base64");
    }
}