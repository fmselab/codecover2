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

import static org.codecover.UtilsForTestingJava.SESSION_CONTAINER;
import static org.codecover.UtilsForTestingJava.SOURCE;
import static org.codecover.UtilsForTestingJava.TARGET;
import static org.codecover.UtilsForTestingJava.TEST_SOURCE;
import static org.codecover.UtilsForTestingJava.TEST_TARGET;
import static org.codecover.UtilsForTestingJava.handleException;
import static org.codecover.UtilsForTestingJava.isCompileableJava;
import static org.codecover.UtilsForTestingJava.locationAssertion;
import static org.codecover.UtilsForTestingJava.runJava;
import static org.codecover.UtilsForTestingJava.runJavac;
import static org.codecover.UtilsForTestingJava.simpleTestSessionContainerTests;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.CONDITIONAL_AND;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.CONDITIONAL_OR;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.NOT;
import static org.codecover.instrumentation.measurement.CoverageResultLogReader.getBooleanAssignmentFromString;

import java.io.File;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.Map;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJava;
import org.codecover.instrumentation.Instrumenter;
import org.codecover.instrumentation.exceptions.InstrumentationException;
import org.codecover.instrumentation.java15.InstrumenterDescriptor;
import org.codecover.instrumentation.measurement.CoverageResultLogReader;
import org.codecover.instrumentation.measurement.MeasurementConstants;
import org.codecover.instrumentation.measurement.parser.CoverageLogParser;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.BasicBooleanTerm;
import org.codecover.model.mast.BasicStatement;
import org.codecover.model.mast.BooleanAssignmentMap;
import org.codecover.model.mast.BooleanOperator;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.Branch;
import org.codecover.model.mast.ConditionalStatement;
import org.codecover.model.mast.CoverableItem;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.LoopingStatement;
import org.codecover.model.mast.OperatorTerm;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.Statement;
import org.codecover.model.mast.BooleanTerm.DefaultVisitor;
import org.codecover.model.mast.BooleanTerm.Visitor;
import org.codecover.model.utils.file.FileTool;
import org.codecover.model.utils.file.SourceTargetContainer;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: MastAndCompileTest.java 18 2008-05-24 22:07:13Z ahija $)
 * 
 */
public class MastAndCompileTest extends TestCase {

    private static final String TEST_SESSION_NAME = "JavaAllInstrumentationTest Run";
    private static final String CLASS_UNDER_TEST = "org.codecover.instrumentation.java15.test.test3.CodeExample";
    
    private Instrumenter instrumenter;

    private MASTBuilder builder;

    private RootTerm term78;

    /**
     * @throws java.lang.Exception
     */
    @Override
    protected void setUp() throws Exception {
        UtilsForTestingJava.clearTarget();
        UtilsForTestingJava.copyMeasurementHelpersToBin();

        this.instrumenter = UtilsForTestingJava.getDefaultJavaInstrumenter();
        this.builder = UtilsForTestingJava.newMASTBuilder();
    }

    @Override
    protected void tearDown() {
        this.instrumenter = null;
    }

    public void testCodeExampleMAST() throws Exception {
        String srcPath = TEST_SOURCE + "test3/CodeExample.java";
        File source = new File(srcPath);
        String targetPath = TEST_TARGET + "test3/CodeExample.java";
        File target = new File(targetPath);
        Date dateBefore = new Date();
        TestSessionContainer testSessionContainer = null;
        Collection<SourceTargetContainer> container = Collections.<SourceTargetContainer>singleton(
                new SourceTargetContainer(source, target));

        try {
          testSessionContainer = this.instrumenter.instrument(new File(SOURCE),
                  new File(TARGET),
                  container,
                  this.builder,
                  new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }

        Assert.assertTrue(target.exists());

        simpleTestSessionContainerTests(dateBefore, testSessionContainer, this.instrumenter);
        HierarchyLevel h1 = testSessionContainer.getCode();
        Assert.assertEquals("default package", h1.getName());
        Assert.assertEquals("default package", h1.getType().getInternalName());
        Assert.assertEquals("default package", h1.getType().getEnglishName());
        Assert.assertTrue(h1.getSequences().isEmpty());
        Assert.assertEquals(1, h1.getChildren().size());

        HierarchyLevel h2 = h1.getChildren().get(0);
        Assert.assertEquals("org", h2.getName());
        Assert.assertEquals("package", h2.getType().getInternalName());
        Assert.assertEquals("package", h2.getType().getEnglishName());
        Assert.assertTrue(h2.getSequences().isEmpty());
        Assert.assertEquals(1, h2.getChildren().size());

        HierarchyLevel h3 = h2.getChildren().get(0);
        Assert.assertEquals("codecover", h3.getName());
        Assert.assertEquals("package", h3.getType().getInternalName());
        Assert.assertEquals("package", h3.getType().getEnglishName());
        Assert.assertTrue(h3.getSequences().isEmpty());
        Assert.assertEquals(1, h3.getChildren().size());

        HierarchyLevel h4 = h3.getChildren().get(0);
        Assert.assertEquals("instrumentation", h4.getName());
        Assert.assertEquals("package", h4.getType().getInternalName());
        Assert.assertEquals("package", h4.getType().getEnglishName());
        Assert.assertTrue(h4.getSequences().isEmpty());
        Assert.assertEquals(1, h4.getChildren().size());

        HierarchyLevel h5 = h4.getChildren().get(0);
        Assert.assertEquals("java15", h5.getName());
        Assert.assertEquals("package", h5.getType().getInternalName());
        Assert.assertEquals("package", h5.getType().getEnglishName());
        Assert.assertTrue(h5.getSequences().isEmpty());
        Assert.assertEquals(1, h5.getChildren().size());

        HierarchyLevel h6 = h5.getChildren().get(0);
        Assert.assertEquals("test", h6.getName());
        Assert.assertEquals("package", h6.getType().getInternalName());
        Assert.assertEquals("package", h6.getType().getEnglishName());
        Assert.assertTrue(h6.getSequences().isEmpty());
        Assert.assertEquals(1, h6.getChildren().size());

        HierarchyLevel h7 = h6.getChildren().get(0);
        Assert.assertEquals("test3", h7.getName());
        Assert.assertEquals("package", h7.getType().getInternalName());
        Assert.assertEquals("package", h7.getType().getEnglishName());
        Assert.assertTrue(h7.getSequences().isEmpty());
        Assert.assertEquals(4, h7.getChildren().size());

        checkCodeExample(h7.getChildren().get(0));
        checkSecondClassOfFile(h7.getChildren().get(1));
        checkAdvancedFolderBackupOrder(h7.getChildren().get(2));

        Assert.assertTrue(isCompileableJava(target));

        try {
            testSessionContainer.save(SESSION_CONTAINER);
        } catch (Exception e) {
            handleException(e);
        }
        Assert.assertTrue(new File(SESSION_CONTAINER).exists());

        runJavac(target);
        String returnFromCall = runJava(CLASS_UNDER_TEST);
        assertNotNull(returnFromCall);

        String expectedResult = FileTool.getContentFromFile(new File(TEST_SOURCE + "test3/ExpectedResult.txt"));
        assertNotNull(expectedResult);

        // The output differs from java 1.5 to java 1.6
        // Assert.assertEquals(expectedResult, returnFromCall);

        // search for clf
        File[] filesOfTarget = new File(TARGET).listFiles();
        File coverageLog = null;
        for (File thisFile : filesOfTarget) {
            if (thisFile.getName().startsWith("coverage-log-")) {
                coverageLog = thisFile;
                break;
            }
        }
        Assert.assertNotNull("Coverage log not found", coverageLog);
        Assert.assertTrue(coverageLog.exists());

        // analyze
        TestSession testSession = testSessionContainer.createTestSession(TEST_SESSION_NAME,
                "no comment needed ;-)", new Date());
        CoverageLogParser logParser = new CoverageLogParser(coverageLog,
                MeasurementConstants.CHARSET);
        CoverageResultLogReader coverageResultLogReader = new CoverageResultLogReader(testSession,
                this.builder);
        logParser.CompilationUnit(coverageResultLogReader,
                testSessionContainer.getId());
        try {
            testSessionContainer.save(SESSION_CONTAINER);
        } catch (Exception e) {
            handleException(e);
        }
        Assert.assertTrue(new File(SESSION_CONTAINER).exists());

        checkCoverabelItemsTerm79(testSessionContainer);
    }

    private void checkCodeExample(HierarchyLevel classCodeExample) {
        Assert.assertEquals("CodeExample", classCodeExample.getName());
        Assert.assertEquals("class", classCodeExample.getType().getInternalName());
        Assert.assertEquals("class", classCodeExample.getType().getEnglishName());
        Assert.assertEquals(1, classCodeExample.getSequences().size());
        Assert.assertEquals(2, classCodeExample.getSequences().get(0).getStatements().size());
        Assert.assertEquals(7, classCodeExample.getChildren().size());
        locationAssertion("public class CodeExample", classCodeExample.getHeader());

        checkCodeExample_main(classCodeExample.getChildren().get(0));

        checkCodeExample_CodeExample(classCodeExample.getChildren().get(1));

        checkCodeExample_approx(classCodeExample.getChildren().get(2));

        HierarchyLevel codeExample3M = classCodeExample.getChildren().get(3);
        Assert.assertEquals("calcIntegral", codeExample3M.getName());
        Assert.assertEquals("method", codeExample3M.getType().getInternalName());
        Assert.assertEquals("method", codeExample3M.getType().getEnglishName());
        Assert.assertEquals(1, codeExample3M.getSequences().size());
        Assert.assertTrue(codeExample3M.getChildren().isEmpty());
        locationAssertion("@org.codecover.instrumentation.java15.test.test3.SecondClassOfFile.NewAnnotation1(true)\n" +
                "    private static double calcIntegral(double left, double right,\n" +
                "            int intervalCount, IFunction function)", codeExample3M.getHeader());

        HierarchyLevel inClassSinFunction = classCodeExample.getChildren().get(4);
        Assert.assertEquals("SinFunction", inClassSinFunction.getName());
        Assert.assertEquals("class", inClassSinFunction.getType().getInternalName());
        Assert.assertEquals("class", inClassSinFunction.getType().getEnglishName());
        Assert.assertEquals(1, inClassSinFunction.getSequences().size());
        Assert.assertEquals(1, inClassSinFunction.getSequences().get(0).getStatements().size());
        Assert.assertEquals(2, inClassSinFunction.getChildren().size());
        locationAssertion("public static class SinFunction implements IFunction", inClassSinFunction.getHeader());

        HierarchyLevel inClassIFunction = classCodeExample.getChildren().get(5);
        Assert.assertEquals("IFunction", inClassIFunction.getName());
        Assert.assertEquals("interface", inClassIFunction.getType().getInternalName());
        Assert.assertEquals("interface", inClassIFunction.getType().getEnglishName());
        Assert.assertTrue(inClassIFunction.getSequences().isEmpty());
        Assert.assertEquals(2, inClassIFunction.getChildren().size());
        locationAssertion("private interface IFunction", inClassIFunction.getHeader());

        HierarchyLevel inClassEApproxMode = classCodeExample.getChildren().get(6);
        Assert.assertEquals("EApproxMode", inClassEApproxMode.getName());
        Assert.assertEquals("enum", inClassEApproxMode.getType().getInternalName());
        Assert.assertEquals("enumeration", inClassEApproxMode.getType().getEnglishName());
        Assert.assertTrue(inClassEApproxMode.getSequences().isEmpty());
        Assert.assertTrue(inClassEApproxMode.getChildren().isEmpty());
        locationAssertion("protected enum EApproxMode", inClassEApproxMode.getHeader());
    }

    private void checkCodeExample_main(HierarchyLevel methodMain) {
        Assert.assertEquals("main", methodMain.getName());
        Assert.assertEquals("method", methodMain.getType().getInternalName());
        Assert.assertEquals("method", methodMain.getType().getEnglishName());
        Assert.assertEquals(1, methodMain.getSequences().size());
        Assert.assertEquals(12, methodMain.getSequences().get(0).getStatements().size());
        Assert.assertTrue(methodMain.getChildren().isEmpty());
        locationAssertion("public static void main(String[] args)", methodMain.getHeader());

        checkCodeExample_S9(methodMain.getSequences().get(0).getStatements().get(5));
        checkCodeExample_S3(methodMain.getSequences().get(0).getStatements().get(11));
    }

    private void checkCodeExample_S9(Statement S9) {
        Assert.assertTrue(S9 instanceof LoopingStatement);
        LoopingStatement S9_L = (LoopingStatement) S9;
        locationAssertion("for (EApproxMode mode : approxModes) {\n"+
                "                cE.approx(mode);\n"+
                "\n"+
                "                if ((mode.ordinal() >= approxModes.length - 1) || (1 == 0))\n"+
                "                    continue;\n"+
                "\n"+
                "                byte seperatorCount = 0;\n"+
                "                while (seperatorCount < 20) {\n"+
                "                    seperatorCount++;\n"+
                "\n"+
                "                    if (seperatorCount < 20)\n"+
                "                        System.out.printf(\"#\");\n"+
                "                    else\n"+
                "                        System.out.printf(\"%n\");\n"+
                "                }\n" +
                "            }", S9_L.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S9.getCoverableItem().getPrefix());
        Assert.assertEquals("S9", S9_L.getCoverableItem().getId());
        Assert.assertTrue(S9_L.getTerms().isEmpty());
        Assert.assertTrue(S9_L.isOptionalBodyExecution());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S9_L.getNeverExecutedItem().getPrefix());
        Assert.assertEquals("L1-0", S9_L.getNeverExecutedItem().getId());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S9_L.getOnceExecutedItem().getPrefix());
        Assert.assertEquals("L1-1", S9_L.getOnceExecutedItem().getId());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S9_L.getMultipleExecutedItem().getPrefix());
        Assert.assertEquals("L1-2", S9_L.getMultipleExecutedItem().getId());
        Assert.assertEquals(4, S9_L.getBody().getStatements().size());

        Statement S14 = S9_L.getBody().getStatements().get(3);
        Assert.assertTrue(S14 instanceof LoopingStatement);
        LoopingStatement S14_L = (LoopingStatement) S14;
        locationAssertion("while (seperatorCount < 20) {\n"+
                "                    seperatorCount++;\n"+
                "\n"+
                "                    if (seperatorCount < 20)\n"+
                "                        System.out.printf(\"#\");\n"+
                "                    else\n"+
                "                        System.out.printf(\"%n\");\n"+
                "                }", S14_L.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S14.getCoverableItem().getPrefix());
        Assert.assertEquals("S14", S14_L.getCoverableItem().getId());
        Assert.assertEquals(1, S14_L.getTerms().size());
        Assert.assertTrue(S14_L.isOptionalBodyExecution());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S14_L.getNeverExecutedItem().getPrefix());
        Assert.assertEquals("L2-0", S14_L.getNeverExecutedItem().getId());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S14_L.getOnceExecutedItem().getPrefix());
        Assert.assertEquals("L2-1", S14_L.getOnceExecutedItem().getId());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S14_L.getMultipleExecutedItem().getPrefix());
        Assert.assertEquals("L2-2", S14_L.getMultipleExecutedItem().getId());
        Assert.assertEquals(2, S14_L.getBody().getStatements().size());

        RootTerm C2 = S14_L.getTerms().iterator().next();
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", C2.getCoverableItem().getPrefix());
        Assert.assertEquals("C2",  C2.getCoverableItem().getId());
        BooleanTerm C2_Term = C2.getTerm();
        locationAssertion("seperatorCount < 20", C2_Term.getLocation());
        Assert.assertTrue(C2_Term instanceof BasicBooleanTerm);

        Statement S16 = S14_L.getBody().getStatements().get(1);
        Assert.assertTrue(S16 instanceof ConditionalStatement);
        ConditionalStatement S16_C = (ConditionalStatement) S16;
        locationAssertion("if (seperatorCount < 20)\n"+
                "                        System.out.printf(\"#\");\n"+
                "                    else\n"+
                "                        System.out.printf(\"%n\");", S16_C.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S16.getCoverableItem().getPrefix());
        Assert.assertEquals("S16", S16_C.getCoverableItem().getId());
        Assert.assertEquals(1, S16_C.getTerms().size());
        Assert.assertEquals(2, S16_C.getBranches().size());

        Branch B4 = S16_C.getBranches().get(0);
        locationAssertion("System.out.printf(\"#\");", B4.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", B4.getCoverableItem().getPrefix());
        Assert.assertEquals("B4", B4.getCoverableItem().getId());
        Assert.assertFalse(B4.isImplicit());
        locationAssertion(null, B4.getDecision());
        Assert.assertEquals(1, B4.getSequence().getStatements().size());

        Branch B5 = S16_C.getBranches().get(1);
        locationAssertion("System.out.printf(\"%n\");", B5.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", B5.getCoverableItem().getPrefix());
        Assert.assertEquals("B5", B5.getCoverableItem().getId());
        Assert.assertFalse(B5.isImplicit());
        locationAssertion("else", B5.getDecision());
        Assert.assertEquals(1, B5.getSequence().getStatements().size());
    }

    private void checkCodeExample_S3(Statement S3) {
        Assert.assertTrue(S3 instanceof ConditionalStatement);
        ConditionalStatement S1_C = (ConditionalStatement) S3;
        Assert.assertTrue(S1_C.getTerms().isEmpty());
        Assert.assertEquals(3, S1_C.getBranches().size());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S3.getCoverableItem().getPrefix());
        Assert.assertEquals("S3", S3.getCoverableItem().getId());

        checkCodeExample_B1(S1_C.getBranches().get(0));

        Branch B6 = S1_C.getBranches().get(1);
        Assert.assertEquals(2, B6.getSequence().getStatements().size());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", B6.getCoverableItem().getPrefix());
        Assert.assertEquals("B6", B6.getCoverableItem().getId());
        locationAssertion("catch (RuntimeException e)", B6.getDecision());
        Assert.assertFalse(B6.isImplicit());

        Branch B7 = S1_C.getBranches().get(2);
        Assert.assertEquals(2, B7.getSequence().getStatements().size());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", B7.getCoverableItem().getPrefix());
        Assert.assertEquals("B7", B7.getCoverableItem().getId());
        locationAssertion("catch (Exception e)", B7.getDecision());
        Assert.assertFalse(B7.isImplicit());
    }

    private void checkCodeExample_B1(Branch B1) {
        Assert.assertEquals(0, B1.getSequence().getStatements().size());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", B1.getCoverableItem().getPrefix());
        Assert.assertEquals("B1", B1.getCoverableItem().getId());
        locationAssertion(null, B1.getDecision());
        Assert.assertTrue(B1.isImplicit());
    }

    private void checkCodeExample_CodeExample(HierarchyLevel methodCodeExample) {
        Assert.assertEquals("CodeExample", methodCodeExample.getName());
        Assert.assertEquals("method", methodCodeExample.getType().getInternalName());
        Assert.assertEquals("method", methodCodeExample.getType().getEnglishName());
        Assert.assertEquals(1, methodCodeExample.getSequences().size());
        Assert.assertTrue(methodCodeExample.getChildren().isEmpty());
        locationAssertion("public CodeExample() throws Exception", methodCodeExample.getHeader());

        //public CodeExample() throws Exception {
        //    this.left = 0.0;
        //    this.right = Math.PI;
        //    this.intervalCount = ((this.right - this.left) > Math.PI ? 20 : 12);
        //    this.function = new SinFunction();
        //
        //    if (!this.function.canBeUsed()) {
        //        throw new Exception("function not usable!");
        //    }
        //    
        //    if (this.left == 0.0 && this.right == Math.PI
        //            && this.function instanceof SinFunction) {
        //        this.exactIntegral = 2.0;
        //    } else if (((this.left == 0.0 & this.right == Math.PI / 2)
        //            | (this.left == 2 * Math.PI & this.right == Math.PI) || true)
        //            & this.function instanceof SinFunction) {
        //        this.exactIntegral = 1.0;
        //    } else {
        //        throw new Exception("unknown correct integral");
        //    }
        //}
        // lets test somethings

        Statement S28 = methodCodeExample.getSequences().get(0).getStatements().get(0);
        Assert.assertTrue(S28 instanceof BasicStatement);
        locationAssertion("this.left = 0.0;", S28.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S28.getCoverableItem().getPrefix());
        Assert.assertEquals("S28", S28.getCoverableItem().getId());
        Assert.assertTrue(S28.getTerms().isEmpty());

        Statement S30 = methodCodeExample.getSequences().get(0).getStatements().get(2);
        Assert.assertTrue(S30 instanceof BasicStatement);
        locationAssertion("this.intervalCount = ((this.right - this.left) > Math.PI ? 20 : 12);", S30.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S30.getCoverableItem().getPrefix());
        Assert.assertEquals("S30", S30.getCoverableItem().getId());
        Assert.assertTrue(S30.getTerms().isEmpty());

        Statement S32 = methodCodeExample.getSequences().get(0).getStatements().get(4);
        Assert.assertTrue(S32 instanceof ConditionalStatement);
        ConditionalStatement S31_C = (ConditionalStatement) S32;
        locationAssertion("if (!this.function.canBeUsed()) {\n"
                + "            throw new Exception(\"function not usable!\");\n"
                + "        }", S31_C.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S32.getCoverableItem().getPrefix());
        Assert.assertEquals("S32", S31_C.getCoverableItem().getId());
        Assert.assertEquals(1, S31_C.getTerms().size());
        Assert.assertEquals(2, S31_C.getBranches().size());

        Branch B8 = S31_C.getBranches().get(0);
        locationAssertion("{\n"
                + "            throw new Exception(\"function not usable!\");\n"
                + "        }", B8.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", B8.getCoverableItem().getPrefix());
        Assert.assertEquals("B8", B8.getCoverableItem().getId());
        Assert.assertFalse(B8.isImplicit());
        locationAssertion(null, B8.getDecision());
        Assert.assertEquals(1, B8.getSequence().getStatements().size());

        Statement S33 = B8.getSequence().getStatements().get(0);
        Assert.assertTrue(S33 instanceof BasicStatement);
        locationAssertion("throw new Exception(\"function not usable!\");", S33.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S32.getCoverableItem().getPrefix());
        Assert.assertEquals("S33", S33.getCoverableItem().getId());

        Branch B9 = S31_C.getBranches().get(1);
        Assert.assertEquals(Branch.class.toString() + " -- ", B9.toString());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", B9.getCoverableItem().getPrefix());
        Assert.assertEquals("B9", B9.getCoverableItem().getId());
        Assert.assertTrue(B9.isImplicit());
        locationAssertion(null, B9.getDecision());
        Assert.assertTrue(B9.getSequence().getStatements().isEmpty());

        // end StatementTests
    }

    private void checkCodeExample_approx(HierarchyLevel methodApprox) {
        Assert.assertEquals("approx", methodApprox.getName());
        Assert.assertEquals("method", methodApprox.getType().getInternalName());
        Assert.assertEquals("method", methodApprox.getType().getEnglishName());
        Assert.assertEquals(1, methodApprox.getSequences().size());
        Assert.assertTrue(methodApprox.getChildren().isEmpty());
        locationAssertion("public void approx(EApproxMode mode)", methodApprox.getHeader());

        // switch test
        Statement S40 = methodApprox.getSequences().get(0).getStatements().get(1);
        Assert.assertTrue(S40 instanceof ConditionalStatement);
        ConditionalStatement S40_C = (ConditionalStatement) S40;
        locationAssertion("switch (mode.ordinal()) {\n" +
                "        case 0:\n" +
                "            System.out.printf(\"> fixed interval count:%n\");\n" +
                "            break;\n" +
                "        case 1:\n" +
                "            System.out.printf(\"> with given precision%n\");\n" +
                "            break;\n" +
                "        default:\n" +
                "            System.out.printf(\"> exact result%n\");\n" +
                "            break;\n" +
                "        }", S40_C.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S40.getCoverableItem().getPrefix());
        Assert.assertEquals("S40", S40_C.getCoverableItem().getId());
        Assert.assertTrue(S40_C.getTerms().isEmpty());
        Assert.assertEquals(3, S40_C.getBranches().size());

        Branch B14 = S40_C.getBranches().get(0);
        locationAssertion("System.out.printf(\"> fixed interval count:%n\");\n" +
                "            break;", B14.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", B14.getCoverableItem().getPrefix());
        Assert.assertEquals("B14", B14.getCoverableItem().getId());
        Assert.assertFalse(B14.isImplicit());
        locationAssertion("case 0", B14.getDecision());
        Assert.assertFalse(B14.getSequence().getStatements().isEmpty());

        Branch B15 = S40_C.getBranches().get(1);
        locationAssertion("System.out.printf(\"> with given precision%n\");\n" +
                "            break;", B15.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", B15.getCoverableItem().getPrefix());
        Assert.assertEquals("B15", B15.getCoverableItem().getId());
        Assert.assertFalse(B15.isImplicit());
        locationAssertion("case 1", B15.getDecision());
        Assert.assertFalse(B15.getSequence().getStatements().isEmpty());

        Branch B16 = S40_C.getBranches().get(2);
        locationAssertion("System.out.printf(\"> exact result%n\");\n" +
                "            break;", B16.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", B16.getCoverableItem().getPrefix());
        Assert.assertEquals("B16", B16.getCoverableItem().getId());
        Assert.assertFalse(B16.isImplicit());
        locationAssertion("default", B16.getDecision());
        Assert.assertFalse(B16.getSequence().getStatements().isEmpty());

        // end Switch test
    }

    private void checkSecondClassOfFile(HierarchyLevel classSecondClassOfFile) {
        Assert.assertEquals("SecondClassOfFile", classSecondClassOfFile.getName());
        Assert.assertEquals("class", classSecondClassOfFile.getType().getInternalName());
        Assert.assertEquals("class", classSecondClassOfFile.getType().getEnglishName());
        Assert.assertEquals(1, classSecondClassOfFile.getSequences().size());
        Assert.assertEquals(6, classSecondClassOfFile.getSequences().get(0).getStatements().size());
        Assert.assertEquals(9, classSecondClassOfFile.getChildren().size());
        locationAssertion("final class SecondClassOfFile extends Object implements Serializable, Runnable", classSecondClassOfFile.getHeader());

        checkSecondClassOfFile_getNumber(classSecondClassOfFile.getChildren().get(0));
        checkSecondClassOfFile_run(classSecondClassOfFile.getChildren().get(4));

        HierarchyLevel secondClass5I = classSecondClassOfFile.getChildren().get(7);
        Assert.assertEquals("InnerInterface", secondClass5I.getName());
        Assert.assertEquals("interface", secondClass5I.getType().getInternalName());
        Assert.assertEquals("interface", secondClass5I.getType().getEnglishName());
        Assert.assertTrue(secondClass5I.getSequences().isEmpty());
        Assert.assertEquals(3, secondClass5I.getChildren().size());
        locationAssertion("interface InnerInterface", secondClass5I.getHeader());

        HierarchyLevel secondClass12IE = secondClass5I.getChildren().get(1);
        Assert.assertEquals("ECarSelection", secondClass12IE.getName());
        Assert.assertEquals("enum", secondClass12IE.getType().getInternalName());
        Assert.assertEquals("enumeration", secondClass12IE.getType().getEnglishName());
        Assert.assertTrue(secondClass12IE.getSequences().isEmpty());
        Assert.assertEquals(2, secondClass12IE.getChildren().size());
        locationAssertion("@Deprecated\n        public enum ECarSelection", secondClass12IE.getHeader());

        HierarchyLevel secondClass111IEC = secondClass12IE.getChildren().get(0);
        Assert.assertEquals("ClassInEnum", secondClass111IEC.getName());
        Assert.assertEquals("class", secondClass111IEC.getType().getInternalName());
        Assert.assertEquals("class", secondClass111IEC.getType().getEnglishName());
        Assert.assertEquals(1, secondClass111IEC.getSequences().size());
        Assert.assertEquals(2, secondClass111IEC.getSequences().get(0).getStatements().size());
        Assert.assertEquals(2, secondClass111IEC.getChildren().size());
        locationAssertion("@SuppressWarnings(\"all\")\n            static class ClassInEnum implements InnerInterface", secondClass111IEC.getHeader());

        HierarchyLevel secondClass112IEA = secondClass12IE.getChildren().get(1);
        Assert.assertEquals("NewAnnotation2", secondClass112IEA.getName());
        Assert.assertEquals("@interface", secondClass112IEA.getType().getInternalName());
        Assert.assertEquals("annotation", secondClass112IEA.getType().getEnglishName());
        Assert.assertTrue(secondClass112IEA.getSequences().isEmpty());
        Assert.assertTrue(secondClass112IEA.getChildren().isEmpty());
        locationAssertion("@Target({LOCAL_VARIABLE})\n            @interface NewAnnotation2", secondClass112IEA.getHeader());

        HierarchyLevel secondClass13IA = secondClass5I.getChildren().get(2);
        Assert.assertEquals("NewAnnotation3", secondClass13IA.getName());
        Assert.assertEquals("@interface", secondClass13IA.getType().getInternalName());
        Assert.assertEquals("annotation", secondClass13IA.getType().getEnglishName());
        Assert.assertTrue(secondClass13IA.getSequences().isEmpty());
        Assert.assertTrue(secondClass13IA.getChildren().isEmpty());
        locationAssertion("@Target({METHOD, CONSTRUCTOR})\n        abstract @interface NewAnnotation3", secondClass13IA.getHeader());

        HierarchyLevel secondClass7A = classSecondClassOfFile.getChildren().get(8);
        Assert.assertEquals("NewAnnotation1", secondClass7A.getName());
        Assert.assertEquals("@interface", secondClass7A.getType().getInternalName());
        Assert.assertEquals("annotation", secondClass7A.getType().getEnglishName());
        Assert.assertTrue(secondClass7A.getSequences().isEmpty());
        Assert.assertEquals(1, secondClass7A.getChildren().size());
        locationAssertion("@Target({METHOD, CONSTRUCTOR})\n    protected @interface NewAnnotation1", secondClass7A.getHeader());

        HierarchyLevel secondClass21AA = secondClass7A.getChildren().get(0);
        Assert.assertEquals("NewAnnotation4", secondClass21AA.getName());
        Assert.assertEquals("@interface", secondClass21AA.getType().getInternalName());
        Assert.assertEquals("annotation", secondClass21AA.getType().getEnglishName());
        Assert.assertTrue(secondClass21AA.getSequences().isEmpty());
        Assert.assertTrue(secondClass21AA.getChildren().isEmpty());
        locationAssertion("@Target({FIELD})\n        @interface NewAnnotation4", secondClass21AA.getHeader());
    }

    private void checkSecondClassOfFile_getNumber(HierarchyLevel methodGetNumber) {
        Assert.assertEquals("getNumber", methodGetNumber.getName());
        Assert.assertEquals("method", methodGetNumber.getType().getInternalName());
        Assert.assertEquals("method", methodGetNumber.getType().getEnglishName());
        Assert.assertEquals(1, methodGetNumber.getSequences().size());
        Assert.assertEquals(0, methodGetNumber.getChildren().size());
        locationAssertion("public static long getNumber(boolean switchNumber)", methodGetNumber.getHeader());

        // switch test
        Statement S92 = methodGetNumber.getSequences().get(0).getStatements().get(6);
        Assert.assertTrue(S92 instanceof ConditionalStatement);
        ConditionalStatement S92_C = (ConditionalStatement) S92;
        locationAssertion("switch (2) {\n"+
                "        case 1: break;\n"+
                "        case 2:\n"+
                "        }", S92_C.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S92_C.getCoverableItem().getPrefix());
        Assert.assertEquals("S92", S92_C.getCoverableItem().getId());
        Assert.assertTrue(S92_C.getTerms().isEmpty());
        Assert.assertEquals(3, S92_C.getBranches().size());

        Branch B21 = S92_C.getBranches().get(0);
        locationAssertion("break;", B21.getLocation());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", B21.getCoverableItem().getPrefix());
        Assert.assertEquals("B21", B21.getCoverableItem().getId());
        Assert.assertFalse(B21.isImplicit());
        locationAssertion("case 1", B21.getDecision());
        Assert.assertFalse(B21.getSequence().getStatements().isEmpty());

        Branch B22 = S92_C.getBranches().get(1);
        Assert.assertEquals(Branch.class.toString() + " -- ", B22.toString());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", B22.getCoverableItem().getPrefix());
        Assert.assertEquals("B22", B22.getCoverableItem().getId());
        Assert.assertFalse(B22.isImplicit());
        locationAssertion("case 2", B22.getDecision());
        Assert.assertTrue(B22.getSequence().getStatements().isEmpty());

        Branch B23 = S92_C.getBranches().get(2);
        Assert.assertEquals(Branch.class.toString() + " -- ", B23.toString());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", B23.getCoverableItem().getPrefix());
        Assert.assertEquals("B23", B23.getCoverableItem().getId());
        Assert.assertTrue(B23.isImplicit());
        locationAssertion(null, B23.getDecision());
        Assert.assertTrue(B23.getSequence().getStatements().isEmpty());

        // end switch test
    }

    private void checkSecondClassOfFile_run(HierarchyLevel methodRun) {
        Assert.assertEquals("run", methodRun.getName());
        Assert.assertEquals("method", methodRun.getType().getInternalName());
        Assert.assertEquals("method", methodRun.getType().getEnglishName());
        Assert.assertEquals(1, methodRun.getSequences().size());
        Assert.assertEquals(4, methodRun.getChildren().size());
        locationAssertion("public void run()", methodRun.getHeader());

        HierarchyLevel runnerClass = methodRun.getChildren().get(0);
        Assert.assertEquals("RunnerClass", runnerClass.getName());
        Assert.assertEquals("class", runnerClass.getType().getInternalName());
        Assert.assertEquals("class", runnerClass.getType().getEnglishName());
        Assert.assertEquals(1, runnerClass.getSequences().size());
        Assert.assertEquals(1, runnerClass.getSequences().get(0).getStatements().size());
        Assert.assertEquals(3, runnerClass.getChildren().size());
        locationAssertion("class RunnerClass", runnerClass.getHeader());

        HierarchyLevel methodRunnerClass = runnerClass.getChildren().get(1);
        Assert.assertEquals("RunnerClass", methodRunnerClass.getName());
        Assert.assertEquals("method", methodRunnerClass.getType().getInternalName());
        Assert.assertEquals("method", methodRunnerClass.getType().getEnglishName());
        Assert.assertEquals(1, methodRunnerClass.getSequences().size());
        Assert.assertEquals(2, methodRunnerClass.getSequences().get(0).getStatements().size());
        Assert.assertEquals(0, methodRunnerClass.getChildren().size());
        locationAssertion("public RunnerClass(int i)", methodRunnerClass.getHeader());

        Statement S329 = methodRunnerClass.getSequences().get(0).getStatements().get(0);
        Assert.assertTrue(S329 instanceof BasicStatement);
        Assert.assertEquals(0, S329.getTerms().size());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S329.getCoverableItem().getPrefix());
        Assert.assertEquals("S329", S329.getCoverableItem().getId());
        locationAssertion("this();", S329.getLocation());

        HierarchyLevel runnerClass2 = methodRun.getChildren().get(1);
        Assert.assertEquals("RunnerClass2", runnerClass2.getName());
        Assert.assertEquals("class", runnerClass2.getType().getInternalName());
        Assert.assertEquals("class", runnerClass2.getType().getEnglishName());
        Assert.assertTrue(runnerClass2.getSequences().isEmpty());
        Assert.assertEquals(2, runnerClass2.getChildren().size());
        locationAssertion("final class RunnerClass2 extends RunnerClass", runnerClass2.getHeader());

        HierarchyLevel methodRunnerClass2 = runnerClass2.getChildren().get(0);
        Assert.assertEquals("RunnerClass2", methodRunnerClass2.getName());
        Assert.assertEquals("method", methodRunnerClass2.getType().getInternalName());
        Assert.assertEquals("method", methodRunnerClass2.getType().getEnglishName());
        Assert.assertEquals(1, methodRunnerClass2.getSequences().size());
        Assert.assertEquals(1, methodRunnerClass2.getSequences().get(0).getStatements().size());
        Assert.assertEquals(0, methodRunnerClass2.getChildren().size());
        locationAssertion("public RunnerClass2()", methodRunnerClass2.getHeader());

        Statement S332 = methodRunnerClass2.getSequences().get(0).getStatements().get(0);
        Assert.assertTrue(S332 instanceof BasicStatement);
        Assert.assertEquals(0, S332.getTerms().size());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S332.getCoverableItem().getPrefix());
        Assert.assertEquals("S332", S332.getCoverableItem().getId());
        locationAssertion("super(2);", S332.getLocation());
    }

    private void checkAdvancedFolderBackupOrder(HierarchyLevel classAdvanced) {
        Assert.assertEquals("AdvancedFolderBackupOrder", classAdvanced.getName());
        Assert.assertEquals("class", classAdvanced.getType().getInternalName());
        Assert.assertEquals("class", classAdvanced.getType().getEnglishName());
        Assert.assertEquals(1, classAdvanced.getSequences().size());
        Assert.assertEquals(1, classAdvanced.getSequences().get(0).getStatements().size());
        Assert.assertEquals(16, classAdvanced.getChildren().size());
        locationAssertion("class AdvancedFolderBackupOrder", classAdvanced.getHeader());

        HierarchyLevel methodAccept = classAdvanced.getChildren().get(12);
        Assert.assertEquals("accept", methodAccept.getName());
        Assert.assertEquals("method", methodAccept.getType().getInternalName());
        Assert.assertEquals("method", methodAccept.getType().getEnglishName());
        Assert.assertEquals(1, methodAccept.getSequences().size());
        Assert.assertEquals(0, methodAccept.getChildren().size());
        locationAssertion("public boolean accept(String strName)", methodAccept.getHeader());

        Statement S425 = methodAccept.getSequences().get(0).getStatements().get(5);
        ConditionalStatement S425_C = (ConditionalStatement) S425;
        Assert.assertEquals(2, S425_C.getBranches().size());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", S425_C.getCoverableItem().getPrefix());
        Assert.assertEquals("S425", S425_C.getCoverableItem().getId());
        Assert.assertEquals(1, S425_C.getTerms().size());

        Branch B159 = S425_C.getBranches().get(0);
        Assert.assertEquals(1, B159.getSequence().getStatements().size());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", B159.getCoverableItem().getPrefix());
        Assert.assertEquals("B159", B159.getCoverableItem().getId());
        locationAssertion(null, B159.getDecision());
        Assert.assertFalse(B159.isImplicit()); 

        Branch B160 = S425_C.getBranches().get(1);
        Assert.assertEquals(1, B160.getSequence().getStatements().size());
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", B160.getCoverableItem().getPrefix());
        Assert.assertEquals("B160", B160.getCoverableItem().getId());
        locationAssertion("else", B160.getDecision());
        Assert.assertFalse(B160.isImplicit()); 

        this.term78 = S425_C.getTerms().iterator().next(); 
        checkTerm79();
    }

    private void checkTerm79() {
        Assert.assertEquals("org.codecover.instrumentation.java15.test.test3.CodeExample.java", this.term78.getCoverableItem().getPrefix());
        Assert.assertEquals("C79", this.term78.getCoverableItem().getId());

        // ((whiteFind == null && whiteMatch == null) ||
        //  (whiteFind != null && whiteFind.matcher(strName).find()) ||
        //  (whiteMatch != null && whiteMatch.matcher(strName).matches())) &&
        // !(blackFind != null && blackFind.matcher(strName).find()) &&
        // !(blackMatch != null && blackMatch.matcher(strName).matches());
        BooleanTerm termRoot = this.term78.getTerm();
        Assert.assertEquals(10, termRoot.getBasicBooleanTerms());
        final LinkedList<BasicBooleanTerm> basicTerms = new LinkedList<BasicBooleanTerm>();
        final LinkedList<OperatorTerm> operatorTerms = new LinkedList<OperatorTerm>();
        final LinkedList<Class<? extends BooleanTerm>> expectedTerms = new LinkedList<Class<? extends BooleanTerm>>();
        expectedTerms.add(OperatorTerm.class);
        expectedTerms.add(OperatorTerm.class);
        expectedTerms.add(OperatorTerm.class);
        expectedTerms.add(OperatorTerm.class);
        expectedTerms.add(OperatorTerm.class);
        expectedTerms.add(BasicBooleanTerm.class);
        expectedTerms.add(BasicBooleanTerm.class);
        expectedTerms.add(OperatorTerm.class);
        expectedTerms.add(BasicBooleanTerm.class);
        expectedTerms.add(BasicBooleanTerm.class);
        expectedTerms.add(OperatorTerm.class);
        expectedTerms.add(BasicBooleanTerm.class);
        expectedTerms.add(BasicBooleanTerm.class);
        expectedTerms.add(OperatorTerm.class);
        expectedTerms.add(OperatorTerm.class);
        expectedTerms.add(BasicBooleanTerm.class);
        expectedTerms.add(BasicBooleanTerm.class);
        expectedTerms.add(OperatorTerm.class);
        expectedTerms.add(OperatorTerm.class);
        expectedTerms.add(BasicBooleanTerm.class);
        expectedTerms.add(BasicBooleanTerm.class);

        Visitor booleanTermVisitor = new Visitor() {
            public void visit(BasicBooleanTerm term) {
                basicTerms.add(term);
                Assert.assertTrue(BasicBooleanTerm.class == expectedTerms.poll());
            }

            public void visit(OperatorTerm term) {
                operatorTerms.add(term);
                Assert.assertTrue(OperatorTerm.class == expectedTerms.poll());
            }
        };
        termRoot.accept(booleanTermVisitor, new DefaultVisitor());
        Assert.assertTrue(expectedTerms.isEmpty());
        Assert.assertEquals(10, basicTerms.size());
        Assert.assertEquals(11, operatorTerms.size());

        BasicBooleanTerm basicTerm;
        basicTerm = basicTerms.poll();
        Assert.assertEquals(1, basicTerm.getBasicBooleanTerms());
        locationAssertion("whiteFind == null", basicTerm.getLocation());
        Assert.assertSame(basicTerm, this.term78.getTermAtPosition(0));

        basicTerm = basicTerms.poll();
        Assert.assertEquals(1, basicTerm.getBasicBooleanTerms());
        locationAssertion("whiteMatch == null", basicTerm.getLocation());
        Assert.assertSame(basicTerm, this.term78.getTermAtPosition(1));

        basicTerm = basicTerms.poll();
        Assert.assertEquals(1, basicTerm.getBasicBooleanTerms());
        locationAssertion("whiteFind != null", basicTerm.getLocation());
        Assert.assertSame(basicTerm, this.term78.getTermAtPosition(2));

        basicTerm = basicTerms.poll();
        Assert.assertEquals(1, basicTerm.getBasicBooleanTerms());
        locationAssertion("whiteFind.matcher(strName).find()", basicTerm.getLocation());
        Assert.assertSame(basicTerm, this.term78.getTermAtPosition(3));

        basicTerm = basicTerms.poll();
        Assert.assertEquals(1, basicTerm.getBasicBooleanTerms());
        locationAssertion("whiteMatch != null", basicTerm.getLocation());
        Assert.assertSame(basicTerm, this.term78.getTermAtPosition(4));

        basicTerm = basicTerms.poll();
        Assert.assertEquals(1, basicTerm.getBasicBooleanTerms());
        locationAssertion("whiteMatch.matcher(strName).matches()", basicTerm.getLocation());
        Assert.assertSame(basicTerm, this.term78.getTermAtPosition(5));

        basicTerm = basicTerms.poll();
        Assert.assertEquals(1, basicTerm.getBasicBooleanTerms());
        locationAssertion("blackFind != null", basicTerm.getLocation());
        Assert.assertSame(basicTerm, this.term78.getTermAtPosition(6));

        basicTerm = basicTerms.poll();
        Assert.assertEquals(1, basicTerm.getBasicBooleanTerms());
        locationAssertion("blackFind.matcher(strName).find()", basicTerm.getLocation());
        Assert.assertSame(basicTerm, this.term78.getTermAtPosition(7));

        basicTerm = basicTerms.poll();
        Assert.assertEquals(1, basicTerm.getBasicBooleanTerms());
        locationAssertion("blackMatch != null", basicTerm.getLocation());
        Assert.assertSame(basicTerm, this.term78.getTermAtPosition(8));

        basicTerm = basicTerms.poll();
        Assert.assertEquals(1, basicTerm.getBasicBooleanTerms());
        locationAssertion("blackMatch.matcher(strName).matches()", basicTerm.getLocation());
        Assert.assertSame(basicTerm, this.term78.getTermAtPosition(9));

        Assert.assertTrue(basicTerms.isEmpty());

        OperatorTerm operatorTerm;
        BooleanOperator operator;
        operatorTerm = operatorTerms.poll();
        Assert.assertEquals(10, operatorTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, operatorTerm.getLocation().getLocations().size());
        Assert.assertEquals("&&", operatorTerm.getLocation().getLocations().get(0).getContent());
        Assert.assertEquals(2, operatorTerm.getOperands().size());
        operator = operatorTerm.getOperator();
        Assert.assertEquals(2, operator.getArity());
        Assert.assertEquals(CONDITIONAL_AND, operator.getName());

        operatorTerm = operatorTerms.poll();
        Assert.assertEquals(8, operatorTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, operatorTerm.getLocation().getLocations().size());
        Assert.assertEquals("&&", operatorTerm.getLocation().getLocations().get(0).getContent());
        Assert.assertEquals(2, operatorTerm.getOperands().size());
        operator = operatorTerm.getOperator();
        Assert.assertEquals(2, operator.getArity());
        Assert.assertEquals(CONDITIONAL_AND, operator.getName());

        operatorTerm = operatorTerms.poll();
        Assert.assertEquals(6, operatorTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, operatorTerm.getLocation().getLocations().size());
        Assert.assertEquals("||", operatorTerm.getLocation().getLocations().get(0).getContent());
        Assert.assertEquals(2, operatorTerm.getOperands().size());
        operator = operatorTerm.getOperator();
        Assert.assertEquals(2, operator.getArity());
        Assert.assertEquals(CONDITIONAL_OR, operator.getName());

        operatorTerm = operatorTerms.poll();
        Assert.assertEquals(4, operatorTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, operatorTerm.getLocation().getLocations().size());
        Assert.assertEquals("||", operatorTerm.getLocation().getLocations().get(0).getContent());
        Assert.assertEquals(2, operatorTerm.getOperands().size());
        operator = operatorTerm.getOperator();
        Assert.assertEquals(2, operator.getArity());
        Assert.assertEquals(CONDITIONAL_OR, operator.getName());

        operatorTerm = operatorTerms.poll();
        Assert.assertEquals(2, operatorTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, operatorTerm.getLocation().getLocations().size());
        Assert.assertEquals("&&", operatorTerm.getLocation().getLocations().get(0).getContent());
        Assert.assertEquals(2, operatorTerm.getOperands().size());
        operator = operatorTerm.getOperator();
        Assert.assertEquals(2, operator.getArity());
        Assert.assertEquals(CONDITIONAL_AND, operator.getName());

        operatorTerm = operatorTerms.poll();
        Assert.assertEquals(2, operatorTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, operatorTerm.getLocation().getLocations().size());
        Assert.assertEquals("&&", operatorTerm.getLocation().getLocations().get(0).getContent());
        Assert.assertEquals(2, operatorTerm.getOperands().size());
        operator = operatorTerm.getOperator();
        Assert.assertEquals(2, operator.getArity());
        Assert.assertEquals(CONDITIONAL_AND, operator.getName());

        operatorTerm = operatorTerms.poll();
        Assert.assertEquals(2, operatorTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, operatorTerm.getLocation().getLocations().size());
        Assert.assertEquals("&&", operatorTerm.getLocation().getLocations().get(0).getContent());
        Assert.assertEquals(2, operatorTerm.getOperands().size());
        operator = operatorTerm.getOperator();
        Assert.assertEquals(2, operator.getArity());
        Assert.assertEquals(CONDITIONAL_AND, operator.getName());

        operatorTerm = operatorTerms.poll();
        Assert.assertEquals(2, operatorTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, operatorTerm.getLocation().getLocations().size());
        Assert.assertEquals("!", operatorTerm.getLocation().getLocations().get(0).getContent());
        Assert.assertEquals(1, operatorTerm.getOperands().size());
        operator = operatorTerm.getOperator();
        Assert.assertEquals(1, operator.getArity());
        Assert.assertEquals(NOT, operator.getName());

        operatorTerm = operatorTerms.poll();
        Assert.assertEquals(2, operatorTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, operatorTerm.getLocation().getLocations().size());
        Assert.assertEquals("&&", operatorTerm.getLocation().getLocations().get(0).getContent());
        Assert.assertEquals(2, operatorTerm.getOperands().size());
        operator = operatorTerm.getOperator();
        Assert.assertEquals(2, operator.getArity());
        Assert.assertEquals(CONDITIONAL_AND, operator.getName());

        operatorTerm = operatorTerms.poll();
        Assert.assertEquals(2, operatorTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, operatorTerm.getLocation().getLocations().size());
        Assert.assertEquals("!", operatorTerm.getLocation().getLocations().get(0).getContent());
        Assert.assertEquals(1, operatorTerm.getOperands().size());
        operator = operatorTerm.getOperator();
        Assert.assertEquals(1, operator.getArity());
        Assert.assertEquals(NOT, operator.getName());

        operatorTerm = operatorTerms.poll();
        Assert.assertEquals(2, operatorTerm.getBasicBooleanTerms());
        Assert.assertEquals(1, operatorTerm.getLocation().getLocations().size());
        Assert.assertEquals("&&", operatorTerm.getLocation().getLocations().get(0).getContent());
        Assert.assertEquals(2, operatorTerm.getOperands().size());
        operator = operatorTerm.getOperator();
        Assert.assertEquals(2, operator.getArity());
        Assert.assertEquals(CONDITIONAL_AND, operator.getName());

        Assert.assertTrue(operatorTerms.isEmpty());
    }

    private void checkCoverabelItemsTerm79(TestSessionContainer testSessionContainer) {
        TestSession testSession = testSessionContainer.getTestSessionWithName(TEST_SESSION_NAME);
        org.codecover.model.TestCase testCase = testSession.getTestCaseWithName("AdvancedFolderBackupOrder condition test");
        Map<CoverableItem, Long> coverageData = testCase.getCoverageData();
        CoverableItem cItemTerm79 = this.builder.createCoverableItem("org.codecover.instrumentation.java15.test.test3.CodeExample.java", "C79");
        Assert.assertNull(coverageData.get(cItemTerm79));

        Map<CoverableItem, BooleanAssignmentMap> assignments = testCase.getAssignmentsMap();
        BooleanAssignmentMap assignmentMap = assignments.get(cItemTerm79);

        Assert.assertEquals(1, assignmentMap.get(getBooleanAssignmentFromString("11110000000010001110")));

        Assert.assertEquals(2, assignmentMap.get(getBooleanAssignmentFromString("10001110100000000000")));

        Assert.assertEquals(4, assignmentMap.get(getBooleanAssignmentFromString("11110000000010001000")));

        Assert.assertEquals(2, assignmentMap.get(getBooleanAssignmentFromString("11101000111000000000")));

        Assert.assertEquals(2, assignmentMap.get(getBooleanAssignmentFromString("10001111000010001000")));

        Assert.assertEquals(2, assignmentMap.get(getBooleanAssignmentFromString("10001110111000000000")));

        Assert.assertEquals(2, assignmentMap.get(getBooleanAssignmentFromString("10001110111110001000")));

        Assert.assertEquals(2, assignmentMap.get(getBooleanAssignmentFromString("11110000000011110000")));

        Assert.assertEquals(1, assignmentMap.get(getBooleanAssignmentFromString("11110000000011101000")));

        Assert.assertEquals(2, assignmentMap.get(getBooleanAssignmentFromString("11110000000010001111")));
    }
}
