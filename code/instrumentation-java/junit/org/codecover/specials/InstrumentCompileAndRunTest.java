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
import static org.codecover.UtilsForTestingJava.runJava;
import static org.codecover.UtilsForTestingJava.runJavac;
import static org.codecover.UtilsForTestingJava.simpleTestSessionContainerTests;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
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
import org.codecover.model.mast.CoverableItem;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;
import org.codecover.model.utils.file.SourceTargetContainer;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: MastAndCompileTest.java 18 2008-05-24 22:07:13Z ahija $)
 * 
 */
public class InstrumentCompileAndRunTest extends TestCase {

    private static final String CLASS_UNDER_TEST = "org.codecover.instrumentation.java15.test.test10.CoverableItemTest";

    private static final String JAVA_FILE_UNDER_TEST = CLASS_UNDER_TEST + ".java";

    private static final String TEST_SESSION_NAME = "InstrumentCompileAndRunTest";

    private Instrumenter instrumenter;

    private MASTBuilder builder;

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
        String srcPath = TEST_SOURCE + "test10/CoverableItemTest.java";
        File source = new File(srcPath);
        String targetPath = TEST_TARGET + "test10/CoverableItemTest.java";
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
        doHiearachyLevelsTests(testSessionContainer);
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
    
        // The output differs from java 1.5 to java 1.6
         Assert.assertEquals("ENDE", returnFromCall);
    
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
                "Anweisungsinstrumentierung, Compilieren und Ausführen", new Date());
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
    
        doTestCaseTest(testSession);
    }

    /**
     * 
     * @param testSessionContainer
     */
    private void doHiearachyLevelsTests(
            TestSessionContainer testSessionContainer) {
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
        Assert.assertEquals("test10", h7.getName());
        Assert.assertEquals("package", h7.getType().getInternalName());
        Assert.assertEquals("package", h7.getType().getEnglishName());
        Assert.assertTrue(h7.getSequences().isEmpty());
        Assert.assertEquals(1, h7.getChildren().size());
    }

    private void doTestCaseTest(TestSession testSession) {
         List<org.codecover.model.TestCase> testCases = testSession.getTestCases();
         Iterator<org.codecover.model.TestCase> testCaseIterator = testCases.iterator();
         Assert.assertEquals(16, testCases.size());

         org.codecover.model.TestCase thisTestCase;
         Map<CoverableItem,Long> coverageData;
         int testcaseCount = 0;
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());

             Assert.assertEquals(0, coverageData.size());
         }
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());

             Assert.assertEquals(1, coverageData.size());
             doStatementIDTest(thisTestCase, new int[]{1});
         }
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());
             
             Assert.assertEquals(1, coverageData.size());
             doStatementIDTest(thisTestCase, new int[]{2});
         }
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());
             
             Assert.assertEquals(1, coverageData.size());
             doStatementIDTest(thisTestCase, new int[]{3});
         }
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());
             
             Assert.assertEquals(1, coverageData.size());
             doStatementIDTest(thisTestCase, new int[]{4});
         }
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());
             
             Assert.assertEquals(1, coverageData.size());
             doStatementIDTest(thisTestCase, new int[]{5});
         }
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());
             
             Assert.assertEquals(4, coverageData.size());
             doStatementIDTest(thisTestCase, new int[]{6,7});
             doBranchIDTest(thisTestCase, new int[]{1,2});
         }
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());
             
             Assert.assertEquals(8, coverageData.size());
             doStatementIDTest(thisTestCase, new int[]{9,11,12,14,15});
             doBranchIDTest(thisTestCase, new int[]{5,7,9});
         }
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());

             Assert.assertEquals(6, coverageData.size());
             doStatementIDTest(thisTestCase, new int[]{17,18,19,20});
             doLoopIDTest(thisTestCase, new int[]{2,5});
         }
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());

             Assert.assertEquals(6, coverageData.size());
             doStatementIDTest(thisTestCase, new int[]{21,22,23,24}, new int[]{1,1,1,2});
             doLoopIDTest(thisTestCase, new int[]{8,12});
         }
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());
             
             Assert.assertEquals(15, coverageData.size());
             doStatementIDTest(thisTestCase, new int[]{25,26,27,28,29,30,31,32,33,34,35}, new int[]{1,1,1,1,1,5,1,2,1,2,2});
             doLoopIDTest(thisTestCase, new int[]{14,18,21,24});
         }
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());

             Assert.assertEquals(2, coverageData.size());
             doStatementIDTest(thisTestCase, new int[]{36,60});
         }
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());

             Assert.assertEquals(8, coverageData.size());
             doStatementIDTest(thisTestCase, new int[]{37,38,40,41,43,44,61});
             doBranchIDTest(thisTestCase, new int[]{11});
         }
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());
             
             Assert.assertEquals(0, coverageData.size());
         }
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());

             Assert.assertEquals(8, coverageData.size());
             doStatementIDTest(thisTestCase, new int[]{45,46,47,48,49,50,52});
             doBranchIDTest(thisTestCase, new int[]{14});
         }
         {
             thisTestCase = testCaseIterator.next();
             coverageData = thisTestCase.getCoverageData();
             Assert.assertEquals("run " + (++testcaseCount), thisTestCase.getName());

             Assert.assertEquals(5, coverageData.size());
             doStatementIDTest(thisTestCase, new int[]{51,55,56,57});
             doBranchIDTest(thisTestCase, new int[]{15});
         }
    }

    private void doStatementIDTest(org.codecover.model.TestCase testCase, int[] statementIdxs) {
        int[] count = new int[statementIdxs.length];
        Arrays.fill(count, 1);
        doStatementIDTest(testCase, statementIdxs, count);
    }

    private void doStatementIDTest(org.codecover.model.TestCase testCase, int[] statementIdxs, int[] count) {
        Assert.assertEquals(statementIdxs.length, count.length);
        String[] IDs = new String[statementIdxs.length];
        for (int i = 0; i < IDs.length; i++) {
            IDs[i] = StatementCoverage.ID_PREFIX + statementIdxs[i];
        }
        doIDTest(testCase, IDs, count);
    }

    private void doBranchIDTest(org.codecover.model.TestCase testCase, int[] statementIdxs) {
        int[] count = new int[statementIdxs.length];
        Arrays.fill(count, 1);
        doBranchIDTest(testCase, statementIdxs, count);
    }
    
    private void doBranchIDTest(org.codecover.model.TestCase testCase, int[] statementIdxs, int[] count) {
        Assert.assertEquals(statementIdxs.length, count.length);
        String[] IDs = new String[statementIdxs.length];
        for (int i = 0; i < IDs.length; i++) {
            IDs[i] = BranchCoverage.ID_PREFIX + statementIdxs[i];
        }
        doIDTest(testCase, IDs, count);
    }

    private void doLoopIDTest(org.codecover.model.TestCase testCase, int[] statementIdxs) {
        int[] count = new int[statementIdxs.length];
        Arrays.fill(count, 1);
        doLoopIDTest(testCase, statementIdxs, count);
    }

    private void doLoopIDTest(org.codecover.model.TestCase testCase, int[] statementIdxs, int[] count) {
        Assert.assertEquals(statementIdxs.length, count.length);
        String[] IDs = new String[statementIdxs.length];
        for (int i = 0; i < IDs.length; i++) {
            int primaryID = (statementIdxs[i] + 2) / 3;
            int subID = (statementIdxs[i] + 2) % 3;
            IDs[i] = LoopCoverage.ID_PREFIX + primaryID + "-" + subID;
        }
        doIDTest(testCase, IDs, count);
    }

    private void doIDTest(org.codecover.model.TestCase testCase, String[] IDs, int[] count) {
        Assert.assertEquals(IDs.length, count.length);
        CoverableItem[] covItems = new CoverableItem[IDs.length];
        for (int i = 0; i < covItems.length; i++) {
            covItems[i] = this.builder.createCoverableItem(JAVA_FILE_UNDER_TEST, IDs[i]);
        }
        doCovItemTest(testCase, covItems, count);
    }

    private void doCovItemTest(org.codecover.model.TestCase testCase, CoverableItem[] covItems, int[] count) {
        Assert.assertEquals(covItems.length, count.length);
        for (int i = 0; i < covItems.length; i++) {
            Assert.assertEquals(count[i], testCase.getCoverageCount(covItems[i]));
        }
    }
}
