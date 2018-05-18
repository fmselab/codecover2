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

package org.codecover.junit;

import java.io.File;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJunit;
import org.codecover.instrumentation.measurement.CoverageResultLog;
import org.codecover.instrumentation.measurement.CoverageResultLogReader;
import org.codecover.instrumentation.measurement.MeasurementConstants;
import org.codecover.instrumentation.measurement.parser.CoverageLogParser;
import org.codecover.junit3.AllTests;
import org.codecover.junit3.ExamplePersonTest;
import org.codecover.junit3.ExampleProgramerTest;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.CoverableItem;

/**
 * This class can not be called from Eclipse. It needs the call of
 * <code>ant testgui</code> in the project folder to start the different
 * TestRunners, launch the tests and create coverage log files.
 * 
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: CoverageLogFileTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CoverageLogFileTest extends TestCase {

    public static final String EXPECTED_TSC_UUID = "e9754141-dfa8-41f5-8e14-02f5795fc346";

    public void testResults() throws Exception {
        Set<String> shortNames = AllTests.testRunners.keySet();
        Iterator<String> entryIterator = shortNames.iterator();
        while (entryIterator.hasNext()) {
            String thisShortName = entryIterator.next();
            checkCoverageLog(thisShortName, true);
            checkCoverageLog(thisShortName, false);
        }
    }

    private void checkCoverageLog(String shortName, boolean methods) throws Exception {
        String suffix = methods ? "m" : "";
        String coveragePath = UtilsForTestingJunit.TARGET + "coverage_log_" +
        shortName + suffix + ".clf";
        File coverageLogFile = new File(coveragePath);
        Assert.assertTrue(coverageLogFile.exists());
        Assert.assertTrue(UtilsForTestingJunit.TEST_SESSION_CONTAINER.exists());

        MASTBuilder mastBuilder = new MASTBuilder(UtilsForTestingJunit.LOGGER);
        TestSessionContainer tsc = TestSessionContainer.load(
                org.codecover.model.extensions.PluginManager.create(),
                UtilsForTestingJunit.LOGGER,
                mastBuilder,
                UtilsForTestingJunit.TEST_SESSION_CONTAINER);
        Assert.assertEquals("FIXED ID DOES NOT MATCH ANYMORE",
                EXPECTED_TSC_UUID, tsc.getId());
        TestSession testSession = tsc.createTestSession(shortName + suffix,
                "",
                new Date());

        CoverageResultLog crlReader = new CoverageResultLogReader(testSession,
                mastBuilder);
        CoverageLogParser logParser = new CoverageLogParser(coverageLogFile,
                MeasurementConstants.CHARSET);

        logParser.CompilationUnit(crlReader, tsc.getId());

        if (methods) {
            checkCoverageResultsMethods(testSession, mastBuilder);
        } else {
            checkCoverageResults(testSession, mastBuilder);
        }

        checkTestCases(testSession, methods);
    }

    private static void addToMap(Map<String, Long> target, Map<String, Long> source) {
        for (Entry<String, Long> thisEntry : source.entrySet()) {
            String ID = thisEntry.getKey();
            Long count = thisEntry.getValue();
            addValueToMap(target, ID, count);
        }
    }

    private static void addValueToMap(Map<String, Long> target, String ID, Long amount) {
        Long oldValue = target.get(ID);
        if (oldValue == null) {
            target.put(ID, amount);
        } else {
            target.put(ID, Long.valueOf(amount.longValue() + oldValue.longValue()));
        }
    }

    private static final Map<String, Long>[] COV_PERSON = new Map[16];
    private static final Map<String, Long>[] COV_PROGRAMMER = new Map[16];
    static {
        for (int i = 0; i < 16; i++) {
            COV_PERSON[i] = new HashMap<String, Long>();
        }
        // ExamplePersonTest
        // testGetFamilyName
        COV_PERSON[0].put("S1", Long.valueOf(1));
        COV_PERSON[0].put("S2", Long.valueOf(1));
        COV_PERSON[0].put("S11", Long.valueOf(1));

        // testSetFamilyName
        COV_PERSON[1].put("S1", Long.valueOf(1));
        COV_PERSON[1].put("S2", Long.valueOf(1));
        COV_PERSON[1].put("S3", Long.valueOf(1));
        COV_PERSON[1].put("S4", Long.valueOf(1));
        COV_PERSON[1].put("S11", Long.valueOf(2));

        // testSetFamilyName2
        COV_PERSON[2].put("S1", Long.valueOf(1));
        COV_PERSON[2].put("S2", Long.valueOf(1));
        COV_PERSON[2].put("S3", Long.valueOf(1));
        COV_PERSON[2].put("S4", Long.valueOf(1));
        COV_PERSON[2].put("S11", Long.valueOf(2));

        // testGetGivenName
        COV_PERSON[3].put("S1", Long.valueOf(1));
        COV_PERSON[3].put("S5", Long.valueOf(1));
        COV_PERSON[3].put("S11", Long.valueOf(1));

        // testSetGivenName
        COV_PERSON[4].put("S1", Long.valueOf(1));
        COV_PERSON[4].put("S5", Long.valueOf(1));
        COV_PERSON[4].put("S6", Long.valueOf(1));
        COV_PERSON[4].put("S7", Long.valueOf(1));
        COV_PERSON[4].put("S11", Long.valueOf(2));

        // testGetSalary
        COV_PERSON[5].put("S1", Long.valueOf(1));
        COV_PERSON[5].put("S8", Long.valueOf(1));
        COV_PERSON[5].put("S11", Long.valueOf(1));

        // testSetSalary
        COV_PERSON[6].put("S1", Long.valueOf(1));
        COV_PERSON[6].put("S8", Long.valueOf(1));
        COV_PERSON[6].put("S9", Long.valueOf(1));
        COV_PERSON[6].put("S10", Long.valueOf(1));
        COV_PERSON[6].put("S11", Long.valueOf(2));

        // testException
        COV_PERSON[7].put("S1", Long.valueOf(1));
        COV_PERSON[7].put("S2", Long.valueOf(1));
        COV_PERSON[7].put("S11", Long.valueOf(1));

        // ExampleProgramerTest
        // testGetFamilyName
        for (int i = 8; i < 16; i++) {
            // these are the calls from the ExampleProgrammer constructor
            addToMap(COV_PERSON[i], COV_PERSON[i - 8]);
            addValueToMap(COV_PERSON[i], "S11", Long.valueOf(1));
            addValueToMap(COV_PERSON[i], "S11", Long.valueOf(1));
            addValueToMap(COV_PERSON[i], "S3", Long.valueOf(1));
            addValueToMap(COV_PERSON[i], "S4", Long.valueOf(1));
            addValueToMap(COV_PERSON[i], "S11", Long.valueOf(1));
            addValueToMap(COV_PERSON[i], "S6", Long.valueOf(1));
            addValueToMap(COV_PERSON[i], "S7", Long.valueOf(1));
        }

        // testSetFamilyName
        addValueToMap(COV_PERSON[9], "S11", Long.valueOf(1));

        // testSetFamilyName2
        addValueToMap(COV_PERSON[10], "S11", Long.valueOf(1));

        // testSetSalary
        addValueToMap(COV_PERSON[14], "S11", Long.valueOf(1));

        for (int i = 0; i < 16; i++) {
            COV_PROGRAMMER[i] = new HashMap<String, Long>();
        }
        for (int i = 8; i < 16; i++) {
            // these are the calls from the ExampleProgrammer constructor
            COV_PROGRAMMER[i].put("S1", Long.valueOf(1));
            COV_PROGRAMMER[i].put("S2", Long.valueOf(1));
            COV_PROGRAMMER[i].put("S3", Long.valueOf(1));
        }

        // ExampleProgramerTest 
        // testSetFamilyName
        addValueToMap(COV_PROGRAMMER[9], "S4", Long.valueOf(1));
        addValueToMap(COV_PROGRAMMER[9], "S5", Long.valueOf(1));
        addValueToMap(COV_PROGRAMMER[9], "B2", Long.valueOf(1));
        addValueToMap(COV_PROGRAMMER[9], "S7", Long.valueOf(1));

        // testSetFamilyName2
        addValueToMap(COV_PROGRAMMER[10], "S4", Long.valueOf(1));
        addValueToMap(COV_PROGRAMMER[10], "S5", Long.valueOf(1));
        addValueToMap(COV_PROGRAMMER[10], "S6", Long.valueOf(1));
        addValueToMap(COV_PROGRAMMER[10], "B1", Long.valueOf(1));
        addValueToMap(COV_PROGRAMMER[10], "S7", Long.valueOf(1));

        // testSetSalary
        addValueToMap(COV_PROGRAMMER[14], "S8", Long.valueOf(1));
        addValueToMap(COV_PROGRAMMER[14], "S9", Long.valueOf(1));
    }

    private void checkCoverageResultsMethods(TestSession testSession, MASTBuilder mastBuilder) {
        List<org.codecover.model.TestCase> testCases = testSession.getTestCases();
        Assert.assertEquals(16, testCases.size());

        for (int i = 0; i < 16; i++) {
            org.codecover.model.TestCase tC = testCases.get(i);

            for (Entry<String, Long> thisEntry : COV_PERSON[i].entrySet()) {
                String ID = thisEntry.getKey();
                long count = thisEntry.getValue().longValue();
                CoverableItem covItem = mastBuilder.createCoverableItem(
                        org.codecover.classes.ExamplePerson.class.getName() + ".java", ID);
                Assert.assertEquals(i + " > " + testSession.getName() + " > " + tC.getName() + " > Person > " + ID,
                        count, tC.getCoverageCount(covItem));
            }

            for (Entry<String, Long> thisEntry : COV_PROGRAMMER[i].entrySet()) {
                String ID = thisEntry.getKey();
                long count = thisEntry.getValue().longValue();
                CoverableItem covItem = mastBuilder.createCoverableItem(
                        org.codecover.classes.ExampleProgramer.class.getName() + ".java", ID);
                Assert.assertEquals(i + " > " + testSession.getName() + " > " + tC.getName() + " > Programer > " + ID,
                        count, tC.getCoverageCount(covItem));
            }
        }
    }

    private static final Map<String, Long>[] TC_PERSON = new Map[2];
    private static final Map<String, Long>[] TC_PROGRAMMER = new Map[2];
    static {
        TC_PERSON[0] = new HashMap<String, Long>();
        TC_PROGRAMMER[0] = new HashMap<String, Long>();
        TC_PERSON[1] = new HashMap<String, Long>();
        TC_PROGRAMMER[1] = new HashMap<String, Long>();

        for (int i = 0; i < 8; i++) {
            addToMap(TC_PERSON[0], COV_PERSON[i]);
            addToMap(TC_PROGRAMMER[0], COV_PROGRAMMER[i]);
        }

        for (int i = 8; i < 16; i++) {
            addToMap(TC_PERSON[1], COV_PERSON[i]);
            addToMap(TC_PROGRAMMER[1], COV_PROGRAMMER[i]);
        }
    }

    private void checkCoverageResults(TestSession testSession, MASTBuilder mastBuilder) {
        List<org.codecover.model.TestCase> testCases = testSession.getTestCases();
        Assert.assertEquals(2, testCases.size());

        org.codecover.model.TestCase[] tCases = {
                testCases.get(0),
                testCases.get(1)
        };
        Assert.assertEquals(ExamplePersonTest.class.getName(), tCases[0].getName());
        Assert.assertEquals(ExampleProgramerTest.class.getName(), tCases[1].getName());

        for (int i = 0; i < 2; i++) {
            org.codecover.model.TestCase tCase = tCases[i];

            for (Entry<String, Long> thisEntry : TC_PERSON[i].entrySet()) {
                String ID = thisEntry.getKey();
                long count = thisEntry.getValue().longValue();
                CoverableItem covItem = mastBuilder.createCoverableItem(
                        org.codecover.classes.ExamplePerson.class.getName() + ".java", ID);
                Assert.assertEquals(i + " > " + testSession.getName() + " > " + tCase.getName() + " > Person > " + ID,
                        count, tCase.getCoverageCount(covItem));
            }

            for (Entry<String, Long> thisEntry : TC_PROGRAMMER[i].entrySet()) {
                String ID = thisEntry.getKey();
                long count = thisEntry.getValue().longValue();
                CoverableItem covItem = mastBuilder.createCoverableItem(
                        org.codecover.classes.ExampleProgramer.class.getName() + ".java", ID);
                Assert.assertEquals(i + " > " + testSession.getName() + " > " + tCase.getName() + " > Programer > " + ID,
                        count, tCase.getCoverageCount(covItem));
            }
        }
    }

    private void checkTestCases(TestSession testSession, boolean methods) {
        if (methods) {
            Assert.assertEquals(16, testSession.getTestCaseNames().size());
            Assert.assertEquals(16, testSession.getTestCases().size());
            org.codecover.model.TestCase testCase8 = testSession.getTestCaseWithName("org.codecover.junit3.ExamplePersonTest:testException");
            Assert.assertNotNull(testCase8);
            String comment = testCase8.getComment();
            Assert.assertNotNull(comment);
            Assert.assertTrue("unexpected start: \"" + comment + "\"", comment.startsWith("Error"));
        }
    }
}
