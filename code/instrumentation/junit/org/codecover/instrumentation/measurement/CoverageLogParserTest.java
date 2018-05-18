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

package org.codecover.instrumentation.measurement;

import static org.codecover.UtilsForTestingInstr.SOURCE;
import static org.codecover.UtilsForTestingInstr.handleException;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.util.Vector;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingInstr;
import org.codecover.instrumentation.measurement.parser.CoverageLogParser;
import org.codecover.instrumentation.measurement.parser.ParseException;
import org.codecover.instrumentation.measurement.parser.WrongUIDException;

/**
 * @author Stefan Franke
 * @version 1.0 ($Id: CoverageLogParserTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CoverageLogParserTest extends TestCase {


    public void testCoverageLogParserMain() throws IOException {
        CoverageLogParser coverageLogParser;
        String expectedTestSessionContainerUID;
        CoverageResultLogOracle coverageResultLogSystem;

        try {
            String srcPath = SOURCE + "coverage_log_for_CoverageLogParserTest.clf";
            File source = new File(srcPath);
            coverageLogParser = new CoverageLogParser(source);
            expectedTestSessionContainerUID = "e86b0228-85cc-41f2-b42b-5e99e5d63fd2";
            coverageResultLogSystem = new CoverageResultLogOracle(expectedTestSessionContainerUID);
            coverageLogParser.CompilationUnit(
                    coverageResultLogSystem,
                    expectedTestSessionContainerUID);
        } catch (WrongUIDException e) {
            handleException(e);
        } catch (ParseException e) {
            handleException(e);
        }
    }

    public void testUnicodeSupport() throws Exception {
        CoverageLogParser coverageLogParser;
        String expectedTestSessionContainerUID;
        CoverageResultLogOracle coverageResultLogSystem;
        StringReader reader = new StringReader(
                "TEST_SESSION_CONTAINER \"e86b0228-85cc-41f2-b42b-5e99e5d63fd2\"\n" +
                "START_TEST_CASE \"Select project for coverage measurement \u2013 non-Java project\" 1179076343191\n" +
                "END_TEST_CASE \"Select project for coverage measurement \u2013 non-Java project\" 1179076343972\n");

        try {
            coverageLogParser = new CoverageLogParser(reader);
            coverageLogParser.CompilationUnit(NullCoverageLog.INSTANCE, "e86b0228-85cc-41f2-b42b-5e99e5d63fd2");
        } catch (WrongUIDException e) {
            handleException(e);
        } catch (ParseException e) {
            handleException(e);
        }
    }

    class CoverageResultLogOracle implements CoverageResultLog {
        
        private static final String TEST_SESSION_CONTAINER = "TEST_SESSION_CONTAINER";

        private static final String START_TEST_CASE = "START_TEST_CASE";

        private static final String START_SECTION = "START_SECTION";

        private static final String END_TEST_CASE = "END_TEST_CASE";

        private Vector<Object> vector;

        private int counter;

        public CoverageResultLogOracle(String testSessionContainerUID) {
            this.counter = 0;
            this.vector = new Vector<Object>();
            this.vector.add(TEST_SESSION_CONTAINER);
            this.vector.add(testSessionContainerUID);
            this.vector.add(START_TEST_CASE);
            this.vector.add("my Name is \"main\"");
            this.vector.add(Long.valueOf("1179076343191"));
            this.vector.add(null);
            this.vector.add(START_SECTION);
            this.vector.add("org.codecover.instrument.java15.test.test3.CodeExample");
            this.vector.add("S2");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S4");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S5");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S6");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S7");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S8");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S9");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S10");
            this.vector.add(Long.valueOf(3));
            this.vector.add("S11");
            this.vector.add(Long.valueOf(3));
            this.vector.add("S12");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S13");
            this.vector.add(Long.valueOf(2));
            this.vector.add("S14");
            this.vector.add(Long.valueOf(2));
            this.vector.add("S15");
            this.vector.add(Long.valueOf(40));
            this.vector.add("S16");
            this.vector.add(Long.valueOf(40));
            this.vector.add("S17");
            this.vector.add(Long.valueOf(38));
            this.vector.add("S18");
            this.vector.add(Long.valueOf(2));
            this.vector.add("S27");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S28");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S29");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S30");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S31");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S32");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S33");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S36");
            this.vector.add(Long.valueOf(3));
            this.vector.add("S37");
            this.vector.add(Long.valueOf(3));
            this.vector.add("S38");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S39");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S40");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S41");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S42");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S43");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S44");
            this.vector.add(Long.valueOf(3));
            this.vector.add("S45");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S46");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S47");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S48");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S49");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S50");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S51");
            this.vector.add(Long.valueOf(243));
            this.vector.add("S52");
            this.vector.add(Long.valueOf(243));
            this.vector.add("S53");
            this.vector.add(Long.valueOf(243));
            this.vector.add("S54");
            this.vector.add(Long.valueOf(243));
            this.vector.add("S55");
            this.vector.add(Long.valueOf(243));
            this.vector.add("S56");
            this.vector.add(Long.valueOf(243));
            this.vector.add("S57");
            this.vector.add(Long.valueOf(243));
            this.vector.add("S58");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S59");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S60");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S61");
            this.vector.add(Long.valueOf(3));
            this.vector.add("S62");
            this.vector.add(Long.valueOf(244));
            this.vector.add("S63");
            this.vector.add(Long.valueOf(244));
            this.vector.add("S64");
            this.vector.add(Long.valueOf(244));
            this.vector.add("S65");
            this.vector.add(Long.valueOf(244));
            this.vector.add("S66");
            this.vector.add(Long.valueOf(2818203));
            this.vector.add("S67");
            this.vector.add(Long.valueOf(244));
            this.vector.add("S68");
            this.vector.add(Long.valueOf(244));
            this.vector.add("S70");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S72");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B2");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B3");
            this.vector.add(Long.valueOf(2));
            this.vector.add("B4");
            this.vector.add(Long.valueOf(38));
            this.vector.add("B5");
            this.vector.add(Long.valueOf(2));
            this.vector.add("B10");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B11");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B15");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B16");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B17");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B18");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B19");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B20");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C1-1010");
            this.vector.add(Long.valueOf(2));
            this.vector.add("C1-1100");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C2-10");
            this.vector.add(Long.valueOf(2));
            this.vector.add("C2-11");
            this.vector.add(Long.valueOf(40));
            this.vector.add("C3-10");
            this.vector.add(Long.valueOf(2));
            this.vector.add("C3-11");
            this.vector.add(Long.valueOf(38));
            this.vector.add("C4-11");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C5-111111");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C7-10");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C7-11");
            this.vector.add(Long.valueOf(242));
            this.vector.add("C8-10");
            this.vector.add(Long.valueOf(244));
            this.vector.add("C8-11");
            this.vector.add(Long.valueOf(2818203));
            this.vector.add("L1-2");
            this.vector.add(Long.valueOf(1));
            this.vector.add("L2-2");
            this.vector.add(Long.valueOf(2));
            this.vector.add("L3-2");
            this.vector.add(Long.valueOf(1));
            this.vector.add("L4-0");
            this.vector.add(Long.valueOf(15));
            this.vector.add("L4-1");
            this.vector.add(Long.valueOf(8));
            this.vector.add("L4-2");
            this.vector.add(Long.valueOf(221));
            this.vector.add(END_TEST_CASE);
            this.vector.add("my Name is \"main\"");
            this.vector.add(Long.valueOf(1179076343972l));
            this.vector.add(null);
            this.vector.add(TEST_SESSION_CONTAINER);
            this.vector.add(testSessionContainerUID);
            this.vector.add(START_TEST_CASE);
            this.vector.add("Second Inner Class");
            this.vector.add(Long.valueOf(-1));
            this.vector.add("Contains some\n\"special statements\"");
            this.vector.add(START_SECTION);
            this.vector.add("org.codecover.instrument.java15.test.test3.CodeExample");
            this.vector.add("S20");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S21");
            this.vector.add(Long.valueOf(1));
            this.vector.add(START_SECTION);
            this.vector.add("org.codecover.instrument.java15.test.test3.SecondClassOfFile");
            this.vector.add("S1");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S2");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S3");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S4");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S5");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S6");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S7");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S8");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S9");
            this.vector.add(Long.valueOf(2));
            this.vector.add("S10");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S11");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S12");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S14");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S15");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S16");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S17");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S18");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S19");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S20");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S22");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S23");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S24");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S26");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S27");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S28");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S29");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S30");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S32");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S33");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S34");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S35");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S36");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S37");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S38");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S39");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S40");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S41");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S42");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S43");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S44");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S45");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S46");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S48");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S49");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S51");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S52");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S53");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S54");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S56");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S57");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S58");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S59");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S60");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S61");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S63");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S64");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S65");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S66");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S67");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S69");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S70");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S71");
            this.vector.add(Long.valueOf(31));
            this.vector.add("S72");
            this.vector.add(Long.valueOf(31));
            this.vector.add("S73");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S74");
            this.vector.add(Long.valueOf(31));
            this.vector.add("S75");
            this.vector.add(Long.valueOf(31));
            this.vector.add("S76");
            this.vector.add(Long.valueOf(998));
            this.vector.add("S77");
            this.vector.add(Long.valueOf(990));
            this.vector.add("S79");
            this.vector.add(Long.valueOf(8));
            this.vector.add("S81");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S82");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S83");
            this.vector.add(Long.valueOf(1));
            this.vector.add("S84");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B1");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B4");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B5");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B8");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B9");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B11");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B14");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B15");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B17");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B19");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B21");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B23");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B25");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B27");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B30");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B31");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B34");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B37");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B39");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B41");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B42");
            this.vector.add(Long.valueOf(30));
            this.vector.add("B43");
            this.vector.add(Long.valueOf(31));
            this.vector.add("B45");
            this.vector.add(Long.valueOf(990));
            this.vector.add("B46");
            this.vector.add(Long.valueOf(8));
            this.vector.add("B47");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C1-10");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C1-11");
            this.vector.add(Long.valueOf(2));
            this.vector.add("C2-10");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C3-10");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C6-11");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C7-11");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C8-1111");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C9-1100");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C10-1111");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C11-1111");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C12-1111");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C13-111100");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C14-110011");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C15-11");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C16-1111");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C17-1111");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C18-1111");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C19-11");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C20-10");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C20-11");
            this.vector.add(Long.valueOf(31));
            this.vector.add("C21-10101010101010101010101010101010");
            this.vector.add(Long.valueOf(30));
            this.vector.add("C21-11101110111000000000000000000000");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C22-1010101010101010101010101010101011");
            this.vector.add(Long.valueOf(30));
            this.vector.add("C22-1110000000000000000000000000000000");
            this.vector.add(Long.valueOf(1));
            this.vector.add("C23-10");
            this.vector.add(Long.valueOf(8));
            this.vector.add("C23-11");
            this.vector.add(Long.valueOf(990));
            this.vector.add("C24-11");
            this.vector.add(Long.valueOf(1));
            this.vector.add("L1-2");
            this.vector.add(Long.valueOf(1));
            this.vector.add("L2-1");
            this.vector.add(Long.valueOf(1));
            this.vector.add("L3-0");
            this.vector.add(Long.valueOf(1));
            this.vector.add("L4-2");
            this.vector.add(Long.valueOf(1));
            this.vector.add(END_TEST_CASE);
            this.vector.add("Second Inner Class");
            this.vector.add(Long.valueOf(1179076344032l));
            this.vector.add("A multiline\ncomment");
            this.vector.add(TEST_SESSION_CONTAINER);
            this.vector.add(testSessionContainerUID);
            this.vector.add(START_TEST_CASE);
            this.vector.add("try catch of CodeExample");
            this.vector.add(Long.valueOf(1179076344032l));
            this.vector.add(null);
            this.vector.add(START_SECTION);
            this.vector.add("org.codecover.instrument.java15.test.test3.CodeExample");
            this.vector.add("S22");
            this.vector.add(Long.valueOf(1));
            this.vector.add("B1");
            this.vector.add(Long.valueOf(1));
            this.vector.add(START_SECTION);
            this.vector.add("org.codecover.instrument.java15.test.test3.SecondClassOfFile");
            this.vector.add(END_TEST_CASE);
            this.vector.add("try catch of CodeExample");
            this.vector.add(Long.valueOf(-1));
            this.vector.add("A comment with a\ttab.");
        }

        private void checkToken(String description, Object expectedObject) {
            checkCounterInBounts();
            assertEquals(description, this.vector.get(this.counter), expectedObject);
            this.counter++;
        }

        private void checkCounterInBounts() {
            boolean check = this.counter <= this.vector.size();
            assertTrue("Parsed more elements than stated in test case.", check);
        }

        public void closeLog() {
            assertTrue(this.counter == this.vector.size());
        }

        public void endTestCase(String testCaseName) {
            fail("This method call was not expected.");
        }

        public void endTestCase(String testCaseName, long timestamp) {
            fail("This method call was not expected.");
        }

        public void endTestCase(String testCaseName, long timestamp, String resultComment) {
            checkToken("A start test case was encountered.", END_TEST_CASE);
            checkToken("A test case name was encountered.", testCaseName);
            checkToken("A timestamp was encountered.", Long.valueOf(timestamp));
            checkToken("A result comment was encountered.", resultComment);
        }

        public void lineComment(String comment) {
            fail("This method call was not expected.");
        }

        public void startLog() {
            checkCounterInBounts();
        }

        public void startTestCase(String testSessionContainerUID, String testCaseName) {
            fail("This method call was not expected.");
        }

        public void startTestCase(String testSessionContainerUID, String testCaseName,
                long timestamp) {
            fail("This method call was not expected.");
        }

        public void startTestCase(String testSessionContainerUID, String testCaseName, 
                String comment) {
            fail("This method call was not expected.");
        }

        public void startTestCase(String testSessionContainerUID, String testCaseName,
                long timestamp, String comment) {

            checkToken("TEST_SESSION_CONTAINER was encountered.", TEST_SESSION_CONTAINER);
            checkToken("A test session container UID was encountered.", testSessionContainerUID);
            checkToken("A start test case was encountered.", START_TEST_CASE);
            checkToken("A test case name was encountered.", testCaseName);
            checkToken("A timestamp was encountered.", Long.valueOf(timestamp));
            checkToken("A test case comment was encountered.", comment);
        }

        public void passCounter(String counterID, long counterValue) {
            checkToken("A counter name was encountered.", counterID);
            checkToken("A counter value was encountered.", Long.valueOf(counterValue));
        }

        public void startNamedSection(String sectionName) {
            checkToken("A start section was encountered.", START_SECTION);
            checkToken("A section name was encountered.", sectionName);
        }
    }
}
