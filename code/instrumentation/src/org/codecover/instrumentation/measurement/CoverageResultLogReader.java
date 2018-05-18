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

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.codecover.model.MASTBuilder;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.BooleanAssignment;
import org.codecover.model.mast.BooleanAssignmentMap;
import org.codecover.model.mast.BooleanResult;
import org.codecover.model.mast.CoverableItem;
import org.codecover.model.utils.criteria.ConditionCoverage;

/**
 * This is an implementation of {@link CoverageResultLog} intended to fill the
 * given {@link TestSession} with the data collected during the parsing of a
 * file containing measured coverage data.
 * 
 * @author Markus Wittlinger
 * 
 * @version 1.0 ($Id: CoverageResultLogReader.java 1 2007-12-12 17:37:26Z t-scheller $)
 * @see CoverageResultLog
 * 
 * TODO write tests
 */
public class CoverageResultLogReader implements CoverageResultLog {

    private final TestSession testSession;

    private Map<CoverableItem, Long> currentTestCaseCoverageData;

    private Map<CoverableItem, Map<BooleanAssignment, Long>> currentTestCaseAssignments;

    private String currentTestCaseName;

    private StringBuilder currentTestCaseComment;

    private Date currentTestCaseStart;

    private String currentCounterIdPrefix = null;

    private String foundTestSessionContainerUID = null;

    private final MASTBuilder builder;

    /**
     * Constructor
     * 
     * @param testSession
     *            the test session which will contain the test cases
     * @param builder
     *            a {@link MASTBuilder} used to create {@link CoverableItem}s
     *            in {@link #passCounter(String, long)}.
     */
    public CoverageResultLogReader(TestSession testSession, MASTBuilder builder) {
        this.testSession = testSession;
        this.builder = builder;
    }

    /**
     * @return The {@link #foundTestSessionContainerUID}.
     */
    public String getTestSessionContainerUID() {
        return this.foundTestSessionContainerUID;
    }

    private static Map<CoverableItem, BooleanAssignmentMap> createAssignmentMaps(
            Map<CoverableItem, Map<BooleanAssignment, Long>> data) {
        final Map<CoverableItem, BooleanAssignmentMap> result;
        result = new HashMap<CoverableItem, BooleanAssignmentMap>();

        for (Map.Entry<CoverableItem, Map<BooleanAssignment, Long>> entry : data
                .entrySet()) {

            if (entry.getValue().size() == 0) {
                // The map is empty, do nothing
            } else {
                final BooleanAssignment firstAssignment = entry.getValue()
                        .keySet().iterator().next();

                result.put(entry.getKey(), new BooleanAssignmentMap(
                        firstAssignment.getLength(), entry.getValue()));
            }
        }
        return result;
    }

    /**
     * Does nothing
     * 
     * @param comment
     *            the given comment
     */
    public void lineComment(String comment) {
        // just empty implementation
    }

    /**
     * Starts the logging.
     */
    public void startLog() {
        // needs to do nothing at all
    }

    /**
     * Calls
     * {@link CoverageResultLogReader#startTestCase(String, String, String)}
     * with an empty {@link String} as comment
     * 
     * @param testSessionContainerUID
     *            The UID of the {@link TestSessionContainer}.
     * @param testCaseName
     *            the given test case name
     */
    public void startTestCase(String testSessionContainerUID,
            String testCaseName) {
        startTestCase(testSessionContainerUID, testCaseName, "");
    }

    /**
     * Calls
     * {@link CoverageResultLogReader#startTestCase(String, String, long, String)}
     * with an empty {@link String} as comment
     * 
     * @param testSessionContainerUID
     *            The UID of the {@link TestSessionContainer}.
     * @param testCaseName
     *            the given test case name
     * @param timestamp
     *            the given time in milliseconds
     */
    public void startTestCase(String testSessionContainerUID,
            String testCaseName, long timestamp) {
        startTestCase(testSessionContainerUID, testCaseName, timestamp, "");
    }

    /**
     * Calls
     * {@link CoverageResultLogReader#startTestCase(String, String, long, String)}
     * with the current time in milliseconds.
     * 
     * @param testSessionContainerUID
     *            The UID of the {@link TestSessionContainer}.
     * @param testCaseName
     *            the given test case name
     * @param comment
     *            the given comment
     */
    public void startTestCase(String testSessionContainerUID,
            String testCaseName, String comment) {
        startTestCase(testSessionContainerUID, testCaseName,
                new Date().getTime(), comment);
    }

    /**
     * Initialises and sets the member variables holding the data of the
     * {@link TestCase}.
     * 
     * @param testSessionContainerUID
     *            The UID of the {@link TestSessionContainer}.
     * @param testCaseName
     *            the given test case name
     * @param timestamp
     *            the given time in milliseconds
     * @param comment
     *            the given comment
     */
    public void startTestCase(String testSessionContainerUID,
            String testCaseName,
            long timestamp,
            String comment) {
        testSessionContainerUIDFound(testSessionContainerUID);

        this.currentTestCaseName = testCaseName;
        this.currentTestCaseComment = new StringBuilder(comment == null ? "" : comment);
        this.currentTestCaseStart = (timestamp == -1 ? new Date() : new Date(timestamp));
        this.currentTestCaseAssignments = new HashMap<CoverableItem, Map<BooleanAssignment, Long>>();
        this.currentTestCaseCoverageData = new HashMap<CoverableItem, Long>();
    }

    /**
     * Checks, if the {@link #foundTestSessionContainerUID} is null. If not, it
     * is assigned with the parameter. If it is no null it is compared to the
     * found UID.
     * 
     * @param testSessionContainerUID
     *            The UID of the {@link TestSessionContainer}.
     */
    private void testSessionContainerUIDFound(String testSessionContainerUID) {
        if (this.foundTestSessionContainerUID == null) {
            this.foundTestSessionContainerUID = testSessionContainerUID;
        } else if (!this.foundTestSessionContainerUID.equals(testSessionContainerUID)) {
            throw new IllegalStateException("Different testSessionUIDs found");
        }
    }

    /**
     * Calls {@link CoverageResultLogReader#endTestCase(String, long)} with the
     * current time in milliseconds
     * 
     * @param testCaseName
     *            the given test case name
     */
    public void endTestCase(String testCaseName) {
        endTestCase(testCaseName, new Date().getTime(), null);
    }

    /**
     * Creates the testCase with the previously collected data
     * 
     * @param testCaseName
     *            the given test case name
     * @param timestamp
     *            the given time in milliseconds
     */
    public void endTestCase(String testCaseName, long timestamp) {
        endTestCase(testCaseName, timestamp, null);
    }

    /**
     * Creates the testCase with the previously collected data
     * 
     * @param testCaseName
     *            the given test case name
     * @param timestamp
     *            the given time in milliseconds
     * @param resultComment 
     *            the comment for the result of the test case&mdash;e.g. JUnit
     */
    public void endTestCase(String testCaseName, long timestamp, String resultComment) {
        if (!this.currentTestCaseName.equals(testCaseName)) {
            throw new IllegalStateException("Test case names differ: " +
                    this.currentTestCaseName + " vs. " + testCaseName);
        }
        if (resultComment != null && resultComment.length() > 0) {
            if (this.currentTestCaseComment.length() > 0) {
                this.currentTestCaseComment.append('\n');
            }
            this.currentTestCaseComment.append(resultComment);
        }
        this.testSession.createTestCase(this.currentTestCaseName,
                this.currentTestCaseComment.toString(),
                this.currentTestCaseStart,
                this.currentTestCaseCoverageData,
                createAssignmentMaps(this.currentTestCaseAssignments));
    }

    /**
     * Saves the given counter value under the given id. The current id prefix
     * is added to the id.
     * <p>
     * If the counter carries the prefix of a counter used for the
     * {@link ConditionCoverage}, the assignment encoded in the id is extracted
     * and saved in the test case data.
     * 
     * @param counterID
     *            the id of the counter
     * @param counterValue
     *            the value of the counter
     */
    public void passCounter(String counterID, long counterValue) {
        String prefix = (this.currentCounterIdPrefix == null ? ""
                : this.currentCounterIdPrefix);
        if (counterID.startsWith(ConditionCoverage.ID_PREFIX)) {
            String rootTermIdSubstring = counterID.substring(0, counterID
                    .indexOf(MeasurementConstants.ID_ASSIGNMENT_SEPERATOR));

            String assignmentSubstring = counterID.substring(counterID
                    .indexOf(MeasurementConstants.ID_ASSIGNMENT_SEPERATOR) + 1);

            CoverableItem coverableItem = this.builder.createCoverableItem(
                    prefix, rootTermIdSubstring);

            Map<BooleanAssignment, Long> subMap = this.currentTestCaseAssignments
                    .get(coverableItem);
            if (subMap == null) {
                subMap = new HashMap<BooleanAssignment, Long>();
            }

            // Extract the boolean assignment encoded in the counter id
            BooleanAssignment currentAssignment = getBooleanAssignmentFromString(assignmentSubstring);

            if (counterValue == 0) {
                // we can skip a value of 0
            } else {
                subMap.put(currentAssignment, Long.valueOf(counterValue));
            }

            this.currentTestCaseAssignments.put(coverableItem, subMap);

        } else {
            CoverableItem coverableItem = this.builder.createCoverableItem(
                    prefix, counterID);
            Long currentValue = this.currentTestCaseCoverageData
                    .get(coverableItem);
            if (currentValue == null) {
                currentValue = new Long(0);
            }
            Long newValue = Long.valueOf(currentValue.longValue()
                    + counterValue);

            this.currentTestCaseCoverageData.put(coverableItem, newValue);
        }
    }

    /**
     * Sets the id prefix to the given sectionName
     * 
     * @param sectionName
     */
    public void startNamedSection(String sectionName) {
        this.currentCounterIdPrefix = sectionName;
    }

    /**
     * Extracts the {@link BooleanAssignment} from the given {@link String}.
     * <p>
     * The length {@link String} must be an even number
     * 
     * @param assignmentString
     *            the {@link String}, which holds the encoded
     *            {@link BooleanAssignment}
     * @return the extracted {@link BooleanAssignment}
     */
    public static BooleanAssignment getBooleanAssignmentFromString(
            String assignmentString) {

        if (assignmentString.length() % 2 != 0) {
            throw new RuntimeException(
                    "Length of assignmentString must be even");
        }

        List<BooleanResult> results = new Vector<BooleanResult>();

        // Traverse the string and check the bits encoded in it. Two characters
        // contain the information for one BooleanResult. The first bit holds,
        // whether or not the BasicBooleanTerm was evaluated, while the second
        // bit holds the boolean value the BasicBooleanTerm evaluated to.
        for (int i = 0; i < assignmentString.length(); i += 2) {
            if (assignmentString.charAt(i) == '0') {
                results.add(BooleanResult.NOT_EVALUATED);
            } else if (assignmentString.charAt(i + 1) == '0') {
                results.add(BooleanResult.FALSE);
            } else if (assignmentString.charAt(i + 1) == '1') {
                results.add(BooleanResult.TRUE);
            }
        }

        return new BooleanAssignment(results);
    }

    /**
     * Does nothing
     */
    public void closeLog() {
        // just empty implementation
    }
}
