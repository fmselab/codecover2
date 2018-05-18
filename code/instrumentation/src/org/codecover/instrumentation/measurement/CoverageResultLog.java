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

/**
 * This is an interface where coverage results can be written to. It extends the
 * interface {@link CoverageCounterLog}<br>
 * <br>
 * For example, this interface can be implemented as a coverage log file.<br>
 * This log accepts the start and the end of a named test case, the start of a
 * section and long counters. All accesses to this log file must be processed
 * after the {@link #startLog()} and before {@link #closeLog()}.<br>
 * Examples for implementation:
 * <ul>
 * <li>CoverageResultLogFile</li>
 * <li>NullCoverageLog</li>
 * </ul>
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: CoverageResultLog.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface CoverageResultLog extends CoverageCounterLog {

    /**
     * This is the name of static method all children will have to get the single
     * instance.<br>
     * <br>
     * This method has to have a single String parameter for further
     * configuration.
     */
    public static final String GET_INSTANCE_METHOD_NAME = "getInstance";

    /**
     * Starts the logging.
     * 
     */
    public void startLog();

    /**
     * Notifies the start of a test case.
     * 
     * @param testSessionContainerUID
     *            The UID of the TestSessionContainer. 
     * @param testCaseName
     *            The name of the test case.
     *            
     * @see #startTestCase(String, String, long, String)
     */
    public void startTestCase(String testSessionContainerUID, String testCaseName);

    /**
     * Notifies the start of a test case.
     * 
     * @param testSessionContainerUID
     *            The UID of the TestSessionContainer.
     * @param testCaseName
     *            The name of the test case.
     * @param timestamp 
     *            A long representing a timestamp of the start of the test case.
     *            
     * @see #startTestCase(String, String, long, String)
     */
    public void startTestCase(String testSessionContainerUID, String testCaseName,
            long timestamp);

    /**
     * Notifies the start of a test case.
     * 
     * @param testSessionContainerUID
     *            The UID of the TestSessionContainer.
     * @param testCaseName
     *            The name of the test case.
     * @param comment
     *            An comment for the test case.
     *            
     * @see #startTestCase(String, String, long, String)
     */
    public void startTestCase(String testSessionContainerUID, String testCaseName,
            String comment);

    /**
     * Notifies the start of a test case.
     * 
     * If timestamp is -1 then timestamp will be ignored. Same as 
     * {@link CoverageResultLog#startTestCase(String, String, String)}.
     * 
     * If comment is null then comment will be ignored. Same as
     * {@link CoverageResultLog#startTestCase(String, String, long)}.
     * 
     * @param testSessionContainerUID
     *            The UID of the TestSessionContainer.
     * @param testCaseName
     *            The name of the test case.
     * @param timestamp 
     *            A long representing a timestamp of the start of the test case.
     * @param comment
     *            An comment for the test case.
     */
    public void startTestCase(String testSessionContainerUID, String testCaseName,
            long timestamp, String comment);

    /**
     * Notifies the end of the test case.
     * 
     * @param testCaseName
     *            The name of the test case.
     */
    public void endTestCase(String testCaseName);

    /**
     * Notifies the end of the test case.
     * 
     * If timestamp is -1 then timestamp will be ignored. Same as 
     * {@link CoverageResultLog#endTestCase(String)}.
     * 
     * @param testCaseName
     *            The name of the test case.
     * @param timestamp 
     *            A long representing a timestamp of the end of the test case.
     */
    public void endTestCase(String testCaseName, long timestamp);

    /**
     * Notifies the end of a JUnit test case.
     * 
     * If timestamp is -1 then timestamp will be ignored. Same as 
     * {@link CoverageResultLog#endTestCase(String)}.
     * 
     * @param testCaseName
     *            The name of the test case.
     * @param timestamp 
     *            A long representing a timestamp of the end of the test case.
     * @param resultComment 
     *            A comment for the result of the test case&mdash;e.g. JUnit.
     */
    public void endTestCase(String testCaseName, long timestamp,
            String resultComment);

    /**
     * Puts out a comment line.
     * 
     * @param comment
     *            A possible comment.
     */
    public void lineComment(String comment);
    
    /**
     * Closes the logging.
     */
    public void closeLog();
}
