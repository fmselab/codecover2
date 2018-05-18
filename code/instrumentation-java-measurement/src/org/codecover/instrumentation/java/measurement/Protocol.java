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

package org.codecover.instrumentation.java.measurement;


/**
 * This is the protocol class for starting and ending test cases for coverage
 * measurement.<br>
 * <br>
 * For starting you can use either {@link #startTestCaseImpl(String, String)} or
 * {@link #startTestCaseImpl(String)}. For ending a test case you can use
 * either {@link #endTestCaseImpl(String, String)} or {@link #endTestCaseImpl()}.
 * If no starting or ending test case is recognized, all the coverage data is
 * measured&mdash;from the beginning till the end of the execution.<br>
 * If this class is used just for compiling the SUT without compiler errors and
 * simple execution, this class will do exactly nothing. If the SUT is
 * instrumented another protocol class is used in addition: {@link ProtocolImpl}.
 * That class replaces the single {@link #instance} of this class by an instance
 * of {@link ProtocolImpl}. So the {@link ProtocolImpl} instance is informed of
 * every start end ending of test cases.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: Protocol.java 1 2007-12-12 17:37:26Z t-scheller $)
 * @see ProtocolImpl
 */
public class Protocol {
    /**
     * The arguments for {@link #startJUnitTestCaseImpl(String, String)}.<br>
     * Needed for {@link Class#getMethod(String, Class[])}
     */
    public static final Class[] START_ARGUMENT = new Class[]{String.class,
        String.class};

    /**
     * The arguments for {@link #endJUnitTestCaseImpl(String, String, String)}.<br>
     * Needed for {@link Class#getMethod(String, Class[])}
     */
    public static final Class[] END_ARGUMENT = new Class[]{String.class,
        String.class, String.class};
    
    static Protocol instance = new Protocol();

    String testCaseStartedName = null;

    String testCaseStartedComment = null;

    String testCaseStartedClass = null;

    boolean isFinished = false;

    /**
     * The coverage measurement is booked on the test case with the given name.<br>
     * <br>
     * If another test case has been started before, but not ended, it is ended
     * implicitly.
     * 
     * @param name
     *            The name of the test case.
     */
    public static void startTestCase(String name) {
        instance.startTestCaseImpl(name);
    }

    /**
     * The coverage measurement is booked on the test case with the given name.<br>
     * <br>
     * If another test case has been started before, but not ended, it is ended
     * implicitly.
     * 
     * @param name
     *            The name of the test case.
     * @param comment
     *            A comment for the test case.
     */
    public static void startTestCase(String name, String comment) {
        instance.startTestCaseImpl(name, comment);
    }
    
    /**
     * The coverage measurement for the current test case with the given name is
     * finished.<br>
     * <br>
     * All counters are stored persistently. If the current test case hasn't got
     * the given name, this method call is ignored.
     * 
     * @param name
     *            The name of the test case.
     * @param resultComment
     *            A comment for the result of the test case. Can be <code>null</code>.
     */
    public static void endTestCase(String name, String resultComment) {
        instance.endTestCaseImpl(name, resultComment);
    }

    /**
     * The coverage measurement for the current test case with the given name is
     * finished.<br>
     * <br>
     * All counters are stored persistently. If the current test case hasn't got
     * the given name, this method call is ignored.
     * 
     * @param name
     *            The name of the test case.
     *            
     * @see #endTestCaseImpl(String, String)
     */
    public static void endTestCase(String name) {
        instance.endTestCaseImpl(name, null);
    }

    /**
     * The coverage measurement for the current test case is finished.<br>
     * <br>
     * All counters are stored persistently.
     * 
     * @see #endTestCaseImpl(String, String)
     */
    public static void endTestCase() {
        instance.endTestCaseImpl();
    }

    /**
     * The coverage measurement for a JUnit test case is started.<br>
     * <br>
     * This method should only be called by a CodeCover TestRunner.
     * 
     * @param jUnitTestClassName
     *            The class of the test case.
     * @param testName
     *            The name of the method of the JUnit TestCase.
     * 
     */
    public static void startJUnitTestCase(String jUnitTestClassName, String testName) {
        instance.startJUnitTestCaseImpl(jUnitTestClassName, testName);
    }

    /**
     * The coverage measurement for a JUnit test case has ended.<br>
     * <br>
     * This method should only be called by a CodeCover TestRunner.
     * 
     * @param jUnitTestClassName
     *            The class of the test case.
     * @param testName
     *            The name of the method of the JUnit TestCase.
     * @param resultComment
     *            A comment for the result of the test case&mdash;e.g. JUnit.
     */
    public static void endJUnitTestCase(String jUnitTestClassName, String testName,
            String resultComment) {
        instance.endJUnitTestCaseImpl(jUnitTestClassName, testName, resultComment);
    }

    /**
     * Finishes the whole test session, writes the coverage log file and accepts
     * no more test cases.
     */
    public static void finishTestSession() {
        instance.finishImpl();
    }

    /**
     * The coverage measurement is booked on the test case with the given name.<br>
     * <br>
     * If another test case has been started before, but not ended, it is ended
     * implicitly.
     * 
     * @param name
     *            The name of the test case.
     * @param comment
     *            A comment for the test case.
     */
    protected void startTestCaseImpl(String name, String comment) {
        if (this.testCaseStartedClass != null) {
            // JUnit test cases have priority
            return;
        }
        // just log this to reuse it later by ProtocolImpl
        this.testCaseStartedName = name;
        this.testCaseStartedComment = comment;
    }

    /**
     * The coverage measurement is booked on the test case with the given name.<br>
     * <br>
     * If another test case has been started before, but not ended, it is ended
     * implicitly.
     * 
     * @param name
     *            The name of the test case.
     */
    protected void startTestCaseImpl(String name) {
        startTestCaseImpl(name, null);
    }

    /**
     * The coverage measurement for the current test case with the given name is
     * finished.<br>
     * <br>
     * All counters are stored persistently. If the current test case hasn't got
     * the given name, this method call is ignored.
     * 
     * @param name
     *            The name of the test case.
     * @param resultComment
     *            A comment for the result of the test case&mdash;e.g. JUnit.
     */
    protected void endTestCaseImpl(String name, String resultComment) {
        if (this.testCaseStartedClass != null) {
            // JUnit test cases have priority
            return;
        }
        if (name.equals(this.testCaseStartedName)) {
            // reset the logged Strings
            this.testCaseStartedName = null;
            this.testCaseStartedComment = null;
        }
    }

    /**
     * The coverage measurement for the current test case is finished.<br>
     * <br>
     * All counters are stored persistently.
     */
    protected void endTestCaseImpl() {
        if (this.testCaseStartedClass != null) {
            // JUnit test cases have priority
            return;
        }
        // reset the logged Strings
        this.testCaseStartedName = null;
        this.testCaseStartedComment = null;
    }

    /**
     * The coverage measurement for a JUnit test case is started.<br>
     * <br>
     * This method should only be called by a CodeCover TestRunner.
     * 
     * @param jUnitTestClassName
     *            The class of the test case.
     * @param testName
     *            The name of the method of the JUnit TestCase.
     */
    protected void startJUnitTestCaseImpl(String jUnitTestClassName, String testName) {
        // forget, that we have started a test case by name
        endTestCaseImpl();
        // just log this to reuse it later by ProtocolImpl
        this.testCaseStartedClass = jUnitTestClassName;
        this.testCaseStartedName = testName;
    }

    /**
     * The coverage measurement for a JUnit test case has ended.<br>
     * <br>
     * This method should only be called by a CodeCover TestRunner.
     * 
     * @param jUnitTestClassName
     *            The class of the test case.
     * @param testName
     *            The name of the method of the JUnit TestCase.
     * @param resultComment
     *            A comment for the result of the test case&mdash;e.g. JUnit.
     */
    protected void endJUnitTestCaseImpl(String jUnitTestClassName, String testName,
            String resultComment) {
        // reset the logged class
        this.testCaseStartedClass = null;
        this.testCaseStartedName = null;
    }

    /**
     * Finishes the whole test session, writes the coverage log file and accepts
     * no more test cases.
     */
    protected void finishImpl() {
        this.isFinished = true;
    }
}
