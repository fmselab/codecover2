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

package org.codecover.junit3.text;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestResult;
import junit.framework.TestSuite;
import junit.runner.BaseTestRunner;

import org.codecover.junit.HelperMethods;
import org.codecover.junit3.JUnitResultListener;
import org.codecover.junit3.JUnitResultListenerMethod;
import org.codecover.junit3.JUnitResultListenerTestCase;

/**
 * This is a CodeCover TestRunner which encapsulates a
 * {@link junit.textui.TestRunner} of JUnit 3.8.x.<br>
 * <br>
 * Usage from commandline:
 * 
 * <pre>
 * org.codecover.junit3.text.TestRunner [-wait] [-methodsAsTestCases] (&lt;Testclass&gt;|-c &lt;Testclass in VA/Java style&gt;)
 * </pre>
 * 
 * The <code>&lt;Testclass.class&gt;</code> can either be a {@link TestSuite}
 * or a {@link TestCase}.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: TestRunner.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @see junit.textui.TestRunner
 * @see BaseTestRunner
 */
public class TestRunner extends junit.textui.TestRunner {

    private boolean useMethodsAsTestcases = false;

    private JUnitResultListener usedListener;

    /**
     * Starts a new {@link TestRunner} with the given arguments.
     * 
     * @param args
     *            see {@link TestRunner} for description.
     *            
     * @see junit.textui.TestRunner#main(String[]) Copied from there.
     */
    public static void main(String args[]) {
        boolean useMethodsAsTestcases = HelperMethods.useMethodsAsArguments(args);

        TestRunner aTestRunner = new TestRunner(useMethodsAsTestcases);
        try {
            TestResult r = aTestRunner.start(args);
            if (!r.wasSuccessful()) {
                System.exit(FAILURE_EXIT);
            }
            System.exit(SUCCESS_EXIT);
        } catch (Exception e) {
            System.err.println(e.getMessage());
            System.exit(EXCEPTION_EXIT);
        }
    }

    /**
     * @see junit.textui.TestRunner#run(Test) Copied from there.
     * @see #run(Test, boolean)
     */
    public static TestResult run(Test test) {
        return run(test, false);
    }

    /**
     * Runs the suite.
     * 
     * @param test
     *            The {@link Test}.
     * @param useMethodsAsTestcases
     *            use test case methods or test cases as test cases for the
     *            protocol
     * @return The {@link TestResult} of the run. 
     * 
     * @see junit.textui.TestRunner#run(Test) Adapted from there.
     */
    public static TestResult run(Test test, boolean useMethodsAsTestcases) {
        return new TestRunner(useMethodsAsTestcases).doRun(test, false);
    }

    /**
     * @see junit.textui.TestRunner#runAndWait(Test) Copied from there.
     * @see #runAndWait(Test, boolean)
     */
    public static void runAndWait(Test suite) {
        runAndWait(suite, false);
    }

    /**
     * Runs the suite and waits for a <code>&lt;RETURN&gt;</code>.
     * 
     * @param suite
     *            The {@link Test}.
     * @param useMethodsAsTestcases
     *            use test case methods or test cases as test cases for the
     *            protocol
     * @see junit.textui.TestRunner#runAndWait(Test) Adapted from there.
     */
    public static void runAndWait(Test suite, boolean useMethodsAsTestcases) {
        new TestRunner(useMethodsAsTestcases).doRun(suite, true);
    }

    /**
     * 
     * @param useMethodsAsTestcases
     *            true &rarr; methods of a {@link TestCase} are used as test
     *            cases<br>
     *            false &rarr; the whole {@link TestCase} is used as a test
     *            case.
     */
    public TestRunner(boolean useMethodsAsTestcases) {
        this.useMethodsAsTestcases = useMethodsAsTestcases;
    }

    public TestResult doRun(Test suite, boolean wait) {
        TestResult testResult = super.doRun(suite, wait);
        this.usedListener.endLastOpenTestCase();
        return testResult;
    }
    
    protected TestResult createTestResult() {
        TestResult newResult = super.createTestResult();
        if (this.useMethodsAsTestcases) {
            this.usedListener = new JUnitResultListenerMethod();
        } else {
            this.usedListener = new JUnitResultListenerTestCase();
        }
        newResult.addListener(this.usedListener);
        return newResult;
    }
}
