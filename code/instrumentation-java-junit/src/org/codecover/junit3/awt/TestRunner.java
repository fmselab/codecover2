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

package org.codecover.junit3.awt;

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
 * {@link junit.awtui.TestRunner} of JUnit 3.8.x.<br>
 * <br>
 * Usage from commandline:
 * 
 * <pre>
 * org.codecover.junit3.awt.TestRunner [-noloading] [-methodsAsTestCases] [-nofilterstack] (&lt;Testclass&gt;|-c &lt;Testclass in VA/Java style&gt;)
 * </pre>
 * 
 * The <code>&lt;Testclass.class&gt;</code> can either be a {@link TestSuite}
 * or a {@link TestCase}.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: TestRunner.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @see junit.awtui.TestRunner
 * @see BaseTestRunner
 */
public class TestRunner extends junit.awtui.TestRunner {

    private boolean useMethodsAsTestcases = false;

    /**
     * Starts a new {@link TestRunner} with the given arguments.
     * 
     * @param args
     *            see {@link TestRunner} for description.
     */
    public static void main(String[] args) {
        boolean useMethodsAsTestcases = HelperMethods.useMethodsAsArguments(args);
        // create the test Runner
        TestRunner testRunner = new TestRunner(useMethodsAsTestcases);

        testRunner.start(args);
    }

    /**
     * Starts a new {@link TestRunner} with the given test class.
     * 
     * @param testClass
     *            The class to test.
     */
    public static void run(Class testClass) {
        main(new String[] { testClass.getName() });
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

    protected TestResult createTestResult() {
        final TestResult newResult = super.createTestResult();
        final JUnitResultListener newListener;
        if (this.useMethodsAsTestcases) {
            newListener = new JUnitResultListenerMethod();
        } else {
            newListener = new JUnitResultListenerTestCase();
        }
        newResult.addListener(newListener);
        
        // it is expected, that this method is called by 
        // link junit.awtui.TestRunner.runSuite()
        // and in an extra TestRunnerThread
        Thread testRunnerThread = Thread.currentThread();
        // this thread calls JUnitResultListener.endLastOpenTestCase() to
        // end the last open JUnit test case, when the current
        // TestRunnerThread is terminated
        EndSuiteThread endSuiteThread = new EndSuiteThread(testRunnerThread,
                newListener);
        endSuiteThread.start();
        return newResult;
    }

    /**
     * This {@link Thread} is used to call
     * {@link JUnitResultListener#endLastOpenTestCase()} after the JUnit test
     * Thread has terminated.
     * 
     * @author Christoph Müller
     * 
     * @version 1.0 ($Id: TestRunner.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public static class EndSuiteThread extends Thread {
        private final Thread testRunnerThread;
        private final JUnitResultListener listener;

        EndSuiteThread(Thread testRunnerThread, JUnitResultListener listener) {
            super(EndSuiteThread.class.getName() + " for " + testRunnerThread.getName());
            this.testRunnerThread = testRunnerThread;
            this.listener = listener;
        }

        public void run() {
            // wait till
            try {
                this.testRunnerThread.join();
            } catch (InterruptedException e) {
                throw new Error(e);
            }
            if (this.testRunnerThread.isAlive()) {
                throw new Error("Expected " + this.testRunnerThread + " to be terminated");
            }
            // The test runner thread is terminated
            this.listener.endLastOpenTestCase();
        }
    }
}
