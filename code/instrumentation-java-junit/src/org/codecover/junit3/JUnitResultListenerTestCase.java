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

package org.codecover.junit3;

import java.lang.reflect.Method;
import java.util.LinkedList;
import java.util.List;

import junit.framework.AssertionFailedError;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestListener;
import junit.runner.BaseTestRunner;
import junit.runner.TestCaseClassLoader;

import org.codecover.instrumentation.java.measurement.Protocol;
import org.codecover.instrumentation.java.measurement.ProtocolImpl;
import org.codecover.instrumentation.java.measurement.TestMethod;
import org.codecover.junit.HelperMethods;

/**
 * This is a {@link TestListener}, which will inform the {@link ProtocolImpl}
 * of the start, end and result of JUnit test case.<br>
 * <br>
 * In constrast to {@link JUnitResultListenerMethod}, this Listener creates
 * test cases for a JUnit {@link TestCase} rather than for every test method.<br>
 * The call of the {@link Protocol} class. has to be done by using the
 * {@link Protocol} class, that is used by the tests as well. Therefore we have
 * to get the {@link Protocol} class by the {@link ClassLoader} of the
 * {@link TestCase}, that is started or ended. If we use the {@link Protocol}
 * directly, there are two coexistent classes of {@link Protocol} and they don't
 * know from each other.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: JUnitResultListenerTestCase.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @see BaseTestRunner#getLoader()
 * @see TestCaseClassLoader
 */
public class JUnitResultListenerTestCase implements JUnitResultListener {

    private TestCase lastTestCase = null;

    /** A {@link List} of {@link TestMethod} */
    private LinkedList testMethods = null;

    public void startTest(Test test) {
        try {
            if (test instanceof TestCase) {
                TestCase testCase = (TestCase) test;

                if (this.lastTestCase != null) {
                    if (this.lastTestCase.getClass() != testCase.getClass()) {
                        // we have to end the last test case first
                        endTest(this.lastTestCase);

                        this.testMethods.clear();
                        this.testMethods = null;
                    }
                }

                if (this.testMethods == null) {
                    // we have to start a new test case
                    this.testMethods = new LinkedList();
                    startTest(testCase);
                }

                this.testMethods.add(new TestMethod(testCase.getName()));
                this.lastTestCase = testCase;
            }
        } catch (Exception e) {
            throw new Error(e);
        }
    }

    private void startTest(TestCase testCase) throws Exception {
        Class protocolClass = HelperMethods.getProtocolClass(testCase);
        Method method = protocolClass.getMethod("startJUnitTestCase",
                Protocol.START_ARGUMENT);
        method.invoke(null, new Object[]{testCase.getClass().getName(), null});
    }

    public void endTest(Test test) {
        // we dont end a test case here
    }

    private void endTest(TestCase testCase) throws Exception {
        Class protocolClass = HelperMethods.getProtocolClass(testCase);
        Method method = protocolClass.getMethod("endJUnitTestCase",
                Protocol.END_ARGUMENT);
        String resultComment = HelperMethods.writeTestMethods(this.testMethods);
        method.invoke(null, new Object[]{testCase.getClass().getName(), null,
                resultComment});
    }

    public void addFailure(Test test, AssertionFailedError assertionFailedError) {
        TestMethod currentTestMethod = (TestMethod) this.testMethods.getLast();
        currentTestMethod.addFailure(new JUnit3FailureContainer(assertionFailedError));
    }

    public void addError(Test test, Throwable t) {
        // assert: we have started a test case before
        // and testMethods != null
        TestMethod currentTestMethod = (TestMethod) this.testMethods.getLast();
        currentTestMethod.addFailure(new JUnit3FailureContainer(t));
    }

    public void endLastOpenTestCase() {
        try {
            if (this.testMethods != null) {
                endTest(this.lastTestCase);
                this.testMethods.clear();
                this.testMethods = null;
                this.lastTestCase = null;
            }
        } catch (Exception e) {
            throw new Error(e);
        }
    }
}