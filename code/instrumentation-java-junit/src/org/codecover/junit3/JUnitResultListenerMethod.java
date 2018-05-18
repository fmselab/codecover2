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
import org.codecover.junit.HelperMethods;
import org.codecover.junit.JUnitFailureContainer;

/**
 * This is a {@link TestListener}, which will inform the {@link ProtocolImpl}
 * of the start, end and result of JUnit test case <b>methods</b>.<br>
 * <br>
 * In constrast to {@link JUnitResultListenerMethod}, this Listener creates
 * test cases for a JUnit {@link TestCase} rather than for every test method.<br>
 * The call has to be done by using the {@link Protocol} class, that is used
 * by the tests as well. Therefore we have to get the {@link Protocol} class
 * by the {@link ClassLoader} of the {@link TestCase}, that is started or ended. 
 * If we use the {@link Protocol} directly, there are two coexistent classes of
 * {@link Protocol} and they don't know from each other. 
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: JUnitResultListenerMethod.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @see BaseTestRunner#getLoader()
 * @see TestCaseClassLoader
 */
public class JUnitResultListenerMethod implements JUnitResultListener {
    /** A {@link List} of {@link JUnitFailureContainer} */
    private List/* JUnitFailureContainer */ failures;

    public void startTest(Test test) {
        if (test instanceof TestCase) {
            startTest((TestCase)test);
        }
    }

    private void startTest(TestCase testCase) {
        try {
            Class protocolClass = HelperMethods.getProtocolClass(testCase);
            Method method = protocolClass.getMethod("startJUnitTestCase",
                    Protocol.START_ARGUMENT);
            method.invoke(null, new Object[]{testCase.getClass().getName(),
                    testCase.getName()});

            // create new Lists for failures and errors
            this.failures = new LinkedList();
        } catch (Exception e) {
            throw new Error(e);
        }
    }

    public void endTest(Test test) {
        if (test instanceof TestCase) {
            endTest((TestCase)test);
        }
    }

    private void endTest(TestCase testCase) {
        try {
            Class protocolClass = HelperMethods.getProtocolClass(testCase);
            Method method = protocolClass.getMethod("endJUnitTestCase",
                    Protocol.END_ARGUMENT);
            StringBuffer resultComment = new StringBuffer();
            JUnitFailureContainer.writeFailures(this.failures, resultComment);
            method.invoke(null, new Object[]{testCase.getClass().getName(),
                    testCase.getName(), resultComment.toString()});
            // clear the Lists for failures and errors
            this.failures.clear();
            this.failures = null;
        } catch (Exception e) {
            throw new Error(e);
        }
    }

    public void addFailure(Test test, AssertionFailedError assertionFailedError) {
        this.failures.add(new JUnit3FailureContainer(assertionFailedError));
    }

    public void addError(Test test, Throwable t) {
        this.failures.add(new JUnit3FailureContainer(t));
    }

    public void endLastOpenTestCase() {
        // no test cases are ever open
    }
}