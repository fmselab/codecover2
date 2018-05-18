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

package org.codecover.junit4;

import java.util.LinkedList;
import java.util.List;

import org.codecover.instrumentation.java.measurement.Protocol;
import org.codecover.instrumentation.java.measurement.TestMethod;
import org.codecover.junit.HelperMethods;
import org.junit.runner.Description;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: JUnitRunListenerTestCase.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class JUnitRunListenerTestCase extends RunListener {

    private String lastJUnitTestClassName = null;

    /** A {@link List} of {@link TestMethod} */
    private LinkedList<TestMethod> testMethods = null;

    @Override
    public void testRunStarted(Description description) throws Exception {
        this.lastJUnitTestClassName = null;
    }

    @Override
    public void testStarted(Description description) throws Exception {
        // {testClassName, methodName}
        String[] splitted = HelperMethods.extractTestCaseAndMethod(description.getDisplayName());
        if (this.lastJUnitTestClassName != null) {
            if (!this.lastJUnitTestClassName.equals(splitted[0])) {
                // we have to end the last test case first
                endTest();
                this.testMethods.clear();
                this.testMethods = null;
            }
        }

        if (this.testMethods == null) {
            // no test for this test class started yet
            this.testMethods = new LinkedList<TestMethod>();
            Protocol.startJUnitTestCase(splitted[0], null);
        }
        this.lastJUnitTestClassName = splitted[0];
        if (splitted[1] != null) {
            this.testMethods.add(new TestMethod(splitted[1]));
        }
    }

    @Override
    public void testFailure(Failure failure) throws Exception {
        TestMethod currentTestMethod = this.testMethods.getLast();
        currentTestMethod.addFailure(new JUnit4FailureContainer(failure));
    }

    @Override
    public void testRunFinished(Result result) throws Exception {
        if (this.lastJUnitTestClassName != null) {
            // we have to end the last unended test case
            endTest();
            this.testMethods.clear();
            this.testMethods = null;
        }
    }

    private void endTest() {
        String resultComment = HelperMethods.writeTestMethods(this.testMethods);
        Protocol.endJUnitTestCase(this.lastJUnitTestClassName, null, resultComment);
    }
}
