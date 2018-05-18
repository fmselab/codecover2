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
package org.codecover.juniteclipse;

import java.util.LinkedList;
import java.util.List;

import org.codecover.instrumentation.java.measurement.Protocol;
import org.codecover.junit.HelperMethods;
import org.codecover.junit.JUnitFailureContainer;
import org.eclipse.jdt.internal.junit.runner.IListensToTestExecutions;
import org.eclipse.jdt.internal.junit.runner.ITestIdentifier;
import org.eclipse.jdt.internal.junit.runner.TestReferenceFailure;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: EclipseJUnitListenerMethods.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class EclipseJUnitListenerMethods implements IListensToTestExecutions {
    /** A {@link List} of {@link AssertionError} */
    private List<JUnitFailureContainer> failures;

    public synchronized void notifyTestStarted(ITestIdentifier test) {
        String[] splitted = HelperMethods.extractTestCaseAndMethod(test.getName());
        Protocol.startJUnitTestCase(splitted[0], splitted[1]);
        this.failures = new LinkedList<JUnitFailureContainer>();
    }

    public synchronized void notifyTestEnded(ITestIdentifier test) {
        String[] splitted = HelperMethods.extractTestCaseAndMethod(test.getName());
        StringBuffer resultComment = new StringBuffer();
        JUnitFailureContainer.writeFailures(this.failures, resultComment);
        Protocol.endJUnitTestCase(splitted[0], splitted[1], resultComment.toString());
        this.failures.clear();
        this.failures = null;
    }

    public synchronized void notifyTestFailed(TestReferenceFailure failure) {
        this.failures.add(new JUnitEclipseFailureContainer(failure));
    }
}
