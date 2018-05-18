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
import org.codecover.junit.HelperMethods;
import org.codecover.junit.JUnitFailureContainer;
import org.junit.runner.Description;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: JUnitRunListenerMethod.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class JUnitRunListenerMethod extends RunListener {
    /** A {@link List} of {@link JUnitFailureContainer} */
    private List<JUnitFailureContainer> failures;

    @Override
    public void testStarted(Description description) throws Exception {
        String[] splitted = HelperMethods.extractTestCaseAndMethod(description.getDisplayName());
        Protocol.startJUnitTestCase(splitted[0], splitted[1]);
        this.failures = new LinkedList<JUnitFailureContainer>();
    }

    @Override
    public void testFinished(Description description) throws Exception {
        String[] splitted = HelperMethods.extractTestCaseAndMethod(description.getDisplayName());
        StringBuffer resultComment = new StringBuffer();
        JUnitFailureContainer.writeFailures(this.failures, resultComment);
        Protocol.endJUnitTestCase(splitted[0], splitted[1], resultComment.toString());
        this.failures.clear();
        this.failures = null;
    }

    @Override
    public void testFailure(Failure failure) throws Exception {
        this.failures.add(new JUnit4FailureContainer(failure));
    }
}
