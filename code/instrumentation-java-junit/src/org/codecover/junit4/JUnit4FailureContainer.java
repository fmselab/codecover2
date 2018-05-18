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

import junit.framework.AssertionFailedError;

import org.codecover.junit.HelperMethods;
import org.codecover.junit.JUnitFailureContainer;
import org.junit.runner.notification.Failure;

/**
 * A {@link JUnitFailureContainer} for JUnit 4.
 * 
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: JUnit4FailureContainer.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class JUnit4FailureContainer extends JUnitFailureContainer {
    /**
     * Sets the properties by analyzing the thrown Failure.
     *
     * @param thrownFailure the {@link Failure} that is thrown
     */
    public JUnit4FailureContainer(Failure thrownFailure) {
        super();
        Throwable t = thrownFailure.getException();

        // set the type
        // assert: we have started a test case before
        // and failures / errors != null
        if (t instanceof AssertionError ||
            t instanceof AssertionFailedError) {
            super.setFailureType(FAILURE_TYPE_FAILURE);
        } else {
            super.setFailureType(FAILURE_TYPE_ERROR);
        }

        // set the class of failure
        super.setClassOfFailure(t.getClass().getSimpleName());

        // set the message
        String failureMessage = thrownFailure.getMessage(); 
        if (failureMessage == null && t.getCause() != null) {
            failureMessage = t.getCause().getMessage();
        }
        super.setMessage(failureMessage);

        StackTraceElement[] failureStackTrace = t.getStackTrace();
        if (super.getFailureType() == FAILURE_TYPE_FAILURE) {
            // we skip all StackTraceElements that are targeting to Assert
            // to receive just the line, which called the Assert
            failureStackTrace = HelperMethods.getFilteredStackTrace(failureStackTrace);
        }
        if (failureStackTrace.length > 0) {
            super.setStackTrace(failureStackTrace[0].toString());
        }
    }
}
