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

import junit.framework.AssertionFailedError;

import org.codecover.junit.HelperMethods;
import org.codecover.junit.JUnitFailureContainer;

/**
 * A {@link JUnitFailureContainer} for JUnit 3.
 * 
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: JUnit3FailureContainer.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class JUnit3FailureContainer extends JUnitFailureContainer {

    public JUnit3FailureContainer(AssertionFailedError failedAssertion) {
        super();
        super.setFailureType(FAILURE_TYPE_FAILURE);

        // we skip all StackTraceElements that are targeting to Assert
        // to receive just the line, which called the Assert
        StackTraceElement[] stackTrace = failedAssertion.getStackTrace();
        stackTrace = HelperMethods.getFilteredStackTrace(stackTrace);
        if (stackTrace.length > 0) {
            super.setStackTrace(stackTrace[0].toString());
        }

        basicAnalysis(failedAssertion);
    }

    public JUnit3FailureContainer(Throwable thrownException) {
        super();
        super.setFailureType(FAILURE_TYPE_ERROR);

        // we skip all StackTraceElements that are targeting to Assert
        // to receive just the line, which called the Assert
        StackTraceElement[] stackTrace = thrownException.getStackTrace();
        if (stackTrace.length > 0) {
            super.setStackTrace(stackTrace[0].toString());
        }

        basicAnalysis(thrownException);
    }

    /**
     * Sets the properties by analyzing the thrown Failure.
     *
     * @param thrownFailure
     */
    private void basicAnalysis(Throwable thrownFailure) {
        // set the class of failure
        String className = thrownFailure.getClass().getName();
        super.setClassOfFailure(className.substring(className.lastIndexOf(".")+1));

        // set the message
        String message = thrownFailure.getMessage(); 
        if (message == null && thrownFailure.getCause() != null) {
            message = thrownFailure.getCause().getMessage();
        }
        super.setMessage(message);
    }
}
