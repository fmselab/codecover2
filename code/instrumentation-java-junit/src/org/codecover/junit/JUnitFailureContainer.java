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
package org.codecover.junit;

import java.util.Iterator;
import java.util.List;

/**
 * This is a container for a failure that occurs in JUnit.<br>
 * <br>
 * A {@link JUnitFailureContainer} contains:
 * <ul>
 * <li>a type {@link #FAILURE_TYPE_FAILURE}, {@link #FAILURE_TYPE_ERROR}</li>
 * <li>the class of the Exception / Error</li>
 * <li>a message</li>
 * <li>a stack trace</li>
 * </ul>
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: JUnitFailureContainer.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class JUnitFailureContainer {

    /** A Failure by Assertion or Comparison */
    public static final int FAILURE_TYPE_FAILURE = 0;

    /** An unexpected Error */
    public static final int FAILURE_TYPE_ERROR = 1;

    /** Type not set or unknown */
    public static final int FAILURE_TYPE_UNKNOWN = 2;
    
    private static final String[] FAILURE_STRINGS = {"Failure", "Error", "Unknown"};

    private int failureType;

    private String classOfFailure;

    private String message;

    private String stackTrace;

    /**
     * Constructor.
     */
    public JUnitFailureContainer() {
        this.failureType = FAILURE_TYPE_UNKNOWN;
        this.classOfFailure = null;
        this.message = null;
        this.stackTrace = null;
    }

    /**
     * @return The failure type of this {@link JUnitFailureContainer}.
     */
    protected int getFailureType() {
        return this.failureType;
    }
    
    /**
     * @return The failure type of this {@link JUnitFailureContainer} as a String.
     */
    protected String getFailureTypeString() {
        return FAILURE_STRINGS[this.failureType];
    }
    
    /**
     * Sets the type of the failure.
     * 
     * @param failureType One of {@link #FAILURE_TYPE_FAILURE}, {@link #FAILURE_TYPE_ERROR}.
     */
    public void setFailureType(int failureType) {
        if (failureType < FAILURE_TYPE_FAILURE ||
            failureType > FAILURE_TYPE_UNKNOWN) {
            throw new IllegalArgumentException("Unknown failure type");
        }
        this.failureType = failureType;
    }

    /**
     * Sets the class of the failure.
     * 
     * @param classOfFailure The <code>Throwable.getClass().getSimpleName()</code>
     */
    public void setClassOfFailure(String classOfFailure) {
        this.classOfFailure = classOfFailure;
    }

    /**
     * Sets the message of the failure.
     * 
     * @param message The message.
     */
    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * Sets the stack trace of the failure.
     * 
     * @param stackTrace The stack trace.
     */
    public void setStackTrace(String stackTrace) {
        this.stackTrace = stackTrace;

    }

    /**
     * Writes the failure to a {@link StringBuffer}.
     * 
     * @param buffer The target {@link StringBuffer}.
     */
    public void writeFailure(StringBuffer buffer) {
        buffer.append(getFailureTypeString());

        if (this.classOfFailure != null) {
            buffer.append(": ");
            buffer.append(this.classOfFailure);
        }

        if (this.message != null) {
            buffer.append('\n');
            buffer.append(this.message);
        }

        if (this.stackTrace != null) {
            buffer.append('\n');
            buffer.append(this.stackTrace);
        }
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        writeFailure(buffer);
        return buffer.toString();
    }

    /**
     * Writes a list in the form:
     * <pre>
     * Failure: "AssertionFailedError" "argument was not null" "at org.codecover.Utils:134"
     * </pre>
     * 
     * @param failuresList A List of {@link JUnitFailureContainer} or other 
     * failure Objects which have overwritten {@link Object#toString()} to
     * provide a message.
     * @param targetBuffer The target {@link StringBuffer}.
     */
    public static void writeFailures(List/*JUnitFailureContainer*/ failuresList,
            StringBuffer targetBuffer) {
        if (failuresList.isEmpty()) {
            return;
        }

        Iterator iterator = failuresList.iterator();
        while (iterator.hasNext()) {
            Object failure = iterator.next();
            if (failure instanceof JUnitFailureContainer) {
                ((JUnitFailureContainer) failure).writeFailure(targetBuffer);
            } else {
                targetBuffer.append(failure.toString());
            }
            if (iterator.hasNext()) {
                targetBuffer.append('\n');
            }
        }
    }
}
