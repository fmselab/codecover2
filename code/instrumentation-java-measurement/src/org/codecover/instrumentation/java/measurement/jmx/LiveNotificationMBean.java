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

package org.codecover.instrumentation.java.measurement.jmx;

import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * This represents all the methods, that an MBean has to implement to provide
 * live notification functionality.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: LiveNotificationMBean.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface LiveNotificationMBean {

    /**
     * Starts a new test case with the given name.
     * 
     * @param name
     *            the name of the test case
     *            
     * @see #startTestCase(String, String)
     */
    public void startTestCase(String name);

    /**
     * Starts a new test case with the given name.
     * 
     * @param name
     *            the name of the test case
     * @param comment
     *            the comment for the test case
     */
    public void startTestCase(String name, String comment);

    /**
     * Ends the current test case.
     * 
     * @see #endTestCase(String, String)
     */
    public void endTestCase();

    /**
     * Ends the current test case.
     * 
     * @param name
     *            the name of the test case
     *            
     * @see #endTestCase(String, String)
     */
    public void endTestCase(String name);

    /**
     * Ends the current test case.
     * 
     * @param name
     *            the name of the test case
     * @param resultComment
     *            the comment for the result of the test case
     */
    public void endTestCase(String name, String resultComment);

    /**
     * The coverage measurement for a JUnit test case is started.<br>
     * <br>
     * This method should only be called by a CodeCover TestRunner or Listener.
     * 
     * @param jUnitTestClassName
     *            The class of the test case.
     * @param testName
     *            The name of the method of the JUnit TestCase.
     */
    public void startJUnitTestCase(String jUnitTestClassName, String testName);

    /**
     * The coverage measurement for a JUnit test case has ended.<br>
     * <br>
     * This method should only be called by a CodeCover TestRunner or Listener.
     * 
     * @param jUnitTestClassName
     *            The class of the test case.
     * @param testName
     *            The name of the method of the JUnit TestCase.
     * @param resultComment
     *            A comment for the result of the test case&mdash;e.g. JUnit.
     */
    public void endJUnitTestCase(String jUnitTestClassName, String testName,
            String resultComment);

    /**
     * Ends the current test session. No new test cases can be started after
     * calling this method.
     */
    public void finish();

    /**
     * Gets the String representing the filename of the coverage log file
     * 
     * @return the filename
     */
    public String getLogFileName();

    /**
     * Fetches a predefined length of the coverage log file put into a character
     * array. It is intended to be called repeatedly, until it returns null,
     * which represents the end of file.
     * 
     * @return the character array holding a part of the coverage log file, or
     *         <code>null</code> if the end of file was reached.
     * @throws JMXFileTransferException
     *             Wraps any {@link FileNotFoundException} or other
     *             {@link IOException} that occurred while accessing the coverage
     *             log file.
     */
    public char[] fetchNextLogFileChunk()
            throws JMXFileTransferException;

    /**
     * Resets the download to the beginning of the coverage log file.
     */
    public void resetDownload();
}
