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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.nio.CharBuffer;

import org.codecover.instrumentation.java.measurement.CoverageLogPath;
import org.codecover.instrumentation.java.measurement.Protocol;

/**
 * This class represents an MBean, that is to be used in the live notification
 * of starting and stopping of test cases and the stopping of the test session.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: LiveNotification.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class LiveNotification implements LiveNotificationMBean {

    private File file;

    private FileReader fileReader = null;

    /**
     * Default Constructor, which initializes the file with the CoverageLogFile.
     * 
     * @see CoverageLogPath#getCoverageLogFile(String)
     */
    public LiveNotification() {
        // creates the coverage log file or get its path if it already created
        this.file = CoverageLogPath.getCoverageLogFile(null);
    }

    /**
     * (non-Javadoc)
     * 
     * @see LiveNotificationMBean#startTestCase(String)
     */
    public void startTestCase(String name) {
        Protocol.startTestCase(name);
    }

    /**
     * (non-Javadoc)
     * 
     * @see LiveNotificationMBean#startTestCase(String, String)
     */
    public void startTestCase(String name, String comment) {
        Protocol.startTestCase(name, comment);
    }

    /**
     * (non-Javadoc)
     * 
     * @see LiveNotificationMBean#endTestCase()
     */
    public void endTestCase() {
        Protocol.endTestCase();
    }

    /**
     * (non-Javadoc)
     * 
     * @see LiveNotificationMBean#endTestCase(String)
     */
    public void endTestCase(String name) {
        Protocol.endTestCase(name);
    }

    /**
     * (non-Javadoc)
     * 
     * @see LiveNotificationMBean#endTestCase(String, String)
     */
    public void endTestCase(String name, String comment) {
        Protocol.endTestCase(name, comment);
    }

    /**
     * (non-Javadoc)
     * 
     * @see LiveNotificationMBean#startJUnitTestCase(String, String)
     */
    public void startJUnitTestCase(String unitTestClassName, String testName) {
        Protocol.startJUnitTestCase(unitTestClassName, testName);
    }

    /**
     * (non-Javadoc)
     * 
     * @see LiveNotificationMBean#endJUnitTestCase(String, String, String)
     */
    public void endJUnitTestCase(String unitTestClassName, String testName,
            String resultComment) {
        Protocol.endJUnitTestCase(unitTestClassName, testName, resultComment);
    }

    /**
     * (non-Javadoc)
     * 
     * @see LiveNotificationMBean#finish()
     */
    public void finish() {
        Protocol.finishTestSession();
    }

    /**
     * (non-Javadoc)
     * 
     * @see LiveNotificationMBean#getLogFileName()
     */
    public String getLogFileName() {
        return CoverageLogPath.getCoverageLogFile(null).getName();
    }

    /**
     * (non-Javadoc)
     * 
     * @see LiveNotificationMBean#resetDownload()
     */
    public void resetDownload() {
        if (this.fileReader == null) {
            return;
        }
        try {
            this.fileReader.close();
            this.fileReader = null;
        } catch (IOException e) {
            // Nothing needs to be done here, since the transfer was
            // completed before this.
        }
    }

    /**
     * (non-Javadoc)
     * 
     * @see LiveNotificationMBean#fetchNextLogFileChunk()
     */
    public char[] fetchNextLogFileChunk() throws JMXFileTransferException {
        final int buffer = 1024;

        if (this.fileReader == null) {
            try {
                this.fileReader = new FileReader(this.file);
            } catch (FileNotFoundException e) {
                throw new JMXFileTransferException(e);
            }
        }

        CharBuffer charBuffer = CharBuffer.allocate(buffer);
        int readSize = -1;

        try {
            readSize = this.fileReader.read(charBuffer);
        } catch (IOException e) {
            throw new JMXFileTransferException(e);
        }

        if (readSize != -1) {
            charBuffer.flip();
            char[] dst = new char[readSize];
            charBuffer.get(dst);

            return dst;
        }
        try {
            this.fileReader.close();
            this.fileReader = null;
        } catch (IOException e) {
            // Nothing needs to be done here, since the transfer was completed
            // before this.
        }
        return null;
    }
}
