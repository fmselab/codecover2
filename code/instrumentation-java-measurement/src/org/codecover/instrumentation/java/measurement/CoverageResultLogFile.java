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

package org.codecover.instrumentation.java.measurement;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

import org.codecover.instrumentation.measurement.CoverageCounterLog;
import org.codecover.instrumentation.measurement.CoverageResultLog;
import org.codecover.instrumentation.measurement.MeasurementConstants;

/**
 * This is an implementation of {@link CoverageResultLog} using an output file.
 * <br>
 * <br>
 * The instance of {@link CoverageResultLogFile} should be used just for one
 * coverage log. After the usage of {@link #closeLog()} no other
 * notifications can be written, cause with {@link #closeLog()} the
 * internal {@link BufferedWriter} is closed. <br>
 * Exceptions for writing are not directly thrown but as a
 * {@link RuntimeException}.<br>
 * The {@link MeasurementConstants#CHARSET} is used.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: CoverageResultLogFile.java 69 2010-01-27 19:31:18Z schmidberger $)
 * @see CoverageResultLog
 */
public class CoverageResultLogFile implements CoverageCounterLog,
CoverageResultLog {

    /** The separator for lines: '\n' */
    public static final String LINE_SEPARATOR = "\n";

    /** The identifier for comment lines: <b>//</b> */
    public static final String COMMENT_IDENTIFIER = "// ";

    /**
     * The String, which is written at the start of the file if there is
     * no comment.
     */
    public static final String TEST_SESSION_CONTAINER = "TEST_SESSION_CONTAINER";

    /**
     * The String, which is written at the start of a test case if there
     * is no test case comment and no timestamp.
     */
    public static final String START_TEST_CASE = "START_TEST_CASE";

    /**
     * The String, which is written at the end of a test case if there is
     * no timestamp.
     */
    public static final String END_TEST_CASE = "END_TEST_CASE";

    /**
     * The String, which is written at the start of a section.
     */
    public static final String START_SECTION = "START_SECTION";

    private static final int BUFFER_SIZE = 8192;

    private static CoverageResultLogFile instance = null;

    private Writer writer;

    private static final Object LOCK = new Object();

    /**
     * Returns the single instance of {@link CoverageResultLogFile} or creates
     * one.<br>
     * <br>
     * If a {@link FileNotFoundException} occurs during opening of the
     * {@link FileOutputStream}, an Error is thrown.
     * 
     * @return The single instance.
     * 
     * @see #getInstance(String)
     * @see #CoverageResultLogFile(File)
     */
    public static CoverageResultLogFile getInstance() {
        return getInstance(null);
    }

    /**
     * Returns the single instance of {@link CoverageResultLogFile} or creates
     * one.<br>
     * <br>
     * If a {@link FileNotFoundException} occurs during opening of the
     * {@link FileOutputStream}, an Error is thrown.
     * 
     * @param preferredPath If this String is not null, this path is taken as
     * a preferred path for the coverage log file, except a property is set.
     * 
     * @return The single instance.
     *
     * @see #CoverageResultLogFile(File)
     * @see CoverageLogPath#getCoverageLogFile(String)
     */
    public static CoverageResultLogFile getInstance(String preferredPath) {
        if (instance != null) {
            return instance;
        }

        synchronized (LOCK) {
            if (instance == null) {
                // let CoverageLogPath decide the path to use
                File clfFile = CoverageLogPath.getCoverageLogFile(preferredPath);
                clfFile = clfFile.getAbsoluteFile();
                File parent = clfFile.getParentFile();
                if (parent != null) {
                    parent.mkdirs();
                }
                instance = new CoverageResultLogFile(clfFile);
            }
            return instance;
        }
    }

    /**
     * A new {@link CoverageResultLogFile} is created. The target file is opened
     * for writing.
     * 
     * @param clfTarget
     *          The target of the Coverage Log File. 
     */
    private CoverageResultLogFile(File clfTarget) {
        try {
            FileOutputStream fileOutputStream = new FileOutputStream(clfTarget);
            OutputStreamWriter outputStreamWriter = new OutputStreamWriter(
                    fileOutputStream, MeasurementConstants.CHARSET);
            this.writer = new BufferedWriter(outputStreamWriter, BUFFER_SIZE);
        } catch (FileNotFoundException e) {
            throw new Error(e);
        }
    }

    public void startLog() {
        lineComment("Charset: " + MeasurementConstants.CHARSET.name());
    }

    public void startTestCase(String testSessionContainerUID,
            String testCaseName) {
        startTestCase(testSessionContainerUID, testCaseName, -1, null);
    }

    public void startTestCase(String testSessionContainerUID,
            String testCaseName, String comment) {
        startTestCase(testSessionContainerUID, testCaseName, -1, comment);
    }

    public void startTestCase(String testSessionContainerUID,
            String testCaseName, long timestamp) {
        startTestCase(testSessionContainerUID, testCaseName, timestamp, null);
    }

    public void startTestCase(String testSessionContainerUID,
            String testCaseName, long timestamp, String comment) {
        try {
            this.writer.write(TEST_SESSION_CONTAINER);
            this.writer.write(" \"");
            this.writer.write(testSessionContainerUID);
            this.writer.write("\"");
            this.writer.write(LINE_SEPARATOR);

            this.writer.write(START_TEST_CASE);
            this.writer.write(" \"");
            this.writer.write(MeasurementConstants.escapeName(testCaseName));
            this.writer.write("\"");
            if (timestamp != -1) {
                this.writer.write(" ");
                this.writer.write(Long.toString(timestamp));
            }
            if (comment != null) {
                this.writer.write(" \"");
                this.writer.write(MeasurementConstants.escapeComment(comment));
                this.writer.write("\"");
            }
            this.writer.write(LINE_SEPARATOR);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void endTestCase(String testCaseName) {
        endTestCase(testCaseName, -1, null);
    }

    public void endTestCase(String testCaseName, long timestamp) {
        endTestCase(testCaseName, timestamp, null);
    }

    public void endTestCase(String testCaseName, long timestamp, String resultComment) {
        try {
            // write the "END_TEST_CASE 'name'"
            this.writer.write(END_TEST_CASE);
            this.writer.write(" \"");
            this.writer.write(MeasurementConstants.escapeName(testCaseName));
            this.writer.write("\"");
            if (timestamp != -1) {
                this.writer.write(" ");
                this.writer.write(Long.toString(timestamp));
            }
            if (resultComment != null) {
                this.writer.write(" \"");
                this.writer.write(MeasurementConstants.escapeComment(resultComment));
                this.writer.write("\"");
            }
            this.writer.write(LINE_SEPARATOR);
            this.writer.flush();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void startNamedSection(String sectionName) {
        try {
            this.writer.write(START_SECTION);
            this.writer.write(" \"");
            this.writer.write(MeasurementConstants.escapeName(sectionName));
            this.writer.write("\"");
            this.writer.write(LINE_SEPARATOR);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void passCounter(String counterID, long counterValue) {
        
        counterValue = counterValue > 0 ? counterValue : 0; // RS, 21.01.10: remove possibly negative values
        try {
            this.writer.write(counterID);
            this.writer.write(" ");
            this.writer.write(Long.toString(counterValue));
            this.writer.write(LINE_SEPARATOR);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void lineComment(String comment) {
        try {
            this.writer.write(COMMENT_IDENTIFIER);
            this.writer.write(comment);
            this.writer.write(LINE_SEPARATOR);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void closeLog() {
        try {
            this.writer.flush();
            this.writer.close();
        } catch (IOException e) {
            // just catch
        }
    }
}
