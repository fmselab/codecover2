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

package org.codecover;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Map;
import java.util.TreeMap;

import junit.framework.Assert;

import org.codecover.instrumentation.measurement.CoverageCounterLog;
import org.codecover.model.MASTBuilder;
import org.codecover.model.utils.LogLevel;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.SimpleLogger;

/**
 * Contains methods used by JUnit-Tests.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: UtilsForTestingJMeasure.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class UtilsForTestingJMeasure {
    /** point to <code>testsource/</code> */
    public static String SOURCE;

    /** point to <code>testtarget/</code> */
    public static String TARGET;

    /** point to the instrumentation folde in code */
    public static String BASEDIR;

    public static String MEASUREMENT_JAR = "java15_measurement.jar";

    static {
        String propSource = System.getProperty("codecover.testsource");
        String propTarget = System.getProperty("codecover.testtarget");
        String baseDir = System.getProperty("basedir");

        if (baseDir == null) {
            BASEDIR = (new File("")).getAbsolutePath() + File.separatorChar;
        } else {
            BASEDIR = (new File(baseDir)).getAbsolutePath() + File.separatorChar;
        }
        if (propSource == null) {
            SOURCE = BASEDIR + "testsource/";
        } else {
            SOURCE = propSource;
        }
        if (propTarget == null) {
            TARGET = BASEDIR + "testtarget/";
        } else {
            TARGET = propTarget;
        }

        MEASUREMENT_JAR = BASEDIR + MEASUREMENT_JAR;

        //System.out.println("TEST_SOURCE: " + TEST_SOURCE);
        //System.out.println("TEST_TARGET: " + TEST_TARGET);
    }

    /** point to <code>testtarget/database.xml</code> */
    public static final String SESSION_CONTAINER = TARGET + "session_container.xml";

    /**
     * Gets an initialised {@link MASTBuilder}<br>
     * <br>
     * The Logger is initialized to write to a {@link ToNoWhereStream}.
     * 
     * @return the {@link MASTBuilder}
     */
    public static MASTBuilder newMASTBuilder(){
        Logger logger = new SimpleLogger(new PrintStream(new ToNoWhereStream(), true), LogLevel.FATAL);
        return new MASTBuilder(logger);
    }

    /**
     * Will get the message and the stacktrace of the Exception and call
     * {@link Assert#fail(String)}.
     * 
     * @param e The Exception.
     */
    public static void handleException(Exception e) {
        StringWriter stringWriter = new StringWriter();
        if (e.getCause() != null) {
            e.getCause().printStackTrace(new PrintWriter(stringWriter));
            Assert.fail("Not expected:\n" + e.getMessage() + "\n" + e.getCause() + "\n" + stringWriter.toString());
        } else {
            e.printStackTrace(new PrintWriter(stringWriter));
            Assert.fail("Not expected:\n" + e.getMessage() + "\n" + stringWriter.toString());
        }
    }

    static class ToNoWhereStream extends OutputStream {
        public void write(int b) throws IOException {
            // do nothing
        }

        public void write(byte b[]) throws IOException {
            // do nothing
        }

        public void write(byte b[], int off, int len) throws IOException {
            // do nothing
        }
    }

    /**
     * A {@link CoverageCounterLog} that dumps
     * {@link CoverageCounterLog#passCounter(String, long)} to a map &rarr;
     * {@link #counters}. It allows <b>no</b>
     * {@link CoverageCounterLog#startNamedSection(String)}.
     * 
     * @author Christoph Müller
     * 
     * @version 1.0 ($Id: UtilsForTestingJMeasure.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public static class TestCoverageCounterLog implements CoverageCounterLog {
        public Map counters = new TreeMap();

        public void passCounter(String counterID, long counterValue) {
            this.counters.put(counterID, Long.valueOf(counterValue));
        }

        public void startNamedSection(String sectionName) {
            Assert.fail("startNamedSection not expected"); 
        }

        public void clear() {
            this.counters.clear();
        }
    }
}
