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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.util.Date;
import java.util.Map;
import java.util.TreeMap;

import junit.framework.Assert;

import org.codecover.instrumentation.Instrumenter;
import org.codecover.instrumentation.cobol85.InstrumenterDescriptor;
import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectivesManipulator;
import org.codecover.instrumentation.cobol85.compilerDirectives.DefaultCompilerDirectivesManipulator;
import org.codecover.instrumentation.cobol85.parser.CobolParser;
import org.codecover.instrumentation.cobol85.parser.SimpleCharStream;
import org.codecover.instrumentation.exceptions.ParseException;
import org.codecover.instrumentation.measurement.CoverageCounterLog;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.utils.LogLevel;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.SimpleLogger;

/**
 * Contains methods used by JUnit-Tests.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: UtilsForTestingCobol.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class UtilsForTestingCobol {
    /** point to <code>testsource/</code> */
    public static String SOURCE;

    /** point to <code>testtarget/</code> */
    public static String TARGET;

    /** point to the instrumentation folder in code */
    public static String BASEDIR;

    public static String MEASUREMENT_JAR = "java15_measurement.jar";

    private static long LAST_TIMESTAMP;

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

    /**
     * Cleares the target path and all potential files.
     */
    public static void clearTarget() {
        deleteFilesInDirectory(new File(TARGET));
    }

    /**
     * Deletes the given file or directory recursively.
     * 
     * @param fileOrDirectory
     *          The file or directory to delete.
     */
    public static void deleteFileRecursively(File fileOrDirectory) {
        if (fileOrDirectory.isDirectory()) {
            for (File file : fileOrDirectory.listFiles()) {
                deleteFileRecursively(file);
            }
        }

        fileOrDirectory.delete();
    }

    /**
     * Deletes all files directly in the given directory.
     * 
     * @param directory
     *          The directory which should be emptied.
     */
    public static void deleteFilesInDirectory(File directory) {
        if (directory.isDirectory()) {
            for (File file : directory.listFiles()) {
                if (file.isFile()) {
                    file.delete();
                }
            }
        }
    }

    /**
     * Calls {@link CobolParser#CompilationUnit()} to test, if an 
     * instrumented file is accepted by the Parser. 
     */
    public static boolean isCompileableCobol(File sourceFile) {
        Assert.assertTrue(sourceFile.exists());
        try {
            FileInputStream fileInputStream = new FileInputStream(sourceFile);
            InputStreamReader inputStreamReader = new InputStreamReader(
                    fileInputStream, "UTF-8");
            BufferedReader bufferedReader = new BufferedReader(inputStreamReader);

            CompilerDirectivesManipulator compilerDirectivesManipulator = new DefaultCompilerDirectivesManipulator();
            SimpleCharStream simpleCharStream = new SimpleCharStream(bufferedReader, compilerDirectivesManipulator);
            CobolParser parser = new CobolParser(simpleCharStream);
            parser.CompilationUnit();
        } catch (ParseException e) {
            handleException(e);
        } catch (FileNotFoundException e) {
            handleException(e);
        } catch (UnsupportedEncodingException e) {
            handleException(e);
        }

        return true;
    }

    /**
     * Simple tests <b>after</b> the instrumentation:
     * <ul>
     * <li>is the date before instrumentation before {@link CodeBase#getDate()}</li>
     * <li>is the date now instrumentation after {@link CodeBase#getDate()}</li>
     * <li>is the {@link CodeBase#getCode()} not null</li>
     * <li>are the criteria of the instrumenter and the code base the same</li>
     * </ul> 
     */
    public static void simpleTestSessionContainerTests(Date dateBefore, TestSessionContainer testSessionContainer, Instrumenter instumenter) {
        Assert.assertFalse(dateBefore.after(testSessionContainer.getDate()));
        Assert.assertFalse(testSessionContainer.getDate().after(new Date()));
        Assert.assertNotNull(testSessionContainer.getCode());
        Assert.assertEquals(instumenter.getCriteria(), testSessionContainer.getCriteria());
    }

    static class ToNoWhereStream extends OutputStream {
        @Override
        public void write(int b) throws IOException {
            // do nothing
        }

        @Override
        public void write(byte b[]) throws IOException {
            // do nothing
        }

        @Override
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
     * @version 1.0 ($Id: UtilsForTestingCobol.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public static class TestCoverageCounterLog implements CoverageCounterLog {
        public Map<String, Long> counters = new TreeMap<String, Long>();

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

    public static void resetTimestamp() {
        LAST_TIMESTAMP = System.currentTimeMillis();
    }

    public static void printTimestamp() {
        long diff = System.currentTimeMillis() - LAST_TIMESTAMP;
        resetTimestamp();
        System.out.printf("TIME: %,11dms",
                new Long (diff));
    }
    
    public static Map<String, Object> getInstrumenterDirectives() {
        return new InstrumenterDescriptor().getDefaultDirectiveValues();
    }
}
