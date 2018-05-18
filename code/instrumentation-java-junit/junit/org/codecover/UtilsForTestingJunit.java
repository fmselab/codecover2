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

import junit.framework.Assert;

import org.codecover.junit3.awt.TestRunner;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.utils.LogLevel;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.SimpleLogger;

/**
 * Contains methods used by JUnit-Tests.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: UtilsForTestingJunit.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class UtilsForTestingJunit {
    /** point to <code>testsource/</code> */
    public static String SOURCE;

    /** point to <code>testtarget/</code> */
    public static String TARGET;

    /** point to the instrumentation folde in code */
    public static String BASEDIR;

    /** the {@link File} of {@link TestSessionContainer} */
    public static File TEST_SESSION_CONTAINER;

    /** A {@link Logger}. */
    public static Logger LOGGER;

    static {
        String propSource = System.getProperty("codecover.testsource");
        String propTarget = System.getProperty("codecover.testtarget");
        String baseDir = System.getProperty("basedir");
        String logLevel = System.getProperty("codecover.loglevel");

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

        TEST_SESSION_CONTAINER = new File(SOURCE + "junit-tests-tsc.xml").getAbsoluteFile();

        if (logLevel != null && logLevel.equals("INFO")) {
            LOGGER = new SimpleLogger(System.out, LogLevel.INFO);
        } else if (logLevel != null && logLevel.equals("WARNING")) {
                LOGGER = new SimpleLogger(System.out, LogLevel.WARNING);
        } else {
            LOGGER = new SimpleLogger(new PrintStream(new ToNoWhereStream(), true), LogLevel.FATAL);
        }
    }

    /** point to <code>testtarget/database.xml</code> */
    public static final String SESSION_CONTAINER = TARGET + "session_container.xml";

    private static long LAST_TIMESTAMP = Long.MIN_VALUE;
    
    /**
     * Will get the message and the stacktrace of the Exception and call
     * {@link Assert#fail(String)}.
     * 
     * @param t The Exception.
     */
    public static void handleException(Throwable t) {
        StringWriter stringWriter = new StringWriter();
        if (t.getCause() != null) {
            t.getCause().printStackTrace(new PrintWriter(stringWriter));
            Assert.fail("Not expected:\n" + t.getMessage() + "\n" + t.getCause() + "\n" + stringWriter.toString());
        } else {
            t.printStackTrace(new PrintWriter(stringWriter));
            Assert.fail("Not expected:\n" + t.getMessage() + "\n" + stringWriter.toString());
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
            File[] subfiles = fileOrDirectory.listFiles();
            for (int i = 0; i < subfiles.length; i++) {
                deleteFileRecursively(subfiles[i]);
            }
        }

        fileOrDirectory.delete();
        Assert.assertFalse(fileOrDirectory.getAbsolutePath(), fileOrDirectory.exists());
    }

    /**
     * Deletes all files directly in the given directory.
     * 
     * @param directory
     *          The directory which should be emptied.
     */
    public static void deleteFilesInDirectory(File directory) {
        if (directory.isDirectory()) {
            File[] subfiles = directory.listFiles();
            for (int i = 0; i < subfiles.length; i++) {
                if (subfiles[i].isFile()) {
                    subfiles[i].delete();
                    Assert.assertFalse(subfiles[i].getAbsolutePath(), subfiles[i].exists());
                }
            }
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
    public static void resetTimestamp() {
        LAST_TIMESTAMP = System.currentTimeMillis();
    }

    public static void putOutTheGarbage(int iCount)
    {
      for (int i = 0 ; i < iCount; i++)
      {
        collectGarbage();      
      }
    }

    public static void collectGarbage()
    {
      try
      {
        System.gc();
        Thread.sleep(10);
        System.runFinalization();
        Thread.sleep(10);
      }
      catch (InterruptedException ex)
      {
        ex.printStackTrace();
      }
    }

    public static void printTimestamp() {
        long diff = System.currentTimeMillis() - LAST_TIMESTAMP;
        resetTimestamp();
        LOGGER.warning("TIME: " + diff + "ms");
    }

    public static void printMemory() {
        long totalMemory = Runtime.getRuntime().totalMemory();
        long freeMemory = Runtime.getRuntime().freeMemory();

        LOGGER.warning("TIME: " + (totalMemory - freeMemory) + "B");
    }

    public static void waitForFinish() {
        int threadCount = Thread.activeCount();
        Thread testRunnerThread = null;
        Thread[] threads = new Thread[threadCount];
        Thread.currentThread().getThreadGroup().enumerate(threads, true);
        for (int i = 0; i < threads.length; i++) {
            Thread thisThread = threads[i];
            String name = thisThread.getName();
            String className = thisThread.getClass().getName();
            if (className.startsWith("junit.")) {
                testRunnerThread = threads[i];
                break;
            }
        }
        if (testRunnerThread == null) {
            return;
        }
        try {
            testRunnerThread.join();
        } catch (InterruptedException e) {
            handleException(e);
        }
        
        threadCount = Thread.activeCount();
        Thread listeningThread = null;
        threads = new Thread[threadCount];
        Thread.currentThread().getThreadGroup().enumerate(threads, true);
        for (int i = 0; i < threads.length; i++) {
            Thread thisThread = threads[i];
            String name = thisThread.getName();
            String className = thisThread.getClass().getName();
            if (className.equals(TestRunner.EndSuiteThread.class.getName())) {
                listeningThread = threads[i];
                break;
            }
        }
        if (listeningThread == null) {
            return;
        }
        try {
            listeningThread.join();
        } catch (InterruptedException e) {
            handleException(e);
        }
    }
}
