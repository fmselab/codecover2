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

//temporary copy

import java.io.*;

import junit.framework.Assert;

import org.codecover.model.*;
import org.codecover.model.utils.*;

/**
 * Contains methods used by JUnit-Tests.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: UtilsForTesting.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class UtilsForTesting {
    /** point to <code>testsource/</code> */
    public static String SOURCE;

    /** point to <code>testsource/org/codecover/instrument/</code> */
    public static String TEST_SOURCE;

    /** point to <code>testtarget/</code> */
    public static String TARGET;

    /** point to <code>testtarget/org/codecover/instrument/</code> */
    public static String TEST_TARGET;

    public static String FILE_SEPARATOR = System.getProperty("file.separator");

    public static String PATH_SEPARATOR = System.getProperty("path.separator");

    /** point to the instrumentation folde in code */
    public static String BASEDIR;

    public static String MEASUREMENT_JAR = "java15_measurement.jar";

    static {
        String propSource = System.getProperty("codecover.testsource");
        String propTarget = System.getProperty("codecover.testtarget");
        String baseDir = System.getProperty("basedir");

        if (baseDir == null) {
            BASEDIR = (new File("")).getAbsolutePath() + FILE_SEPARATOR;
        } else {
            BASEDIR = (new File(baseDir)).getAbsolutePath() + FILE_SEPARATOR;
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

        TEST_SOURCE = SOURCE + "org/codecover/instrument/";
        TEST_TARGET = TARGET + "org/codecover/instrument/";
        MEASUREMENT_JAR = BASEDIR + MEASUREMENT_JAR;

        // System.out.println("TEST_SOURCE: " + TEST_SOURCE);
        // System.out.println("TEST_TARGET: " + TEST_TARGET);
    }

    /** point to <code>testtarget/database.xml</code> */
    public static final String DATABASE_TARGET = TARGET + "database.xml";

    /**
     * Clears the path for the {@link TestSessionContainer} for testing.<br>
     */
    public static void newTestDatabase() {
        deleteFileRecursively(new File(DATABASE_TARGET));

    }

    /**
     * Gets an initialised {@link MASTBuilder}<br>
     * <br>
     * The Logger is initialized to write to a {@link ToNoWhereStream}.
     * 
     * @return the {@link MASTBuilder}
     */
    public static MASTBuilder newMASTBuilder() {
        Logger logger = new SimpleLogger(new PrintStream(new ToNoWhereStream(),
                                                         true), LogLevel.FATAL);
        return new MASTBuilder(logger);
    }

    /**
     * Will get the message and the stacktrace of the Exception and call
     * {@link Assert#fail(String)}.
     * 
     * @param e
     *            The Exception.
     */
    public static void handleException(Exception e) {
        StringWriter stringWriter = new StringWriter();
        if (e.getCause() != null) {
            e.getCause().printStackTrace(new PrintWriter(stringWriter));
            Assert.fail("Not expected:\n" + e.getMessage() + "\n"
                    + e.getCause() + "\n" + stringWriter.toString());
        } else {
            e.printStackTrace(new PrintWriter(stringWriter));
            Assert.fail("Not expected:\n" + e.getMessage() + "\n"
                    + stringWriter.toString());
        }
    }

    /**
     * Cleares the target path and all potential files.
     */
    public static void clearTarget() {
        File target = new File(TEST_TARGET);
        deleteFileRecursively(target);
        target.mkdirs();
        deleteFilesInDirectory(new File(TARGET));
    }

    /**
     * Deletes the given file or directory recursively.
     * 
     * @param fileOrDirectory
     *            The file or directory to delete.
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
     *            The directory which should be emptied.
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

    public static void runJavac(File fileToCompile) {
        fileToCompile = fileToCompile.getAbsoluteFile();
        Assert.assertTrue(fileToCompile.exists());
        Assert.assertTrue(fileToCompile.isFile());
        Assert.assertTrue((new File(MEASUREMENT_JAR)).exists());

        String[] command = new String[] { "javac", "-classpath",
                MEASUREMENT_JAR, "-sourcepath", TARGET, "-d", TARGET,
                "-encoding", "UTF-8", "-source", "1.5", "-target", "1.5",
                fileToCompile.getAbsolutePath() };

        try {
            Process process = Runtime.getRuntime().exec(command);

            StringBuilder sBOut = new StringBuilder();
            StreamGobbler gobblerOut = new StreamGobbler(process.getInputStream(),
                                                         sBOut);
            gobblerOut.start();

            StringBuilder sBErr = new StringBuilder();
            StreamGobbler gobblerErr = new StreamGobbler(process.getErrorStream(),
                                                         sBErr);
            gobblerErr.start();

            process.waitFor();
            Assert.assertEquals("", sBOut.toString());
            Assert.assertEquals("", sBErr.toString());
            Assert.assertEquals(0, process.exitValue());

            String classFilePath = fileToCompile.getPath();
            File classFile = new File(classFilePath.substring(0,
                                                              classFilePath.length() - 4)
                    + "class");
            classFile = classFile.getAbsoluteFile();
            Assert.assertTrue(classFile.exists());
            Assert.assertTrue(classFile.isFile());
        } catch (IOException e) {
            handleException(e);
        } catch (InterruptedException e) {
            handleException(e);
        }
    }

    public static String runJava(String className) {
        Assert.assertTrue((new File(MEASUREMENT_JAR)).exists());

        String[] command = new String[] { "java", "-classpath",
                "." + PATH_SEPARATOR + MEASUREMENT_JAR, className };

        try {
            Process process = Runtime.getRuntime().exec(command,
                                                        new String[0],
                                                        new File(TARGET));

            StringBuilder sBOut = new StringBuilder();
            StreamGobbler gobblerOut = new StreamGobbler(process.getInputStream(),
                                                         sBOut);
            gobblerOut.start();

            StringBuilder sBErr = new StringBuilder();
            StreamGobbler gobblerErr = new StreamGobbler(process.getErrorStream(),
                                                         sBErr);
            gobblerErr.start();

            process.waitFor();
            Assert.assertEquals("", sBErr.toString());
            Assert.assertEquals(0, process.exitValue());

            return sBOut.toString().replaceAll("\\r", "");
        } catch (IOException e) {
            handleException(e);
        } catch (InterruptedException e) {
            handleException(e);
        }

        return null;
    }

    /**
     * Reads out a file with <code>UTF-8</code> and dumps it to a String.
     */
    public static String getContentFromFile(File file) {
        try {
            Assert.assertTrue(file.exists());
            Assert.assertTrue(file.isFile());

            FileInputStream fileInputStream = new FileInputStream(file);
            InputStreamReader inputStreamReader = new InputStreamReader(fileInputStream,
                                                                        "UTF-8");

            int sourceFileLength = (int) file.length();
            char[] sourceFileArray = new char[sourceFileLength];
            sourceFileLength = Math.max(inputStreamReader.read(sourceFileArray,
                                                               0,
                                                               sourceFileLength),
                                        0);

            return new String(sourceFileArray, 0, sourceFileLength);
        } catch (FileNotFoundException e) {
            handleException(e);
        } catch (UnsupportedEncodingException e) {
            handleException(e);
        } catch (IOException e) {
            handleException(e);
        }

        return null;
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
     * code copied from
     * http://www.physionet.org/physiotools/puka/sourceCode/puka/StreamGobbler.java
     * which redirects to
     * http://www.javaworld.com/javaworld/jw-12-2000/jw-1229-traps.html When
     * Runtime.exec() won't: Navigate yourself around pitfalls related to the
     * Runtime.exec() method
     * 
     * @author Michael Daconta
     * @version 1.0 ($Id: UtilsForTesting.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public static class StreamGobbler extends Thread {
        InputStream inputStream;

        StringBuilder stringBuilder;

        StreamGobbler(InputStream is, StringBuilder stringBuilder) {
            this.inputStream = is;
            this.stringBuilder = stringBuilder;
        }

        /**
         * creates readers to handle the text created by the external program
         */
        @Override
        public void run() {
            try {
                InputStreamReader reader = new InputStreamReader(this.inputStream);

                char[] buff = new char[128];
                while (true) {
                    int iReadSize = reader.read(buff);
                    if (iReadSize > 0) {
                        this.stringBuilder.append(buff, 0, iReadSize);
                    } else {
                        break;
                    }
                }
            } catch (IOException e) {
                this.stringBuilder.append("IOException" + e.getMessage());
            }
        }
    }
}
