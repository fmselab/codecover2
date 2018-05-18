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
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import junit.framework.Assert;

import org.codecover.instrumentation.DefaultInstrumenterFactory;
import org.codecover.instrumentation.Instrumenter;
import org.codecover.instrumentation.InstrumenterFactory;
import org.codecover.instrumentation.exceptions.FactoryMisconfigurationException;
import org.codecover.instrumentation.exceptions.InstrumentationException;
import org.codecover.instrumentation.java15.InstrumenterDescriptor;
import org.codecover.instrumentation.java15.parser.JavaParser;
import org.codecover.instrumentation.measurement.CoverageCounterLog;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.LocationList;
import org.codecover.model.utils.LogLevel;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.SimpleLogger;
import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.ConditionCoverage;
import org.codecover.model.utils.criteria.Criterion;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;
import org.codecover.model.utils.file.FileTool;
import org.codecover.model.utils.file.SourceTargetContainer;

/**
 * Contains methods used by JUnit-Tests.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: UtilsForTestingJava.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class UtilsForTestingJava {
    /** point to <code>testsource/</code> */
    public static String SOURCE;

    /** point to <code>testsource/org/codecover/instrumentation/java15/test/</code> */
    public static String TEST_SOURCE;

    /** point to <code>testtarget/</code> */
    public static String TARGET;

    /** point to <code>testtarget/org/codecover/instrumentation/java15/test/</code> */
    public static String TEST_TARGET;

    /** point to the instrumentation folder in code */
    public static String BASEDIR;

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

        TEST_SOURCE = SOURCE + "org/codecover/instrumentation/java15/test/";
        TEST_TARGET = TARGET + "org/codecover/instrumentation/java15/test/";

        if (logLevel != null && logLevel.equals("INFO")) {
            LOGGER = new SimpleLogger(System.out, LogLevel.INFO);
        } else if (logLevel != null && logLevel.equals("WARNING")) {
                LOGGER = new SimpleLogger(System.out, LogLevel.WARNING);
        } else {
            LOGGER = new SimpleLogger(new PrintStream(new ToNoWhereStream(), true), LogLevel.FATAL);
        }

        LOGGER.info("TEST_SOURCE: " + TEST_SOURCE);
        LOGGER.info("TEST_TARGET: " + TEST_TARGET);
    }

    /** point to <code>testtarget/database.xml</code> */
    public static final String SESSION_CONTAINER = TARGET + "session_container.xml";

    private static long LAST_TIMESTAMP = Long.MIN_VALUE;
    
    /**
     * Gets an initialized {@link MASTBuilder}<br>
     * <br>
     * The Logger is initialized to write to a {@link ToNoWhereStream}.
     * 
     * @return the {@link MASTBuilder}
     */
    public static MASTBuilder newMASTBuilder(){
        return new MASTBuilder(LOGGER);
    }

    public static void locationAssertion(String expected, LocationList locationList) {
        if (expected == null) {
            Assert.assertTrue(locationList.getLocations().isEmpty());
        } else {
            Assert.assertEquals(1, locationList.getLocations().size());
            Location singleLocation = locationList.getLocations().get(0);
            Assert.assertEquals(expected, singleLocation.getContent());
        }
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
     * Clears the target path and all potential files.
     */
    public static void clearTarget() {
        deleteFileRecursively(new File(TARGET + "/org"));
        deleteFileRecursively(new File(TARGET + "/de"));
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
        if (fileOrDirectory.exists()) {
            String message = String.format("Could not delete %s.\nis Dir: %s\nfiles in Dir: %d",
                    fileOrDirectory.getAbsolutePath(),
                    Boolean.toString(fileOrDirectory.isDirectory()),
                    (fileOrDirectory.isDirectory() ? new Integer(fileOrDirectory.listFiles().length) : Integer.valueOf(0)));
            Assert.fail(message);
        }
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
                    Assert.assertFalse("Could not delete: " + file.getAbsolutePath(), file.exists());
                }
            }
        }
    }
    
    public static String locationToString(LocationList locationList) {
        return locationToString(locationList.getLocations());
    }

    public static String locationToString(List<Location> locationList) {
        if (locationList.isEmpty()) {
            return "EMPTY";
        }
        if (locationList.size() == 1) {
            return locationList.get(0).getContent();
        }
        StringBuilder content = new StringBuilder();
        for (Location thisLocation : locationList) {
            content.append(thisLocation.getContent());
        }
        return content.toString();
    }

    public static void runJavac(File fileToCompile) {
        fileToCompile = fileToCompile.getAbsoluteFile();
        Assert.assertTrue(fileToCompile.getAbsolutePath(), fileToCompile.exists());
        Assert.assertTrue(fileToCompile.getAbsolutePath(), fileToCompile.isFile());

        final String javacCommand;
        final File javacWindows = new File("C:\\Program Files\\Java\\jdk1.5.0_12\\bin\\javac.exe");
        if (javacWindows.exists()) {
            javacCommand = javacWindows.getPath();
        } else {
            javacCommand = "javac";
        }

        String[] command = new String[]{
                javacCommand,
                "-sourcepath", TARGET,
                "-d", TARGET,
                "-encoding", "UTF-8",
                "-source", "1.5",
                "-target", "1.5",
                fileToCompile.getAbsolutePath()
        };

        try {
            Process process = Runtime.getRuntime().exec(command);

            StringBuilder sBOut = new StringBuilder();
            StreamGobbler gobblerOut = new StreamGobbler(process.getInputStream(), sBOut);
            gobblerOut.start();

            StringBuilder sBErr = new StringBuilder();
            StreamGobbler gobblerErr = new StreamGobbler(process.getErrorStream(), sBErr);
            gobblerErr.start();

            process.waitFor();
            Assert.assertEquals("", sBOut.toString());
            String sbErrString = sBErr.toString();
            if (sbErrString.length() > 0) {
                sbErrString = sbErrString.replaceAll("\\r\\n", "\n");
                sbErrString = sbErrString.replaceAll("\\r", "\n");
                LineNumberReader lineReader = new LineNumberReader(new StringReader(sbErrString));
                String thisLine = lineReader.readLine();
                while (thisLine != null) {
                    Assert.assertTrue(sbErrString, thisLine.startsWith("Note: "));
                    thisLine = lineReader.readLine();
                }
            }

            String classFilePath = fileToCompile.getPath();
            File classFile = new File(classFilePath.substring(0, classFilePath.length() - 4) + "class");
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
        return runJava(className, new String[0], new String[0]);
    }

    public static String runJava(String className,
                                 String[] jvmArgs,
                                 String[] progArgs) {
        String[] command = new String[4 + jvmArgs.length + progArgs.length];
        command[0] = "java";
        command[1] = "-classpath";
        command[2] = ".";
        System.arraycopy(jvmArgs, 0, command, 3, jvmArgs.length);
        command[jvmArgs.length + 3] = className;
        System.arraycopy(progArgs, 0, command, jvmArgs.length + 4, progArgs.length);

        try {
            Process process = Runtime.getRuntime().exec(command, new String[0], new File(TARGET));

            StringBuilder sBOut = new StringBuilder();
            StreamGobbler gobblerOut = new StreamGobbler(process.getInputStream(), sBOut);
            gobblerOut.start();

            StringBuilder sBErr = new StringBuilder();
            StreamGobbler gobblerErr = new StreamGobbler(process.getErrorStream(), sBErr);
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
     * Is targetFile a copy of sourceFile
     */
    public static void checkIsACopy(String sourceFile, String targetFile) {
        checkIsACopy(new File(sourceFile), new File(targetFile));
    }

    /**
     * Is targetFile a copy of sourceFile
     */
    public static void checkIsACopy(File sourceFile, File targetFile) {
        Assert.assertTrue(sourceFile.exists());
        Assert.assertTrue(targetFile.exists());
        Assert.assertEquals(sourceFile.length(), targetFile.length());
        Assert.assertTrue(sourceFile.lastModified() <= targetFile.lastModified());
    }

    /**
     * Calls {@link JavaParser#CompilationUnit()} to test, if an 
     * instrumented file is accepted by the Parser. Uses <code>UTF-8</code>
     * as encoding 
     */
    public static boolean isCompileableJava(File sourceFile) {
        return isCompileableJava(sourceFile, Charset.forName("UTF-8"));
    }

    /**
     * Calls {@link JavaParser} to test, if an 
     * instrumented file is accepted by the Parser.
     */
    public static boolean isCompileableJava(File sourceFile, Charset charset) {
        Assert.assertTrue(sourceFile.exists());
        try {
            JavaParser parser = new JavaParser(sourceFile, charset);
            parser.CompilationUnit();
        } catch (Exception e) {
            handleException(e);
        }

        return true;
    }

    /**
     * Simple tests <b>after</b> the instrumentation:
     * <ul>
     * <li>is the date before instrumentation before {@link TestSessionContainer#getDate()}</li>
     * <li>is the date now instrumentation after {@link TestSessionContainer#getDate()}</li>
     * <li>is the {@link TestSessionContainer#getCode()} not null</li>
     * <li>are the criteria of the instrumenter and the code base the same</li>
     * </ul> 
     */
    public static void simpleTestSessionContainerTests(Date dateBefore,
                                                       TestSessionContainer testSessionContainer,
                                                       Instrumenter instumenter) {
        Assert.assertFalse(dateBefore.after(testSessionContainer.getDate()));
        Assert.assertFalse(testSessionContainer.getDate().after(new Date()));
        Assert.assertNotNull(testSessionContainer.getCode());
        Assert.assertEquals(instumenter.getCriteria(), testSessionContainer.getCriteria());
    }

    public static final String[] FILES_TO_COPY_BASIC = {
            "org/codecover/instrumentation/measurement/CoverageCounterLog.java",
            "org/codecover/instrumentation/measurement/CoverageResultLog.java",
            "org/codecover/instrumentation/measurement/MeasurementConstants.java"
    };

    public static final String[] FILES_TO_COPY_MEASUREMENT = {
            "org/codecover/instrumentation/java/measurement/ConditionCounter.java",
            "org/codecover/instrumentation/java/measurement/CounterContainer.java",
            "org/codecover/instrumentation/java/measurement/CoverageLogPath.java",
            "org/codecover/instrumentation/java/measurement/CoverageResultLogFile.java",
            "org/codecover/instrumentation/java/measurement/LargeConditionCounter.java",
            "org/codecover/instrumentation/java/measurement/LongContainer.java",
            "org/codecover/instrumentation/java/measurement/MeasurementTimer.java",
            "org/codecover/instrumentation/java/measurement/MediumConditionCounter.java",
            "org/codecover/instrumentation/java/measurement/Protocol.java",
            "org/codecover/instrumentation/java/measurement/ProtocolImpl.java",
            "org/codecover/instrumentation/java/measurement/SmallOneConditionCounter.java",
            "org/codecover/instrumentation/java/measurement/SmallTwoConditionCounter.java",
            "org/codecover/instrumentation/java/measurement/TestMethod.java"
    };

    public static final String[] FILES_TO_COPY_MEASUREMENT_JAVA15 = {
        "org/codecover/instrumentation/java/measurement/jmx/JMXFileTransferException.java",
        "org/codecover/instrumentation/java/measurement/jmx/LiveNotification.java",
        "org/codecover/instrumentation/java/measurement/jmx/LiveNotificationMBean.java"
    };

    public static final String[] FILES_TO_COPY_MEASUREMENT_JAVA14 = {
        "org/codecover/instrumentation/java/measurement/ProtocolImpl.java14"
    };

    public static void copyMeasurementHelpersToBin() {
        try {
            File source;
            File target;

            for (String thisFile : FILES_TO_COPY_BASIC) {
                source = new File(BASEDIR + "../instrumentation/src/" + thisFile);
                target = new File(BASEDIR + "../instrumentation-java-measurement/bin/" + thisFile); 
                
                if (target.exists()) {
                    continue;
                }
                FileTool.copy(source, target);
            }

            for (String thisFile : FILES_TO_COPY_MEASUREMENT) {
                source = new File(BASEDIR + "../instrumentation-java-measurement/src/" + thisFile);
                target = new File(BASEDIR + "../instrumentation-java-measurement/bin/" + thisFile); 

                if (target.exists()) {
                    continue;
                }
                FileTool.copy(source, target);
            }
            
            for (String thisFile : FILES_TO_COPY_MEASUREMENT_JAVA15) {
                source = new File(BASEDIR + "../instrumentation-java-measurement/src/" + thisFile);
                target = new File(BASEDIR + "../instrumentation-java-measurement/bin/" + thisFile); 
                
                if (target.exists()) {
                    continue;
                }
                FileTool.copy(source, target);
            }

            for (String thisFile : FILES_TO_COPY_MEASUREMENT_JAVA14) {
                source = new File(BASEDIR + "../instrumentation-java-measurement/src/" + thisFile);
                target = new File(BASEDIR + "../instrumentation-java-measurement/bin/" + thisFile); 
                
                if (target.exists()) {
                    continue;
                }
                FileTool.copy(source, target);
            }
        } catch (IOException e) {
            handleException(e);
        }
    }

    public static void checkAreMeasurementHelpersCopied() {
        checkAreMeasurementHelpersCopied(new String[][]{
           FILES_TO_COPY_BASIC,
           FILES_TO_COPY_MEASUREMENT,
           FILES_TO_COPY_MEASUREMENT_JAVA15
        });
    }

    public static void checkAreMeasurementHelpersCopied(String[][] arraysToBeChecked) {
        for (int indexArray = 0; indexArray < arraysToBeChecked.length; indexArray++) {
            for (int indexFile = 0; indexFile < arraysToBeChecked[indexArray].length; indexFile++) {
                File fileToSearch = new File(TARGET + arraysToBeChecked[indexArray][indexFile]).getAbsoluteFile();
                Assert.assertTrue(fileToSearch.getAbsolutePath(), fileToSearch.exists()); 
            }
        }
    }

    public static final Criterion[] ALL_JAVA_CRITERIA = new Criterion[]{
            StatementCoverage.getInstance(),
            BranchCoverage.getInstance(),
            ConditionCoverage.getInstance(),
            LoopCoverage.getInstance(),
    };
    
    /**
     * This will run the Java Instrumenter, run some simple tests and try to
     * compile the instrumented file.
     * 
     * @param subpackage
     *                The sub package under
     *                <code>testsource/org/codecover/instrumentation/java15/test/</code>.
     * @param className
     *                The class name.
     */
    public static HierarchyLevel instrumentAndCompile(String subpackage,
                                                      String className) throws Exception {
        return instrumentAndCompile(subpackage,
                                    className,
                                    ALL_JAVA_CRITERIA);
    }

    /**
     * This will run the Java Instrumenter, run some simple tests and try to
     * compile the instrumented file.
     * 
     * @param subpackage
     *                The sub package under
     *                <code>testsource/org/codecover/instrumentation/java15/test/</code>.
     * @param className
     *                The class name.
     *                
     * @see #instrumentAndCompile(String, String)
     * @see #defaultInstrumentationRoutine(String, String, Criterion[])
     */
    public static HierarchyLevel instrumentAndCompile(String subpackage,
                                                      String className,
                                                      Criterion[] criteria) throws Exception {
        
        final String targetPath = TEST_TARGET + subpackage + "/" + className + ".java";
        final File target = new File(targetPath);

        HierarchyLevel instrumentedClass = defaultInstrumentationRoutine(subpackage,
                                                                         className,
                                                                         criteria);

        runJavac(target);

        return instrumentedClass;
    }

    public static Instrumenter getDefaultJavaInstrumenter() {
        return getDefaultJavaInstrumenter(ALL_JAVA_CRITERIA);
    }

    public static Instrumenter getDefaultJavaInstrumenter(Criterion[] criteria) {
        org.codecover.instrumentation.InstrumenterDescriptor descriptor = new InstrumenterDescriptor();
        InstrumenterFactory factory = new DefaultInstrumenterFactory();
        factory.setDescriptor(descriptor);
        factory.setCharset(Charset.forName("UTF-8"));
        factory.setPretendMode(false);

        for (Criterion thisCriterion : criteria) {
            factory.addCriterion(thisCriterion);
        }

        try {
            return factory.getInstrumenter();
        } catch (FactoryMisconfigurationException e) {
            handleException(e);
            return null;
        }
    }

    /**
     * This will run the Java Instrumenter and run some simple tests.
     * 
     * @param subpackage
     *                The sub package under
     *                <code>testsource/org/codecover/instrumentation/java15/test/</code>.
     * @param className
     *                The class name.
     * @throws FactoryMisconfigurationException 
     */
    public static HierarchyLevel defaultInstrumentationRoutine(String subpackage,
                                                               String className,
                                                               Criterion[] criteria) throws Exception {

        UtilsForTestingJava.clearTarget();
        UtilsForTestingJava.copyMeasurementHelpersToBin();

        Instrumenter instrumenter = getDefaultJavaInstrumenter(criteria);
        MASTBuilder builder = UtilsForTestingJava.newMASTBuilder();

        // instrument
        final String srcPath = TEST_SOURCE + subpackage + "/" + className + ".java";
        final File source = new File(srcPath);
        final String targetPath = TEST_TARGET + subpackage + "/" + className + ".java";
        final File target = new File(targetPath);
        Date dateBefore = new Date();
        TestSessionContainer testSessionContainer = null;
        Collection<SourceTargetContainer> container = Collections.<SourceTargetContainer>singleton(
                new SourceTargetContainer(source, target));

        try {
            testSessionContainer = instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    container,
                    builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }
    
        Assert.assertTrue(target.exists());

        simpleTestSessionContainerTests(dateBefore, testSessionContainer, instrumenter);
        return hierarchyLevelTest(testSessionContainer, subpackage, className);
    }

    private static HierarchyLevel hierarchyLevelTest(TestSessionContainer testSessionContainer, String subpackage, String className) {
        HierarchyLevel h1 = testSessionContainer.getCode();
        Assert.assertEquals("default package", h1.getName());
        Assert.assertEquals("default package", h1.getType().getInternalName());
        Assert.assertEquals("default package", h1.getType().getEnglishName());
        Assert.assertTrue(h1.getSequences().isEmpty());
        Assert.assertEquals(1, h1.getChildren().size());
    
        HierarchyLevel h2 = h1.getChildren().get(0);
        Assert.assertEquals("org", h2.getName());
        Assert.assertEquals("package", h2.getType().getInternalName());
        Assert.assertEquals("package", h2.getType().getEnglishName());
        Assert.assertTrue(h2.getSequences().isEmpty());
        Assert.assertEquals(1, h2.getChildren().size());
    
        HierarchyLevel h3 = h2.getChildren().get(0);
        Assert.assertEquals("codecover", h3.getName());
        Assert.assertEquals("package", h3.getType().getInternalName());
        Assert.assertEquals("package", h3.getType().getEnglishName());
        Assert.assertTrue(h3.getSequences().isEmpty());
        Assert.assertEquals(1, h3.getChildren().size());
    
        HierarchyLevel h4 = h3.getChildren().get(0);
        Assert.assertEquals("instrumentation", h4.getName());
        Assert.assertEquals("package", h4.getType().getInternalName());
        Assert.assertEquals("package", h4.getType().getEnglishName());
        Assert.assertTrue(h4.getSequences().isEmpty());
        Assert.assertEquals(1, h4.getChildren().size());
    
        HierarchyLevel h5 = h4.getChildren().get(0);
        Assert.assertEquals("java15", h5.getName());
        Assert.assertEquals("package", h5.getType().getInternalName());
        Assert.assertEquals("package", h5.getType().getEnglishName());
        Assert.assertTrue(h5.getSequences().isEmpty());
        Assert.assertEquals(1, h5.getChildren().size());
    
        HierarchyLevel h6 = h5.getChildren().get(0);
        Assert.assertEquals("test", h6.getName());
        Assert.assertEquals("package", h6.getType().getInternalName());
        Assert.assertEquals("package", h6.getType().getEnglishName());
        Assert.assertTrue(h6.getSequences().isEmpty());
        Assert.assertEquals(1, h6.getChildren().size());
    
        HierarchyLevel h7 = h6.getChildren().get(0);
        Assert.assertEquals(subpackage, h7.getName());
        Assert.assertEquals("package", h7.getType().getInternalName());
        Assert.assertEquals("package", h7.getType().getEnglishName());
        Assert.assertTrue(h7.getSequences().isEmpty());
        Assert.assertEquals(1, h7.getChildren().size());
    
        HierarchyLevel h8 = h7.getChildren().get(0);
        Assert.assertEquals(className, h8.getName());
        Assert.assertEquals("class", h8.getType().getInternalName());
        Assert.assertEquals("class", h8.getType().getEnglishName());
        
        return h8;
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
     * @version 1.0 ($Id: UtilsForTestingJava.java 1 2007-12-12 17:37:26Z t-scheller $)
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

    /**
     * code copied from
     * http://www.physionet.org/physiotools/puka/sourceCode/puka/StreamGobbler.java
     * 
     * which redirects to
     * http://www.javaworld.com/javaworld/jw-12-2000/jw-1229-traps.html When
     * Runtime.exec() won't: Navigate yourself around pitfalls related to the
     * Runtime.exec() method
     * 
     * @author Michael Daconta
     *
     * @version 1.0 ($Id: UtilsForTestingJava.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public static class StreamGobbler extends Thread
    {
        InputStream inputStream;
        StringBuilder stringBuilder;

        StreamGobbler(InputStream is, StringBuilder stringBuilder)
        {
            this.inputStream = is;
            this.stringBuilder = stringBuilder;
        }

        /**
         * creates readers to handle the text created by the external
         * program
         */             
        @Override
        public void run() {
            try {
                InputStreamReader reader = new InputStreamReader(
                        this.inputStream);

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

    public static void resetTimestamp() {
        LAST_TIMESTAMP = System.currentTimeMillis();
    }

    public static void printTimestamp() {
        long diff = System.currentTimeMillis() - LAST_TIMESTAMP;
        resetTimestamp();
        LOGGER.warning(String.format("TIME: %,11dms",
                new Long (diff)));
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

    public static void printMemory() {
        long totalMemory = Runtime.getRuntime().totalMemory();
        long freeMemory = Runtime.getRuntime().freeMemory();

        LOGGER.warning(String.format("MEM: %,12dB",
                        new Long (totalMemory - freeMemory)));
    }
}
