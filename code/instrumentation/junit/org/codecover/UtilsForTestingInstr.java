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
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.util.Map;

import junit.framework.Assert;

import org.codecover.instrumentation.HierarchyLevelContainer;
import org.codecover.instrumentation.Instrumenter;
import org.codecover.instrumentation.InstrumenterDescriptor;
import org.codecover.instrumentation.exceptions.ParseException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.HierarchyLevelType;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.utils.LogLevel;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.SimpleLogger;
import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.ConditionCoverage;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;

/**
 * Contains methods used by JUnit-Tests.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: UtilsForTestingInstr.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class UtilsForTestingInstr {
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

    public static class TestInstrumenterDescriptor2 extends InstrumenterDescriptor {

        public TestInstrumenterDescriptor2() {
            super(TestInstrumenterDescriptor2.class.getName());
            super.setLanguageName("java");
            super.setDescription("description2");
            super.setAuthor("author2");
            super.setDefaultCharset(Charset.defaultCharset());
    
            super.addSupportedCriteria(StatementCoverage.getInstance());
            super.addSupportedCriteria(BranchCoverage.getInstance());
            super.addSupportedCriteria(ConditionCoverage.getInstance());
            super.addSupportedCriteria(LoopCoverage.getInstance());
        }
    
        @Override
        public Instrumenter getInstrumenter() {
            return new Instrumenter(){

                @Override
                public boolean allowsFileListInstrumentation() {
                    return false;
                }

                @Override
                protected HierarchyLevelType getPackageHierarchyLevelType(MASTBuilder builder) {
                    return new HLTProvider(builder).getPackageType();
                }

                @Override
                protected void instrumentThis(Reader source, Writer target, MASTBuilder builder, SourceFile sourceFile, HierarchyLevelContainer rootContainer, String testSessionContainerUID, Map<String, Object> instrumenterDirectives) throws ParseException, IOException {
                    // do nothing at all
                }
            };
        }

        @Override
        public boolean accept(File file) {
            return false;
        }
    }

    public static class TestInstrumenterDescriptor1 extends InstrumenterDescriptor {
    
        public TestInstrumenterDescriptor1() {
            super(TestInstrumenterDescriptor1.class.getName());
            super.setLanguageName("java");
            super.setDescription("description1");
            super.setAuthor("author1");
            super.setDefaultCharset(Charset.defaultCharset());
    
            super.addSupportedCriteria(StatementCoverage.getInstance());
            super.addSupportedCriteria(BranchCoverage.getInstance());
            super.addSupportedCriteria(ConditionCoverage.getInstance());
            super.addSupportedCriteria(LoopCoverage.getInstance());
    
        }
    
        @Override
        public Instrumenter getInstrumenter() {
            return new Instrumenter(){

                @Override
                public boolean allowsFileListInstrumentation() {
                    return true;
                }

                @Override
                protected HierarchyLevelType getPackageHierarchyLevelType(MASTBuilder builder) {
                    return new HLTProvider(builder).getPackageType();
                }

                @Override
                protected void instrumentThis(Reader source, Writer target, MASTBuilder builder, SourceFile sourceFile, HierarchyLevelContainer rootContainer, String testSessionContainerUID, Map<String, Object> instrumenterDirectives) throws ParseException, IOException {
                    // do nothing at all
                }
            };
        }
        
        @Override
        public boolean accept(File file) {
            return false;
        }
    }

    public static class HLTProvider {
    
        private final MASTBuilder builder;
    
        private final HierarchyLevelType defPakType;
        
        private final HierarchyLevelType packageType;
    
        private final HierarchyLevelType classType;
        
        public HLTProvider(MASTBuilder builder) {
            this.builder = builder;
            this.defPakType = this.builder.createHierarchyLevelType("default", "default");
            this.packageType = this.builder.createHierarchyLevelType("package", "package");
            this.classType = this.builder.createHierarchyLevelType("class", "class");
        }
    
        public HierarchyLevelType getDefaultPackageType() {
            return this.defPakType;
        }
    
        public HierarchyLevelType getPackageType() {
            return this.packageType;
        }
        
        public HierarchyLevelType getClassType() {
            return this.classType;
        }
    }
}
