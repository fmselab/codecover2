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

package org.codecover.instrumentation.cobol85;

import static org.codecover.UtilsForTestingCobol.SOURCE;
import static org.codecover.UtilsForTestingCobol.handleException;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.io.Writer;
import java.util.UUID;

import org.codecover.UtilsForTestingCobol;
import org.codecover.instrumentation.HierarchyLevelContainer;
import org.codecover.instrumentation.NullWriter;
import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectives;
import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectivesManipulator;
import org.codecover.instrumentation.cobol85.compilerDirectives.DefaultCompilerDirectivesManipulator;
import org.codecover.instrumentation.cobol85.manipulators.DefaultBranchManipulator;
import org.codecover.instrumentation.cobol85.manipulators.DefaultConditionManipulator;
import org.codecover.instrumentation.cobol85.manipulators.DefaultLoopManipulator;
import org.codecover.instrumentation.cobol85.manipulators.DefaultStatementManipulator;
import org.codecover.instrumentation.cobol85.parser.CobolParser;
import org.codecover.instrumentation.cobol85.parser.ParseException;
import org.codecover.instrumentation.cobol85.parser.SimpleCharStream;
import org.codecover.instrumentation.cobol85.syntaxtree.CompilationUnit;
import org.codecover.instrumentation.cobol85.visitor.InstrumentationVisitor;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.HierarchyLevelType;
import org.codecover.model.mast.SourceFile;

import junit.framework.TestCase;

/**
 * @author Stefan Franke
 * @version 1.0 ($Id: CobolCounterTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CobolCounterTest extends TestCase {

    private char[] sourceFileArray;

    private Writer writer = NullWriter.INSTANCE;

    private MASTBuilder builder = UtilsForTestingCobol.newMASTBuilder();

    public void testCounterAllCoverage() {
        String srcPath = SOURCE
                + "AllCoverage/allCoverage.cob";
        File source = new File(srcPath);
        
        try {
            NodeCounter nodeCounter = NodeCounter.getInstance();
            nodeCounter.reset();
            SourceFile sourceFile = this.getSourceFile(source);
            StringReader reader = new StringReader(sourceFile.getContent());
            CompilerDirectivesManipulator compilerDirectivesManipulator = new DefaultCompilerDirectivesManipulator();
            SimpleCharStream simpleCharStream = new SimpleCharStream(reader, compilerDirectivesManipulator);
            CobolParser cobolParser = new CobolParser(simpleCharStream);
            CompilationUnit compilationUnit = cobolParser.CompilationUnit();
            
            CounterProvider counterProvider = new CounterProvider();
            
            InstrumentationVisitor instrumentationVisitor = this
                    .getInstrumentationVisitor(sourceFile, counterProvider);
            
            instrumentationVisitor
                    .setStatementManipulator(new DefaultStatementManipulator(
                            counterProvider, compilerDirectivesManipulator));
            instrumentationVisitor
                    .setBranchManipulator(new DefaultBranchManipulator(
                            counterProvider, compilerDirectivesManipulator));
            instrumentationVisitor
                    .setConditionManipulator(new DefaultConditionManipulator(
                            counterProvider, compilerDirectivesManipulator));
            instrumentationVisitor
                    .setLoopManipulator(new DefaultLoopManipulator(
                            counterProvider, compilerDirectivesManipulator));
            
            instrumentationVisitor.visit(compilationUnit);
            
            assertEquals(nodeCounter.getStatementCounter(), counterProvider
                    .getStatementCounter());
            // the source code contains 81 statements
            assertEquals(81, nodeCounter.getStatementCounter());
            assertEquals(nodeCounter.getBranchCounter(), counterProvider
                    .getBranchCounter());
            // the source code contains 52 branches, including implicit branches
            assertEquals(52, nodeCounter.getBranchCounter());
            assertEquals(nodeCounter.getConditionCounter(), counterProvider
                    .getConditionCounter());
            // the source code contains 30 conditions
            assertEquals(30, nodeCounter.getConditionCounter());
            assertEquals(nodeCounter.getLoopCounter(), counterProvider
                    .getLoopCounter());
            // the source code contains 4 looping statements
            assertEquals(4, nodeCounter.getLoopCounter());
        } catch (FileNotFoundException e) {
            handleException(e);
        } catch (ParseException e) {
            handleException(e);
        } catch (IOException e) {
            handleException(e);
        }

    }

    public void testCounterStatementCoverage() {
        String srcPath = SOURCE
                + "StatementCoverage/statementCoverage.cob";
        File source = new File(srcPath);
        
        try {
            NodeCounter nodeCounter = NodeCounter.getInstance();
            nodeCounter.reset();
            SourceFile sourceFile = this.getSourceFile(source);
            StringReader reader = new StringReader(sourceFile.getContent());
            CompilerDirectivesManipulator compilerDirectivesManipulator = new DefaultCompilerDirectivesManipulator();
            SimpleCharStream simpleCharStream = new SimpleCharStream(reader, compilerDirectivesManipulator);
            CobolParser cobolParser = new CobolParser(simpleCharStream);
            CompilationUnit compilationUnit = cobolParser.CompilationUnit();
            
            CounterProvider counterProvider = new CounterProvider();
            
            InstrumentationVisitor instrumentationVisitor = this
                    .getInstrumentationVisitor(sourceFile, counterProvider);
            
            instrumentationVisitor
                    .setStatementManipulator(new DefaultStatementManipulator(
                            counterProvider, compilerDirectivesManipulator));
            
            instrumentationVisitor.visit(compilationUnit);
            
            assertEquals(nodeCounter.getStatementCounter(), counterProvider
                    .getStatementCounter());
            // the source code contains 10 statements
            assertEquals(10, nodeCounter.getStatementCounter());
        } catch (FileNotFoundException e) {
            handleException(e);
        } catch (ParseException e) {
            handleException(e);
        } catch (IOException e) {
            handleException(e);
        }

    }

    public void testCounterBranchCoverage() {
        String srcPath = SOURCE
                + "BranchCoverage/branchCoverage.cob";
        File source = new File(srcPath);
        
        try {
            NodeCounter nodeCounter = NodeCounter.getInstance();
            nodeCounter.reset();
            SourceFile sourceFile = this.getSourceFile(source);
            StringReader reader = new StringReader(sourceFile.getContent());
            CompilerDirectivesManipulator compilerDirectivesManipulator = new DefaultCompilerDirectivesManipulator();
            SimpleCharStream simpleCharStream = new SimpleCharStream(reader, compilerDirectivesManipulator);
            CobolParser cobolParser = new CobolParser(simpleCharStream);
            CompilationUnit compilationUnit = cobolParser.CompilationUnit();
            
            CounterProvider counterProvider = new CounterProvider();
            
            InstrumentationVisitor instrumentationVisitor = this
                    .getInstrumentationVisitor(sourceFile, counterProvider);

            instrumentationVisitor
                    .setBranchManipulator(new DefaultBranchManipulator(
                            counterProvider, compilerDirectivesManipulator));
            
            instrumentationVisitor.visit(compilationUnit);
            
            assertEquals(nodeCounter.getBranchCounter(), counterProvider
                    .getBranchCounter());
            // the source code contains 24 branches, including implicit branches
            assertEquals(24, nodeCounter.getBranchCounter());
        } catch (FileNotFoundException e) {
            handleException(e);
        } catch (ParseException e) {
            handleException(e);
        } catch (IOException e) {
            handleException(e);
        }

    }

    public void testCounterConditionCoverage() {
        String srcPath = SOURCE
                + "ConditionCoverage/conditionCoverage.cob";
        File source = new File(srcPath);
        
        try {
            NodeCounter nodeCounter = NodeCounter.getInstance();
            nodeCounter.reset();
            SourceFile sourceFile = this.getSourceFile(source);
            StringReader reader = new StringReader(sourceFile.getContent());
            CompilerDirectivesManipulator compilerDirectivesManipulator = new DefaultCompilerDirectivesManipulator();
            SimpleCharStream simpleCharStream = new SimpleCharStream(reader, compilerDirectivesManipulator);
            CobolParser cobolParser = new CobolParser(simpleCharStream);
            CompilationUnit compilationUnit = cobolParser.CompilationUnit();
            
            CounterProvider counterProvider = new CounterProvider();
            
            InstrumentationVisitor instrumentationVisitor = this
                    .getInstrumentationVisitor(sourceFile, counterProvider);

            instrumentationVisitor
                    .setConditionManipulator(new DefaultConditionManipulator(
                            counterProvider, compilerDirectivesManipulator));
            
            instrumentationVisitor.visit(compilationUnit);
            
            assertEquals(nodeCounter.getConditionCounter(), counterProvider
                    .getConditionCounter());
            // the source code contains 11 conditions
            assertEquals(11, nodeCounter.getConditionCounter());
        } catch (FileNotFoundException e) {
            handleException(e);
        } catch (ParseException e) {
            handleException(e);
        } catch (IOException e) {
            handleException(e);
        }

    }

    public void testCounterLoopCoverage() {
        String srcPath = SOURCE
                + "LoopCoverage/loopCoverage.cob";
        File source = new File(srcPath);
        
        try {
            NodeCounter nodeCounter = NodeCounter.getInstance();
            nodeCounter.reset();
            SourceFile sourceFile = this.getSourceFile(source);
            StringReader reader = new StringReader(sourceFile.getContent());
            CompilerDirectivesManipulator compilerDirectivesManipulator = new DefaultCompilerDirectivesManipulator();
            SimpleCharStream simpleCharStream = new SimpleCharStream(reader, compilerDirectivesManipulator);
            CobolParser cobolParser = new CobolParser(simpleCharStream);
            CompilationUnit compilationUnit = cobolParser.CompilationUnit();
            
            CounterProvider counterProvider = new CounterProvider();
            
            InstrumentationVisitor instrumentationVisitor = this
                    .getInstrumentationVisitor(sourceFile, counterProvider);

            instrumentationVisitor
                    .setLoopManipulator(new DefaultLoopManipulator(
                            counterProvider, compilerDirectivesManipulator));
            
            instrumentationVisitor.visit(compilationUnit);
            
            assertEquals(nodeCounter.getLoopCounter(), counterProvider
                    .getLoopCounter());
            // the source code contains 3 looping statements
            assertEquals(3, nodeCounter.getLoopCounter());
        } catch (FileNotFoundException e) {
            handleException(e);
        } catch (ParseException e) {
            handleException(e);
        } catch (IOException e) {
            handleException(e);
        }

    }

    /**
     * Returns the source file as MAST object.
     * 
     * @param source
     *            source file as java-file object
     * @return source file as MAST object
     * @throws FileNotFoundException
     * @throws IOException
     */
    private SourceFile getSourceFile(File source) throws FileNotFoundException,
            IOException {
        FileInputStream fileInputStream = new FileInputStream(source);
        InputStreamReader inputStreamReader = new InputStreamReader(
                fileInputStream);

        // the number of characters is appropriated with the length of the
        // source file -> the number of characters might be smaller if
        // UTF-8 is used
        int sourceFileLength = (int) source.length();
        // prepate an array, that is long enough
        if (this.sourceFileArray == null) {
            this.sourceFileArray = new char[sourceFileLength];
        } else if (this.sourceFileArray.length < sourceFileLength) {
            this.sourceFileArray = new char[sourceFileLength];
        }
        // read from the inputStreamReader and get the exact character count
        sourceFileLength = Math.max(inputStreamReader.read(
                this.sourceFileArray, 0, sourceFileLength), 0);

        String sourceFileContent = new String(this.sourceFileArray, 0,
                sourceFileLength);
        String sourceFileName = source.getName();
        SourceFile sourceFile = this.builder.createSourceFile(sourceFileName,
                sourceFileContent);

        inputStreamReader.close();
        return sourceFile;
    }

    /**
     * Returns the instrumentation visitor.
     * 
     * @param sourceFile the source file as MAST object
     * @param counterProvider the counter provider
     * @return instrumentation visitor
     */
    private InstrumentationVisitor getInstrumentationVisitor(
            SourceFile sourceFile, CounterProvider counterProvider) {
        HierarchyLevelType rootType = HierarchyLevelTypes
                .getSourceFileType(this.builder);

        HierarchyLevelContainer rootHierarchyLevelContainer = new HierarchyLevelContainer(
                rootType.getInternalName(), rootType, rootType);

        String testSessionContainerUID = UUID.randomUUID().toString();

        CompilerDirectives compilerDirectives = new CompilerDirectives();
        CompilerDirectivesManipulator compilerDirectivesManipulator = new DefaultCompilerDirectivesManipulator();
        InstrumentationVisitor instrumentationVisitor = new InstrumentationVisitor(
                this.writer, this.builder, sourceFile,
                rootHierarchyLevelContainer, counterProvider,
                testSessionContainerUID, compilerDirectives,
                compilerDirectivesManipulator);
        return instrumentationVisitor;
    }

}
