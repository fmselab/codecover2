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

package org.codecover.instrumentation.java15;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;

import org.codecover.instrumentation.HierarchyLevelContainer;
import org.codecover.instrumentation.exceptions.InstrumentationException;
import org.codecover.instrumentation.exceptions.InstrumentationFileNotFoundException;
import org.codecover.instrumentation.exceptions.InstrumentationIOException;
import org.codecover.instrumentation.exceptions.ParseException;
import org.codecover.instrumentation.java.measurement.CoverageResultLogFile;
import org.codecover.instrumentation.java15.manipulators.ArrayBranchManipulator;
import org.codecover.instrumentation.java15.manipulators.ArrayConditionManipulator;
import org.codecover.instrumentation.java15.manipulators.ArrayLoopManipulator;
import org.codecover.instrumentation.java15.manipulators.ArrayStatementManipulator;
import org.codecover.instrumentation.java15.manipulators.ArraySynchronizedManipulator;
import org.codecover.instrumentation.java15.manipulators.ArrayQMOManipulator;
import org.codecover.instrumentation.java15.manipulators.BranchManipulator;
import org.codecover.instrumentation.java15.manipulators.CommentManipulator;
import org.codecover.instrumentation.java15.manipulators.ConditionManipulator;
import org.codecover.instrumentation.java15.manipulators.DefaultCommentManipulator;
import org.codecover.instrumentation.java15.manipulators.LoopManipulator;
import org.codecover.instrumentation.java15.manipulators.StatementManipulator;
import org.codecover.instrumentation.java15.manipulators.SynchronizedManipulator;
import org.codecover.instrumentation.java15.manipulators.QMOManipulator;
import org.codecover.instrumentation.java15.parser.JavaParser;
import org.codecover.instrumentation.java15.syntaxtree.CompilationUnit;
import org.codecover.instrumentation.java15.visitor.InstrumentationVisitor;
import org.codecover.instrumentation.measurement.CoverageResultLog;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.HierarchyLevelType;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.ConditionCoverage;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;
import org.codecover.model.utils.criteria.SynchronizedStatementCoverage;
import org.codecover.model.utils.criteria.QMOCoverage;
import org.codecover.model.utils.file.SourceTargetContainer;

import static org.codecover.instrumentation.java15.InstrumenterDescriptor.CoverageLogPathDirective;

/**
 * This is a Instrumenter for the Java programming language.<br>
 * <br>
 * When updating this file - please update {@link InstrumenterDescriptor} too.
 * 
 * @author Christoph Müller, Stefan Franke
 * @version 1.0 ($Id: Instrumenter.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
class Instrumenter extends org.codecover.instrumentation.Instrumenter {

    HierarchyLevelTypeProvider hierarchyLevelTypeProvider = null;

    /**
     * The order to instrument the source and write the instrumented source code
     * in target.
     * 
     * @param source
     *            The source code to instrument.
     * @param target
     *            The instrumented source code.
     * @param builder
     *            The {@link MASTBuilder}
     * @param sourceFile
     *            The source file
     * @param testSessionContainerUID
     *            The UID of the {@link TestSessionContainer}.
     * @param hierarchyLevelContainer
     *            This is the {@link HierarchyLevelContainer}, where all top
     *            level {@link HierarchyLevel}s of the source file can be added
     *            using
     *            {@link HierarchyLevelContainer#addHierarchyLevels(Collection, LinkedList)}.
     * @throws ParseException
     *             If an exceptions occurs during the parsing: e.g.
     *             {@link JavaParser#CompilationUnit()}.
     * @throws IOException
     *             On the occurrence of write exceptions.
     */
    @Override
    protected void instrumentThis(Reader source,
            Writer target,
            MASTBuilder builder,
            SourceFile sourceFile,
            HierarchyLevelContainer hierarchyLevelContainer,
            String testSessionContainerUID,
            Map<String, Object> instrumenterDirectives) throws ParseException,
            IOException {
        JavaParser javaParser;
        CompilationUnit compilationUnit;
        InstrumentationVisitor instrumentationVisitor;
        StatementManipulator statementManipulator;
        BranchManipulator branchManipulator;
        ConditionManipulator conditionManipulator;
        LoopManipulator loopManipulator;
        CommentManipulator commentManipulator;
        SynchronizedManipulator syncStatementManipulator;
        QMOManipulator qmoManipulator;
        Class<? extends CoverageResultLog> coveragResultLog;
        String preferredCoverageLogPath;

        javaParser = new JavaParser(sourceFile.getContent());
        compilationUnit = javaParser.CompilationUnit();
        // assert: there was no ParseException

        coveragResultLog = CoverageResultLogFile.class;
        preferredCoverageLogPath = (String)instrumenterDirectives.get(CoverageLogPathDirective.KEY);
        instrumentationVisitor = new InstrumentationVisitor(target,
                coveragResultLog,
                builder,
                sourceFile,
                hierarchyLevelContainer,
                testSessionContainerUID,
                preferredCoverageLogPath);

        if (super.isCriterionSet(StatementCoverage.getInstance())) {
            statementManipulator = new ArrayStatementManipulator();
            instrumentationVisitor.setStatementManipulator(statementManipulator);
        }

        if (super.isCriterionSet(BranchCoverage.getInstance())) {
            branchManipulator = new ArrayBranchManipulator();
            instrumentationVisitor.setBranchManipulator(branchManipulator);
        }

        if (super.isCriterionSet(ConditionCoverage.getInstance())) {
            conditionManipulator = new ArrayConditionManipulator();
            instrumentationVisitor.setConditionManipulator(conditionManipulator);
        }

        if (super.isCriterionSet(LoopCoverage.getInstance())) {
            loopManipulator = new ArrayLoopManipulator();
            instrumentationVisitor.setLoopManipulator(loopManipulator);
        }

        if (super.isCriterionSet(SynchronizedStatementCoverage.getInstance())) {
            syncStatementManipulator = new ArraySynchronizedManipulator();
            instrumentationVisitor.setSyncStatementManipulator(syncStatementManipulator);
        }

        if (super.isCriterionSet(QMOCoverage.getInstance())) {
            qmoManipulator = new ArrayQMOManipulator();
            instrumentationVisitor.setQMOManipulator(qmoManipulator);
        }
        
        commentManipulator = new DefaultCommentManipulator();
        instrumentationVisitor.setCommentManipulator(commentManipulator);

        instrumentationVisitor.visit(compilationUnit);
    }

    @Override
    protected void notifyEnd(File rootFolder,
            File targetFolder,
            Collection<SourceTargetContainer> jobs,
            HierarchyLevelContainer rootHierarchyLevelContainer,
            MASTBuilder builder,
            String testSessionContainerUID,
            Map<String, Object> instrumenterDirectives)
            throws InstrumentationException {
        if (pretend()) {
            // we needn't copy the helper files
            return;
        }

        // purpose -> we want to copy helper classes, that are needed for
        // measurement
        Boolean java14Compatiblity = (Boolean) instrumenterDirectives.get(InstrumenterDescriptor.Java14Compatibility.KEY);
        try {
            JavaInstrumentationHelper.copyMeasurementHelpersToInstrumentFolder(targetFolder,
                    java14Compatiblity.booleanValue(),
                    super.getCharset());
            builder.getLogger().info("Additional classes added to:\n" + targetFolder.getAbsolutePath());
        } catch (FileNotFoundException e) {
            throw new InstrumentationFileNotFoundException(e);
        } catch (IOException e) {
            throw new InstrumentationIOException(e);
        } catch (RuntimeException e) {
            throw new InstrumentationException(e);
        }  
    }

    /**
     * Returns {@link HierarchyLevelType} which should be used to build the
     * {@link HierarchyLevel} tree out of the generated {@link HierarchyLevel}s
     * that are found during the
     * {@link #instrumentThis(Reader, Writer, MASTBuilder, SourceFile, HierarchyLevelContainer, String, Map)}.
     * 
     * @param builder
     *            The {@link MASTBuilder} for
     *            {@link MASTBuilder#createHierarchyLevelType(String, String)}.
     * 
     * @return {@link HierarchyLevelTypeProvider#getPackageType()}
     */
    @Override
    protected HierarchyLevelType getPackageHierarchyLevelType(MASTBuilder builder) {
        if (this.hierarchyLevelTypeProvider == null) {
            this.hierarchyLevelTypeProvider = new HierarchyLevelTypeProvider(
                    builder);
        }

        return this.hierarchyLevelTypeProvider.getPackageType();
    }

    /**
     * Returns {@link HierarchyLevelType} which should be used to build the root
     * of the {@link HierarchyLevel} tree out of the generated
     * {@link HierarchyLevel}s that are found during the
     * {@link #instrumentThis(Reader, Writer, MASTBuilder, SourceFile, HierarchyLevelContainer, String, Map)}.
     * 
     * @param builder
     *            The {@link MASTBuilder} for
     *            {@link MASTBuilder#createHierarchyLevelType(String, String)}.
     * 
     * @return {@link HierarchyLevelTypeProvider#getDefaultPackageType()}
     */
    @Override
    protected HierarchyLevelType getRootHierarchyLevelType(MASTBuilder builder) {
        if (this.hierarchyLevelTypeProvider == null) {
            this.hierarchyLevelTypeProvider = new HierarchyLevelTypeProvider(
                    builder);
        }

        return this.hierarchyLevelTypeProvider.getDefaultPackageType();
    }

    /**
     * This instrumenter allows the instrumentation of more than one source
     * file.
     * 
     * @return true.
     */
    @Override
    public boolean allowsFileListInstrumentation() {
        return true;
    }
}
