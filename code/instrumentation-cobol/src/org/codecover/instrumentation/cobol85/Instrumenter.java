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

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;

import org.codecover.instrumentation.HierarchyLevelContainer;
import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectives;
import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectivesManipulator;
import org.codecover.instrumentation.cobol85.manipulators.DefaultBranchManipulator;
import org.codecover.instrumentation.cobol85.manipulators.DefaultConditionManipulator;
import org.codecover.instrumentation.cobol85.manipulators.DefaultLoopManipulator;
import org.codecover.instrumentation.cobol85.manipulators.DefaultStatementManipulator;
import org.codecover.instrumentation.cobol85.manipulators.DefaultStaticPartManipulator;
import org.codecover.instrumentation.cobol85.parser.CobolParser;
import org.codecover.instrumentation.cobol85.parser.SimpleCharStream;
import org.codecover.instrumentation.cobol85.syntaxtree.CompilationUnit;
import org.codecover.instrumentation.cobol85.visitor.InstrumentationVisitor;
import org.codecover.instrumentation.exceptions.InstrumentationException;
import org.codecover.instrumentation.exceptions.InstrumentationRuntimeException;
import org.codecover.instrumentation.exceptions.ParseException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.HierarchyLevelType;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.ConditionCoverage;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;
import org.codecover.model.utils.file.SourceTargetContainer;

/**
 * This class is the main class of the COBOL instrumenter. It is derived from
 * the {@link org.codecover.instrumentation.Instrumenter} and overrides the
 * instrumentThis method to controls the instrumentation process.<br>
 * <br>
 * <b>Attention</b> dont use {@link #instrument(File, File, Collection, MASTBuilder, Map)}
 * with a Collection with more than one file. This will cause an
 * {@link InstrumentationRuntimeException}.
 *
 * @author Stefan Franke
 * @version 1.0 ($Id: Instrumenter.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 * @see #allowsFileListInstrumentation()
 */
class Instrumenter extends org.codecover.instrumentation.Instrumenter {

    /**
     * Constructs the COBOL {@link Instrumenter}.
     */
    public Instrumenter() {
        super();
    }

    @Override
    public TestSessionContainer instrument(File rootFolder, File targetFolder, Collection<SourceTargetContainer> jobs, MASTBuilder builder, Map<String, Object> instrumenterDirectives) throws InstrumentationException {
        if (jobs.size() > 1) {
           String message = "instrumentation of more than one file not allowed for this instrumenter";
              throw new InstrumentationRuntimeException(message);
        }
        return super.instrument(rootFolder, targetFolder, jobs, builder, instrumenterDirectives);
    }

    /**
     * The order to instrument the source and write the instrumented source code
     * in target.
     *
     * @param source
     *            The source code to instrument.
     * @param target
     *            The instrumented source code.
     * @param sourceFile
     *            The source file
     * @param testSessionContainerUID
     *            The UID of the {@link TestSessionContainer}.
     * @param database
     *            The database
     * @param hierarchyLevelContainer
     *            This is the {@link HierarchyLevelContainer}, where all top
     *            level {@link HierarchyLevel}s of the source file can be added
     *            using
     *            {@link HierarchyLevelContainer#addHierarchyLevels(Collection, LinkedList)}.
     * @throws ParseException
     *             If an exceptions occurs during the parsing: e.g.
     *             {@link CobolParser#CompilationUnit()}.
     * @throws IOException
     *             On the occurrence of write exceptions.
     */
    @Override
    protected void instrumentThis(Reader source,
            Writer target,
            MASTBuilder database,
            SourceFile sourceFile,
            HierarchyLevelContainer hierarchyLevelContainer,
            String testSessionContainerUID,
            Map<String, Object> instrumenterDirectives) throws ParseException,
            IOException {
        NodeCounter.getInstance().reset();

        CompilerDirectivesManipulator compilerDirectivesManipulator = (CompilerDirectivesManipulator) instrumenterDirectives
                .get(InstrumenterDescriptor.COMPILER_DIRECTIVES_KEY);
        if (compilerDirectivesManipulator == null) {
            database.getLogger().fatal(
                    "Directive "
                            + InstrumenterDescriptor.COMPILER_DIRECTIVES_KEY
                            + " is not set.");
        }

        SimpleCharStream simpleCharStream = new SimpleCharStream(source,
                compilerDirectivesManipulator);
        CobolParser cobolParser = new CobolParser(simpleCharStream);
        CompilerDirectives compilerDirectives = simpleCharStream
                .getCompilerDirectives();
        CompilationUnit compilationUnit = cobolParser.CompilationUnit();
        CounterProvider counterProvider = new CounterProvider();
        InstrumentationVisitor instrumentationVisitor = new InstrumentationVisitor(
                target, database, sourceFile, hierarchyLevelContainer,
                counterProvider, testSessionContainerUID, compilerDirectives,
                compilerDirectivesManipulator);

        if (super.isCriterionSet(StatementCoverage.getInstance())) {
            instrumentationVisitor
                    .setStatementManipulator(new DefaultStatementManipulator(
                            counterProvider, compilerDirectivesManipulator));
        }
        if (super.isCriterionSet(BranchCoverage.getInstance())) {
            instrumentationVisitor
                    .setBranchManipulator(new DefaultBranchManipulator(
                            counterProvider, compilerDirectivesManipulator));
        }
        if (super.isCriterionSet(ConditionCoverage.getInstance())) {
            instrumentationVisitor
                    .setConditionManipulator(new DefaultConditionManipulator(
                            counterProvider, compilerDirectivesManipulator));
        }
        if (super.isCriterionSet(LoopCoverage.getInstance())) {
            instrumentationVisitor
                    .setLoopManipulator(new DefaultLoopManipulator(
                            counterProvider, compilerDirectivesManipulator));
        }
        if (!super.getCriteria().isEmpty()) {
            instrumentationVisitor
                    .setStaticPartManipulator(new DefaultStaticPartManipulator(
                            compilerDirectivesManipulator));
        }

        instrumentationVisitor.visit(compilationUnit);
    }

    /**
     * Returns {@link HierarchyLevelType} which should be used to build the
     * {@link HierarchyLevel} tree out of the generated {@link HierarchyLevel}s
     * that are found during the
     * {@link #instrumentThis(Reader, Writer, MASTBuilder, SourceFile)}.
     *
     * @param database
     *            The {@link MASTBuilder} for
     *            {@link MASTBuilder#createHierarchyLevelType(String, String)}.
     *
     * @return {@link HierarchyLevelTypes#getSourceFileType(MASTBuilder)}
     */
    @Override
    protected HierarchyLevelType getPackageHierarchyLevelType(MASTBuilder database) {
        return HierarchyLevelTypes.getSourceFileType(database);
    }

    /**
     * This instrumenter does only allow the instrumentation of a single source
     * file.
     *
     * @return false.
     */
    @Override
    public boolean allowsFileListInstrumentation() {
        return false;
    }
}
