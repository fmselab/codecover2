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

package org.codecover.instrumentation.xampil;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.Collection;
import java.util.Map;

import org.codecover.instrumentation.HierarchyLevelContainer;
import org.codecover.instrumentation.exceptions.InstrumentationException;
import org.codecover.instrumentation.exceptions.InstrumentationRuntimeException;
import org.codecover.instrumentation.exceptions.ParseException;
import org.codecover.instrumentation.xampil.manipulator.DefaultBranchManipulator;
import org.codecover.instrumentation.xampil.manipulator.DefaultConditionManipulator;
import org.codecover.instrumentation.xampil.manipulator.DefaultLoopManipulator;
import org.codecover.instrumentation.xampil.manipulator.DefaultStatementManipulator;
import org.codecover.instrumentation.xampil.manipulator.DummyBranchManipulator;
import org.codecover.instrumentation.xampil.manipulator.DummyConditionManipulator;
import org.codecover.instrumentation.xampil.manipulator.DummyLoopManipulator;
import org.codecover.instrumentation.xampil.manipulator.DummyStatementManipulator;
import org.codecover.instrumentation.xampil.parser.InstrumentableItemCounter;
import org.codecover.instrumentation.xampil.parser.SimpleCharStream;
import org.codecover.instrumentation.xampil.parser.XampilParser;
import org.codecover.instrumentation.xampil.syntaxtree.CompilationUnit;
import org.codecover.instrumentation.xampil.visitor.InstrumentationVisitor;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.HierarchyLevelType;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.ConditionCoverage;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;
import org.codecover.model.utils.file.SourceTargetContainer;

/**
 * This class is the main class of the Xampil instrumenter. It is derived from
 * the {@link org.codecover.instrumentation.Instrumenter} and overrides the
 * instrumentThis method to controls the instrumentation process.<br>
 * <br>
 * <b>Attention</b> don't use {@link #instrument(File, File, Collection, MASTBuilder, Map)}
 * with a Collection with more than one file. This will cause an
 * {@link InstrumentationRuntimeException}.
 *
 * @author Stefan Franke
 *
 * @see #allowsFileListInstrumentation()
 */
class Instrumenter extends org.codecover.instrumentation.Instrumenter {

    /**
     * Constructs the Xampil {@link Instrumenter}.
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

    @Override
    protected void instrumentThis(Reader source,
            Writer target,
            MASTBuilder database,
            SourceFile sourceFile,
            HierarchyLevelContainer hierarchyLevelContainer,
            String testSessionContainerUID,
            Map<String, Object> instrumenterDirectives) throws ParseException,
            IOException {
        SimpleCharStream simpleCharStream = new SimpleCharStream(source);
        XampilParser xampilParser = new XampilParser(simpleCharStream);
        InstrumentableItemCounter counter = new InstrumentableItemCounter();
        CompilationUnit compilationUnit = xampilParser.CompilationUnit(counter);
        PrintWriter targetPrintWriter = new PrintWriter(target);
        InstrumentationVisitor instrumentationVisitor = new InstrumentationVisitor(
                targetPrintWriter,
                counter,
                database,
                sourceFile,
                hierarchyLevelContainer,
                testSessionContainerUID);

        if (super.isCriterionSet(StatementCoverage.getInstance())) {
            instrumentationVisitor.setStatementManipulator(
                new DefaultStatementManipulator());
        } else {
            instrumentationVisitor.setStatementManipulator(
                new DummyStatementManipulator());
        }

        if (super.isCriterionSet(BranchCoverage.getInstance())) {
            instrumentationVisitor.setBranchManipulator(
                new DefaultBranchManipulator());
        } else {
            instrumentationVisitor.setBranchManipulator(
                new DummyBranchManipulator());
        }

        if (super.isCriterionSet(LoopCoverage.getInstance())) {
            instrumentationVisitor.setLoopManipulator(
                new DefaultLoopManipulator());
        } else {
            instrumentationVisitor.setLoopManipulator(
                new DummyLoopManipulator());
        }

        if (super.isCriterionSet(ConditionCoverage.getInstance())) {
            instrumentationVisitor.setConditionManipulator(
                new DefaultConditionManipulator());
        } else {
            instrumentationVisitor.setConditionManipulator(
                new DummyConditionManipulator());
        }

        instrumentationVisitor.visit(compilationUnit);

        targetPrintWriter.flush();
    }

    @Override
    protected HierarchyLevelType getPackageHierarchyLevelType(MASTBuilder database) {
        return HierarchyLevelTypes.getSourceFileType(database);
    }

    @Override
    public boolean allowsFileListInstrumentation() {
        return false;
    }
}