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

package org.codecover.instrumentation.xampil.visitor;

import java.io.PrintWriter;
import java.util.Collection;
import java.util.LinkedList;

import org.codecover.instrumentation.HierarchyLevelContainer;
import org.codecover.instrumentation.xampil.CounterIDProvider;
import org.codecover.instrumentation.xampil.manipulator.BranchManipulator;
import org.codecover.instrumentation.xampil.manipulator.ConditionManipulator;
import org.codecover.instrumentation.xampil.manipulator.LoopManipulator;
import org.codecover.instrumentation.xampil.manipulator.StatementManipulator;
import org.codecover.instrumentation.xampil.syntaxtree.Statement;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.SourceFile;

/**
 * The instrumentation visitor traverses the parsed syntax tree and adds counter to
 * the source code.
 *
 * @author Stefan Franke
 */
public class InstrumentationVisitor extends TreeDumper {

    private MASTBuilder builder;
    private SourceFile sourceFile;
    private HierarchyLevelContainer hierarchyLevelContainer;
    private String testSessionContainerUID;
    private CounterIDProvider counterIDProvider;
    private StatementManipulator statementManipulator;
    private BranchManipulator branchManipulator;
    private LoopManipulator loopManipulator;
    private ConditionManipulator conditionManipulator;

    /**
     * Constructor of a new {@link InstrumentationVisitor}.
     *
     * @param writer
     *            target writer object
     * @param builder
     *            the MASTBuilder which creates the more abstract syntax tree objects
     * @param sourceFile
     *            the source code file
     * @param hierarchyLevelContainer
     *            This is the {@link HierarchyLevelContainer}, where all top
     *            level {@link HierarchyLevel}s of the source file can be added
     *            using {@link HierarchyLevelContainer#addHierarchyLevels(Collection, LinkedList)}.
     * @param testSessionContainerUID
     *            the test session container UID of the instrumented source code
     */
    public InstrumentationVisitor(PrintWriter writer,
                                  MASTBuilder builder,
                                  SourceFile sourceFile,
                                  HierarchyLevelContainer hierarchyLevelContainer,
                                  String testSessionContainerUID) {
        super(writer);
        this.builder = builder;
        this.sourceFile = sourceFile;
        this.hierarchyLevelContainer = hierarchyLevelContainer;
        this.testSessionContainerUID = testSessionContainerUID;
        this.counterIDProvider = new CounterIDProvider();
    }

    /**
     * @param statementManipulator The {@link StatementManipulator} to set.
     */
    public void setStatementManipulator(StatementManipulator statementManipulator) {
        this.statementManipulator = statementManipulator;
        this.statementManipulator.setWriter(super.getTargetWriter());
    }
    
    /**
     * @param branchManipulator The {@link BranchManipulator} to set.
     */
    public void setBranchManipulator(BranchManipulator branchManipulator) {
        this.branchManipulator = branchManipulator;
        this.branchManipulator.setWriter(super.getTargetWriter());
    }
    
    /**
     * @param loopManipulator The {@link LoopManipulator} to set.
     */
    public void setLoopManipulator(LoopManipulator loopManipulator) {
        this.loopManipulator = loopManipulator;
        this.loopManipulator.setWriter(super.getTargetWriter());
    }
    
    /**
     * @param conditionManipulator The {@link ConditionManipulator} to set.
     */
    public void setConditionManipulator(ConditionManipulator conditionManipulator) {
        this.conditionManipulator = conditionManipulator;
        this.conditionManipulator.setWriter(super.getTargetWriter());
    }

    @Override
    public void visit(Statement n) {
       super.visit(n);
       String statementID = this.counterIDProvider.nextStatementID();
       this.statementManipulator.manipulate(n, statementID);
    }
}