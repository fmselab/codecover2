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
    }
}