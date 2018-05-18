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

package org.codecover.instrumentation.cobol85.visitor;

import java.io.Writer;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import org.codecover.instrumentation.HierarchyLevelContainer;
import org.codecover.instrumentation.booleanterms.InstrBasicBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrBooleanTerm;
import org.codecover.instrumentation.cobol85.CobolConditionParser;
import org.codecover.instrumentation.cobol85.CounterProvider;
import org.codecover.instrumentation.cobol85.HierarchyLevelTypes;
import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectives;
import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectivesManipulator;
import org.codecover.instrumentation.cobol85.manipulators.BranchManipulator;
import org.codecover.instrumentation.cobol85.manipulators.ConditionManipulator;
import org.codecover.instrumentation.cobol85.manipulators.DummyBranchManipulator;
import org.codecover.instrumentation.cobol85.manipulators.DummyConditionManipulator;
import org.codecover.instrumentation.cobol85.manipulators.DummyLoopManipulator;
import org.codecover.instrumentation.cobol85.manipulators.DummyStatementManipulator;
import org.codecover.instrumentation.cobol85.manipulators.DummyStaticPartManipulator;
import org.codecover.instrumentation.cobol85.manipulators.LoopManipulator;
import org.codecover.instrumentation.cobol85.manipulators.StatementManipulator;
import org.codecover.instrumentation.cobol85.manipulators.StaticPartManipulator;
import org.codecover.instrumentation.cobol85.syntaxtree.AddStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.CallStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.CompilationUnit;
import org.codecover.instrumentation.cobol85.syntaxtree.ComputeStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.Condition;
import org.codecover.instrumentation.cobol85.syntaxtree.ConfigurationSection;
import org.codecover.instrumentation.cobol85.syntaxtree.ContinueStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.DataDivision;
import org.codecover.instrumentation.cobol85.syntaxtree.DataDivisionSection;
import org.codecover.instrumentation.cobol85.syntaxtree.Declaratives;
import org.codecover.instrumentation.cobol85.syntaxtree.DeleteStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.DivideStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.EndProgramStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.EnvironmentDivision;
import org.codecover.instrumentation.cobol85.syntaxtree.EnvironmentSection;
import org.codecover.instrumentation.cobol85.syntaxtree.EvaluatePhrase;
import org.codecover.instrumentation.cobol85.syntaxtree.EvaluateStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.ExitProgramStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.FileControlEntry;
import org.codecover.instrumentation.cobol85.syntaxtree.FileControlParagraph;
import org.codecover.instrumentation.cobol85.syntaxtree.FileSection;
import org.codecover.instrumentation.cobol85.syntaxtree.GobackStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.GotoStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.IOControlParagraph;
import org.codecover.instrumentation.cobol85.syntaxtree.Identifier;
import org.codecover.instrumentation.cobol85.syntaxtree.IfStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.InputOutputSection;
import org.codecover.instrumentation.cobol85.syntaxtree.InputOutputSectionParagraph;
import org.codecover.instrumentation.cobol85.syntaxtree.LinkageSection;
import org.codecover.instrumentation.cobol85.syntaxtree.Literal;
import org.codecover.instrumentation.cobol85.syntaxtree.MultiplyStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.NestedProgramUnit;
import org.codecover.instrumentation.cobol85.syntaxtree.Node;
import org.codecover.instrumentation.cobol85.syntaxtree.NodeChoice;
import org.codecover.instrumentation.cobol85.syntaxtree.NodeList;
import org.codecover.instrumentation.cobol85.syntaxtree.NodeListOptional;
import org.codecover.instrumentation.cobol85.syntaxtree.NodeOptional;
import org.codecover.instrumentation.cobol85.syntaxtree.NodeSequence;
import org.codecover.instrumentation.cobol85.syntaxtree.NodeToken;
import org.codecover.instrumentation.cobol85.syntaxtree.Paragraph;
import org.codecover.instrumentation.cobol85.syntaxtree.Paragraphs;
import org.codecover.instrumentation.cobol85.syntaxtree.PerformAfterClause;
import org.codecover.instrumentation.cobol85.syntaxtree.PerformFlavour;
import org.codecover.instrumentation.cobol85.syntaxtree.PerformStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.PerformTestPositionClause;
import org.codecover.instrumentation.cobol85.syntaxtree.PerformUntilClause;
import org.codecover.instrumentation.cobol85.syntaxtree.PerformUntilWithTestClause;
import org.codecover.instrumentation.cobol85.syntaxtree.PerformVaryingClause;
import org.codecover.instrumentation.cobol85.syntaxtree.PerformVaryingWithTestClause;
import org.codecover.instrumentation.cobol85.syntaxtree.ProcedureDivision;
import org.codecover.instrumentation.cobol85.syntaxtree.ProcedureSection;
import org.codecover.instrumentation.cobol85.syntaxtree.ProgramName;
import org.codecover.instrumentation.cobol85.syntaxtree.ProgramUnit;
import org.codecover.instrumentation.cobol85.syntaxtree.QualifiedDataName;
import org.codecover.instrumentation.cobol85.syntaxtree.ReadStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.RewriteStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.SearchPhrase;
import org.codecover.instrumentation.cobol85.syntaxtree.SearchStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.SectionHeader;
import org.codecover.instrumentation.cobol85.syntaxtree.Sentence;
import org.codecover.instrumentation.cobol85.syntaxtree.StartStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.Statement;
import org.codecover.instrumentation.cobol85.syntaxtree.StatementList;
import org.codecover.instrumentation.cobol85.syntaxtree.StopStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.StringStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.SubtractStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.UnstringStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.WorkingStorageSection;
import org.codecover.instrumentation.cobol85.syntaxtree.WriteStatement;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.BasicStatement;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.Branch;
import org.codecover.model.mast.ConditionalStatement;
import org.codecover.model.mast.CoverableItem;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.HierarchyLevelType;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.LocationList;
import org.codecover.model.mast.LoopingStatement;
import org.codecover.model.mast.QuestionMarkOperator;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.mast.StatementSequence;
import org.codecover.model.utils.Attic;

/**
 * The InstrumentationVisitor traverses the parsed syntax tree and adds counter to
 * the source code.
 *
 * @author Stefan Franke
 * @version 1.0 ($Id: InstrumentationVisitor.java 69 2010-01-27 19:31:18Z schmidberger $)
 *
 */
public class InstrumentationVisitor extends TreeDumper {

    private static final String EMPTY_STRING = "";

    private static final String DEFAULT_PARAGRAPH = "Default Paragraph";

    private static final String END_TEST_CASE = "*>ENDTESTCASE";

    /**
     * Specifies the start test case comment string
     */
    public static final String START_TEST_CASE = "*>STARTTESTCASE";

    private static final List<org.codecover.model.mast.Statement> EMPTY_STATEMENT_LIST = Collections.emptyList();

    private static final List<StatementSequence> EMPTY_STATEMENT_SEQUENCE_LIST = Collections.emptyList();

    private static final List<HierarchyLevel> EMPTY_HIERARCHY_LEVEL_LIST = Collections.emptyList();

    private BranchManipulator branchManipulator;

    private ConditionManipulator conditionManipulator;

    private Vector<CompilationUnit> editedCompilationUnits;

    private String fileName = "cclog.clf";

    private LoopManipulator loopManipulator;

    private String progName;

    private StatementManipulator statementManipulator;

    private StaticPartManipulator staticPartManipulator;

    private SourceFile sourceFile;

    private MASTBuilder builder;

    private Attic<List<org.codecover.model.mast.Statement>> statementAttic;

    private Attic<List<org.codecover.model.mast.HierarchyLevel>> hierarchyAttic;

    private CounterProvider counterProvider;

    private HierarchyLevelContainer hierarchyLevelContainer;

    private final String testSessionContainerUID;

    /**
     * Constructor
     *
     * @param writer
     *            target writer object
     * @param builder
     *            the MASTBuilder which creates the abstract syntax tree
     * @param sourceFile
     *            the source code file
     * @param hierarchyLevelContainer
     *            This is the {@link HierarchyLevelContainer}, where all top
     *            level {@link HierarchyLevel}s of the source file can be added
     *            using
     *            {@link HierarchyLevelContainer#addHierarchyLevels(Collection, LinkedList)}.
     * @param counterProvider
     *            the counter provider
     * @param testSessionContainerUID
     *            the test session container UID of the instrumented source code
     * @param compilerDirectives the compiler directives saved in the simple char stream
     * @param compilerDirectivesManipulator the compiler directives manipulator
     */
    public InstrumentationVisitor(Writer writer, MASTBuilder builder,
            SourceFile sourceFile,
            HierarchyLevelContainer hierarchyLevelContainer,
            CounterProvider counterProvider, String testSessionContainerUID,
            CompilerDirectives compilerDirectives,
            CompilerDirectivesManipulator compilerDirectivesManipulator) {
        super(writer, compilerDirectives);
        this.statementManipulator = new DummyStatementManipulator();
        this.branchManipulator = new DummyBranchManipulator();
        this.conditionManipulator = new DummyConditionManipulator();
        this.loopManipulator = new DummyLoopManipulator();
        this.staticPartManipulator = new DummyStaticPartManipulator();
        this.editedCompilationUnits = new Vector<CompilationUnit>();

        this.builder = builder;
        this.sourceFile = sourceFile;
        this.hierarchyLevelContainer = hierarchyLevelContainer;
        this.testSessionContainerUID = testSessionContainerUID;

        this.statementAttic = new Attic<List<org.codecover.model.mast.Statement>>();
        this.hierarchyAttic = new Attic<List<HierarchyLevel>>();
        this.counterProvider = counterProvider;
    }

    /**
     * Adds a new hierarchy level list to the hierarchy level stack
     */
    private void addNewHierarchyLevelListToAttic() {
        List<org.codecover.model.mast.HierarchyLevel> hierarchyLevelList = new LinkedList<org.codecover.model.mast.HierarchyLevel>();
        this.hierarchyAttic.push(hierarchyLevelList);
    }

    /**
     * Adds a new statement list to the statement stack
     */
    private void addNewStatementListToAttic() {
        List<org.codecover.model.mast.Statement> statementList = new LinkedList<org.codecover.model.mast.Statement>();
        this.statementAttic.push(statementList);
    }

    /**
     * Returns the created basic statement.
     *
     * @param startOffset
     *            start offset of the branch
     * @param endOffset
     *            end offset of the branch
     * @param id
     *            counter id of the branch
     * @param booleanTerm the boolean term
     * @return the created basic statement
     */
    private BasicStatement createBasicStatement(int startOffset, int endOffset,
            String id, Set<RootTerm> rootTerms, Set<QuestionMarkOperator> questionMarkOperators) {
        Location location = this.builder.createLocation(this.sourceFile,
                startOffset, endOffset);
        List<Location> locations = new LinkedList<Location>();
        locations.add(location);
        LocationList locationList = this.builder.createLocationList(locations);
        CoverableItem coverableItem = this.builder.createCoverableItem(EMPTY_STRING, id);
        if (rootTerms == null) {
            rootTerms = Collections.emptySet();
        }
        return this.builder.createBasicStatement(locationList, coverableItem,
                rootTerms, questionMarkOperators);
    }

    /**
     * Returns the created branch.
     *
     * @param startOffset
     *            start offset of the branch
     * @param endOffset
     *            end offset of the branch
     * @param id
     *            counter id of the branch
     * @param decision
     *            location of the branch keyword
     * @param implicit
     *            is branch not stated in the source code
     * @return the created branch
     */
    private Branch createBranch(int startOffset, int endOffset, String id,
            Location decision, boolean implicit) {
        Location location = this.builder.createLocation(this.sourceFile,
                startOffset, endOffset);
        LocationList locationList;
        CoverableItem coverableItem = this.builder.createCoverableItem(EMPTY_STRING, id);
        StatementSequence statementSequence;
        if (implicit) {
            locationList = this.builder.createEmptyLocationList();
            List<org.codecover.model.mast.Statement> statementList = EMPTY_STATEMENT_LIST;
            statementSequence = this.builder.createStatementSequence(
                    locationList, statementList);
        } else {
            List<Location> locations = new LinkedList<Location>();
            locations.add(location);
            locationList = this.builder.createLocationList(locations);
            statementSequence = this.builder.createStatementSequence(
                    locationList, this.statementAttic.pop());
        }
        List<Location> decisions;
        LocationList decisionList;
        if (decision == null) {
            decisionList = this.builder.createEmptyLocationList();
        } else {
            decisions = new LinkedList<Location>();
            decisions.add(decision);
            decisionList = this.builder.createLocationList(decisions);
        }
        return this.builder.createBranch(locationList, coverableItem,
                implicit, decisionList, statementSequence);
    }

    /**
     * Returns the created conditional statement.
     *
     * @param startOffset start offset of the conditional statement
     * @param endOffset end offset of the conditional statement
     * @param id counter id of the conditional statement
     * @param branches the branches
     * @param keyword location of the conditional statement keyword
     * @param rootTerms the root terms
     * @return the created conditional statement
     */
    private ConditionalStatement createConditionalStatement(int startOffset,
            int endOffset, String id,
            List<Branch> branches, Location keyword, Set<RootTerm> rootTerms) {
        Location location = this.builder.createLocation(this.sourceFile,
                startOffset, endOffset);
        List<Location> locations = new LinkedList<Location>();
        locations.add(location);
        LocationList locationList = this.builder.createLocationList(locations);
        CoverableItem coverableItem = this.builder.createCoverableItem(EMPTY_STRING, id);
        if (rootTerms == null) {
            rootTerms = Collections.emptySet();
        }

        // assume empty ?-Operator Set
        Set<QuestionMarkOperator> questionMarkOperators = new HashSet<QuestionMarkOperator>();
        
        return this.builder.createConditionalStatement(locationList,
                coverableItem, rootTerms, branches, keyword, questionMarkOperators);
    }

    /**
     * Returns the created hierarchy level.
     *
     * @param startOffset start offset of the hierarchy level
     * @param endOffset end offset of the hierarchy level
     * @param name name of the hierarchy level
     * @param hierarchyLevelType the hierarchy level type
     * @param childrenList the children list
     * @param statementList the statement list
     * @return the created hierarchy level
     */
    private HierarchyLevel createHierarchyLevel(int startOffset, int endOffset,
            String name, HierarchyLevelType hierarchyLevelType,
            List<HierarchyLevel> childrenList,
            List<org.codecover.model.mast.Statement> statementList) {
        Location location = this.builder.createLocation(this.sourceFile,
                startOffset, endOffset);
        List<Location> locations = new LinkedList<Location>();
        locations.add(location);
        List<Location> headers = Collections.emptyList();
        LocationList locationList = this.builder.createLocationList(locations);
        LocationList headerList = this.builder.createLocationList(headers);
        List<StatementSequence> statementSequenceList;
        if (statementList == EMPTY_STATEMENT_LIST) {
            statementSequenceList = EMPTY_STATEMENT_SEQUENCE_LIST;
        } else {
            statementSequenceList = new LinkedList<StatementSequence>();
            statementSequenceList.add(this.builder.createStatementSequence(locationList, statementList));
        }
        return this.builder.createHierarchyLevel(locationList, name,
                headerList, hierarchyLevelType, childrenList, statementSequenceList);
    }

    /**
     * Returns the created looping statement.
     *
     * @param startOffset
     *            start offset of the looping statement
     * @param endOffset
     *            end offset of the looping statement
     * @param loopID
     *            counter id of the looping statement
     * @param id
     *            counter id of the statement
     * @param keyword location of the keyword
     * @param performTestBefore
     *            test before execution of loop body
     * @param rootTerms the root terms
     * @return the created looping statement
     */
    private LoopingStatement createLoopingStatement(int startOffset,
            int endOffset, String loopID, String id, Location keyword,
            boolean performTestBefore, Set<RootTerm> rootTerms) {
        Location location = this.builder.createLocation(this.sourceFile,
                startOffset, endOffset);
        List<Location> locations = new LinkedList<Location>();
        locations.add(location);
        LocationList locationList = this.builder.createLocationList(locations);
        CoverableItem coverableItem = this.builder.createCoverableItem(EMPTY_STRING, id);
        CoverableItem neverExecutedItem = this.builder.createCoverableItem(EMPTY_STRING, loopID + "0");
        CoverableItem onceExecutedItem = this.builder.createCoverableItem(EMPTY_STRING, loopID + "1");
        CoverableItem multipleExecutedItem = this.builder.createCoverableItem(EMPTY_STRING, loopID + "2");
        if (rootTerms == null) {
            rootTerms = Collections.emptySet();
        }
        StatementSequence statementSequence = this.builder.createStatementSequence(
                locationList, this.statementAttic.pop());
        return this.builder.createLoopingStatement(locationList, coverableItem,
                rootTerms, statementSequence, keyword, neverExecutedItem,
                onceExecutedItem, multipleExecutedItem, performTestBefore, new HashSet<QuestionMarkOperator>());
    }

    /**
     * Parses the given condition for basic boolean terms.
     *
     * @param condition
     *            the condition
     * @param booleanTermContainer
     *            container for boolean term
     * @return the basic boolean terms
     */
    private List<InstrBasicBooleanTerm> processCondition(Condition condition,
            @SuppressWarnings("unused") BooleanTermContainer booleanTermContainer) {
        CobolConditionParser cobolConditionParser = new CobolConditionParser();
        InstrBooleanTerm instrBooleanTerm = cobolConditionParser
                .parse(condition);
        List<InstrBasicBooleanTerm> basicBooleanTerms = new LinkedList<InstrBasicBooleanTerm>();
        instrBooleanTerm.getAllBasicBooleanTerms(basicBooleanTerms);
        booleanTermContainer.booleanTerm = instrBooleanTerm.toBooleanTerm(this.builder,
                this.sourceFile);
        return basicBooleanTerms;
    }

    /**
     * Search for conditions in a PerformFlavour object and starts
     * processCondition method if necessary.
     *
     * @param performFlavour
     *            the token to be considered
     * @param booleanContainer
     *            this container contains a boolean that will be changed by
     *            the method to handover if the
     *            test is before or after the execution of the perform body
     * @param booleanTermContainer
     *            container for boolean term
     * @return the basic boolean terms
     */
    private List<InstrBasicBooleanTerm> processPerformFlavourForCondition(
            PerformFlavour performFlavour, @SuppressWarnings("unused")
            BooleanContainer booleanContainer, @SuppressWarnings("unused")
            BooleanTermContainer booleanTermContainer) {
        booleanContainer.performTestBefore = false;
        if (performFlavour.f0.which == 1) {
            booleanContainer.performTestBefore = true;
            PerformUntilClause performUntilClause = (PerformUntilClause) performFlavour.f0.choice;
            return this.processCondition(performUntilClause.f1, booleanTermContainer);
        } else if (performFlavour.f0.which == 2) {
            PerformUntilWithTestClause performUntilWithTestClause = (PerformUntilWithTestClause) performFlavour.f0.choice;
            NodeSequence nSequence = (NodeSequence) performUntilWithTestClause.f0.choice;
            if (nSequence.nodes.get(1) instanceof PerformUntilClause) {
                PerformTestPositionClause performTestPositionClause = (PerformTestPositionClause) nSequence.nodes
                        .get(0);
                /*
                 * test before execution of perform body
                 */
                if (performTestPositionClause.f2.which == 0) {
                    booleanContainer.performTestBefore = true;
                }
                PerformUntilClause performUntilClause = (PerformUntilClause) nSequence.nodes
                        .get(1);
                return this.processCondition(performUntilClause.f1,
                        booleanTermContainer);
            }
            PerformTestPositionClause performTestPositionClause = (PerformTestPositionClause) nSequence.nodes
                    .get(1);
            /*
             * test before execution of perform body
             */
            if (performTestPositionClause.f2.which == 0) {
                booleanContainer.performTestBefore = true;
            }
            PerformUntilClause performUntilClause = (PerformUntilClause) nSequence.nodes
                    .get(0);
            return this.processCondition(performUntilClause.f1,
                    booleanTermContainer);
        } else if (performFlavour.f0.which == 3) {
            PerformVaryingWithTestClause performVaryingWithTestClause = (PerformVaryingWithTestClause) performFlavour.f0.choice;
            NodeSequence nSequence = (NodeSequence) performVaryingWithTestClause.f0.choice;
            if (nSequence.nodes.get(1) instanceof PerformVaryingClause) {
                PerformTestPositionClause performTestPositionClause = (PerformTestPositionClause) nSequence.nodes
                        .get(0);
                /*
                 * test before execution of perform body
                 */
                if (performTestPositionClause.f2.which == 0) {
                    booleanContainer.performTestBefore = true;
                }
                PerformVaryingClause performVaryingClause = (PerformVaryingClause) nSequence.nodes
                        .get(1);
                return this.processCondition(performVaryingClause.f6.f1,
                        booleanTermContainer);
            }
            NodeOptional nodeOptional = (NodeOptional) nSequence.nodes.get(1);
            if (nodeOptional.present()) {
                PerformTestPositionClause performTestPositionClause = (PerformTestPositionClause) nodeOptional.node;
                /*
                 * test before execution of perform body
                 */
                if (performTestPositionClause.f2.which == 0) {
                    booleanContainer.performTestBefore = true;
                }
                PerformVaryingClause performVaryingClause = (PerformVaryingClause) nSequence.nodes
                        .get(0);
                return this.processCondition(performVaryingClause.f6.f1,
                        booleanTermContainer);
            }
            /*
             * There is not stated if the test of condition is before or after
             * execution of the perform body. Default is chosen which is test
             * before.
             */
            booleanContainer.performTestBefore = true;
            PerformVaryingClause performVaryingClause = (PerformVaryingClause) nSequence.nodes
                    .get(0);
            return this.processCondition(performVaryingClause.f6.f1, booleanTermContainer);
        }
        return null;
    }

    private void generateWorkingStorageSection(
            WorkingStorageSection workingStorageSection) {
        super.visit(workingStorageSection.f0);
        super.visit(workingStorageSection.f1);
        super.visit(workingStorageSection.f2);
        this.staticPartManipulator.generateDataEntries(this.counterProvider.getProgramUnitCounter(), super.getOut());
        this.loopManipulator.generateAuxiliaryLoopCounterHeader(super.getOut());
        this.loopManipulator.generateAuxiliaryLoopCounter(this.counterProvider
                .getProgramUnitCounter(), super.getOut());
        super.visit(workingStorageSection.f3);
    }

    private void generateFileSection(FileSection fileSection) {
        super.visit(fileSection.f0);
        super.visit(fileSection.f1);
        super.visit(fileSection.f2);
        this.staticPartManipulator.generateFileDescription(this.testSessionContainerUID, super.getOut());
        this.statementManipulator.generateStatementCounter(this.counterProvider
                .getProgramUnitCounter(), super.getOut());
        this.branchManipulator.generateBranchCounter(this.counterProvider
                .getProgramUnitCounter(), super.getOut());
        this.conditionManipulator.generateConditionCounter(this.counterProvider
                .getProgramUnitCounter(), super.getOut());
        this.loopManipulator.generateLoopCounter(this.counterProvider
                .getProgramUnitCounter(), super.getOut());
        this.staticPartManipulator.generateEndTestCaseDeclaration(super.getOut());
        super.visit(fileSection.f3);
    }

    private void generateWorkingStorageSection() {
        this.staticPartManipulator.generateWorkingStorageSection(
                this.counterProvider.getProgramUnitCounter(), super.getOut());
        this.loopManipulator.generateAuxiliaryLoopCounterHeader(super.getOut());
        this.loopManipulator.generateAuxiliaryLoopCounter(this.counterProvider
                .getProgramUnitCounter(), super.getOut());
    }

    private void generateFileSection() {
        this.staticPartManipulator.generateFileSection(this.testSessionContainerUID,
                super.getOut());
        this.statementManipulator.generateStatementCounter(this.counterProvider
                .getProgramUnitCounter(), super.getOut());
        this.branchManipulator.generateBranchCounter(this.counterProvider
                .getProgramUnitCounter(), super.getOut());
        this.conditionManipulator.generateConditionCounter(this.counterProvider
                .getProgramUnitCounter(), super.getOut());
        this.loopManipulator.generateLoopCounter(this.counterProvider
                .getProgramUnitCounter(), super.getOut());
        this.staticPartManipulator.generateEndTestCaseDeclaration(super.getOut());
    }

    private void generateFileAndWSSection() {
        generateFileSection();
        generateWorkingStorageSection();
    }

    /**
     * Transforms the start and end test case comments to real COBOL statements
     *
     * @param nodeToken
     *            token to be considered
     */
    @Override
    protected void printSpecials(NodeToken nodeToken) {
        if (nodeToken.numSpecials() > 0) {
            for (Enumeration<NodeToken> e = nodeToken.specialTokens.elements(); e
                    .hasMoreElements();) {
                NodeToken nToken = e.nextElement();
                if (nToken.tokenImage.length() >= START_TEST_CASE.length() &&
                        nToken.tokenImage.startsWith(START_TEST_CASE)) {
                    this.staticPartManipulator.replaceStartTestCase(nToken,
                            super.getOut());
                } else if (nToken.tokenImage.length() >= END_TEST_CASE.length() &&
                        nToken.tokenImage.startsWith(END_TEST_CASE)) {
                    this.staticPartManipulator.replaceEndTestCase(super.getOut());
                } else {
                    visit(nToken);
                }
            }
        }
    }

    /**
     * Sets the branch manipulator.
     *
     * @param branchManipulator
     *            the branchManipulator to set
     */
    public void setBranchManipulator(BranchManipulator branchManipulator) {
        this.branchManipulator = branchManipulator;
    }

    /**
     * Sets the condition manipulator.
     *
     * @param conditionManipulator
     *            the conditionManipulator to set
     */
    public void setConditionManipulator(
            ConditionManipulator conditionManipulator) {
        this.conditionManipulator = conditionManipulator;
    }

    /**
     * Sets the loop manipulator.
     *
     * @param loopManipulator
     *            the loopManipulator to set
     */
    public void setLoopManipulator(LoopManipulator loopManipulator) {
        this.loopManipulator = loopManipulator;
    }

    /**
     * Sets the statement manipulator.
     *
     * @param statementManipulator
     *            the statementManipulator to set
     */
    public void setStatementManipulator(
            StatementManipulator statementManipulator) {
        this.statementManipulator = statementManipulator;
    }

    /**
     * Sets the static part manipulator.
     *
     * @param staticPartManipulator
     *            the fileSectionManipulator to set
     */
    public void setStaticPartManipulator(
            StaticPartManipulator staticPartManipulator) {
        this.staticPartManipulator = staticPartManipulator;
    }

    /**
     * Includes branch counter and creates model object.
     *
     * <PRE>
     * f0 -> &lt;ADD&gt;
     * f1 -> (
     *         (
     *           ( &lt;CORRESPONDING&gt; | &lt;CORR&gt; )
     *           Identifier()
     *           &lt;TO&gt;
     *           Identifier()
     *           [
     *             &lt;GIVING&gt;
     *             (
     *               Identifier()
     *               [ &lt;ROUNDED&gt; ]
     *             )+
     *           ]
     *         )
     *         |
     *         (
     *           Identifier()
     *           |
     *           Literal()
     *         )+
     *         (
     *           [
     *             &lt;TO&gt;
     *             (
     *               Identifier()
     *               |
     *               Literal()
     *             )
     *           ]
     *           &lt;GIVING&gt;
     *           (
     *             Identifier()
     *             [ &lt;ROUNDED&gt; ]
     *           )+
     *           |
     *           &lt;TO&gt;
     *           (
     *             Identifier()
     *             [ &lt;ROUNDED&gt; ]
     *           )+
     *         )
     *       )
     * f2 -> [ [ &lt;ON&gt; ] &lt;SIZE&gt; &lt;ERROR&gt; StatementList() ]
     * f3 -> [ &lt;NOT&gt; [ &lt;ON&gt; ] &lt;SIZE&gt; &lt;ERROR&gt; StatementList() ]
     * f4 -> [ &lt;END_ADD&gt; ]
     * </PRE>
     *
     * @param addStatement
     */
    @Override
    public void visit(AddStatement addStatement) {
        if (addStatement.f2.present() || addStatement.f3.present()) {
            // writes add-keyword and saves its location in the source file
            super.visit(addStatement.f0);
            int startOffsetAdd = addStatement.f0.startOffset;
            int endOffsetAdd = super.getPosition();
            Location addLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetAdd, endOffsetAdd);

            List<Branch> branches = new LinkedList<Branch>();

            super.visit((NodeSequence) addStatement.f1.choice);
            if (addStatement.f2.present()) {
                NodeSequence nodeSequence = (NodeSequence) addStatement.f2.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(1));
                super.visit((NodeToken) nodeSequence.nodes.get(2));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(3));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (addStatement.f3.present()) {
                NodeSequence nodeSequence = (NodeSequence) addStatement.f3.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(1));
                super.visit((NodeToken) nodeSequence.nodes.get(2));
                super.visit((NodeToken) nodeSequence.nodes.get(3));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(4));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (addStatement.f4.present()) {
                super.visit(addStatement.f4);
            } else {
                this.statementManipulator.generateEndAdd(super.getOut());
            }
            endOffsetAdd = super.getPosition();

            // instrumentation
            this.counterProvider.incrementStatementCounter();
            this.statementManipulator.manipulate(super.getOut());
            String id = this.counterProvider.getStatementCounterID();

            // stores the branches in a conditional statement
            ConditionalStatement conditionalStatement = this
                    .createConditionalStatement(startOffsetAdd, endOffsetAdd, id,
                            branches, addLocation, null);
            this.statementAttic.bottom().add(conditionalStatement);

            return;
        }
        int startOffset = addStatement.f0.startOffset;
        super.visit(addStatement);
        int endOffset = super.getPosition();

        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        // stores statement into the latest statement list
        BasicStatement basicStatement = this.createBasicStatement(startOffset,
                endOffset, id, null, null);
        this.statementAttic.bottom().add(basicStatement);
    }

    /**
     * Includes branch counter and creates model object.
     *
     * <PRE>
     * f0 -> &lt;CALL&gt;
     * f1 -> ( Identifier() | Literal() )
     * f2 -> [ &lt;USING&gt; ( ( [ [ &lt;BY&gt; ] &lt;REFERENCE&gt; ] ( ( Identifier() | &lt;ADDRESS&gt; &lt;OF&gt; Identifier() | FileName() ) )+ | [ &lt;BY&gt; ] &lt;CONTENT&gt; ( ( [ &lt;LENGTH&gt; &lt;OF&gt; ] Identifier() | &lt;ADDRESS&gt; &lt;OF&gt; Identifier() | Literal() ) )+ ) )+ ]
     * f3 -> [ [ &lt;ON&gt; ] &lt;OVERFLOW&gt; StatementList() ]
     * f4 -> [ [ &lt;ON&gt; ] &lt;EXCEPTION&gt; StatementList() ]
     * f5 -> [ &lt;NOT&gt; [ &lt;ON&gt; ] &lt;EXCEPTION&gt; StatementList() ]
     * f6 -> [ &lt;END_CALL&gt; ]
     * </PRE>
     *
     * @param callStatement
     *            token to be considered
     */
    @Override
    public void visit(CallStatement callStatement) {
        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String callID = this.counterProvider.getStatementCounterID();

        if (callStatement.f3.present() || callStatement.f4.present() || callStatement.f5.present()) {
            // writes add-keyword and saves its location in the source file
            super.visit(callStatement.f0);
            int startOffsetCal = callStatement.f0.startOffset;
            int endOffsetCal = super.getPosition();
            Location addLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetCal, endOffsetCal);

            List<Branch> branches = new LinkedList<Branch>();

            if (callStatement.f1.choice instanceof Identifier) {
                super.visit((Identifier) callStatement.f1.choice);
            } else {
                super.visit((Literal) callStatement.f1.choice);
            }
            super.visit(callStatement.f2);
            if (callStatement.f3.present()) {
                NodeSequence nodeSequence = (NodeSequence) callStatement.f3.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(1));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(2));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (callStatement.f4.present()) {
                NodeSequence nodeSequence = (NodeSequence) callStatement.f4.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(1));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(2));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (callStatement.f5.present()) {
                NodeSequence nodeSequence = (NodeSequence) callStatement.f5.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(1));
                super.visit((NodeToken) nodeSequence.nodes.get(2));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(3));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (callStatement.f6.present()) {
                super.visit(callStatement.f6);
            } else {
                this.statementManipulator.generateEndCall(super.getOut());
            }
            endOffsetCal = super.getPosition();

            // stores the branches in a conditional statement
            ConditionalStatement conditionalStatement = this
                    .createConditionalStatement(startOffsetCal, endOffsetCal, callID,
                            branches, addLocation, null);
            this.statementAttic.bottom().add(conditionalStatement);

            return;
        }
        int startOffset = callStatement.f0.startOffset;
        super.visit(callStatement);
        int endOffset = super.getPosition();

        // stores statement into the latest statement list
        BasicStatement basicStatement = this.createBasicStatement(startOffset,
                endOffset, callID, null, null);
        this.statementAttic.bottom().add(basicStatement);
    }

    /**
     * Saves the compilation unit to the hierarchy level container.
     *
     * <PRE>
     * f0 -> ProgramUnit()
     * f1 -> ( NestedProgramUnit() EndProgramStatement() )*
     * f2 -> [ EndProgramStatement() ( CompilationUnit() )* ]
     * f3 -> &lt;EOF&gt;
     * </PRE>
     *
     * @param compilationUnit
     *            token to be considered
     */
    @Override
    public void visit(CompilationUnit compilationUnit) {
        // hierarchy level list for this compilation unit
        this.addNewHierarchyLevelListToAttic();

        int startOffset = compilationUnit.f0.f0.f0.startOffset;
        compilationUnit.f0.accept(this);
        super.visit(compilationUnit.f1);

        this.staticPartManipulator.generateWriteLogic(super.getOut());

        int endOffset = super.getPosition();
        NodeSequence nodeSequence = null;
        if (compilationUnit.f2.present()) {
            nodeSequence = (NodeSequence) compilationUnit.f2.node;

            super.visit((EndProgramStatement) nodeSequence.nodes.get(0));
            endOffset = super.getPosition();
        }

        HierarchyLevel hierarchyLevel = this.createHierarchyLevel(startOffset,
                endOffset, this.progName, HierarchyLevelTypes
                        .getProgramUnitType(this.builder), this.hierarchyAttic
                        .pop(), EMPTY_STATEMENT_LIST);
        this.hierarchyLevelContainer.addHierarchyLevelToRoot(hierarchyLevel);
        this.editedCompilationUnits.add(compilationUnit);

        if (nodeSequence != null) {
            NodeListOptional nodeListOptional = (NodeListOptional) nodeSequence.nodes.get(1);
            if (nodeListOptional.present()) {
                super.visit(nodeListOptional);
            }
        }

        super.visit(compilationUnit.f3);
        super.flushWriter();
    }

    /**
     * Includes branch counter and creates model object.
     *
     * <PRE>
     *
     * f0 -> &lt;COMPUTE&gt; f1 -> ( Identifier() [ &lt;ROUNDED&gt; ] )+ f2 -> (
     * &lt;EQUALCHAR&gt; | &lt;EQUAL&gt; ) f3 -> ArithmeticExpression() f4 -> [ [
     * &lt;ON&gt; ] &lt;SIZE&gt; &lt;ERROR&gt; StatementList() ] f5 -> [
     * &lt;NOT&gt; [ &lt;ON&gt; ] &lt;SIZE&gt; &lt;ERROR&gt; StatementList() ]
     * f6 -> [ &lt;END_COMPUTE&gt; ]
     *
     * </PRE>
     *
     * @param computeStatement
     *            token to be considered
     */
    @Override
    public void visit(ComputeStatement computeStatement) {
        if (computeStatement.f4.present() || computeStatement.f5.present()) {
            // writes add-keyword and saves its location in the source file
            super.visit(computeStatement.f0);
            int startOffsetCom = computeStatement.f0.startOffset;
            int endOffsetCom = super.getPosition();
            Location computeLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetCom, endOffsetCom);

            List<Branch> branches = new LinkedList<Branch>();

            super.visit(computeStatement.f1);
            super.visit((NodeToken) computeStatement.f2.choice);
            super.visit(computeStatement.f3);
            if (computeStatement.f4.present()) {
                NodeSequence nodeSequence = (NodeSequence) computeStatement.f4.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(1));
                super.visit((NodeToken) nodeSequence.nodes.get(2));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(3));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (computeStatement.f5.present()) {
                NodeSequence nodeSequence = (NodeSequence) computeStatement.f5.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(1));
                super.visit((NodeToken) nodeSequence.nodes.get(2));
                super.visit((NodeToken) nodeSequence.nodes.get(3));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(4));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (computeStatement.f6.present()) {
                super.visit(computeStatement.f6);
            } else {
                this.statementManipulator.generateEndCompute(super.getOut());
            }
            endOffsetCom = super.getPosition();

            // instrumentation
            this.counterProvider.incrementStatementCounter();
            this.statementManipulator.manipulate(super.getOut());
            String id = this.counterProvider.getStatementCounterID();

            // stores the branches in a conditional statement
            ConditionalStatement conditionalStatement = this
                    .createConditionalStatement(startOffsetCom, endOffsetCom, id,
                            branches, computeLocation, null);
            this.statementAttic.bottom().add(conditionalStatement);

            return;
        }
        int startOffset = computeStatement.f0.startOffset;
        super.visit(computeStatement);
        int endOffset = super.getPosition();

        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        // stores statement into the latest statement list
        BasicStatement basicStatement = this.createBasicStatement(startOffset,
                endOffset, id, null, null);
        this.statementAttic.bottom().add(basicStatement);
    }

    /**
     *
     *
     * Grammar production:
     * <PRE>
     * f0 -> &lt;DATA&gt;
     * f1 -> &lt;DIVISION&gt;
     * f2 -> &lt;DOT&gt;
     * f3 -> ( DataDivisionSection() )*
     * </PRE>
     */
    @Override
    public void visit(DataDivision dataDivision) {
        super.visit(dataDivision.f0);
        super.visit(dataDivision.f1);
        super.visit(dataDivision.f2);
        if (!dataDivision.f3.present()) {
            this.generateFileAndWSSection();
        } else {
            Iterator<Node> dataDivisionSections = dataDivision.f3.nodes.iterator();
            DataDivisionSection dataDivisionSection = null;
            while (dataDivisionSections.hasNext()
                    && (dataDivisionSection = (DataDivisionSection) dataDivisionSections
                            .next()).f0.choice instanceof LinkageSection) {
                super.visit(dataDivisionSection);
            }
            if (dataDivisionSection.f0.choice instanceof FileSection) {
                FileSection fileSection = (FileSection) dataDivisionSection.f0.choice;
                this.generateFileSection(fileSection);
                while (dataDivisionSections.hasNext()
                        && ((dataDivisionSection = (DataDivisionSection) dataDivisionSections
                                .next()).f0.choice instanceof LinkageSection
                                || dataDivisionSection.f0.choice instanceof FileSection)) {
                    super.visit(dataDivisionSection);
                }
                if (dataDivisionSection.f0.choice instanceof WorkingStorageSection) {
                    WorkingStorageSection workingStorageSection = (WorkingStorageSection) dataDivisionSection.f0.choice;
                    this.generateWorkingStorageSection(workingStorageSection);
                } else {
                    this.generateWorkingStorageSection();
                }
            } else if (dataDivisionSection.f0.choice instanceof WorkingStorageSection) {
                WorkingStorageSection workingStorageSection = (WorkingStorageSection) dataDivisionSection.f0.choice;
                this.generateWorkingStorageSection(workingStorageSection);
                while (dataDivisionSections.hasNext()
                        && ((dataDivisionSection = (DataDivisionSection) dataDivisionSections
                                .next()).f0.choice instanceof LinkageSection
                                || dataDivisionSection.f0.choice instanceof WorkingStorageSection)) {
                    super.visit(dataDivisionSection);
                }
                if (dataDivisionSection.f0.choice instanceof FileSection) {
                    FileSection fileSection = (FileSection) dataDivisionSection.f0.choice;
                    this.generateFileSection(fileSection);
                } else {
                    this.generateFileSection();
                }
            } else {
                this.generateFileAndWSSection();
            }
            while (dataDivisionSections.hasNext()) {
                super.visit((DataDivisionSection) dataDivisionSections.next());
            }
        }
    }

    /**
     *
     *
     * <PRE>
     * f0 -> &lt;DECLARATIVES&gt;
     * f1 -> &lt;DOT&gt;
     * f2 -> ( SectionHeader() &lt;DOT&gt; UseStatement() &lt;DOT&gt; Paragraphs() )+
     * f3 -> &lt;END&gt;
     * f4 -> &lt;DECLARATIVES&gt;
     * f5 -> &lt;DOT&gt;
     * </PRE>
     *
     * @param declaratives
     *            token to be considered
     */
    @Override
    public void visit(Declaratives declaratives) {
        super.visit(declaratives.f0);
        super.visit(declaratives.f1);
        for (Node node : declaratives.f2.nodes) {
            NodeSequence nodeSequence = (NodeSequence) node;
            SectionHeader sectionHeader = (SectionHeader) nodeSequence.nodes
                    .get(0);

            this.addNewHierarchyLevelListToAttic();

            int startOffset = sectionHeader.f0.f0.f0.startOffset;
            String sectionName = sectionHeader.f0.f0.f0.tokenImage;

            super.visit(nodeSequence);

            int endOffset = super.getPosition();
            HierarchyLevel hierarchyLevel = this.createHierarchyLevel(
                    startOffset, endOffset, sectionName, HierarchyLevelTypes
                            .getSectionType(this.builder), this.hierarchyAttic
                            .pop(), EMPTY_STATEMENT_LIST);
            this.hierarchyAttic.bottom().add(hierarchyLevel);
        }
    }

    /**
     * Includes branch counter and creates model object.
     *
     * <PRE>
     * f0 -> &lt;DELETE&gt;
     * f1 -> FileName()
     * f2 -> [ &lt;RECORD&gt; ]
     * f3 -> [ &lt;INVALID&gt; [ &lt;KEY&gt; ] StatementList() ]
     * f4 -> [ &lt;NOT&gt; &lt;INVALID&gt; [ &lt;KEY&gt; ] StatementList() ]
     * f5 -> [ &lt;END_DELETE&gt; ]
     * </PRE>
     *
     * @param deleteStatement
     *            token to be considered
     */
    @Override
    public void visit(DeleteStatement deleteStatement) {
        if (deleteStatement.f3.present() || deleteStatement.f4.present()) {
            // writes add-keyword and saves its location in the source file
            super.visit(deleteStatement.f0);
            int startOffsetDel = deleteStatement.f0.startOffset;
            int endOffsetDel = super.getPosition();
            Location deleteLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetDel, endOffsetDel);

            List<Branch> branches = new LinkedList<Branch>();

            super.visit(deleteStatement.f1);
            super.visit(deleteStatement.f2);
            if (deleteStatement.f3.present()) {
                NodeSequence nodeSequence = (NodeSequence) deleteStatement.f3.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(1));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(2));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (deleteStatement.f4.present()) {
                NodeSequence nodeSequence = (NodeSequence) deleteStatement.f4.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(1));
                super.visit((NodeOptional) nodeSequence.nodes.get(2));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(3));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (deleteStatement.f5.present()) {
                super.visit(deleteStatement.f5);
            } else {
                this.statementManipulator.generateEndDelete(super.getOut());
            }
            endOffsetDel = super.getPosition();

            // instrumentation
            this.counterProvider.incrementStatementCounter();
            this.statementManipulator.manipulate(super.getOut());
            String id = this.counterProvider.getStatementCounterID();

            // stores the branches in a conditional statement
            ConditionalStatement conditionalStatement = this
                    .createConditionalStatement(startOffsetDel, endOffsetDel, id,
                            branches, deleteLocation, null);
            this.statementAttic.bottom().add(conditionalStatement);

            return;
        }
        int startOffset = deleteStatement.f0.startOffset;
        super.visit(deleteStatement);
        int endOffset = super.getPosition();

        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        // stores statement into the latest statement list
        BasicStatement basicStatement = this.createBasicStatement(startOffset,
                endOffset, id, null, null);
        this.statementAttic.bottom().add(basicStatement);
    }

    /**
     * Includes branch counter and creates model object.
     *
     * <PRE>
     * f0 -> &lt;DIVIDE&gt;
     * f1 -> ( QualifiedDataName() | Literal() )
     * f2 -> (
     *         &lt;INTO&gt;
     *         Literal()
     *         [
     *           &lt;GIVING&gt;
     *           (
     *             Identifier()
     *             [ &lt;ROUNDED&gt; ]
     *           )+
     *         ]
     *         |
     *         &lt;INTO&gt;
     *         (
     *           Identifier()
     *           [ &lt;ROUNDED&gt; ]
     *         )+
     *         |
     *         &lt;BY&gt;
     *         (
     *           Identifier()
     *           |
     *           Literal()
     *         )
     *         [
     *           &lt;GIVING&gt;
     *           (
     *             Identifier()
     *             [ &lt;ROUNDED&gt; ]
     *           )+
     *         ]
     *       )
     * f3 -> [ &lt;REMAINDER&gt; Identifier() ]
     * f4 -> [ [ &lt;ON&gt; ] &lt;SIZE&gt; &lt;ERROR&gt; StatementList() ]
     * f5 -> [ &lt;NOT&gt; [ &lt;ON&gt; ] &lt;SIZE&gt; &lt;ERROR&gt; StatementList() ]
     * f6 -> [ &lt;END_DIVIDE&gt; ]
     * </PRE>
     *
     * @param divideStatement
     *            token to be considered
     */
    @Override
    public void visit(DivideStatement divideStatement) {
        if (divideStatement.f4.present() || divideStatement.f5.present()) {
            // writes add-keyword and saves its location in the source file
            super.visit(divideStatement.f0);
            int startOffsetDiv = divideStatement.f0.startOffset;
            int endOffsetDiv = super.getPosition();
            Location divideLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetDiv, endOffsetDiv);

            List<Branch> branches = new LinkedList<Branch>();

            if (divideStatement.f1.choice instanceof QualifiedDataName) {
                super.visit((QualifiedDataName) divideStatement.f1.choice);
            } else {
                super.visit((Literal) divideStatement.f1.choice);
            }
            super.visit((NodeSequence) divideStatement.f2.choice);
            super.visit(divideStatement.f3);
            if (divideStatement.f4.present()) {
                NodeSequence nodeSequence = (NodeSequence) divideStatement.f4.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(1));
                super.visit((NodeToken) nodeSequence.nodes.get(2));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(3));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (divideStatement.f5.present()) {
                NodeSequence nodeSequence = (NodeSequence) divideStatement.f5.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(1));
                super.visit((NodeToken) nodeSequence.nodes.get(2));
                super.visit((NodeToken) nodeSequence.nodes.get(3));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(4));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (divideStatement.f6.present()) {
                super.visit(divideStatement.f6);
            } else {
                this.statementManipulator.generateEndDivide(super.getOut());
            }
            endOffsetDiv = super.getPosition();

            // instrumentation
            this.counterProvider.incrementStatementCounter();
            this.statementManipulator.manipulate(super.getOut());
            String id = this.counterProvider.getStatementCounterID();

            // stores the branches in a conditional statement
            ConditionalStatement conditionalStatement = this
                    .createConditionalStatement(startOffsetDiv, endOffsetDiv, id,
                            branches, divideLocation, null);
            this.statementAttic.bottom().add(conditionalStatement);

            return;
        }
        int startOffset = divideStatement.f0.startOffset;
        super.visit(divideStatement);
        int endOffset = super.getPosition();

        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        // stores statement into the latest statement list
        BasicStatement basicStatement = this.createBasicStatement(startOffset,
                endOffset, id, null, null);
        this.statementAttic.bottom().add(basicStatement);
    }

    /**
     * Includes the select file phrase in the first file control paragraph.
     *
     * Grammar production:
     * <PRE>
     * f0 -> &lt;ENVIRONMENT&gt;
     * f1 -> &lt;DIVISION&gt;
     * f2 -> &lt;DOT&gt;
     * f3 -> ( EnvironmentSection() )*
     * </PRE>
     */
    @Override
    public void visit(EnvironmentDivision environmentDivision) {
        super.visit(environmentDivision.f0);
        super.visit(environmentDivision.f1);
        super.visit(environmentDivision.f2);
        if (!environmentDivision.f3.present()) {
            this.staticPartManipulator.generateInputOutputSection(this.fileName, super.getOut());
        } else {
            Iterator<Node> environmentSections = environmentDivision.f3.nodes.iterator();
            EnvironmentSection environmentSection = null;
            while (environmentSections.hasNext()
                    && (environmentSection = (EnvironmentSection) environmentSections
                            .next()).f0.choice instanceof ConfigurationSection) {
                super.visit(environmentSection);
            }
            if (environmentSection.f0.choice instanceof InputOutputSection) {
                InputOutputSection inputOutputSection = (InputOutputSection) environmentSection.f0.choice;
                super.visit(inputOutputSection.f0);
                super.visit(inputOutputSection.f1);
                super.visit(inputOutputSection.f2);
                if (!inputOutputSection.f3.present()) {
                    this.staticPartManipulator.generateFileControl(this.fileName,
                            super.getOut());
                } else {
                    Iterator<Node> inputOutputSectionParagraphs = inputOutputSection.f3.nodes.iterator();
                    InputOutputSectionParagraph inputOutputSectionParagraph = null;
                    while (inputOutputSectionParagraphs.hasNext()
                            && (inputOutputSectionParagraph = (InputOutputSectionParagraph) inputOutputSectionParagraphs
                                    .next()).f0.choice instanceof IOControlParagraph) {
                        super.visit(inputOutputSectionParagraph);
                    }
                    if (inputOutputSectionParagraph.f0.choice instanceof FileControlParagraph) {
                        FileControlParagraph fileControlParagraph = (FileControlParagraph) inputOutputSectionParagraph.f0.choice;
                        super.visit(fileControlParagraph.f0);
                        if (!fileControlParagraph.f1.present()) {
                            super.visit(fileControlParagraph.f2);
                            this.staticPartManipulator.generateSelectFile(
                                    this.fileName, super.getOut());
                        } else {
                            Iterator<Node> fileControlEntries = fileControlParagraph.f1.nodes
                                    .iterator();
                            NodeSequence nodeSequence = (NodeSequence) fileControlEntries
                                    .next();
                            super.visit((NodeOptional) nodeSequence.nodes.get(0));
                            this.staticPartManipulator.generateSelectFile(
                                    this.fileName, super.getOut());
                            super.visit((FileControlEntry) nodeSequence.nodes.get(1));
                            while (fileControlEntries.hasNext()) {
                                super.visit((NodeSequence) fileControlEntries.next());
                            }
                            super.visit(fileControlParagraph.f2);
                        }
                    }
                    while (inputOutputSectionParagraphs.hasNext()) {
                        super.visit((InputOutputSectionParagraph) inputOutputSectionParagraphs.next());
                    }
                }
            }
            while (environmentSections.hasNext()) {
                super.visit((EnvironmentSection) environmentSections.next());
            }
        }
    }

    /**
     * Includes branch counter to evaluate-statements.
     *
     * <PRE>
     * f0 -> &lt;EVALUATE&gt;
     * f1 -> EvaluateValue()
     * f2 -> ( &lt;ALSO&gt; EvaluateValue() )*
     * f3 -> ( ( &lt;WHEN&gt; EvaluatePhrase() ( &lt;ALSO&gt; EvaluatePhrase() )* )+ StatementList() )+
     * f4 -> [ &lt;WHEN&gt; &lt;OTHER&gt; StatementList() ]
     * f5 -> [ &lt;END_EVALUATE&gt; ]
     * </PRE>
     *
     * @param evaluateStatement
     *            token to be considered
     */
    @Override
    public void visit(EvaluateStatement evaluateStatement) {

        // writes evaluate-keyword and saves its location in the source file
        int startOffsetEv = super.getPosition();
        super.visit(evaluateStatement.f0);
        int endOffsetEv = super.getPosition();
        Location evaluateLocation = this.builder.createLocation(this.sourceFile,
                startOffsetEv, endOffsetEv);

        // list for when-branches
        List<Branch> branches = new LinkedList<Branch>();

        super.visit(evaluateStatement.f1);
        super.visit(evaluateStatement.f2);
        for (Node node : evaluateStatement.f3.nodes) {
            NodeSequence nodeSequence = (NodeSequence) node;
            /*
             * nodeSequence.nodes.get(0) is a NodeList object containing the
             * when-phrase
             */
            NodeList nodeList = (NodeList) nodeSequence.nodes.get(0);
            NodeSequence nSequence = (NodeSequence) nodeList.nodes.get(0);
            int startOffsetWh = StartOffset.getStartOffset(nSequence.nodes.get(0));
            super.visit((NodeToken) nSequence.nodes.get(0));
            int endOffsetWh = super.getPosition();
            super.visit((EvaluatePhrase) nSequence.nodes.get(1));
            super.visit((NodeListOptional) nSequence.nodes.get(2));

            // location of when-keyword
            Location whenLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetWh, endOffsetWh);

            // instrumentation
            this.counterProvider.incrementBranchCounter();
            this.branchManipulator.manipulate(super.getOut());
            String whenID = this.counterProvider.getBranchCounterID();

            this.addNewStatementListToAttic();

            /*
             * nodeSequence.nodes.get(1) is a NodeList object containing the
             * statement list of that when-phrase
             */
            int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(1));
            super.visit((StatementList) nodeSequence.nodes.get(1));
            int endOffset = super.getPosition();

            branches.add(this.createBranch(startOffset, endOffset, whenID, whenLocation, false));
        }
        NodeSequence nodeSequence = (NodeSequence) evaluateStatement.f4.node;
        if (nodeSequence != null) {
            /*
             * Source code contains when-other-branch.
             */
            int startOffsetWh = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
            super.visit((NodeToken) nodeSequence.nodes.get(0));
            super.visit((NodeToken) nodeSequence.nodes.get(1));
            int endOffsetWh = super.getPosition();

            // location of when-other-keyword
            Location whenOtherLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetWh, endOffsetWh);

            this.counterProvider.incrementBranchCounter();
            this.branchManipulator.manipulate(super.getOut());
            String whenID = this.counterProvider.getBranchCounterID();

            this.addNewStatementListToAttic();

            int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(2));
            super.visit((StatementList) nodeSequence.nodes.get(2));
            int endOffset = super.getPosition();

            branches.add(this.createBranch(startOffset, endOffset, whenID, whenOtherLocation, false));
        } else {
            int startOffset = super.getPosition();
            this.counterProvider.incrementBranchCounter();
            this.branchManipulator.generateWhenOtherBranch(super.getOut());
            String whenID = this.counterProvider.getBranchCounterID();
            int endOffset = super.getPosition();
            branches.add(this.createBranch(startOffset, endOffset, whenID, null, true));
        }
        if (evaluateStatement.f5.node != null) {
            /*
             * Source code contains end-evaluate keyword.
             */
            super.visit(evaluateStatement.f5);
        } else {
            /*
             * Source code does not contain end-evaluate keyword.
             */
            this.branchManipulator.generateEndEvaluate(super.getOut());
            if (this.branchManipulator instanceof DummyBranchManipulator) {
                this.statementManipulator.generateEndEvaluate(super.getOut());
            }
        }
        endOffsetEv = super.getPosition();

        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        // stores the branches in a conditional statement
        ConditionalStatement conditionalStatement = this
                .createConditionalStatement(startOffsetEv, endOffsetEv, id,
                        branches, evaluateLocation, null);
        this.statementAttic.bottom().add(conditionalStatement);
    }

    /**
     * Includes branch and condition counter to if-statements.
     *
     * <PRE>
     * f0 -> &lt;IF&gt;
     * f1 -> Condition()
     * f2 -> [ &lt;THEN&gt; ]
     * f3 -> ( ( Statement() )+ | &lt;NEXT&gt; &lt;SENTENCE&gt; )
     * f4 -> [ &lt;ELSE&gt; ( ( Statement() )+ | &lt;NEXT&gt; &lt;SENTENCE&gt; ) ]
     * f5 -> [ &lt;END_IF&gt; ]
     * </PRE>
     *
     * @param ifStatement
     *            token to be considered
     */
    @Override
    public void visit(IfStatement ifStatement) {
        BooleanTermContainer booleanTermContainer = new BooleanTermContainer();
        this.counterProvider.incrementConditionCounter();
        List<InstrBasicBooleanTerm> basicBooleanTerms = this.processCondition(
                ifStatement.f1, booleanTermContainer);
        this.conditionManipulator.manipulate(super.getOut(), basicBooleanTerms);
        String ifID = this.counterProvider.getConditionCounterID();

        // writes if-keyword and saves its location in the source file
        int startOffsetIf = ifStatement.f0.startOffset;
        super.visit(ifStatement.f0);
        int endOffsetIf = super.getPosition();
        Location ifLocation = this.builder.createLocation(this.sourceFile,
                startOffsetIf, endOffsetIf);

        // list for then- and else-branch
        List<Branch> branches = new LinkedList<Branch>();

        super.visit(ifStatement.f1);
        super.visit(ifStatement.f2);

        // instrumentation
        this.counterProvider.incrementBranchCounter();
        this.branchManipulator.manipulate(super.getOut());
        String thenID = this.counterProvider.getBranchCounterID();

        this.addNewStatementListToAttic();

        // processes the then-branch
        int startOffset = StartOffset.getStartOffset(ifStatement.f3.choice);
        int endOffset = startOffset;
        if (ifStatement.f3.choice instanceof NodeList) {
            super.visit((NodeList) ifStatement.f3.choice);
            endOffset = super.getPosition();
        } else if (ifStatement.f3.choice instanceof NodeSequence) {
            NodeSequence nodeSequence = (NodeSequence) ifStatement.f3.choice;
            this.branchManipulator.generateNextSentenceGoTo(this, nodeSequence, super.getOut());
            endOffset = ((NodeToken) nodeSequence.nodes.get(1)).endOffset;
        }
        branches.add(this.createBranch(startOffset, endOffset, thenID, null, false));

        // processes the else-branch
        NodeSequence nodeSequence = (NodeSequence) ifStatement.f4.node;
        String elseID;

        if (nodeSequence != null) {
            /*
             * Source code contains else-branch.
             *
             * nodeSequence.nodes.get(0) is the NodeToken object containing the
             * else keyword
             */
            int startOffsetEl = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
            super.visit((NodeToken) nodeSequence.nodes.get(0));
            int endOffsetEl = super.getPosition();
            Location elseLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetEl, endOffsetEl);

            this.counterProvider.incrementBranchCounter();
            this.branchManipulator.manipulate(super.getOut());
            elseID = this.counterProvider.getBranchCounterID();

            this.addNewStatementListToAttic();

            /*
             * nodeSequence.nodes.get(1) is a NodeChoice object containing the
             * else-branch-statements
             */
            startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(1));
            NodeChoice nodeChoice = (NodeChoice) nodeSequence.nodes.get(1);
            if (nodeChoice.choice instanceof NodeList) {
                super.visit((NodeList) nodeChoice.choice);
                endOffset = super.getPosition();
            } else if (nodeChoice.choice instanceof NodeSequence) {
                NodeSequence nSequence = (NodeSequence) nodeChoice.choice;
                this.branchManipulator.generateNextSentenceGoTo(this, nSequence, super.getOut());
                endOffset = ((NodeToken) nSequence.nodes.get(1)).endOffset;
            }
            branches.add(this.createBranch(startOffset, endOffset, elseID, elseLocation, false));
        } else {
            startOffset = super.getPosition();
            this.counterProvider.incrementBranchCounter();
            this.branchManipulator.generateElseBranch(super.getOut());
            elseID = this.counterProvider.getBranchCounterID();
            endOffset = super.getPosition();
            branches.add(this.createBranch(startOffset, endOffset, elseID, null, true));
        }

        // writes the end-if-keyword
        if (ifStatement.f5.node != null) {
            /*
             * Source code contains end if keyword.
             */
            super.visit(ifStatement.f5);
        } else {
            /*
             * Source code does not contain end if keyword.
             */
            this.branchManipulator.generateEndIf(super.getOut());
            if (this.branchManipulator instanceof DummyBranchManipulator) {
                this.statementManipulator.generateEndIf(super.getOut());
            }
        }
        endOffsetIf = super.getPosition();

        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        Set<RootTerm> rootTerms = new HashSet<RootTerm>();
        CoverableItem cItem = this.builder.createCoverableItem(EMPTY_STRING, ifID);
        RootTerm rootTerm = this.builder.createRootTerm(booleanTermContainer.booleanTerm,
                cItem);
        rootTerms.add(rootTerm);

        // stores the branches in a conditional statement
        ConditionalStatement conditionalStatement = this
                .createConditionalStatement(startOffsetIf, endOffsetIf, id,
                        branches, ifLocation, rootTerms);
        this.statementAttic.bottom().add(conditionalStatement);
    }

    /**
     * Includes branch counter and creates model object.
     *
     * <PRE>
     * f0 -> &lt;MULTIPLY&gt;
     * f1 -> ( Identifier() | Literal() )
     * f2 -> &lt;BY&gt;
     * f3 -> ( Identifier() | Literal() )
     * f4 -> [ &lt;GIVING&gt; ( Identifier() [ &lt;ROUNDED&gt; ] )+ ]
     * f5 -> [ [ &lt;ON&gt; ] &lt;SIZE&gt; &lt;ERROR&gt; StatementList() ]
     * f6 -> [ &lt;NOT&gt; [ &lt;ON&gt; ] &lt;SIZE&gt; &lt;ERROR&gt; StatementList() ]
     * f7 -> [ &lt;END_MULTIPLY&gt; ]
     * </PRE>
     *
     * @param multiplyStatement
     *            token to be considered
     */
    @Override
    public void visit(MultiplyStatement multiplyStatement) {
        if (multiplyStatement.f5.present() || multiplyStatement.f6.present()) {
            // writes add-keyword and saves its location in the source file
            super.visit(multiplyStatement.f0);
            int startOffsetMul = multiplyStatement.f0.startOffset;
            int endOffsetMul = super.getPosition();
            Location multiplyLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetMul, endOffsetMul);

            List<Branch> branches = new LinkedList<Branch>();

            if (multiplyStatement.f1.choice instanceof Identifier) {
                super.visit((Identifier) multiplyStatement.f1.choice);
            } else {
                super.visit((Literal) multiplyStatement.f1.choice);
            }
            super.visit(multiplyStatement.f2);
            if (multiplyStatement.f3.choice instanceof Identifier) {
                super.visit((Identifier) multiplyStatement.f3.choice);
            } else {
                super.visit((Literal) multiplyStatement.f3.choice);
            }
            super.visit(multiplyStatement.f4);
            if (multiplyStatement.f5.present()) {
                NodeSequence nodeSequence = (NodeSequence) multiplyStatement.f5.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(1));
                super.visit((NodeToken) nodeSequence.nodes.get(2));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(3));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (multiplyStatement.f6.present()) {
                NodeSequence nodeSequence = (NodeSequence) multiplyStatement.f6.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(1));
                super.visit((NodeToken) nodeSequence.nodes.get(2));
                super.visit((NodeToken) nodeSequence.nodes.get(3));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(4));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (multiplyStatement.f7.present()) {
                super.visit(multiplyStatement.f7);
            } else {
                this.statementManipulator.generateEndMultiply(super.getOut());
            }
            endOffsetMul = super.getPosition();

            // instrumentation
            this.counterProvider.incrementStatementCounter();
            this.statementManipulator.manipulate(super.getOut());
            String id = this.counterProvider.getStatementCounterID();

            // stores the branches in a conditional statement
            ConditionalStatement conditionalStatement = this
                    .createConditionalStatement(startOffsetMul, endOffsetMul, id,
                            branches, multiplyLocation, null);
            this.statementAttic.bottom().add(conditionalStatement);

            return;
        }
        int startOffset = multiplyStatement.f0.startOffset;
        super.visit(multiplyStatement);
        int endOffset = super.getPosition();

        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        // stores statement into the latest statement list
        BasicStatement basicStatement = this.createBasicStatement(startOffset,
                endOffset, id, null,  null);
        this.statementAttic.bottom().add(basicStatement);
    }

    /**
     * Saves nested program into the hierarchy level attic.
     *
     * <PRE>
     * f0 -> NestedIdentificationDivision()
     * f1 -> [ EnvironmentDivision() ]
     * f2 -> [ DataDivision() ]
     * f3 -> [ ProcedureDivision() ]
     * </PRE>
     *
     * @param nestedProgramUnit
     *            token to be considered
     */
    @Override
    public void visit(NestedProgramUnit nestedProgramUnit) {
        this.addNewHierarchyLevelListToAttic();

        int startOffset = ((NodeToken) nestedProgramUnit.f0.f0.choice).startOffset;
        String nestedProgramName = nestedProgramUnit.f0.f3.f2.f0.f0.tokenImage;
        super.visit(nestedProgramUnit);
        if (nestedProgramUnit.f3.present()) {
            this.staticPartManipulator.generateWriteLogicForNestedPrograms(super.getOut());
        }
        int endOffset = super.getPosition();

        HierarchyLevel hierarchyLevel = this.createHierarchyLevel(startOffset, endOffset,
                nestedProgramName, HierarchyLevelTypes
                        .getNestedProgramType(this.builder),
                EMPTY_HIERARCHY_LEVEL_LIST, this.statementAttic.pop());
        this.hierarchyAttic.bottom().add(hierarchyLevel);
    }

    /**
     * Saves paragraph into the hierarchy level attic.
     *
     * <PRE>
     * f0 -> ParagraphName()
     * f1 -> &lt;DOT&gt;
     * f2 -> ( ExitStatement() | AlteredGoto() | ( Sentence() )* )
     * </PRE>
     *
     * @param paragraph
     *            token to be considered
     */
    @Override
    public void visit(Paragraph paragraph) {
        this.addNewStatementListToAttic();

        int startOffset = paragraph.f0.f0.f0.startOffset;
        String paragraphName = paragraph.f0.f0.f0.tokenImage;
        super.visit(paragraph);
        int endOffset = super.getPosition();

        HierarchyLevel hierarchyLevel = this.createHierarchyLevel(startOffset, endOffset,
                paragraphName, HierarchyLevelTypes
                        .getParagraphType(this.builder),
                EMPTY_HIERARCHY_LEVEL_LIST, this.statementAttic.pop());
        this.hierarchyAttic.bottom().add(hierarchyLevel);
    }

    /**
     * Saves sentences without a paragraph to default paragraph.
     *
     * <PRE>
     * f0 -> (Sentence() )+ | ( Paragraph() )+
     * </PRE>
     *
     * @param paragraphs
     *            token to be considered
     */
    @Override
    public void visit(Paragraphs paragraphs) {
        if (paragraphs.f0.which == 0) {
            this.addNewStatementListToAttic();

            int startOffset = StartOffset.getStartOffset(paragraphs.f0);
            super.visit((NodeList) paragraphs.f0.choice);
            int endOffset = super.getPosition();

            HierarchyLevel hierarchyLevel = this.createHierarchyLevel(startOffset, endOffset,
                    DEFAULT_PARAGRAPH, HierarchyLevelTypes
                            .getParagraphType(this.builder),
                    EMPTY_HIERARCHY_LEVEL_LIST, this.statementAttic.pop());
            this.hierarchyAttic.bottom().add(hierarchyLevel);
        } else {
            super.visit((NodeList) paragraphs.f0.choice);
        }
    }

    /**
     * Includes loop and condition counter to perform-statements.
     *
     * <PRE>
     * f0 -> &lt;PERFORM&gt;
     * f1 -> ( PerformProcedureScopeClause() [ PerformFlavour() ] ( PerformAfterClause() )*
     *       | PerformFlavour() ( StatementList() &lt;END_PERFORM&gt; | PerformProcedureScopeClause() ) )
     * </PRE>
     *
     * @param performStatement
     *            token to be considered
     */
    @Override
    public void visit(PerformStatement performStatement) {
        BooleanContainer booleanContainer = new BooleanContainer();
        Set<RootTerm> rootTerms = new HashSet<RootTerm>();
        if (performStatement.f1.which == 0) {
            /*
             * grammar production:
             *  ( PerformProcedureScopeClause() [ PerformFlavour() ] (
             * PerformAfterClause() )*
             */
            NodeSequence nodeSequence = (NodeSequence) performStatement.f1.choice;
            NodeOptional nodeOptional = (NodeOptional) nodeSequence.nodes
                    .get(1);
            if (nodeOptional.present()) {
                /*
                 * Source code contains PerformFlavour()
                 */
                PerformFlavour performFlavour = (PerformFlavour) nodeOptional.node;
                BooleanTermContainer booleanTermContainer = new BooleanTermContainer();
                this.counterProvider.incrementConditionCounter();
                List<InstrBasicBooleanTerm> conditionTerms = processPerformFlavourForCondition(
                        performFlavour, booleanContainer, booleanTermContainer);
                if (booleanContainer.performTestBefore) {
                    this.conditionManipulator.manipulate(super.getOut(),
                            conditionTerms);
                    String perID = this.counterProvider.getConditionCounterID();
                    CoverableItem cItem = this.builder
                            .createCoverableItem(EMPTY_STRING, perID);
                    RootTerm rootTerm = this.builder.createRootTerm(
                            booleanTermContainer.booleanTerm, cItem);
                    rootTerms.add(rootTerm);
                }
            }
            NodeListOptional nodeListOptional = (NodeListOptional) nodeSequence.nodes
                    .get(2);
            for (Node node : nodeListOptional.nodes) {
                PerformAfterClause performAfterClause = (PerformAfterClause) node;
                BooleanTermContainer booleanTermContainer = new BooleanTermContainer();
                this.counterProvider.incrementConditionCounter();
                List<InstrBasicBooleanTerm> conditionTerms = this
                        .processCondition(performAfterClause.f6.f1, booleanTermContainer);
                this.conditionManipulator.manipulate(super.getOut(), conditionTerms);
                String perID = this.counterProvider.getConditionCounterID();
                CoverableItem cItem = this.builder.createCoverableItem(EMPTY_STRING, perID);
                RootTerm rootTerm = this.builder.createRootTerm(booleanTermContainer.booleanTerm,
                        cItem);
                rootTerms.add(rootTerm);
            }
        } else {
            /*
             * grammar production:
             *
             * PerformFlavour() ( StatementList() &lt;END_PERFORM&gt; |
             * PerformProcedureScopeClause() )
             */
            NodeSequence nodeSequence = (NodeSequence) performStatement.f1.choice;
            PerformFlavour performFlavour = (PerformFlavour) nodeSequence.nodes
                    .get(0);
            BooleanTermContainer booleanTermContainer = new BooleanTermContainer();
            List<InstrBasicBooleanTerm> conditionTerms = processPerformFlavourForCondition(
                    performFlavour, booleanContainer, booleanTermContainer);
            int conditionNumber = this.counterProvider.getConditionCounter() + 1;
            if (conditionTerms != null) {
                this.counterProvider.incrementConditionCounter();
                if (booleanContainer.performTestBefore) {
                    this.conditionManipulator.manipulate(super.getOut(), conditionTerms);
                }
                String perID = this.counterProvider.getConditionCounterID();
                CoverableItem cItem = this.builder.createCoverableItem(EMPTY_STRING, perID);
                RootTerm rootTerm = this.builder.createRootTerm(booleanTermContainer.booleanTerm,
                        cItem);
                rootTerms.add(rootTerm);
            }

            NodeChoice nodeChoice = (NodeChoice) nodeSequence.nodes.get(1);
            if (nodeChoice.choice instanceof NodeSequence) {
                /*
                 * Source code contains end-perform statement.
                 */
                NodeSequence nSequence = (NodeSequence) nodeChoice.choice;

                // instrumentation
                this.counterProvider.incrementLoopCounter();
                int loopCounter = this.counterProvider.getLoopCounter();
                this.loopManipulator.manipulate(super.getOut(), loopCounter);

                // writes perform-keyword and saves its location in the source
                // file
                int startOffset = performStatement.f0.startOffset;
                super.visit(performStatement.f0);
                int endOffset = super.getPosition();
                Location performLocation = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                super.visit((PerformFlavour) nodeSequence.nodes.get(0));
                this.loopManipulator.increment(super.getOut(), loopCounter);

                this.addNewStatementListToAttic();

                super.visit((StatementList) nSequence.nodes.get(0));

                // inserts the program logic needed for condition coverage
                // measurement
                if (conditionTerms != null) {
                    this.conditionManipulator.manipulate(super.getOut(),
                            conditionTerms, conditionNumber);
                }

                // writes the end-perform-keyword
                super.visit((NodeToken) nSequence.nodes.get(1));
                endOffset = super.getPosition();

                // instrumentation
                this.loopManipulator.evaluate(super.getOut(), loopCounter);
                String loopID = this.counterProvider.getLoopCounterID();
                this.counterProvider.incrementStatementCounter();
                this.statementManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getStatementCounterID();

                // stores statement into the latest statement list
                LoopingStatement loopingStatement = this
                        .createLoopingStatement(startOffset, endOffset, loopID,
                                id, performLocation,
                                booleanContainer.performTestBefore, rootTerms);
                this.statementAttic.bottom().add(loopingStatement);

                return;
            }
        }
        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        // gets start and end position
        int startOffset = performStatement.f0.startOffset;
        super.visit(performStatement);
        int endOffset = super.getPosition();

        // stores statement into the latest statement list
        BasicStatement basicStatement = this.createBasicStatement(startOffset,
                endOffset, id, rootTerms, null); // Assume: there is no ?-Operator in COBOL
        this.statementAttic.bottom().add(basicStatement);
    }

    /**
     * Inserts the section into the hierarchy level attic.
     *
     * <PRE>
     * f0 -> SectionHeader()
     * f1 -> &lt;DOT&gt;
     * f2 -> Paragraphs()
     * </PRE>
     *
     * @param procedureSection
     *            token to be considered
     */
    @Override
    public void visit(ProcedureSection procedureSection) {
        this.addNewHierarchyLevelListToAttic();
        int startOffset = procedureSection.f0.f0.f0.f0.startOffset;
        String sectionName = procedureSection.f0.f0.f0.f0.tokenImage;
        super.visit(procedureSection);
        int endOffset = super.getPosition();
        HierarchyLevel hierarchyLevel = this.createHierarchyLevel(startOffset, endOffset, sectionName,
                HierarchyLevelTypes.getSectionType(this.builder),
                this.hierarchyAttic.pop(), EMPTY_STATEMENT_LIST);
        this.hierarchyAttic.bottom().add(hierarchyLevel);
    }

    /**
     * Stores the program name of the active program unit and generates the
     * coverage log file name.
     *
     * @param programName
     *            token to be considered
     */
    @Override
    public void visit(ProgramName programName) {
        this.progName = programName.f0.f0.tokenImage;
        this.fileName = this.progName + ".clf";
        super.visit(programName);
    }

    /**
     * Writes file section and file data declaration to output.
     *
     * <PRE>
     * f0 -> IdentificationDivision()
     * f1 -> [ EnvironmentDivision() ]
     * f2 -> [ DataDivision() ]
     * f3 -> [ ProcedureDivision() ]
     * </PRE>
     *
     * @param programUnit
     *            token to be considered
     */
    @Override
    public void visit(ProgramUnit programUnit) {
        this.counterProvider.incrementProgramUnitCounter();
        super.visit(programUnit.f0);
        if (programUnit.f1.node != null) {
            /*
             * Source code contains environment division.
             */
            super.visit(programUnit.f1);
        } else {
            /*
             * Source code does not contain environment division.
             */
            this.staticPartManipulator.generateEnvironmentDivision(
                    this.fileName, super.getOut());
        }
        if (programUnit.f2.node != null) {
            /*
             * Source code contains data division.
             */
            super.visit(programUnit.f2);
        } else {
            /*
             * Source code does not contain data division.
             */
            this.staticPartManipulator.generateDataDivision(
                    this.testSessionContainerUID, super.getOut());
            this.statementManipulator.generateStatementCounter(this.counterProvider
                    .getProgramUnitCounter(), super.getOut());
            this.branchManipulator.generateBranchCounter(this.counterProvider
                    .getProgramUnitCounter(), super.getOut());
            this.conditionManipulator.generateConditionCounter(this.counterProvider
                    .getProgramUnitCounter(), super.getOut());
            this.loopManipulator.generateLoopCounter(this.counterProvider
                    .getProgramUnitCounter(), super.getOut());
            this.staticPartManipulator.generateEndTestCaseDeclaration(super.getOut());
            generateWorkingStorageSection();
        }
        if (programUnit.f3.present()) {
            ProcedureDivision procedureDivision = (ProcedureDivision) programUnit.f3.node;
            super.visit(procedureDivision.f0);
            super.visit(procedureDivision.f1);
            super.visit(procedureDivision.f2);
            super.visit(procedureDivision.f3);
            super.visit(procedureDivision.f4);
            this.staticPartManipulator.generateOpenCoverageFile(super.getOut());
            super.visit(procedureDivision.f5);
        }
    }

    /**
     * Includes branch counter and creates model object.
     *
     * <PRE>
     * f0 -> &lt;READ&gt;
     * f1 -> FileName()
     * f2 -> [ &lt;NEXT&gt; ]
     * f3 -> [ &lt;RECORD&gt; ]
     * f4 -> [ &lt;INTO&gt; QualifiedDataName() ]
     * f5 -> [ &lt;KEY&gt; [ &lt;IS&gt; ] QualifiedDataName() ]
     * f6 -> [ &lt;INVALID&gt; [ &lt;KEY&gt; ] StatementList() ]
     * f7 -> [ &lt;NOT&gt; &lt;INVALID&gt; [ &lt;KEY&gt; ] StatementList() ]
     * f8 -> [ [ &lt;AT&gt; ] &lt;END&gt; StatementList() ]
     * f9 -> [ &lt;NOT&gt; [ &lt;AT&gt; ] &lt;END&gt; StatementList() ]
     * f10 -> [ &lt;END_READ&gt; ]
     * </PRE>
     *
     * @param readStatement
     *            token to be considered
     */
    @Override
    public void visit(ReadStatement readStatement) {
        if (readStatement.f6.present() || readStatement.f7.present()
                || readStatement.f8.present() || readStatement.f9.present()) {
            // writes add-keyword and saves its location in the source file
            super.visit(readStatement.f0);
            int startOffsetRea = readStatement.f0.startOffset;
            int endOffsetRea = super.getPosition();
            Location readLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetRea, endOffsetRea);

            List<Branch> branches = new LinkedList<Branch>();

            super.visit(readStatement.f1);
            super.visit(readStatement.f2);
            super.visit(readStatement.f3);
            super.visit(readStatement.f4);
            super.visit(readStatement.f5);
            if (readStatement.f6.present()) {
                NodeSequence nodeSequence = (NodeSequence) readStatement.f6.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(1));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(2));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (readStatement.f7.present()) {
                NodeSequence nodeSequence = (NodeSequence) readStatement.f7.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(1));
                super.visit((NodeOptional) nodeSequence.nodes.get(2));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(3));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (readStatement.f8.present()) {
                NodeSequence nodeSequence = (NodeSequence) readStatement.f8.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(1));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(2));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (readStatement.f9.present()) {
                NodeSequence nodeSequence = (NodeSequence) readStatement.f9.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(1));
                super.visit((NodeToken) nodeSequence.nodes.get(2));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(3));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (readStatement.f10.present()) {
                super.visit(readStatement.f10);
            } else {
                this.statementManipulator.generateEndRead(super.getOut());
            }
            endOffsetRea = super.getPosition();

            // instrumentation
            this.counterProvider.incrementStatementCounter();
            this.statementManipulator.manipulate(super.getOut());
            String id = this.counterProvider.getStatementCounterID();

            // stores the branches in a conditional statement
            ConditionalStatement conditionalStatement = this
                    .createConditionalStatement(startOffsetRea, endOffsetRea, id,
                            branches, readLocation, null);
            this.statementAttic.bottom().add(conditionalStatement);

            return;
        }
        int startOffset = readStatement.f0.startOffset;
        super.visit(readStatement);
        int endOffset = super.getPosition();

        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        // stores statement into the latest statement list
        BasicStatement basicStatement = this.createBasicStatement(startOffset,
                endOffset, id, null, null);
        this.statementAttic.bottom().add(basicStatement);
    }

    /**
     * Includes branch counter and creates model object.
     *
     * <PRE>
     * f0 -> &lt;REWRITE&gt;
     * f1 -> RecordName()
     * f2 -> [ &lt;FROM&gt; QualifiedDataName() ]
     * f3 -> [ &lt;INVALID&gt; [ &lt;KEY&gt; ] StatementList() ]
     * f4 -> [ &lt;NOT&gt; &lt;INVALID&gt; [ &lt;KEY&gt; ] StatementList() ]
     * f5 -> [ &lt;END_REWRITE&gt; ]
     * </PRE>
     *
     * @param rewriteStatement
     *            token to be considered
     */
    @Override
    public void visit(RewriteStatement rewriteStatement) {
        if (rewriteStatement.f3.present() || rewriteStatement.f4.present()) {
            // writes add-keyword and saves its location in the source file
            super.visit(rewriteStatement.f0);
            int startOffsetRew = rewriteStatement.f0.startOffset;
            int endOffsetRew = super.getPosition();
            Location rewriteLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetRew, endOffsetRew);

            List<Branch> branches = new LinkedList<Branch>();

            super.visit(rewriteStatement.f1);
            super.visit(rewriteStatement.f2);
            if (rewriteStatement.f3.present()) {
                NodeSequence nodeSequence = (NodeSequence) rewriteStatement.f3.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(1));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(2));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (rewriteStatement.f4.present()) {
                NodeSequence nodeSequence = (NodeSequence) rewriteStatement.f4.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(1));
                super.visit((NodeOptional) nodeSequence.nodes.get(2));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(3));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (rewriteStatement.f5.present()) {
                super.visit(rewriteStatement.f5);
            } else {
                this.statementManipulator.generateEndRewrite(super.getOut());
            }
            endOffsetRew = super.getPosition();

            // instrumentation
            this.counterProvider.incrementStatementCounter();
            this.statementManipulator.manipulate(super.getOut());
            String id = this.counterProvider.getStatementCounterID();

            // stores the branches in a conditional statement
            ConditionalStatement conditionalStatement = this
                    .createConditionalStatement(startOffsetRew, endOffsetRew, id,
                            branches, rewriteLocation, null);
            this.statementAttic.bottom().add(conditionalStatement);

            return;
        }
        int startOffset = rewriteStatement.f0.startOffset;
        super.visit(rewriteStatement);
        int endOffset = super.getPosition();

        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        // stores statement into the latest statement list
        BasicStatement basicStatement = this.createBasicStatement(startOffset,
                endOffset, id, null, null);
        this.statementAttic.bottom().add(basicStatement);
    }

    /**
     * Includes branch and condition counter to perform-statements.
     *
     * <PRE>
     * f0 -> &lt;SEARCH&gt;
     * f1 -> [ &lt;ALL&gt; ]
     * f2 -> QualifiedDataName()
     * f3 -> [ &lt;VARYING&gt; QualifiedDataName() ]
     * f4 -> [ [ &lt;AT&gt; ] &lt;END&gt; StatementList() ]
     * f5 -> ( SearchPhrase() )+
     * f6 -> [ &lt;END_SEARCH&gt; ]
     * </PRE>
     *
     * @param searchStatement
     *            token to be considered
     */
    @Override
    public void visit(SearchStatement searchStatement) {
        Set<RootTerm> rootTerms = new HashSet<RootTerm>();
        for (Node node : searchStatement.f5.nodes) {
            SearchPhrase searchPhrase = (SearchPhrase) node;

            BooleanTermContainer booleanTermContainer = new BooleanTermContainer();
            this.counterProvider.incrementConditionCounter();
            List<InstrBasicBooleanTerm> conditionTerms = this.processCondition(
                    searchPhrase.f1, booleanTermContainer);
            this.conditionManipulator.manipulate(super.getOut(), conditionTerms);
            String seID = this.counterProvider.getConditionCounterID();
            CoverableItem cItem = this.builder.createCoverableItem(EMPTY_STRING, seID);
            RootTerm rootTerm = this.builder
                    .createRootTerm(booleanTermContainer.booleanTerm, cItem);
            rootTerms.add(rootTerm);
        }

        int startOffsetSe = searchStatement.f0.startOffset;
        super.visit(searchStatement.f0);
        int endOffsetSe = super.getPosition();
        Location searchLocation = this.builder.createLocation(this.sourceFile,
                startOffsetSe, endOffsetSe);

        // list for at-end- and when-branches
        List<Branch> branches = new LinkedList<Branch>();

        super.visit(searchStatement.f1);
        super.visit(searchStatement.f2);
        super.visit(searchStatement.f3);
        NodeSequence nodeSequence = (NodeSequence) searchStatement.f4.node;
        if (nodeSequence != null) {
            /*
             * Source code contains at-end-phrase.
             */
            int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
            super.visit((NodeOptional) nodeSequence.nodes.get(0));
            super.visit((NodeToken) nodeSequence.nodes.get(1));
            int endOffset = super.getPosition();
            Location atEndLocation = this.builder.createLocation(this.sourceFile,
                    startOffset, endOffset);

            this.counterProvider.incrementBranchCounter();
            this.branchManipulator.manipulate(super.getOut());
            String atEndID = this.counterProvider.getBranchCounterID();

            this.addNewStatementListToAttic();

            super.visit((StatementList) nodeSequence.nodes.get(2));

            branches.add(this.createBranch(startOffset, endOffset, atEndID, atEndLocation, false));
        }
        for (Node node : searchStatement.f5.nodes) {
            /*
             * Source code contains when-phrase.
             */
            SearchPhrase searchPhrase = (SearchPhrase) node;
            int startOffset = searchPhrase.f0.startOffset;
            super.visit(searchPhrase.f0);
            int endOffset = super.getPosition();
            Location whenLocation = this.builder.createLocation(this.sourceFile,
                    startOffset, endOffset);

            super.visit(searchPhrase.f1);

            this.counterProvider.incrementBranchCounter();
            this.branchManipulator.manipulate(super.getOut());
            String whenID = this.counterProvider.getBranchCounterID();

            this.addNewStatementListToAttic();

            NodeChoice nodeChoice = searchPhrase.f2;
            if (nodeChoice.choice instanceof StatementList) {
                super.visit((StatementList) nodeChoice.choice);
                endOffset = super.getPosition();
            } else if (nodeChoice.choice instanceof NodeSequence) {
                NodeSequence nSequence = (NodeSequence) nodeChoice.choice;
                this.branchManipulator.generateNextSentenceGoTo(this, nSequence, super.getOut());
                endOffset = ((NodeToken) nSequence.nodes.get(1)).endOffset;
            }

            branches.add(this.createBranch(startOffset, endOffset, whenID, whenLocation, false));
        }
        if (searchStatement.f6.present()) {
            super.visit(searchStatement.f6);
        } else {
            this.statementManipulator.generateEndSearch(super.getOut());
        }
        endOffsetSe = super.getPosition();

        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        if (branches.size() == 0) {
            BasicStatement basicStatement = this.createBasicStatement(
                    startOffsetSe, endOffsetSe, id, null, null);
            this.statementAttic.bottom().add(basicStatement);
        } else {
            // stores the branches in a conditional statement
            ConditionalStatement conditionalStatement = this
                    .createConditionalStatement(startOffsetSe, endOffsetSe, id,
                            branches, searchLocation, rootTerms);
            this.statementAttic.bottom().add(conditionalStatement);
        }
    }

    /**
     * Writes next sentence marker after the sentence. Is needed when an if statement
     * contains next sentence statement.
     *
     * <PRE>
     * f0 -> StatementList()
     * f1 -> &lt;DOT&gt;
     * </PRE>
     *
     * @param sentence
     */
    @Override
    public void visit(Sentence sentence) {
        super.visit(sentence);
        this.branchManipulator.generateNextSentenceMark(super.getOut());
        this.branchManipulator.resetNextSentenceMark();
    }

    /**
     * Includes branch counter and creates model object.
     *
     * <PRE>
     * f0 -> &lt;START&gt;
     * f1 -> FileName()
     * f2 -> [ &lt;KEY&gt; [ &lt;IS&gt; ] ( &lt;EQUAL&gt; [ &lt;TO&gt; ] | &lt;EQUALCHAR&gt; | &lt;GREATER&gt; [ &lt;THAN&gt; ] | &lt;MORETHANCHAR&gt; | &lt;NOT&gt; &lt;LESS&gt; [ &lt;THAN&gt; ] | &lt;NOT&gt; &lt;LESSTHANCHAR&gt; | &lt;GREATER&gt; [ &lt;THAN&gt; ] &lt;OR&gt; &lt;EQUAL&gt; [ &lt;TO&gt; ] | &lt;MORETHANOREQUAL&gt; ) QualifiedDataName() ]
     * f3 -> [ &lt;INVALID&gt; [ &lt;KEY&gt; ] StatementList() ]
     * f4 -> [ &lt;NOT&gt; &lt;INVALID&gt; [ &lt;KEY&gt; ] StatementList() ]
     * f5 -> [ &lt;END_START&gt; ]
     * </PRE>
     *
     * @param startStatement
     *            token to be considered
     */
    @Override
    public void visit(StartStatement startStatement) {
        if (startStatement.f3.present() || startStatement.f4.present()) {
            // writes add-keyword and saves its location in the source file
            super.visit(startStatement.f0);
            int startOffsetSta = startStatement.f0.startOffset;
            int endOffsetSta = super.getPosition();
            Location startLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetSta, endOffsetSta);

            List<Branch> branches = new LinkedList<Branch>();

            super.visit(startStatement.f1);
            super.visit(startStatement.f2);
            if (startStatement.f3.present()) {
                NodeSequence nodeSequence = (NodeSequence) startStatement.f3.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(1));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(2));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (startStatement.f4.present()) {
                NodeSequence nodeSequence = (NodeSequence) startStatement.f4.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(1));
                super.visit((NodeOptional) nodeSequence.nodes.get(2));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(3));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (startStatement.f5.present()) {
                super.visit(startStatement.f5);
            } else {
                this.statementManipulator.generateEndStart(super.getOut());
            }
            endOffsetSta = super.getPosition();

            // instrumentation
            this.counterProvider.incrementStatementCounter();
            this.statementManipulator.manipulate(super.getOut());
            String id = this.counterProvider.getStatementCounterID();

            // stores the branches in a conditional statement
            ConditionalStatement conditionalStatement = this
                    .createConditionalStatement(startOffsetSta, endOffsetSta, id,
                            branches, startLocation, null);
            this.statementAttic.bottom().add(conditionalStatement);

            return;
        }
        int startOffset = startStatement.f0.startOffset;
        super.visit(startStatement);
        int endOffset = super.getPosition();

        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        // stores statement into the latest statement list
        BasicStatement basicStatement = this.createBasicStatement(startOffset,
                endOffset, id, null, null);
        this.statementAttic.bottom().add(basicStatement);
    }

    /**
     * Includes statement counter to all normal and harmless jumping statements.
     * Stop and goback statements are replaced with the "stop the program now"
     * call.
     *
     * @param statement
     *            token to be considered
     */
    @Override
    public void visit(Statement statement) {
        if (statement.f0.choice instanceof StopStatement
                || statement.f0.choice instanceof GobackStatement) {
            this.staticPartManipulator.replaceStopRun(this, statement, super.getOut());
        } else if (statement.f0.choice instanceof AddStatement
                || statement.f0.choice instanceof CallStatement
                || statement.f0.choice instanceof ComputeStatement
                || statement.f0.choice instanceof DeleteStatement
                || statement.f0.choice instanceof DivideStatement
                || statement.f0.choice instanceof EvaluateStatement
                || statement.f0.choice instanceof ExitProgramStatement
                || statement.f0.choice instanceof IfStatement
                || statement.f0.choice instanceof MultiplyStatement
                || statement.f0.choice instanceof PerformStatement
                || statement.f0.choice instanceof ReadStatement
                || statement.f0.choice instanceof RewriteStatement
                || statement.f0.choice instanceof SearchStatement
                || statement.f0.choice instanceof StartStatement
                || statement.f0.choice instanceof StringStatement
                || statement.f0.choice instanceof SubtractStatement
                || statement.f0.choice instanceof UnstringStatement
                || statement.f0.choice instanceof WriteStatement) {
            /*
             * Most of these statements are instrumented when visit them. Exit
             * just stops a subprogram. For that, they are not instrumented.
             */
            super.visit(statement);
        } else if (statement.f0.choice instanceof GotoStatement
                || statement.f0.choice instanceof ContinueStatement) {
            /*
             * Goto and continue are harmless jumping statements. The counter
             * appears before the statement.
             */
            this.counterProvider.incrementStatementCounter();
            this.statementManipulator.manipulate(super.getOut());
            String id = this.counterProvider.getStatementCounterID();

            int startOffset = StartOffset.getStartOffset(statement);
            super.visit(statement);
            int endOffset = super.getPosition();

            // saves statement to database
            BasicStatement basicStatement = this.createBasicStatement(
                    startOffset, endOffset, id, null, null);
            this.statementAttic.bottom().add(basicStatement);
        } else {
            /*
             * All other statements are normal statements. The counter appears
             * after the statement.
             */
            int startOffset = StartOffset.getStartOffset(statement);
            super.visit(statement);
            int endOffset = super.getPosition();
            // instruments the source code

            this.counterProvider.incrementStatementCounter();
            this.statementManipulator.manipulate(super.getOut());
            String id = this.counterProvider.getStatementCounterID();

            // saves statement to database
            BasicStatement basicStatement = this.createBasicStatement(
                    startOffset, endOffset, id, null, null);
            this.statementAttic.bottom().add(basicStatement);
        }
    }

    /**
     * Includes branch counter and creates model object.
     *
     * <PRE>
     * f0 -> &lt;STRING&gt;
     * f1 -> ( ( Identifier() | QualifiedDataName() | Literal() )+ &lt;DELIMITED&gt; [ &lt;BY&gt; ] ( Identifier() | Literal() | &lt;SIZE&gt; ) )+
     * f2 -> &lt;INTO&gt;
     * f3 -> Identifier()
     * f4 -> [ [ &lt;WITH&gt; ] &lt;POINTER&gt; QualifiedDataName() ]
     * f5 -> [ [ &lt;ON&gt; ] &lt;OVERFLOW&gt; StatementList() ]
     * f6 -> [ &lt;NOT&gt; [ &lt;ON&gt; ] &lt;OVERFLOW&gt; StatementList() ]
     * f7 -> [ &lt;END_STRING&gt; ]
     * </PRE>
     *
     * @param stringStatement
     *            token to be considered
     */
    @Override
    public void visit(StringStatement stringStatement) {
        if (stringStatement.f5.present() || stringStatement.f6.present()) {
            // writes add-keyword and saves its location in the source file
            super.visit(stringStatement.f0);
            int startOffsetStr = stringStatement.f0.startOffset;
            int endOffsetStr = super.getPosition();
            Location stringLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetStr, endOffsetStr);

            List<Branch> branches = new LinkedList<Branch>();

            super.visit(stringStatement.f1);
            super.visit(stringStatement.f2);
            super.visit(stringStatement.f3);
            super.visit(stringStatement.f4);
            if (stringStatement.f5.present()) {
                NodeSequence nodeSequence = (NodeSequence) stringStatement.f5.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(1));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(2));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (stringStatement.f6.present()) {
                NodeSequence nodeSequence = (NodeSequence) stringStatement.f6.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(1));
                super.visit((NodeToken) nodeSequence.nodes.get(2));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(3));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (stringStatement.f7.present()) {
                super.visit(stringStatement.f7);
            } else {
                this.statementManipulator.generateEndString(super.getOut());
            }
            endOffsetStr = super.getPosition();

            // instrumentation
            this.counterProvider.incrementStatementCounter();
            this.statementManipulator.manipulate(super.getOut());
            String id = this.counterProvider.getStatementCounterID();

            // stores the branches in a conditional statement
            ConditionalStatement conditionalStatement = this
                    .createConditionalStatement(startOffsetStr, endOffsetStr, id,
                            branches, stringLocation, null);
            this.statementAttic.bottom().add(conditionalStatement);

            return;
        }
        int startOffset = stringStatement.f0.startOffset;
        super.visit(stringStatement);
        int endOffset = super.getPosition();

        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        // stores statement into the latest statement list
        BasicStatement basicStatement = this.createBasicStatement(startOffset,
                endOffset, id, null, null);
        this.statementAttic.bottom().add(basicStatement);
    }

    /**
     * Includes branch counter and creates model object.
     *
     * <PRE>
     * f0 -> &lt;SUBTRACT&gt;
     * f1 -> (
     *         (
     *           Identifier()
     *           |
     *           Literal()
     *         )+
     *         &lt;FROM&gt;
     *         (
     *           Identifier()
     *           [ &lt;ROUNDED&gt; ]
     *         )+
     *         |
     *         (
     *           Identifier()
     *           |
     *           Literal()
     *         )
     *         &lt;FROM&gt;
     *         Identifier()
     *         [ &lt;ROUNDED&gt; ]
     *         &lt;GIVING&gt;
     *         Identifier()
     *         |
     *         (
     *           &lt;CORRESPONDING&gt;
     *           |
     *           &lt;CORR&gt;
     *         )
     *         QualifiedDataName()
     *         &lt;FROM&gt;
     *         QualifiedDataName()
     *       )
     * f2 -> [ [ &lt;ON&gt; ] &lt;SIZE&gt; &lt;ERROR&gt; StatementList() ]
     * f3 -> [ &lt;NOT&gt; [ &lt;ON&gt; ] &lt;SIZE&gt; &lt;ERROR&gt; StatementList() ]
     * f4 -> [ &lt;END_SUBTRACT&gt; ]
     * </PRE>
     *
     * @param subtractStatement
     *            token to be considered
     */
    @Override
    public void visit(SubtractStatement subtractStatement) {
        if (subtractStatement.f2.present() || subtractStatement.f3.present()) {
            // writes add-keyword and saves its location in the source file
            super.visit(subtractStatement.f0);
            int startOffsetSub = subtractStatement.f0.startOffset;
            int endOffsetSub = super.getPosition();
            Location subtractLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetSub, endOffsetSub);

            List<Branch> branches = new LinkedList<Branch>();

            super.visit((NodeSequence) subtractStatement.f1.choice);
            if (subtractStatement.f2.present()) {
                NodeSequence nodeSequence = (NodeSequence) subtractStatement.f2.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(1));
                super.visit((NodeToken) nodeSequence.nodes.get(2));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(3));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (subtractStatement.f3.present()) {
                NodeSequence nodeSequence = (NodeSequence) subtractStatement.f3.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(1));
                super.visit((NodeToken) nodeSequence.nodes.get(2));
                super.visit((NodeToken) nodeSequence.nodes.get(3));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(4));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (subtractStatement.f4.present()) {
                super.visit(subtractStatement.f4);
            } else {
                this.statementManipulator.generateEndSubtract(super.getOut());
            }
            endOffsetSub = super.getPosition();

            // instrumentation
            this.counterProvider.incrementStatementCounter();
            this.statementManipulator.manipulate(super.getOut());
            String id = this.counterProvider.getStatementCounterID();

            // stores the branches in a conditional statement
            ConditionalStatement conditionalStatement = this
                    .createConditionalStatement(startOffsetSub, endOffsetSub, id,
                            branches, subtractLocation, null);
            this.statementAttic.bottom().add(conditionalStatement);

            return;
        }
        int startOffset = subtractStatement.f0.startOffset;
        super.visit(subtractStatement);
        int endOffset = super.getPosition();

        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        // stores statement into the latest statement list
        BasicStatement basicStatement = this.createBasicStatement(startOffset,
                endOffset, id, null, null);
        this.statementAttic.bottom().add(basicStatement);
    }

    /**
     * Includes branch counter and creates model object.
     *
     * <PRE>
     * f0 -> &lt;UNSTRING&gt;
     * f1 -> QualifiedDataName()
     * f2 -> [ &lt;DELIMITED&gt; [ &lt;BY&gt; ] [ &lt;ALL&gt; ] ( Identifier() | Literal() ) ( &lt;OR&gt; [ &lt;ALL&gt; ] ( Identifier() | Literal() ) )* ]
     * f3 -> &lt;INTO&gt;
     * f4 -> ( Identifier() [ &lt;DELIMITER&gt; [ &lt;IN&gt; ] Identifier() ] [ &lt;COUNT&gt; [ &lt;IN&gt; ] Identifier() ] )+
     * f5 -> [ [ &lt;WITH&gt; ] &lt;POINTER&gt; QualifiedDataName() ]
     * f6 -> [ &lt;TALLYING&gt; [ &lt;IN&gt; ] QualifiedDataName() ]
     * f7 -> [ [ &lt;ON&gt; ] &lt;OVERFLOW&gt; StatementList() ]
     * f8 -> [ &lt;NOT&gt; [ &lt;ON&gt; ] &lt;OVERFLOW&gt; StatementList() ]
     * f9 -> [ &lt;END_UNSTRING&gt; ]
     * </PRE>
     *
     * @param unstringStatement
     *            token to be considered
     */
    @Override
    public void visit(UnstringStatement unstringStatement) {
        if (unstringStatement.f7.present() || unstringStatement.f8.present()) {
            // writes add-keyword and saves its location in the source file
            super.visit(unstringStatement.f0);
            int startOffsetUns = unstringStatement.f0.startOffset;
            int endOffsetUns = super.getPosition();
            Location unstringLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetUns, endOffsetUns);

            List<Branch> branches = new LinkedList<Branch>();

            super.visit(unstringStatement.f1);
            super.visit(unstringStatement.f2);
            super.visit(unstringStatement.f3);
            super.visit(unstringStatement.f4);
            super.visit(unstringStatement.f5);
            super.visit(unstringStatement.f6);
            if (unstringStatement.f7.present()) {
                NodeSequence nodeSequence = (NodeSequence) unstringStatement.f7.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(1));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(2));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (unstringStatement.f8.present()) {
                NodeSequence nodeSequence = (NodeSequence) unstringStatement.f8.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(1));
                super.visit((NodeToken) nodeSequence.nodes.get(2));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(3));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (unstringStatement.f9.present()) {
                super.visit(unstringStatement.f9);
            } else {
                this.statementManipulator.generateEndUnstring(super.getOut());
            }
            endOffsetUns = super.getPosition();

            // instrumentation
            this.counterProvider.incrementStatementCounter();
            this.statementManipulator.manipulate(super.getOut());
            String id = this.counterProvider.getStatementCounterID();

            // stores the branches in a conditional statement
            ConditionalStatement conditionalStatement = this
                    .createConditionalStatement(startOffsetUns, endOffsetUns, id,
                            branches, unstringLocation, null);
            this.statementAttic.bottom().add(conditionalStatement);

            return;
        }
        int startOffset = unstringStatement.f0.startOffset;
        super.visit(unstringStatement);
        int endOffset = super.getPosition();

        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        // stores statement into the latest statement list
        BasicStatement basicStatement = this.createBasicStatement(startOffset,
                endOffset, id, null, null);
        this.statementAttic.bottom().add(basicStatement);
    }

    /**
     * Includes branch counter and creates model object.
     *
     * <PRE>
     * f0 -> &lt;WRITE&gt;
     * f1 -> RecordName()
     * f2 -> [ &lt;FROM&gt; QualifiedDataName() ]
     * f3 -> [ AdvancingPhrase() ]
     * f4 -> [ [ &lt;AT&gt; ] ( &lt;END_OF_PAGE&gt; | &lt;EOP&gt; ) StatementList() ]
     * f5 -> [ &lt;NOT&gt; [ &lt;AT&gt; ] ( &lt;END_OF_PAGE&gt; | &lt;EOP&gt; ) StatementList() ]
     * f6 -> [ &lt;INVALID&gt; [ &lt;KEY&gt; ] StatementList() ]
     * f7 -> [ &lt;NOT&gt; &lt;INVALID&gt; [ &lt;KEY&gt; ] StatementList() ]
     * f8 -> [ &lt;END_WRITE&gt; ]
     * </PRE>
     *
     * @param writeStatement
     *            token to be considered
     */
    @Override
    public void visit(WriteStatement writeStatement) {
        if (writeStatement.f4.present() || writeStatement.f5.present()
                || writeStatement.f6.present() || writeStatement.f7.present()) {
            // writes add-keyword and saves its location in the source file
            super.visit(writeStatement.f0);
            int startOffsetWri = writeStatement.f0.startOffset;
            int endOffsetWri = super.getPosition();
            Location writeLocation = this.builder.createLocation(this.sourceFile,
                    startOffsetWri, endOffsetWri);

            List<Branch> branches = new LinkedList<Branch>();

            super.visit(writeStatement.f1);
            super.visit(writeStatement.f2);
            super.visit(writeStatement.f3);
            if (writeStatement.f4.present()) {
                NodeSequence nodeSequence = (NodeSequence) writeStatement.f4.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(0));
                NodeChoice nodeChoice = (NodeChoice) nodeSequence.nodes.get(1);
                super.visit((NodeToken) nodeChoice.choice);
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(2));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (writeStatement.f5.present()) {
                NodeSequence nodeSequence = (NodeSequence) writeStatement.f5.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(1));
                NodeChoice nodeChoice = (NodeChoice) nodeSequence.nodes.get(2);
                super.visit((NodeToken) nodeChoice.choice);
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(3));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (writeStatement.f6.present()) {
                NodeSequence nodeSequence = (NodeSequence) writeStatement.f6.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeOptional) nodeSequence.nodes.get(1));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(2));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (writeStatement.f7.present()) {
                NodeSequence nodeSequence = (NodeSequence) writeStatement.f7.node;

                // writes add-keyword and saves its location in the source file
                int startOffset = StartOffset.getStartOffset(nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(0));
                super.visit((NodeToken) nodeSequence.nodes.get(1));
                super.visit((NodeOptional) nodeSequence.nodes.get(2));
                int endOffset = super.getPosition();
                Location location = this.builder.createLocation(
                        this.sourceFile, startOffset, endOffset);

                // instrumentation
                this.counterProvider.incrementBranchCounter();
                this.branchManipulator.manipulate(super.getOut());
                String id = this.counterProvider.getBranchCounterID();

                this.addNewStatementListToAttic();

                super.visit((StatementList) nodeSequence.nodes.get(3));

                endOffset = super.getPosition();
                branches.add(this.createBranch(startOffset, endOffset,
                        id, location, false));
            }
            if (writeStatement.f8.present()) {
                super.visit(writeStatement.f8);
            } else {
                this.statementManipulator.generateEndWrite(super.getOut());
            }
            endOffsetWri = super.getPosition();

            // instrumentation
            this.counterProvider.incrementStatementCounter();
            this.statementManipulator.manipulate(super.getOut());
            String id = this.counterProvider.getStatementCounterID();

            // stores the branches in a conditional statement
            ConditionalStatement conditionalStatement = this
                    .createConditionalStatement(startOffsetWri, endOffsetWri, id,
                            branches, writeLocation, null);
            this.statementAttic.bottom().add(conditionalStatement);

            return;
        }
        int startOffset = writeStatement.f0.startOffset;
        super.visit(writeStatement);
        int endOffset = super.getPosition();

        // instrumentation
        this.counterProvider.incrementStatementCounter();
        this.statementManipulator.manipulate(super.getOut());
        String id = this.counterProvider.getStatementCounterID();

        // stores statement into the latest statement list
        BasicStatement basicStatement = this.createBasicStatement(startOffset,
                endOffset, id, null, null);
        this.statementAttic.bottom().add(basicStatement);
    }

    class BooleanTermContainer {
        BooleanTerm booleanTerm;
    }

    class BooleanContainer {
        boolean performTestBefore = false;
    }

}
