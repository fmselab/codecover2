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

package org.codecover.instrumentation.java15.visitor;

import java.io.IOException;
import java.io.Writer;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import org.codecover.instrumentation.HierarchyLevelContainer;
import org.codecover.instrumentation.booleanterms.InstrBooleanTerm;
import org.codecover.instrumentation.java.measurement.CounterContainer;
import org.codecover.instrumentation.java.measurement.Protocol;
import org.codecover.instrumentation.java15.HierarchyLevelTypeProvider;
import org.codecover.instrumentation.java15.JavaExpressionParser;
import org.codecover.instrumentation.java15.counter.CounterIDManager;
import org.codecover.instrumentation.java15.counter.CounterManager;
import org.codecover.instrumentation.java15.location.LocateableManager;
import org.codecover.instrumentation.java15.manipulators.ArrayStatementManipulator;
import org.codecover.instrumentation.java15.manipulators.BranchManipulator;
import org.codecover.instrumentation.java15.manipulators.CommentManipulator;
import org.codecover.instrumentation.java15.manipulators.ConditionManipulator;
import org.codecover.instrumentation.java15.manipulators.DummyBranchManipulator;
import org.codecover.instrumentation.java15.manipulators.DummyCommentManipulator;
import org.codecover.instrumentation.java15.manipulators.DummyConditionManipulator;
import org.codecover.instrumentation.java15.manipulators.DummyLoopManipulator;
import org.codecover.instrumentation.java15.manipulators.DummyStatementManipulator;
import org.codecover.instrumentation.java15.manipulators.DummySynchronizedManipulator;
import org.codecover.instrumentation.java15.manipulators.DummyQMOManipulator;
import org.codecover.instrumentation.java15.manipulators.LoopManipulator;
import org.codecover.instrumentation.java15.manipulators.Manipulator;
import org.codecover.instrumentation.java15.manipulators.QMOManipulator;
import org.codecover.instrumentation.java15.manipulators.StatementManipulator;
import org.codecover.instrumentation.java15.manipulators.SynchronizedManipulator;
import org.codecover.instrumentation.java15.manipulators.ConditionManipulator.ConditionManipualtionResult;
import org.codecover.instrumentation.java15.parser.JavaParser;
import org.codecover.instrumentation.java15.syntaxtree.AllocationExpression;
import org.codecover.instrumentation.java15.syntaxtree.AnnotationTypeDeclaration;
import org.codecover.instrumentation.java15.syntaxtree.AnnotationTypeMemberDeclaration;
import org.codecover.instrumentation.java15.syntaxtree.Block;
import org.codecover.instrumentation.java15.syntaxtree.BlockStatement;
import org.codecover.instrumentation.java15.syntaxtree.BreakStatement;
import org.codecover.instrumentation.java15.syntaxtree.ClassOrInterfaceBody;
import org.codecover.instrumentation.java15.syntaxtree.ClassOrInterfaceBodyDeclaration;
import org.codecover.instrumentation.java15.syntaxtree.ClassOrInterfaceDeclaration;
import org.codecover.instrumentation.java15.syntaxtree.CompilationUnit;
import org.codecover.instrumentation.java15.syntaxtree.ConditionalExpression;
import org.codecover.instrumentation.java15.syntaxtree.ConditionalOrExpression;
import org.codecover.instrumentation.java15.syntaxtree.ConstructorDeclaration;
import org.codecover.instrumentation.java15.syntaxtree.ContinueStatement;
import org.codecover.instrumentation.java15.syntaxtree.DoStatement;
import org.codecover.instrumentation.java15.syntaxtree.EmptyStatement;
import org.codecover.instrumentation.java15.syntaxtree.EnumConstant;
import org.codecover.instrumentation.java15.syntaxtree.EnumDeclaration;
import org.codecover.instrumentation.java15.syntaxtree.ExplicitConstructorInvocation;
import org.codecover.instrumentation.java15.syntaxtree.Expression;
import org.codecover.instrumentation.java15.syntaxtree.FieldDeclaration;
import org.codecover.instrumentation.java15.syntaxtree.ForStatement;
import org.codecover.instrumentation.java15.syntaxtree.FormalParameter;
import org.codecover.instrumentation.java15.syntaxtree.IfStatement;
import org.codecover.instrumentation.java15.syntaxtree.LabeledStatement;
import org.codecover.instrumentation.java15.syntaxtree.LocalVariableDeclaration;
import org.codecover.instrumentation.java15.syntaxtree.MethodDeclaration;
import org.codecover.instrumentation.java15.syntaxtree.Modifiers;
import org.codecover.instrumentation.java15.syntaxtree.Node;
import org.codecover.instrumentation.java15.syntaxtree.NodeChoice;
import org.codecover.instrumentation.java15.syntaxtree.NodeListOptional;
import org.codecover.instrumentation.java15.syntaxtree.NodeOptional;
import org.codecover.instrumentation.java15.syntaxtree.NodeSequence;
import org.codecover.instrumentation.java15.syntaxtree.NodeToken;
import org.codecover.instrumentation.java15.syntaxtree.PackageDeclaration;
import org.codecover.instrumentation.java15.syntaxtree.ReturnStatement;
import org.codecover.instrumentation.java15.syntaxtree.Statement;
import org.codecover.instrumentation.java15.syntaxtree.StatementExpression;
import org.codecover.instrumentation.java15.syntaxtree.SwitchLabel;
import org.codecover.instrumentation.java15.syntaxtree.SwitchStatement;
import org.codecover.instrumentation.java15.syntaxtree.SynchronizedStatement;
import org.codecover.instrumentation.java15.syntaxtree.ThrowStatement;
import org.codecover.instrumentation.java15.syntaxtree.TryStatement;
import org.codecover.instrumentation.java15.syntaxtree.Type;
import org.codecover.instrumentation.java15.syntaxtree.TypeDeclaration;
import org.codecover.instrumentation.java15.syntaxtree.VariableDeclarator;
import org.codecover.instrumentation.java15.syntaxtree.WhileStatement;
import org.codecover.instrumentation.measurement.CoverageCounterLog;
import org.codecover.instrumentation.measurement.CoverageResultLog;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.BasicStatement;
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
import org.codecover.model.mast.*;


/**
 * This is the instrumenter for Java 1.5.<br>
 * <br>
 * Its job is to visit the syntaxtree made by {@link JavaParser#CompilationUnit()}
 * and write all the nodes to a Writer. Additionally instrumented code is written
 * to the target file. Therefore we use {@link Manipulator}s. There are:
 * <ul>
 * <li>{@link StatementManipulator}</li>
 * <li>{@link BranchManipulator}</li>
 * <li>{@link ConditionManipulator}</li>
 * <li>{@link LoopManipulator}</li>
 * <li>{@link SynchronizedManipulator}</li>
 * </ul>
 * Which are informed, when a Statement, Branch, BooleanTerm or Looping Statement
 * is found in the syntaxtree. These {@link Manipulator}s each exist in a dummy
 * and a default version&mdash;e.g. the {@link DummyStatementManipulator} and
 * {@link ArrayStatementManipulator}. This {@link InstrumentationVisitor} does
 * not know what kind of {@link Manipulator} is set by
 * {@link #setStatementManipulator(StatementManipulator)}. So the caller of this
 * {@link InstrumentationVisitor} can say, whether statement should be instrumented
 * using an array of longs ({@link ArrayStatementManipulator}) or should not be
 * instrumented ({@link DummyStatementManipulator}). The same procedure is done 
 * with the other {@link Manipulator}s. They each add code to the writer
 * of the {@link InstrumentationVisitor} or not. So what they add, or if they
 * add statements, is no known by this {@link InstrumentationVisitor}.<br>
 * To manage IDs a {@link CounterIDManager} is initiated in
 * {@link #visit(CompilationUnit)}. This {@link CounterIDManager} is informed of
 * the Manipulators which also implement the {@link CounterManager} interface. 
 * This {@link CounterIDManager} knows, how the IDs of the {@link CoverableItem}s
 * are generated and increments the ID to tell a caller, what is the next ID of
 * a statement ({@link CounterIDManager#nextStatementID()}). The
 * {@link CounterIDManager} finally adds a new class to the target file. This
 * class implements the {@link CounterContainer} and will contain all counters
 * for statements, branches, terms, loops and their amount. when the source of
 * this class is added to the target file, the {@link CounterIDManager} uses
 * {@link CounterManager#writeDeclarations()}, {@link CounterManager#writeReset()}
 * and {@link CounterManager#writeSerialzeAndReset()} to tell the Manipulators
 * to write code to the target, which is used to read out the instrumented
 * counters and write it to {@link CoverageCounterLog#passCounter(String, long)}.<br>
 * A last purpose of this class&mdash;as you can see there are many&mdash; is
 * the transformation of the syntaxtree of {@link JavaParser#CompilationUnit()}
 * into a MAST (<i>More Abstract Syntax Tree</i>) build by {@link MASTBuilder}.
 * This is an abstract syntaxtree designed to fit to many programming languages.
 * This {@link InstrumentationVisitor} creates a {@link HierarchyLevel} for
 * each top level class, interface, enum or annotation, found in this source file.
 * Moreover it creates {@link BasicStatement}s, {@link ConditionalStatement}s,
 * {@link LoopingStatement}s and {@link RootTerm}s out of the given productions
 * got from the syntaxtree. Therefore a number of {@link Attic} members are used,
 * which each represent a stack of {@link List}s. The top level list of this attic
 * is filled with elements found at the current level of the syntaxtree. E.g.
 * all statements, that where found, are added to {@link #statementAttic}. If
 * there is a new <code>if</code> statement, a new list is pushed to the
 * {@link #statementAttic}. Now all statements of the then branch of the
 * <code>if</code> are pushed to this new list. If the then branch is finished,
 * the prior pushed list is popped from the {@link #statementAttic} and a
 * {@link StatementSequence} is created, representing the whole then branch.
 * Finally the <code>if</code> statement is added as an {@link ConditionalStatement}
 * to the {@link #statementAttic}. The example:
 * <pre>
 * doSomething() {
 *   i = 0;
 *   if (condition) {
 *     i = 2;
 *     k = 3;
 *   }
 *   print(i);
 * }
 * </pre>
 * has the following statements at the attic (over the state of parsing):
 * <pre>
 * 1) (method) {}
 * 2) (method) {i = 0}
 *    -> push new list for the if
 * 3) (method) {i = 0}
 *    (if)     {}
 * 4) (method) {i = 0}
 *    (if)     {i = 2}
 * 5) (method) {i = 0}
 *    (if)     {i = 2, k = 3}
 *    -> pop list of if and create a sequence
 * 6) (method) {i = 0, {i = 2, k = 3}}
 * 7) (method) {i = 0, {i = 2, k = 3}, print(i)}
 *    -> pop the list of the method to create a sequence
 * </pre>
 * To create the {@link Location}s, for nearly all mast elements, a single
 * {@link LocateableManager} is used. This has levels of {@link Location}s and
 * is used to create a {@link LocationList} out of start and end offset.
 * Moreover, this manager can slice out location information of explicit child
 * levels from a level below them.<br>
 * This approach is used for {@link HierarchyLevel}s too: {@link #hierarchyAttic}.<br>
 * The {@link InstrumentationVisitor} is rather complex, but satisfy all three
 * <b>main interfaces</b>:
 * <ul>
 * <li>MAST</li>
 * <li>syntaxtree of Java</li>
 * <li>behavior of the instrumented code</li>
 * </ul>
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: InstrumentationVisitor.java 70 2010-04-02 09:20:58Z schmidberger $)
 * 
 * @see Manipulator
 * @see JavaParser
 * @see DepthFirstVisitorWithException
 * @see CounterIDManager
 * @see LocateableManager
 */
public class InstrumentationVisitor extends TreeDumperWithException {
    /**
     * States whether or whether not {@link HierarchyLevel}s of packages
     * should have the full name.<br>
     * <br>
     * The name <code>org.codecover.instrumentation</code> can have the
     * hierarchical package names:
     * <ul>
     * <li><code>true</code> &rarr; <code>org</code>,
     * <code>org.codecover</code>, <code>org.codecover.instrumentation</code></li>
     * <li><code>false</code> &rarr; <code>org</code>,
     * <code>codecover</code>, <code>instrumentation</code></li>
     * </ul>
     * 
     * @see HierarchyLevelContainer#packagePathToList(String, boolean)
     */
    public static final boolean USE_FULL_PACKAGE_NAMES_IN_HIERARCHY = false;

    /**
     * A class that is used as the {@link CoverageResultLog} when instrumenting.
     */
    private Class<? extends CoverageResultLog> coverageResultLog;

    private MASTBuilder builder;

    private SourceFile sourceFile;

    private HierarchyLevelTypeProvider hierarchyTypeProvider;

    /**
     * The {@link StatementManipulator} which is responsible for instrumenting
     * statements.
     */
    private StatementManipulator statementManipulator;

    /**
     * The {@link BranchManipulator} which is responsible for instrumenting
     * branches.
     */
    private BranchManipulator branchManipulator;

    /**
     * The {@link ConditionManipulator} which is responsible for instrumenting
     * basic boolean terms in if, while, do while, for and ternary operator.
     */
    private ConditionManipulator conditionManipulator;

    /**
     * The {@link LoopManipulator} which is responsible for instrumenting
     * loops.
     */
    private LoopManipulator loopManipulator;

    /**
     * The {@link CommentManipulator}, which is responsible for parsing 
     * "// startTestCase("Name");" in comments and creating a start statement
     * of the {@link Protocol}.
     */
    private CommentManipulator commentManipulator;
    
    private SynchronizedManipulator syncStatementManipulator;


    /**
     * RS, 30.10.2009, the question mark operator
     */
    private QMOManipulator qmoManipulator;

    /**
     * This is an instance of {@link JavaExpressionParser} which is used for
     * the parsing of {@link Expression} to {@link InstrBooleanTerm}s.
     */
    private JavaExpressionParser expressionParser;

    /**
     * For every top level type declaration a {@link CounterIDManager} is used
     * to handle ID's for statements, branches and so on. It is responsible for
     * creating the inner class CodeCoverCoverageCounter.
     */
    private CounterIDManager counterIDManager;

    /**
     * The name of the java package. This is parsed during the visiting.
     */
    private String packageName;

    /**
     * The full name of the class <b>packageName.className</b>
     */
    private String fullSourceFileName;

    /**
     * For some instrumentations, it is required to expand single line statement
     * to block statements. Here statements in if / else are considered.
     * 
     * <pre>
     * if (a == 1)
     *     a++;
     * </pre>
     * 
     * into
     * 
     * <pre>
     * if (a == 1) {
     *     a++;
     * }
     * </pre>
     */
    private boolean needToExpandBranchesToBlock;

    /**
     * For some instrumentations, it is required to expand single line statement
     * to block statements. Here statements in loops are considered.
     * 
     * <pre>
     * while ( a <= 10 )
     *     a++;
     * </pre>
     * 
     * into
     * 
     * <pre>
     * while ( a <= 10 ) {
     *     a++;
     * }
     * </pre>
     */
    private boolean needToExpandLoopsToBlock;

    /**
     * This is a stack containing lists of hierarchy levels for each depth
     * 
     * @see #pushNewHieraryLevelToAttic(HierarchyLevelType)
     * @see #createHierarchyLevelFromAtticAndPushIt(String, LocationList, LocationList)
     * @see #popTopLevelHieraryLevelsFromAttic()
     */
    private Attic<List<HierarchyLevel>> hierarchyAttic;

    /**
     * Needed, cause in most productions, the {@link Modifiers} are not contained,
     * but they are needed to get the correct header of {@link HierarchyLevel}s.
     * 
     * @see #recalculateStartOffsetUsingModifiers(int)
     * @see #pushModifiersToAttic(Modifiers)
     * @see #popModifiersFromAttic()
     * @see MASTBuilder#createHierarchyLevel(LocationList, String, LocationList, HierarchyLevelType, List, List)
     */
    private Attic<Modifiers> modifiersAttic;

    /**
     * This is a stack containing the types of the parsed {@link HierarchyLevel}s
     * 
     * @see #inClass() checks, whether the last declared level is a class
     */
    private Attic<HierarchyLevelType> hierarchyLevelTypeAttic;

    /**
     * This is a stack containing lists of statement levels for each depth
     * 
     * @see #acceptAndAtticBasicStatement(Statement)
     * @see #createStatementSequenceFromAttic()
     * @see #createStatementSequenceListFromAttic()
     * @see #pushNewStatementLevelToAttic()
     * @see #createConditionalStatementAndPushIt(String, LocationList, RootTerm, List, Location)
     * @see #createLoopingStatementAntPushIt(String, LocationList, RootTerm, Location, String, String, String, boolean)
     */
    private Attic<List<org.codecover.model.mast.Statement>> statementAttic;

    private HierarchyLevelContainer hierarchyLevelContainer;
    
    private LocateableManager locateableManager;

    private final String testSessionContainerUID;

    private final String preferedCoverageLogPath;
    
    /**
     * RS, 15.12.09
     */
    private Set<QuestionMarkOperator> questionMarkOperators = new HashSet<QuestionMarkOperator>();

    /**
     * RS, 15.12.09
     */
    private Set<org.codecover.model.mast.SynchronizedStatement> synchronizedStatements = new HashSet<org.codecover.model.mast.SynchronizedStatement>();

    /**
     * Constructs a new {@link InstrumentationVisitor}.<br>
     * <br>
     * Some fields are initialized and DummyManipulators set.
     * 
     * @param writer
     *                This writer is delegated to the parent class
     *                {@link TreeDumperWithException}.
     * @param coverageResultLog
     *                A class that is used as the {@link CoverageResultLog} when
     *                instrumenting.
     * @param builder
     *                The database for the MAST creation.
     * @param sourceFile
     *                The source code file.
     * @param hierarchyLevelContainer
     *                This is the {@link HierarchyLevelContainer}, where all
     *                top level {@link HierarchyLevel}s of the source file can
     *                be added using
     *                {@link HierarchyLevelContainer#addHierarchyLevels(Collection, LinkedList)}.
     * @param testSessionContainerUID
     *                The UID of the {@link TestSessionContainer}.
     * @param preferedCoverageLogPath
     *                <code>null</code> or an absolute path to the coverage
     *                log file got by directive.(see
     *                {@link CounterIDManager#writeInnerClass(String)})
     */
    public InstrumentationVisitor(Writer writer,
            Class<? extends CoverageResultLog> coverageResultLog,
            MASTBuilder builder,
            SourceFile sourceFile,
            HierarchyLevelContainer hierarchyLevelContainer,
            String testSessionContainerUID,
            String preferedCoverageLogPath) {
        super(writer);
        this.coverageResultLog = coverageResultLog;
        this.builder = builder;
        this.sourceFile = sourceFile;
        this.hierarchyLevelContainer = hierarchyLevelContainer;
        this.testSessionContainerUID = testSessionContainerUID;
        this.preferedCoverageLogPath = preferedCoverageLogPath;
        this.hierarchyTypeProvider = new HierarchyLevelTypeProvider(this.builder);
        this.statementManipulator = new DummyStatementManipulator();
        this.branchManipulator = new DummyBranchManipulator();
        this.conditionManipulator = new DummyConditionManipulator();
        this.loopManipulator = new DummyLoopManipulator();
        this.commentManipulator = new DummyCommentManipulator();
        this.syncStatementManipulator = new DummySynchronizedManipulator();
        this.qmoManipulator = new DummyQMOManipulator();
        
        this.expressionParser = new JavaExpressionParser();
        this.counterIDManager = null;
        this.packageName = null;
        this.fullSourceFileName = null;
        this.hierarchyAttic = new Attic<List<HierarchyLevel>>();
        this.modifiersAttic = new Attic<Modifiers>();
        this.hierarchyLevelTypeAttic = new Attic<HierarchyLevelType>();
        this.statementAttic = new Attic<List<org.codecover.model.mast.Statement>>();
        this.locateableManager = new LocateableManager(this,
                this.sourceFile, this.builder);

        recalcExpandToBlock();
    }

    private void recalcExpandToBlock() {
        this.needToExpandBranchesToBlock = false;
        this.needToExpandBranchesToBlock |= this.statementManipulator
                .requiresBlockExpansionsForBranches();
        this.needToExpandBranchesToBlock |= this.branchManipulator
                .requiresBlockExpansionsForBranches();
        this.needToExpandBranchesToBlock |= this.conditionManipulator
                .requiresBlockExpansionsForBranches();
        this.needToExpandBranchesToBlock |= this.loopManipulator
                .requiresBlockExpansionsForBranches();
        this.needToExpandBranchesToBlock |= this.commentManipulator
                .requiresBlockExpansionsForBranches();
        this.needToExpandBranchesToBlock |= this.syncStatementManipulator
                .requiresBlockExpansionsForBranches();
        this.needToExpandBranchesToBlock |= this.qmoManipulator
                .requiresBlockExpansionsForBranches();

        this.needToExpandLoopsToBlock = false;
        this.needToExpandLoopsToBlock |= this.statementManipulator
                .requiresBlockExpansionsForLoops();
        this.needToExpandLoopsToBlock |= this.branchManipulator
                .requiresBlockExpansionsForLoops();
        this.needToExpandLoopsToBlock |= this.conditionManipulator
                .requiresBlockExpansionsForLoops();
        this.needToExpandLoopsToBlock |= this.loopManipulator
                .requiresBlockExpansionsForLoops();
        this.needToExpandLoopsToBlock |= this.commentManipulator
                .requiresBlockExpansionsForLoops();
        this.needToExpandLoopsToBlock |= this.syncStatementManipulator
                .requiresBlockExpansionsForLoops();
        this.needToExpandLoopsToBlock |= this.qmoManipulator
                .requiresBlockExpansionsForLoops();
    }

    private boolean isClass(HierarchyLevelType type) {
        return type == this.hierarchyTypeProvider.getClassType();
    }

    private boolean isEnum(HierarchyLevelType type) {
        return type == this.hierarchyTypeProvider.getEnumType();
    }

    private boolean isMethod(HierarchyLevelType type) {
        return type == this.hierarchyTypeProvider.getMethodType();
    }

    /**
     * Is the last opened {@link HierarchyLevel} a class.<br>
     * <br>
     * <code>isClass(this.hierarchyLevelTypeAttic.top())</code>
     */
    private boolean inClass() {
        return !this.hierarchyLevelTypeAttic.isEmpty()
                && isClass(this.hierarchyLevelTypeAttic.bottom());
    }

    /**
     * Is the last opened {@link HierarchyLevel} an enum.<br>
     * <br>
     * <code>isEnum(this.hierarchyLevelTypeAttic.top())</code>
     */
    private boolean inEnum() {
        return !this.hierarchyLevelTypeAttic.isEmpty()
        && isEnum(this.hierarchyLevelTypeAttic.bottom());
    }

    /**
     * Is the last opened {@link HierarchyLevel} a method.<br>
     * <br>
     * <code>isMethod(this.hierarchyLevelTypeAttic.top())</code>
     */
    private boolean inMethod() {
        return !this.hierarchyLevelTypeAttic.isEmpty()
        && isMethod(this.hierarchyLevelTypeAttic.bottom());
    }

    /////////////////////////////////////////////////////////////////
    //
    // private helpers for MAST creation
    //
    /////////////////////////////////////////////////////////////////

    private Location createLocation(int startOffset, int endOffset) {
        return this.builder.createLocation(this.sourceFile, startOffset, endOffset);
    }

    private void startLocateableLevel(int startOffset) {
        this.locateableManager.pushNewLevel(startOffset, false);
    }

    private void startLocateableLevel() {
        this.locateableManager.pushNewLevel(false);
    }

    private void startExplicitLocateableLevel(int startOffset) {
        this.locateableManager.pushNewLevel(startOffset, true);
    }
    
    private void startExplicitLocateableLevel() {
        this.locateableManager.pushNewLevel(true);
    }

    private LocationList endLocateableLevel(int endOffset) {
        return this.locateableManager.popLevel(endOffset);
    }

    private LocationList endLocateableLevel() {
        return this.locateableManager.popLevel();
    }

    private void popTopLevelHieraryLevelsFromAttic() {
        if (this.hierarchyAttic.size() != 1) {
            String message = "this.hierarchyAttic.size() != 1";
            Exception exception = new IllegalStateException(message);
            this.builder.getLogger().fatal(message, exception);
        }
        if (!this.hierarchyLevelTypeAttic.isEmpty()) {
            String message = "!this.hierarchyLevelTypeAttic.isEmpty()";
            Exception exception = new IllegalStateException(message);
            this.builder.getLogger().fatal(message, exception);
        }
        if (!this.locateableManager.isEmpty()) {
            String message = "!this.locateableManager.isEmpty()";
            Exception exception = new IllegalStateException(message);
            this.builder.getLogger().fatal(message, exception);
        }

        LinkedList<String> packagesInAList = HierarchyLevelContainer
                .packagePathToList(this.packageName, USE_FULL_PACKAGE_NAMES_IN_HIERARCHY);
        this.hierarchyLevelContainer.addHierarchyLevels(this.hierarchyAttic.pop(),
                packagesInAList);
    }

    /**
     * Creates a new level at the {@link #hierarchyAttic}. If the
     * hierarchyLevelType is a class {@link #pushNewStatementLevelToAttic()} is
     * called.
     * 
     * @param hierarchyLevelType
     *            The type of the new HierarchyLevel.
     */
    private void pushNewHieraryLevelToAttic(HierarchyLevelType hierarchyLevelType) {
        this.hierarchyAttic.push(new LinkedList<HierarchyLevel>());

        // hierarchyLevelType is null for the top level HierarchyLevel
        if (hierarchyLevelType != null) {
            this.hierarchyLevelTypeAttic.push(hierarchyLevelType);

            pushNewStatementLevelToAttic();
        }
    }

    /** Creates a new level at the {@link #statementAttic} */
    private void pushNewStatementLevelToAttic() {
        this.statementAttic.push(new LinkedList<org.codecover.model.mast.Statement>());
    }

    /**
     * Calls <code>this.hierarchyAttic.pop()</code> and
     * <code>this.hierarchyLevelTypeAttic.pop()</code>. If the type is a
     * class {@link #createStatementSequenceListFromAttic()} is called too. Then a
     * new {@link HierarchyLevel} is add at the top of the
     * {@link #hierarchyAttic}.
     */
    private void createHierarchyLevelFromAtticAndPushIt(String name,
            LocationList locationList,
            LocationList locationListHeader) {
        List<HierarchyLevel> levels = this.hierarchyAttic.pop();
        HierarchyLevelType type = this.hierarchyLevelTypeAttic.pop();
        List<StatementSequence> statementSequence = createStatementSequenceListFromAttic();

        HierarchyLevel newHierarchyLevel = this.builder
                .createHierarchyLevel(locationList,
                                      name,
                                      locationListHeader,
                                      type,
                                      levels,
                                      statementSequence);
        this.hierarchyAttic.bottom().add(newHierarchyLevel);
    }

    /**
     * Calls <code>this.statementAttic.pop()</code>, creates a
     * {@link StatementSequence}.
     */
    private StatementSequence createStatementSequenceFromAttic() {
        List<org.codecover.model.mast.Statement> statementList = this.statementAttic.pop();
        List<Location> locationsOfSequence = new Vector<Location>(statementList.size());
        for (org.codecover.model.mast.Statement thisStatement : statementList) {
            locationsOfSequence.addAll(thisStatement.getLocation().getLocations());
        }

        return this.builder.createStatementSequence(this.builder.createLocationList(locationsOfSequence), 
                statementList);
    }

    /**
     * Calls {@link #createStatementSequenceFromAttic()} and puts it in a
     * List.
     */
    private List<StatementSequence> createStatementSequenceListFromAttic() {
        StatementSequence statementSequence = createStatementSequenceFromAttic();

        if (statementSequence.getStatements().isEmpty()) {
            return Collections.<StatementSequence>emptyList();
        }

        return Collections.<StatementSequence>singletonList(statementSequence);
    }

    /**
     * Calls {@link #createStatementSequenceListFromAttic()}, creates an
     * explicit {@link Branch}.
     */
    private Branch createExplicitBranchFromAttic(String branchID,
            LocationList locationList,
            LocationList locationListDecision) {
        StatementSequence statementSequence = createStatementSequenceFromAttic();

        return this.builder.createBranch(locationList,
                createCoverableItem(branchID),
                false,
                locationListDecision,
                statementSequence);
    }

    /**
     * Calls creates an implicit {@link Branch}.
     */
    private Branch createImplicitBranch(String branchID) {
        LocationList locationList = this.builder.createEmptyLocationList();
        LocationList locationListDecision = this.builder.createEmptyLocationList();
        StatementSequence statementSequence = this.builder.createStatementSequence(
                this.builder.createEmptyLocationList(),
                Collections.<org.codecover.model.mast.Statement> emptyList());

        return this.builder.createBranch(locationList,
                createCoverableItem(branchID),
                true,
                locationListDecision,
                statementSequence);
    }

    private void createConditionalStatementAndPushIt(String statementID,
            LocationList locationList,
            RootTerm rootTerm,
            List<Branch> branchList,
            Location locationKeyword) {
        Set<RootTerm> setRootTerms;

        if (rootTerm == null) {
            setRootTerms = Collections.<RootTerm> emptySet();
        } else {
            setRootTerms = Collections.<RootTerm> singleton(rootTerm);
        }
        
        ConditionalStatement conditionalStatement = this.builder.createConditionalStatement(
                locationList,
                createCoverableItem(statementID),
                setRootTerms,
                branchList,
                locationKeyword, this.questionMarkOperators);
        this.statementAttic.bottom().add(conditionalStatement);
    }

    private void createLoopingStatementAntPushIt(String statementID,
            LocationList locationList,
            RootTerm rootTerm,
            Location locationKeyword,
            String loopIDZero,
            String loopIDOnce,
            String loopIDAbove,
            boolean optionalBodyExecution) {
        StatementSequence statementSequence = createStatementSequenceFromAttic();
        Set<RootTerm> setRootTerms;

        if (rootTerm == null) {
            setRootTerms = Collections.<RootTerm> emptySet();
        } else {
            setRootTerms = Collections.<RootTerm> singleton(rootTerm);
        }

        LoopingStatement loopingStatement = this.builder.createLoopingStatement(
                locationList,
                createCoverableItem(statementID),
                setRootTerms,
                statementSequence,
                locationKeyword,
                createCoverableItem(loopIDZero),
                createCoverableItem(loopIDOnce),
                createCoverableItem(loopIDAbove),
                optionalBodyExecution, this.questionMarkOperators);
        this.statementAttic.bottom().add(loopingStatement);
    }

    /**
     * Creates a {@link CoverableItem} using an ID an the
     * {@link #fullSourceFileName}.
     */
    private CoverableItem createCoverableItem(String id) {
        return this.builder.createCoverableItem(this.fullSourceFileName, id);
    }

    /**
     * Visits the statement to print it and get its start and end position. Then
     * a MAST {@link BasicStatement} is created and added to
     * {@link #statementAttic}.
     * 
     * @param statement
     *            The {@link Statement} which will be accepted. The
     *            {@link Statement#statementID} has to be set before.
     */
    private void acceptAndAtticBasicStatement(Statement statement) throws IOException {
        // we have to use the super, because the InstrumentationVisitor
        // has already visited the Statement
        if (statement.statementID == null) {
            this.builder.getLogger().fatal("statementID == null");
        }

        startLocateableLevel();
        super.visit(statement);
        LocationList locationList = endLocateableLevel();

        org.codecover.model.mast.Statement newStatement = this.builder
                .createBasicStatement(locationList,
                        createCoverableItem(statement.statementID),
                        Collections.<RootTerm> emptySet(), this.questionMarkOperators);
        this.statementAttic.bottom().add(newStatement);
    }
    
    private void pushModifiersToAttic(Modifiers modifiers) {
        this.modifiersAttic.push(modifiers);
    }

    private void popModifiersFromAttic() {
        this.modifiersAttic.pop();
    }

    /**
     * If the {@link Modifiers} of {@link #modifiersAttic} is not empty, its
     * top level Modifier is got. If its {@link Modifiers#startOffset} is
     * greater than -1, this offset is returned. Otherwise offsetWithoutModifiers
     * is returned.<br>
     * By the way, the start offset of a {@link HierarchyLevel} can be extended
     * to include the {@link Modifiers}' start offset.  
     * 
     * @param offsetWithoutModifiers
     *            The start offset without considering the modifiers.
     * 
     * @return The corrected offset of the Node.
     */
    private int recalculateStartOffsetUsingModifiers(int offsetWithoutModifiers) {
        if (this.modifiersAttic.isEmpty()) {
            String message = "this.modifiersAttic.isEmpty()";
            Exception exception = new IllegalStateException(message);
            this.builder.getLogger().fatal(message, exception);
        }

        Modifiers modifiers = this.modifiersAttic.bottom();

        if (modifiers.startOffset > -1) {
            return modifiers.startOffset;
        }
        return offsetWithoutModifiers;
    }

    /////////////////////////////////////////////////////////////////
    //
    // Some setters and getters
    //
    /////////////////////////////////////////////////////////////////

    /**
     * 
     * @param warningMessage
     */
    private void handleConditionManipulatorMessage(String warningMessage,
                                                   int lineOfSourceFile) {
        if (warningMessage == null) {
            return;
        }

        String message = String.format("Conditional instrumentation at " +
                                       "%s, line %,d:\n%s",
                this.fullSourceFileName,
                new Integer(lineOfSourceFile),
                warningMessage);
        this.builder.getLogger().warning(message);
    }

    /**
     * If the {@link StatementManipulator} needs block statements, we get it
     * here. See {@link Manipulator#requiresBlockExpansionsForBranches()} and
     * {@link Manipulator#requiresBlockExpansionsForLoops()}.
     * 
     * @param statementManipulator
     *            The statementManipulator to set.
     * 
     * @see #statementManipulator
     */
    public void setStatementManipulator(
            StatementManipulator statementManipulator) {
        this.statementManipulator = statementManipulator;
        this.statementManipulator.setTreeDumper(this);

        recalcExpandToBlock();
    }

    /**
     * If the {@link BranchManipulator} needs block statements, we get it
     * here. See {@link Manipulator#requiresBlockExpansionsForBranches()} and
     * {@link Manipulator#requiresBlockExpansionsForLoops()}.
     * 
     * @param branchManipulator
     *            The branchManipulator to set.
     * 
     * @see #branchManipulator
     */
    public void setBranchManipulator(
            BranchManipulator branchManipulator) {
        this.branchManipulator = branchManipulator;
        this.branchManipulator.setTreeDumper(this);

        recalcExpandToBlock();
    }

    /**
     * If the {@link ConditionManipulator} needs block statements, we get it
     * here. See {@link Manipulator#requiresBlockExpansionsForBranches()} and
     * {@link Manipulator#requiresBlockExpansionsForLoops()}.
     * 
     * @param conditionManipulator
     *            The conditionManipulator to set.
     * 
     * @see #conditionManipulator
     */
    public void setConditionManipulator(
            ConditionManipulator conditionManipulator) {
        this.conditionManipulator = conditionManipulator;
        this.conditionManipulator.setTreeDumper(this);

        recalcExpandToBlock();
    }

    /**
     * If the {@link LoopManipulator} needs block statements, we get it here.
     * See {@link Manipulator#requiresBlockExpansionsForBranches()} and
     * {@link Manipulator#requiresBlockExpansionsForLoops()}.
     * 
     * @param loopManipulator
     *            The loopManipulator to set.
     *            
     * @see #loopManipulator
     */
    public void setLoopManipulator(LoopManipulator loopManipulator) {
        this.loopManipulator = loopManipulator;
        this.loopManipulator.setTreeDumper(this);

        recalcExpandToBlock();
    }

    /**
     * The {@link CommentManipulator}, which parses "// startTestCase("Name");
     * in comments and  
     * 
     * @param commentManipulator
     *            The commentManipulator to set.
     *            
     * @see #commentManipulator 
     */
    public void setCommentManipulator(CommentManipulator commentManipulator) {
        this.commentManipulator = commentManipulator;
        this.commentManipulator.setTreeDumper(this);

        recalcExpandToBlock();
    }
    
    /**
     * 
     * @param syncStatementManipulator
     */
    public void setSyncStatementManipulator(SynchronizedManipulator syncStatementManipulator) {
        this.syncStatementManipulator = syncStatementManipulator;
        this.syncStatementManipulator.setTreeDumper(this);
        
        recalcExpandToBlock();
    }

    /**
     * 
     * @param syncStatementManipulator
     */
    public void setQMOManipulator(QMOManipulator qmoManipulator) {
        this.qmoManipulator = qmoManipulator;
        this.qmoManipulator.setTreeDumper(this);
        
        recalcExpandToBlock();
    }
    
    
    ///////////////////////////////////////////////////////////////////////////
    //
    // All visit statements for instrumentation purpose
    //
    ///////////////////////////////////////////////////////////////////////////

    /**
     * We instrument a {@link FieldDeclaration} if it has assignments.<br>
     * <br>
     * It depents on the type (static / unstatic) of the fiedl and the current
     * type in the hierarchy how it is instrumented.<br>
     * Grammar production of FieldDeclaration:
     * 
     * <PRE>
     * 
     * f0 -> Type()
     * f1 -> VariableDeclarator()
     * f2 -> ( "," VariableDeclarator() )*
     * f3 -> ";"
     * 
     * </PRE>
     * 
     * @return true &rarr; es wurde instrumentiert; false &rarr; es wurde nicht instrumentiert
     */
    private boolean manipulateFieldDeclaration(Modifiers modifiers,
            FieldDeclaration fieldDeclaration) throws IOException {
        // we have to get to know, whether there are expressions and
        // assignments used here.
        VariableDeclarator variableDeclarator = fieldDeclaration.f1;

        // search for assignments in f1
        boolean foundExpression = variableDeclarator.f1.present();

        // search for assignments in f2
        if (!foundExpression && fieldDeclaration.f2.present()) {
            Vector<Node> declarations = fieldDeclaration.f2.nodes;
            for (Node thisNode : declarations) {
                NodeSequence nodeSequence = (NodeSequence) thisNode;
                variableDeclarator = (VariableDeclarator) nodeSequence.nodes.get(1);
                foundExpression |= variableDeclarator.f1.present();
            }
        }

        // there are assignments -> instrument
        if (foundExpression && (inClass() || inEnum())) {
            // we have to get to know, whether there is a static
            // modifier
            String modifierString = TreeParsedImageDumper.convertToString(modifiers);
            boolean isStaticPresent = modifierString.contains("static");

            // we have to get to know, whether we can add an initializer after
            // the FieldDeclaration; it would like this
            // {counter++} or static {counter++}
            // only classes and enums allow initializers, but only in the top
            // level or if it is not static

            if (!inClass() && !inEnum()) {
                return false;
            }

            // if it is a type, that is not top level and the field is static
            if (this.hierarchyLevelTypeAttic.size() > 1 && isStaticPresent) {
                return false;
            }

            // now we are sure, that we can instrument it
            fieldDeclaration.statementID = this.counterIDManager.nextStatementID();
            this.statementManipulator.manipulate(fieldDeclaration,
                    isStaticPresent, fieldDeclaration.statementID);
            return true;
        }

        return false;
    }

    /**
     * <PRE>
     * 
     * f0 -> [ PackageDeclaration() ]
     * f1 -> ( ImportDeclaration() )*
     * f2 -> (TypeDeclaration() )*
     * f3 -> ( &lt;"\u001a"&gt; )?
     * f4 -> (&lt;STUFF_TO_IGNORE: ~[]&gt; )?
     * f5 -> &lt;EOF&gt;
     * 
     * </PRE>
     * @throws IOException 
     */
    @Override
    public void visit(CompilationUnit n) throws IOException {
        // [ PackageDeclaration() ]
        n.f0.accept(this);
        // this.packageName is set iff not in default package
        // -> set it to "" else
        if (this.packageName == null) {
            this.packageName = "";
        }

        // a top level type declaration is a class, whose 
        // declaration is directly in the file under CompilationUnit
        // or has only no class hierarchy levels above
        if (this.packageName.equals("")) {
            this.fullSourceFileName = this.sourceFile.getFileName();
        } else {
            this.fullSourceFileName = this.packageName + "." + 
            this.sourceFile.getFileName();
        }

        // create a new CounterIDManager and hand over CounterManager
        this.counterIDManager = new CounterIDManager(
                this.sourceFile.getFileName(),
                this.fullSourceFileName,
                super.getTargetWriter(),
                this.coverageResultLog,
                this.testSessionContainerUID);

        this.counterIDManager.addCounterManager(this.statementManipulator);
        this.counterIDManager.addCounterManager(this.branchManipulator);
        this.counterIDManager.addCounterManager(this.conditionManipulator);
        this.counterIDManager.addCounterManager(this.loopManipulator);
        this.counterIDManager.addCounterManager(this.syncStatementManipulator);
        this.counterIDManager.addCounterManager(this.qmoManipulator);

        // ( ImportDeclaration() )*
        n.f1.accept(this);

        // this is the hierarchy level for the sourceFile
        pushNewHieraryLevelToAttic(null);
        // (TypeDeclaration() )*
        n.f2.accept(this);

        // hand the hierarchy levels over to this.hierarchyLevelContainer
        popTopLevelHieraryLevelsFromAttic();

        // Write the inner class
        super.getTargetWriter().write(LINE_SEPARATOR);
        this.counterIDManager.writeInnerClass(this.preferedCoverageLogPath);

        // accept this at the end - Bug 234
        // ( &lt;"\u001a"&gt; )?
        n.f3.accept(this);
        // (&lt;STUFF_TO_IGNORE: ~[]&gt; )?
        n.f4.accept(this);
        // &lt;EOF&gt;
        n.f5.accept(this);
    }

    /**
     * gets the package name
     * 
     * <PRE>
     * 
     * f0 -> Modifiers()
     * f1 -> "package"
     * f2 -> Name()
     * f3 -> ";"
     * 
     * </PRE>
     */
    @Override
    public void visit(PackageDeclaration n) throws IOException {
        this.packageName = TreeParsedImageDumper.convertToString(n.f2);
        super.visit(n);
    }

    /**
     * Get the modifiers and {@link #pushModifiersToAttic(Modifiers)}.<br>
     * <PRE>
     * 
     * f0 -> ";" |
     *       Modifiers() ( ClassOrInterfaceDeclaration() |
     *                     EnumDeclaration() |
     *                     AnnotationTypeDeclaration() )
     * 
     * </PRE>
     */
    @Override
    public void visit(TypeDeclaration n) throws IOException {
        if (n.f0.which == 0) {
            super.visit(n);
        } else {
            NodeSequence sequence = (NodeSequence) n.f0.choice;
            Modifiers modifiers = (Modifiers) sequence.nodes.get(0);
            modifiers.accept(this);
            modifiers.startOffset = StartOffset.getStartOffset(modifiers);
            pushModifiersToAttic(modifiers);

            // ( ClassOrInterfaceDeclaration ...)
            sequence.nodes.get(1).accept(this);
            popModifiersFromAttic();
        }
    }

    /**
     * Creates a {@link HierarchyLevel}.<br>
     * Setting of {@link #counterIDManager} and {@link #fullSourceFileName}
     * if creating a top level type declaration which is a class.
     * 
     * <PRE>
     * 
     * f0 -> ( "class" | "interface" )
     * f1 -> &lt;IDENTIFIER&gt;
     * f2 -> [ TypeParameters() ]
     * f3 -> [ ExtendsList(isInterface) ]
     * f4 -> [ ImplementsList(isInterface) ]
     * f5 -> ClassOrInterfaceBody(isInterface)
     * 
     * </PRE>
     */
    @Override
    public void visit(ClassOrInterfaceDeclaration n) throws IOException {
        HierarchyLevelType hierarchyLevelType;
        if (n.f0.which == 0) {
            hierarchyLevelType = this.hierarchyTypeProvider.getClassType();
        } else {
            hierarchyLevelType = this.hierarchyTypeProvider.getInterfaceType();
        }

        int startOffset = ((NodeToken)n.f0.choice).startOffset;
        // if there is an modifier, we move the begin to the modifiers begin
        startOffset = recalculateStartOffsetUsingModifiers(startOffset);
        startExplicitLocateableLevel(startOffset);
        startLocateableLevel(startOffset);

        // a new Level for this class / interface
        pushNewHieraryLevelToAttic(hierarchyLevelType);

        n.f0.accept(this);
        n.f1.accept(this);
        n.f2.accept(this);
        n.f3.accept(this);
        n.f4.accept(this);
        LocationList locationListHeader = endLocateableLevel();
        n.f5.accept(this);

        LocationList locationList = endLocateableLevel();
        createHierarchyLevelFromAtticAndPushIt(n.f1.getParsedImage(),
                locationList,
                locationListHeader);
    }

    /**
     * <PRE>
     * 
     * f0 -> "new" PrimitiveType() ArrayDimsAndInits() |
     *       "new" ClassOrInterfaceType() [ TypeArguments() ]
     *          ( ArrayDimsAndInits() |
     *            Arguments() [ ClassOrInterfaceBody(false) ] )
     * 
     * </PRE>
     */
    @Override
    public void visit(AllocationExpression n) throws IOException {
        // we try to get, whether ClassOrInterfaceBody is present
        if (n.f0.which == 1) {
            NodeSequence secondSequence = (NodeSequence) n.f0.choice;
            NodeChoice innerChoice = (NodeChoice) secondSequence.elementAt(3);
            if (innerChoice.which == 1) {
                NodeSequence innerSequence = (NodeSequence) innerChoice.choice;
                NodeOptional classOrInterfaceOptional = (NodeOptional) innerSequence.elementAt(1);
                if (classOrInterfaceOptional.present()) {
                    // a new Level for this class
                    pushNewHieraryLevelToAttic(this.hierarchyTypeProvider.getAnonymousClassType());

                    startExplicitLocateableLevel();
                    // new
                    secondSequence.elementAt(0).accept(this);
                    startLocateableLevel();
                    // ClassOrInterfaceType()
                    secondSequence.elementAt(1).accept(this);
                    // [ TypeArguments() ]
                    secondSequence.elementAt(2).accept(this);
                    // Arguments()
                    innerSequence.elementAt(0).accept(this);

                    LocationList locationListHeader = endLocateableLevel();

                    // ClassOrInterfaceBody
                    innerSequence.elementAt(1).accept(this);

                    LocationList locationList = endLocateableLevel();
                    String className = TreeParsedImageDumper.convertToString(secondSequence.elementAt(1));
                    createHierarchyLevelFromAtticAndPushIt(className,
                            locationList,
                            locationListHeader);
                } else {
                    n.f0.accept(this);
                }
            } else {
                n.f0.accept(this);
            }
        } else {
            n.f0.accept(this);
        }
    }

    /**
     * <PRE>
     * 
     * f0 -> Modifiers()
     * f1 -> &lt;IDENTIFIER&gt;
     * f2 -> [ Arguments() ]
     * f3 -> [ ClassOrInterfaceBody() ]
     * 
     * </PRE>
     */
    @Override
    public void visit(EnumConstant n) throws IOException {
        if (n.f3.present()) {
            // a new Level for this class
            pushNewHieraryLevelToAttic(this.hierarchyTypeProvider.getEnumConstantType());

            startLocateableLevel();
            startLocateableLevel();

            n.f0.accept(this);
            n.f1.accept(this);
            n.f2.accept(this);

            LocationList locationListHeader = endLocateableLevel();

            n.f3.accept(this);

            LocationList locationList = endLocateableLevel();
            createHierarchyLevelFromAtticAndPushIt(n.f1.getParsedImage(),
                    locationList,
                    locationListHeader);
        } else {
            n.f0.accept(this);
            n.f1.accept(this);
            n.f2.accept(this);
            n.f3.accept(this);
        }
    }

    /**
     * <PRE>
     * 
     * f0 -> "{"
     * f1 -> ( ClassOrInterfaceBodyDeclaration(isInterface) )*
     * f2 -> "}"
     * 
     * </PRE>
     */
    @Override
    public void visit(ClassOrInterfaceBody n) throws IOException {
        n.f0.accept(this);
        // write the static block, if in a top level class or enumeration
        if ((inClass() || inEnum()) && (this.hierarchyLevelTypeAttic.size() == 1)) {
            this.counterIDManager.writeStaticBlock();
        }
        n.f1.accept(this);
        n.f2.accept(this);
    }

    /**
     * Get the modifiers and {@link #pushModifiersToAttic(Modifiers)}.<br>
     * Some fields for {@link FieldDeclaration} are 
     * 
     * <PRE>
     * 
     * f0 -> Initializer() |
     *       Modifiers() ( ClassOrInterfaceDeclaration() |
     *                     EnumDeclaration() |
     *                     AnnotationTypeDeclaration |
     *                     ConstructorDeclaration() |
     *                     FieldDeclaration() |
     *                     MethodDeclaration()
     *                   ) |
     *      ";"
     * 
     * </PRE>
     */
    @Override
    public void visit(ClassOrInterfaceBodyDeclaration n) throws IOException {
        if (n.f0.which == 1) {
            NodeSequence unknownDeclarationSequence = (NodeSequence) n.f0.choice;
            Modifiers modifiers = (Modifiers) unknownDeclarationSequence.nodes.get(0);
            NodeChoice unknownDeclarationChoice = (NodeChoice) unknownDeclarationSequence.nodes.get(1);

            // we are in Modifiers() (.. | ..)
            if (unknownDeclarationChoice.which == 4) {
                // we have a FieldDeclaration
                FieldDeclaration fieldDeclaration = (FieldDeclaration) unknownDeclarationChoice.choice;
                boolean isInstrumented = manipulateFieldDeclaration(modifiers, fieldDeclaration);

                modifiers.accept(this);
                modifiers.startOffset = StartOffset.getStartOffset(modifiers);
                pushModifiersToAttic(modifiers);

                int startOffset = StartOffset.getStartOffset(unknownDeclarationChoice);
                startOffset = recalculateStartOffsetUsingModifiers(startOffset);

                startLocateableLevel(startOffset);
                unknownDeclarationChoice.accept(this);
                LocationList locationList = endLocateableLevel();
                popModifiersFromAttic();

                // create a MAST Statement out of this
                if (isInstrumented) {
                    org.codecover.model.mast.Statement newStatement = this.builder
                            .createBasicStatement(locationList,
                                    createCoverableItem(fieldDeclaration.statementID),
                                    Collections.<RootTerm> emptySet(), this.questionMarkOperators);
                    this.statementAttic.bottom().add(newStatement);
                }
            } else {
                modifiers.accept(this);
                modifiers.startOffset = StartOffset.getStartOffset(modifiers);
                pushModifiersToAttic(modifiers);
                unknownDeclarationChoice.accept(this);
                popModifiersFromAttic();
            }
        } else {
            // n.f0.which != 1
            super.visit(n);
        } 
    }

    /**
     * Create a {@link HierarchyLevel}.
     * 
     * <PRE>
     * 
     * f0 -> "enum"
     * f1 -> &lt;IDENTIFIER&gt;
     * f2 -> [ ImplementsList(false) ]
     * f3 -> EnumBody()
     * 
     * </PRE>
     */
    @Override
    public void visit(EnumDeclaration n) throws IOException {
        int startOffset = n.f0.startOffset;
        pushNewHieraryLevelToAttic(this.hierarchyTypeProvider.getEnumType());

        // if there is an modifier, we move the begin to the modifiers begin
        startOffset = recalculateStartOffsetUsingModifiers(startOffset);
        startExplicitLocateableLevel(startOffset);
        startLocateableLevel(startOffset);

        n.f0.accept(this);
        n.f1.accept(this);
        n.f2.accept(this);
        // header finished
        LocationList locationListHeader = endLocateableLevel();
        n.f3.accept(this);
        // body finished

        LocationList locationList = endLocateableLevel();

        createHierarchyLevelFromAtticAndPushIt(n.f1.getParsedImage(),
                locationList,
                locationListHeader);
    }

    /**
     * Creating a {@link HierarchyLevel}.
     * 
     * <PRE>
     * 
     * f0 -> "@"
     * f1 -> "interface"
     * f2 -> &lt;IDENTIFIER&gt;
     * f3 -> AnnotationTypeBody()
     * 
     * </PRE>
     */
    @Override
    public void visit(AnnotationTypeDeclaration n) throws IOException {
        int startOffset = n.f0.startOffset;
        // if there is an modifier, we move the begin to the modifiers begin
        startOffset = recalculateStartOffsetUsingModifiers(startOffset);
        startExplicitLocateableLevel(startOffset);
        startLocateableLevel(startOffset);

        pushNewHieraryLevelToAttic(this.hierarchyTypeProvider.getAnnotationType());

        n.f0.accept(this);
        n.f1.accept(this);
        n.f2.accept(this);
        // header finished
        LocationList locationListHeader = endLocateableLevel();
        n.f3.accept(this);
        // body finished

        LocationList locationList = endLocateableLevel();;
        createHierarchyLevelFromAtticAndPushIt(n.f2.getParsedImage(),
                locationList,
                locationListHeader);
    }

    /**
     * Get the modifiers and {@link #pushModifiersToAttic(Modifiers)}.<br>
     * We do not instrument this FieldDeclaration, because we cannot add code within the
     * AnnotationTypeBody.
     * 
     * <PRE>
     * 
     * f0 -> Modifiers() ( Type() &lt;IDENTIFIER&gt; "(" ")" [ DefaultValue() ] ";" |
     *                     ClassOrInterfaceDeclaration() |
     *                     EnumDeclaration() |
     *                     AnnotationTypeDeclaration() |
     *                     FieldDeclaration() ) |
     *       ( ";" )
     * 
     * </PRE>
     */
    @Override
    public void visit(AnnotationTypeMemberDeclaration n) throws IOException {
        if (n.f0.which == 0) {
            NodeSequence unknownDeclarationSequence = (NodeSequence) n.f0.choice;
            Modifiers modifiers = (Modifiers) unknownDeclarationSequence.nodes.get(0);
            NodeChoice unknownDeclarationChoice = (NodeChoice) unknownDeclarationSequence.nodes.get(1);

            modifiers.accept(this);
            modifiers.startOffset = StartOffset.getStartOffset(modifiers);
            pushModifiersToAttic(modifiers);

            // ( Type() ... | ... )
            unknownDeclarationChoice.accept(this);

            popModifiersFromAttic();
        } else {
            super.visit(n);
        }
    }

    /**
     * Create a new {@link HierarchyLevel}.
     * <PRE>
     * 
     * f0 -> [ TypeParameters() ]
     * f1 -> &lt;IDENTIFIER&gt;
     * f2 -> FormalParameters()
     * f3 -> [ "throws" NameList() ]
     * f4 -> "{"
     * f5 -> [ ExplicitConstructorInvocation() ]
     * f6 -> ( BlockStatement() )
     * f7 -> "}"
     * 
     * </PRE>
     */
    @Override
    public void visit(ConstructorDeclaration n) throws IOException {
        pushNewHieraryLevelToAttic(this.hierarchyTypeProvider.getMethodType());
        
        int startOffset = StartOffset.getStartOffset(n);
        // if there is an modifier, we move the begin to the modifiers begin
        startOffset = recalculateStartOffsetUsingModifiers(startOffset);
        startLocateableLevel(startOffset);
        startLocateableLevel(startOffset);

        n.f0.accept(this);
        n.f1.accept(this);
        n.f2.accept(this);
        n.f3.accept(this);

        // header finished
        LocationList locationListHeader = endLocateableLevel();

        n.f4.accept(this);

        if (n.f5.present()) {
            // this(..) or super(..) is a statement too
            ExplicitConstructorInvocation constrDecl = (ExplicitConstructorInvocation) n.f5.node;

            constrDecl.statementID = this.counterIDManager.nextStatementID();

            // visit the statement first
            // put the statement to the statementAttic
            startLocateableLevel();
            constrDecl.accept(this);
            LocationList locationList = endLocateableLevel();

            org.codecover.model.mast.Statement newStatement = this.builder
                    .createBasicStatement(locationList,
                            createCoverableItem(constrDecl.statementID),
                            Collections.<RootTerm> emptySet(), this.questionMarkOperators);
            this.statementAttic.bottom().add(newStatement);
            // manipulate after
            this.statementManipulator.manipulate(constrDecl, constrDecl.statementID);
        }

        n.f6.accept(this);
        n.f7.accept(this);
        // body finished
        LocationList locationList = endLocateableLevel();

        createHierarchyLevelFromAtticAndPushIt(n.f1.getParsedImage(),
                locationList,
                locationListHeader);
    }

    /**
     * Create a new {@link HierarchyLevel}.
     * <PRE>
     * 
     * f0 -> [ TypeParameters() ]
     * f1 -> ResultType()
     * f2 -> MethodDeclarator()
     * f3 -> [ "throws" NameList() ]
     * f4 -> ( Block() | ";" )
     * 
     * </PRE>
     */
    @Override
    public void visit(MethodDeclaration n) throws IOException {
        n.f0.accept(this);
        n.f1.accept(this);
        n.f2.accept(this);
        n.f3.accept(this);

        pushNewHieraryLevelToAttic(this.hierarchyTypeProvider.getMethodType());

        int startOffset = StartOffset.getStartOffset(n);
        // if there is an modifier, we move the begin to the modifiers begin
        startOffset = recalculateStartOffsetUsingModifiers(startOffset);
        startLocateableLevel(startOffset);
        startLocateableLevel(startOffset);
        LocationList locationListHeader = endLocateableLevel();

        n.f4.accept(this);
        // body finished

        LocationList locationList = endLocateableLevel();
        createHierarchyLevelFromAtticAndPushIt(n.f2.f0.getParsedImage(),
                locationList,
                locationListHeader);
    }

    /**
     * Creating a {@link org.codecover.model.mast.Statement}.<br>
     * Instrumentation of basic statements using
     * {@link #statementManipulator}.<br>
     * <br>
     * These basic statements are:
     * <ul>
     * <li>{@link EmptyStatement}</li>
     * <li>{@link StatementExpression}</li>
     * <li>{@link BreakStatement}</li>
     * <li>{@link ContinueStatement}</li>
     * <li>{@link ReturnStatement} without return field</li>
     * </ul>
     * <br>
     * Moreover for other statements at least a statementID is generated:
     * <ul>
     * <li>{@link SwitchStatement}</li>
     * <li>{@link IfStatement}</li>
     * <li>{@link WhileStatement}</li>
     * <li>{@link DoStatement}</li>
     * <li>{@link ForStatement}</li>
     * <li>{@link TryStatement}</li>
     * </ul>
     * 
     * <PRE>
     * f0 -> LabeledStatement() |
     *       AssertStatement() |
     *       Block() |
     *       EmptyStatement() |
     *       StatementExpression() ";" |
     *       SwitchStatement() |
     *       IfStatement() | 
     *       WhileStatement() | 
     *       DoStatement() | 
     *       ForStatement() | 
     *       BreakStatement() |
     *       ContinueStatement() |
     *       ReturnStatement() |
     *       ThrowStatement() |
     *       SynchronizedStatement() |
     *       TryStatement()
     * </PRE>
     */
    @Override
    public void visit(Statement n) throws IOException {
        // we have to have a look, which statements are instrumented,
        // which get a statementID and which are just dumped

        StatementSwitch: switch (n.f0.which) {
            case 0 : { // LabeledStatement
                LabeledStatement labeledStatement = (LabeledStatement) n.f0.choice;

                // we copy the collected labels in the labelToken-Vector of the labeled statement
                labeledStatement.f2.labelToken = new NodeSequence(n.labelToken.size() + 2);
                labeledStatement.f2.labelToken.nodes.addAll(n.labelToken.nodes);
                labeledStatement.f2.labelToken.nodes.add(labeledStatement.f0);
                labeledStatement.f2.labelToken.nodes.add(labeledStatement.f1);

                labeledStatement.f2.accept(this);

                break StatementSwitch;
            }
            case 3: { // EmptyStatement
                n.statementID = this.counterIDManager.nextStatementID();
                EmptyStatement emptyStatement = (EmptyStatement) n.f0.choice;
                this.statementManipulator.manipulate(emptyStatement, n.statementID);
                // write the collected labelToken first
                n.labelToken.accept(this);
                acceptAndAtticBasicStatement(n);
                break StatementSwitch;
            }
            case 4: { // StatementExpression
                n.statementID = this.counterIDManager.nextStatementID();
                NodeSequence nodeList = (NodeSequence) n.f0.choice;
                StatementExpression statementExpression = (StatementExpression) nodeList.nodes.get(0);
                this.statementManipulator.manipulate(statementExpression,
                        n.statementID);
                // write the collected labelToken first
                n.labelToken.accept(this);
                acceptAndAtticBasicStatement(n);
                break StatementSwitch;
            }
            case 5: { // SwitchStatement
                n.statementID = this.counterIDManager.nextStatementID();
                SwitchStatement switchStatement = (SwitchStatement) n.f0.choice;
                switchStatement.statementID = n.statementID;
                this.statementManipulator.manipulate(switchStatement, n.statementID);
                // the switchStatement needs the labels, because instrumentations have to be done in front of it
                switchStatement.labelToken = n.labelToken;
                switchStatement.accept(this);
                break StatementSwitch;
            }
            case 6: { // IfStatement
                n.statementID = this.counterIDManager.nextStatementID();
                IfStatement ifStatement = (IfStatement) n.f0.choice;
                ifStatement.statementID = n.statementID;
                this.statementManipulator.manipulate(ifStatement, n.statementID);
                // the ifStatement needs the labels, because instrumentations have to be done in front of it
                ifStatement.labelToken = n.labelToken;
                ifStatement.accept(this);
                break StatementSwitch;
            }
            case 7: { // WhileStatement
                n.statementID = this.counterIDManager.nextStatementID();
                WhileStatement whileStatement = (WhileStatement) n.f0.choice;
                whileStatement.statementID = n.statementID;
                this.statementManipulator.manipulate(whileStatement, n.statementID);
                // the whileStatement needs the labels, because instrumentations have to be done in front of it
                whileStatement.labelToken = n.labelToken;
                whileStatement.accept(this);
                break StatementSwitch;
            }
            case 8: { // DoStatement
                n.statementID = this.counterIDManager.nextStatementID();
                DoStatement doStatement = (DoStatement) n.f0.choice;
                doStatement.statementID = n.statementID;
                this.statementManipulator.manipulate(doStatement, n.statementID);
                // the doStatement needs the labels, because instrumentations have to be done in front of it
                doStatement.labelToken = n.labelToken;
                doStatement.accept(this);
                break StatementSwitch;
            }
            case 9: { // ForStatement
                n.statementID = this.counterIDManager.nextStatementID();
                ForStatement forStatement = (ForStatement) n.f0.choice;
                forStatement.statementID = n.statementID;
                this.statementManipulator.manipulate(forStatement, n.statementID);
                // the forStatement needs the labels, because instrumentations have to be done in front of it
                forStatement.labelToken = n.labelToken;
                forStatement.accept(this);
                break StatementSwitch;
            }
            case 10: { // BreakStatement
                n.statementID = this.counterIDManager.nextStatementID();
                BreakStatement breakStatement = (BreakStatement) n.f0.choice;
                this.statementManipulator.manipulate(breakStatement, n.statementID);
                // write the collected labelToken first
                n.labelToken.accept(this);
                acceptAndAtticBasicStatement(n);
                break StatementSwitch;
            }
            case 11: { // ContinueStatement
                n.statementID = this.counterIDManager.nextStatementID();
                ContinueStatement continueStatement = (ContinueStatement) n.f0.choice;
                this.statementManipulator.manipulate(continueStatement, n.statementID);
                // write the collected labelToken first
                n.labelToken.accept(this);
                acceptAndAtticBasicStatement(n);
                break StatementSwitch;
            }
            case 12: { // ReturnStatement
                n.statementID = this.counterIDManager.nextStatementID();
                ReturnStatement returnStatement = (ReturnStatement) n.f0.choice;
                this.statementManipulator.manipulate(returnStatement, n.statementID);
                // write the collected labelToken first
                n.labelToken.accept(this);
                acceptAndAtticBasicStatement(n);
                break StatementSwitch;
            }
            case 13: { // ThrowStatement
                n.statementID = this.counterIDManager.nextStatementID();
                ThrowStatement throwStatement = (ThrowStatement) n.f0.choice;
                this.statementManipulator.manipulate(throwStatement, n.statementID);
                // write the collected labelToken first
                n.labelToken.accept(this);
                acceptAndAtticBasicStatement(n);
                break StatementSwitch;
            }
            case 15: { // TryStatement
                n.statementID = this.counterIDManager.nextStatementID();
                TryStatement tryStatement = (TryStatement) n.f0.choice;
                tryStatement.statementID = n.statementID;
                this.statementManipulator.manipulate(tryStatement, n.statementID);
                // the tryStatement needs the labels, because instrumentations have to be done in front of it
                tryStatement.labelToken = n.labelToken;
                tryStatement.accept(this);
                break StatementSwitch;
            }
            default: {
                // AssertStatement() | Block() | SynchronizedStatement()
                // write the collected labelToken first
                n.labelToken.accept(this);
                // the InstrumentationVisitor has visited n -> use super
                super.visit(n);
                break StatementSwitch;
            }
        }
    }

    /**
     * Get the modifiers and {@link #pushModifiersToAttic(Modifiers)}.<br>
     * Instrumentation of {@link LocalVariableDeclaration} using
     * {@link #statementManipulator} but just in classes.
     * 
     * <PRE>
     * 
     * f0 -> LocalVariableDeclaration() ";"
     *       | ( Modifiers() ClassOrInterfaceDeclaration() )
     *       | Statement()
     *
     * </PRE>
     */
    @Override
    public void visit(BlockStatement n) throws IOException {
        if ((inClass() || inEnum() || inMethod()) && n.f0.which == 0) {
            // we have to get to know, whether there are expressions and
            // assignments used here.
            NodeSequence nodeSequence = (NodeSequence) n.f0.choice;
            LocalVariableDeclaration locVarDeclaration = (LocalVariableDeclaration) nodeSequence.nodes
                    .get(0);
            VariableDeclarator variableDeclarator = locVarDeclaration.f2;
            boolean foundExpression = variableDeclarator.f1.present();

            if (!foundExpression && locVarDeclaration.f3.present()) {
                Vector<Node> declarations = locVarDeclaration.f3.nodes;
                for (Node thisNode : declarations) {
                    nodeSequence = (NodeSequence) thisNode;
                    variableDeclarator = (VariableDeclarator) nodeSequence.nodes.get(1);
                    foundExpression |= variableDeclarator.f1.present();
                }
            }

            if (foundExpression) {
                String statementID = this.counterIDManager.nextStatementID();

                this.statementManipulator.manipulate(locVarDeclaration, statementID);

                // create a MAST Statement out of this  
                int startOffset = StartOffset.getStartOffset(n);
                startLocateableLevel(startOffset);
                super.visit(n);
                LocationList locationList = endLocateableLevel();

                org.codecover.model.mast.Statement newStatement = this.builder
                        .createBasicStatement(locationList,
                                createCoverableItem(statementID), Collections.<RootTerm> emptySet(), this.questionMarkOperators);
                this.statementAttic.bottom().add(newStatement);
            } else {
                super.visit(n);
            }
        } else if (n.f0.which == 1) {
            // ( Modifiers() ClassOrInterfaceDeclaration() )
            NodeSequence sequence = (NodeSequence) n.f0.choice;
            Modifiers modifiers = (Modifiers) sequence.nodes.get(0);
            
            modifiers.accept(this);
            modifiers.startOffset = StartOffset.getStartOffset(modifiers);
            pushModifiersToAttic(modifiers);

            // ( ClassOrInterfaceDeclaration ...)
            sequence.nodes.get(1).accept(this);
            popModifiersFromAttic();
        } else {
            super.visit(n);
        }
    }

    /**
     * Creation of a {@link ConditionalStatement}.<br>
     * Usage of the {@link #branchManipulator}.<br>
     * <PRE>
     * 
     * f0 -> "if"
     * f1 -> "("
     * f2 -> Expression()
     * f3 -> ")"
     * f4 -> Statement()
     * f5 -> [ "else" Statement() ]
     * 
     * </PRE>
     */
    @Override
    public void visit(IfStatement n) throws IOException {
        int keywordStartOffset = n.f0.startOffset;
        int keywordEndOffset = n.f0.endOffset;

        String ifBranchID = this.counterIDManager.nextBranchID();
        String elseBranchID = this.counterIDManager.nextBranchID();
        String conditionID = this.counterIDManager.nextConditionID();

        List<Branch> branchList = new Vector<Branch>(2);

        startLocateableLevel();

        // parse the Expression to a InstrBooleanTerm
        InstrBooleanTerm instrBooleanTerm = this.expressionParser.parse(n.f2);

        // instrument the InstrBooleanTerm and write the declaration for the bit
        // mask before the if(..)
        ConditionManipualtionResult manipulationResult = this.conditionManipulator.
                manipulateAndDeclare(instrBooleanTerm,
                                     conditionID,
                                     createCoverableItem(conditionID),
                                     this.builder, this.sourceFile);
        handleConditionManipulatorMessage(manipulationResult.warningMessage,
                n.f0.startLine);

        // write the collected labelToken after manipulation
        n.labelToken.accept(this);
        n.f0.accept(this);
        n.f1.accept(this);

        // instead of writing the Expression, we write the boolean term,
        // that is possibly instrumented
        manipulationResult.instrumentedTerm.writeToTarget(super.getTargetWriter());

        n.f3.accept(this);

        // now we have to check the Statement()
        Statement ifStatement = n.f4;
        // for the then branch
        pushNewStatementLevelToAttic();

        // for the branch
        startLocateableLevel();

        if (ifStatement.f0.choice instanceof Block) {
            // if it is a block statement, we have to call the branch
            // manipulator after the opening bracket
            Block ifBlock = (Block) ifStatement.f0.choice;

            // {
            ifBlock.f0.accept(this);
            super.getTargetWriter().write(LINE_SEPARATOR);
            this.branchManipulator.manipulateIf(n, ifBranchID);
            // ( BlockStatement() )*
            ifBlock.f1.accept(this);
            // }
            super.getTargetWriter().write(LINE_SEPARATOR);
            ifBlock.f2.accept(this);
        } else {
            // the if statement is not a block, possibly we have to expand it to
            // a block
            if (this.needToExpandBranchesToBlock) {
                super.getTargetWriter().write(" {");
                super.getTargetWriter().write(LINE_SEPARATOR);
                this.branchManipulator.manipulateIf(n, ifBranchID);
                ifStatement.accept(this);
                super.getTargetWriter().write(LINE_SEPARATOR);
                super.getTargetWriter().write("}");
            } else {
                // this should add nothing, cause otherwise we have a default
                // branch manipulator and needToExpandBranchesToBlock set to
                // false -> this would be very bad
                this.branchManipulator.manipulateIf(n, ifBranchID);
                ifStatement.accept(this);
            }
        } // ! (ifStatement.f0.choice instanceof Block)
        LocationList locationListIf = endLocateableLevel();

        // create the if branch
        {
            // create the explicit branch for "then"
            branchList.add(createExplicitBranchFromAttic(ifBranchID,
                    locationListIf,
                    this.builder.createEmptyLocationList()));
        }

        if (n.f5.present()) {
            // the else is present
            // for the else branch
            pushNewStatementLevelToAttic();

            // f5 -> "else" Statement()
            NodeSequence elseSequence = (NodeSequence) n.f5.node;
            NodeToken elseKeyword = (NodeToken) elseSequence.nodes.get(0);

            // accept the "else"
            startLocateableLevel();
            elseKeyword.accept(this);
            LocationList locationListElseKeyword = endLocateableLevel();

            startLocateableLevel();

            // now we have to check the Statement()
            Statement elseStatement = (Statement) elseSequence.nodes.get(1);

            if (elseStatement.f0.choice instanceof Block) {
                // if it is a block statement, we have to call the branch
                // manipulator after the openening bracket
                Block elseBlock = (Block) elseStatement.f0.choice;

                // {
                elseBlock.f0.accept(this);
                super.getTargetWriter().write(LINE_SEPARATOR);
                this.branchManipulator.manipulateElse(n, elseBranchID, false);
                // ( BlockStatement() )*
                elseBlock.f1.accept(this);
                // }
                elseBlock.f2.accept(this);
            } else {
                // the else statement is not a block, possibly we have to expand
                // it to a block
                if (this.needToExpandBranchesToBlock) {
                    super.getTargetWriter().write(" {");
                    super.getTargetWriter().write(LINE_SEPARATOR);
                    this.branchManipulator.manipulateElse(n, elseBranchID, false);
                    elseStatement.accept(this);
                    super.getTargetWriter().write(LINE_SEPARATOR);
                    super.getTargetWriter().write("}");
                } else {
                    // this should add nothing, cause otherwise we have a default
                    // branch manipulator and needToExpandBranchesToBlock set to
                    // false -> this would be very bad
                    this.branchManipulator.manipulateElse(n, elseBranchID, false);
                    elseStatement.accept(this);
                }
            } // ! (elseStatement.f0.choice instanceof Block)

            LocationList locationListElse = endLocateableLevel();
            // create the explicit branch for "else"
            branchList.add(createExplicitBranchFromAttic(elseBranchID,
                    locationListElse,
                    locationListElseKeyword));
        } else {
            // there is no else branch -> maybe, we have to add one
            this.branchManipulator.manipulateElse(n, elseBranchID, true);

            branchList.add(createImplicitBranch(elseBranchID));
        }

        LocationList locationList = endLocateableLevel();
        createConditionalStatementAndPushIt(n.statementID,
                locationList,
                manipulationResult.rootTermForMast,
                branchList,
                createLocation(keywordStartOffset, keywordEndOffset));
    }

    /**
     * <PRE>
     * 
     * f0 -> "switch"
     * f1 -> "("
     * f2 -> Expression()
     * f3 -> ")"
     * f4 -> "{"
     * f5 -> ( SwitchLabel() ( BlockStatement() )* )*
     * f6 -> "}"
     * 
     * </PRE>
     * @throws IOException
     */
    @Override
    public void visit(SwitchStatement n) throws IOException {
        int keywordStartOffset = n.f0.startOffset;
        int keywordEndOffset = n.f0.endOffset;

        boolean isDefaultPresent = false;

        List<Branch> branchList = new Vector<Branch>(n.f5.size());

        startLocateableLevel();

        // write the collected labelToken at this position
        n.labelToken.accept(this);
        n.f0.accept(this);
        n.f1.accept(this);
        n.f2.accept(this);
        n.f3.accept(this);
        n.f4.accept(this);

        // we do not access n.f5 directly
        NodeListOptional branches = n.f5;
        for (Node thisBranch : branches.nodes) {
            LocationList locationListBranch;
            LocationList locationListDecision;

            NodeSequence branchSequence = (NodeSequence) thisBranch;
            SwitchLabel switchLabel = (SwitchLabel) branchSequence.nodes.get(0);
            NodeListOptional blockStatements = (NodeListOptional) branchSequence.nodes.get(1);
            NodeSequence switchLabelSequence = (NodeSequence) switchLabel.f0.choice;

            switchLabel.accept(this);

            String branchID = this.counterIDManager.nextBranchID();

            if (switchLabel.f0.which == 1) {
                // we found a "default" branch
                isDefaultPresent = true;
                this.branchManipulator.manipulateSwitchCase(switchLabel, branchID);

                // get the location of the condition
                // "default" ":"
                NodeToken defaultToken = (NodeToken) switchLabelSequence.nodes.get(0); 
                startLocateableLevel(defaultToken.startOffset);
                locationListDecision = endLocateableLevel(defaultToken.endOffset);
            } else {
                this.branchManipulator.manipulateSwitchDefault(n, branchID, false);

                // get the location of the condition
                // "case" Expression() ":"
                NodeToken caseToken = (NodeToken) switchLabelSequence.nodes.get(0); 
                startLocateableLevel(caseToken.startOffset);
                locationListDecision = endLocateableLevel(
                        EndOffset.getEndOffset(switchLabelSequence.nodes.get(1)));
            }

            pushNewStatementLevelToAttic();

            if (blockStatements.present()) {
                // get the location of the branch body
                startLocateableLevel();
                blockStatements.accept(this);
                locationListBranch = endLocateableLevel();
            } else {
                // the blockStatements are empty
                locationListBranch = this.builder.createEmptyLocationList();
            }

            // create the explicit branch for case or default
            branchList.add(createExplicitBranchFromAttic(branchID,
                    locationListBranch,
                    locationListDecision));
        }

        if (!isDefaultPresent) {
            String defaultBranchID = this.counterIDManager.nextBranchID();
            this.branchManipulator.manipulateSwitchDefault(n, defaultBranchID, true);

            branchList.add(createImplicitBranch(defaultBranchID));
        }

        n.f6.accept(this);

        LocationList locationList = endLocateableLevel();
        createConditionalStatementAndPushIt(n.statementID,
                locationList,
                null,
                branchList,
                createLocation(keywordStartOffset, keywordEndOffset));
    }

    /**
     * <PRE>
     * 
     * f0 -> "try"
     * f1 -> Block()
     * f2 -> ( "catch" "(" FormalParameter() ")" Block() )*
     * f3 -> [ "finally" Block() ]
     * 
     * </PRE>
     */
    @Override
    public void visit(TryStatement n) throws IOException {
        int keywordStartOffset = n.f0.startOffset;
        int keywordEndOffset = n.f0.endOffset;

        List<Branch> branchList = new Vector<Branch>(2 + n.f2.size());

        startLocateableLevel();

        // this is the branchID for the successful finish of the try branch
        String tryBranchID = this.counterIDManager.nextBranchID();
        String tryHelperBranchID = this.counterIDManager.nextTryBranchID();

        // lets have a look at the try statements
        // the statements of the try block are added to the current
        // StatementSequence rather than into the try branch
        this.branchManipulator.manipulateHelperDeclaration(n, tryBranchID,
                tryHelperBranchID);

        // write the collected labelToken after manipulation
        n.labelToken.accept(this);
        // try
        n.f0.accept(this);

        // try Block()
        Block tryBlock = n.f1;

        // {
        tryBlock.f0.accept(this);
        this.branchManipulator.manipulateTry(n, tryBranchID, tryHelperBranchID);
        // ( BlockStatement() )*
        tryBlock.f1.accept(this);
        // }
        tryBlock.f2.accept(this);

        branchList.add(createImplicitBranch(tryBranchID));

        // f2 -> ( "catch" "(" FormalParameter() ")" Block() )*
        // we do not accept n.f2 directly - we do it catch by catch
        NodeListOptional catchBranches = n.f2;
        for (Node catchBranch : catchBranches.nodes) {
            NodeSequence catchSequence = (NodeSequence) catchBranch;
            Vector<Node> nodes = catchSequence.nodes;

            // catch
            NodeToken catchToken = (NodeToken) nodes.get(0);
            catchToken.accept(this);

            // (
            nodes.get(1).accept(this);

            FormalParameter formalParameter = (FormalParameter) nodes.get(2);
            Type catchType = formalParameter.f1;

            formalParameter.accept(this);

            // )
            nodes.get(3).accept(this);

            startLocateableLevel(catchToken.startOffset);
            LocationList locationListDecision = endLocateableLevel();

            // this is the sequence for the "catch"
            pushNewStatementLevelToAttic();

            startLocateableLevel();

            Block catchBlock = (Block) nodes.get(4);
            // {
            catchBlock.f0.accept(this);
            String catchBranchId = this.counterIDManager.nextBranchID();
            this.branchManipulator.manipulateCatch(n, catchType, catchBranchId, tryHelperBranchID);
            // ( BlockStatement() )*
            catchBlock.f1.accept(this);
            // }
            catchBlock.f2.accept(this);

            // create a MAST branch for the "catch" 
            // get the location of the branch body
            LocationList locationList = endLocateableLevel();

            // create the explicit branch for case or default
            branchList.add(createExplicitBranchFromAttic(catchBranchId,
                    locationList,
                    locationListDecision));
        }

        // finish the try-catch-ConditionalStatement BEFORE the finally
        LocationList locationList = endLocateableLevel();
        createConditionalStatementAndPushIt(n.statementID,
                locationList,
                null,
                branchList,
                createLocation(keywordStartOffset, keywordEndOffset));

        // we do not access the finally direct
        if (n.f3.present()) {
            NodeSequence finallySequence = (NodeSequence) n.f3.node;

            // finally
            finallySequence.nodes.get(0).accept(this);

            // the block
            Block finallyBlock = (Block) finallySequence.nodes.get(1);
            // {
            finallyBlock.f0.accept(this);
            this.branchManipulator.manipulateFinally(n, tryBranchID, tryHelperBranchID, false);
            // ( BlockStatement() )*
            finallyBlock.f1.accept(this);
            // }
            finallyBlock.f2.accept(this);
        } else {
            this.branchManipulator.manipulateFinally(n, tryBranchID, tryHelperBranchID, true);
        }
    }

    /**
     * <PRE>
     * 
     * f0 -> "for"
     * f1 -> "("
     * f2 -> ( Modifiers() Type() &lt;IDENTIFIER&gt; ":" Expression() | 
     *       [ ForInit() ] ";" [ Expression() ] ";" [ ForUpdate() ] )
     * f3 -> ")"
     * f4 -> Statement()
     * 
     * </PRE>
     */
    @Override
    public void visit(ForStatement n) throws IOException {
        int keywordStartOffset = n.f0.startOffset;
        int keywordEndOffset = n.f0.endOffset;

        startLocateableLevel();

        String primaryLoopID = this.counterIDManager.nextLoopID();
        RootTerm rootTerm = null;

        this.loopManipulator.manipulateBefore(n, primaryLoopID);

        if (n.f2.which == 0) {
            // here is no expression -> visit directly
            
            // write the collected labelToken after manipulation
            n.labelToken.accept(this);

            // for ( .. )
            n.f0.accept(this);
            n.f1.accept(this);
            n.f2.accept(this);
            n.f3.accept(this);
        } else {
            String conditionID = this.counterIDManager.nextConditionID();

            // in n.f2 is possibly an Expression -> get it
            NodeSequence sequence = (NodeSequence) n.f2.choice;

            // [ Expression() ]
            NodeOptional optionalExpression = (NodeOptional) sequence.nodes.get(2);
            if (optionalExpression.present()) {
                Expression expression = (Expression) optionalExpression.node;
                // parse the Expression to a InstrBooleanTerm
                InstrBooleanTerm instrBooleanTerm = this.expressionParser
                        .parse(expression);

                // instrument the InstrBooleanTerm and write the declaration for
                // the bit mask before the for ()
                ConditionManipualtionResult manipulationResult = this.conditionManipulator.
                manipulateAndDeclare(instrBooleanTerm,
                                     conditionID,
                                     createCoverableItem(conditionID),
                                     this.builder, this.sourceFile);
                handleConditionManipulatorMessage(manipulationResult.warningMessage,
                        n.f0.startLine);
                rootTerm = manipulationResult.rootTermForMast;

                // write the collected labelToken after manipulation
                n.labelToken.accept(this);
                // for "("
                n.f0.accept(this);
                n.f1.accept(this);
                // [ ForInit() ]
                sequence.nodes.get(0).accept(this);
                // ";"
                sequence.nodes.get(1).accept(this);

                // instead of writing the Expression, we write the boolean term,
                // that is possibly instrumented
                manipulationResult.instrumentedTerm.writeToTarget(super.getTargetWriter());

                // ";"
                sequence.nodes.get(3).accept(this);
                // [ ForUpdate() ]
                sequence.nodes.get(4).accept(this);

                // ")"
                n.f3.accept(this);
            } else {
                // write the collected labelToken at this position
                n.labelToken.accept(this);
                // for ( .. )
                n.f0.accept(this);
                n.f1.accept(this);
                n.f2.accept(this);
                n.f3.accept(this);
            }
        }

        // this is the sequence for the loop body
        pushNewStatementLevelToAttic();

        if (n.f4.f0.choice instanceof Block) {
            Block forBlock = (Block) n.f4.f0.choice;

            // {
            forBlock.f0.accept(this);
            this.loopManipulator.manipulateInner(n, primaryLoopID);
            // ( BlockStatement() )*
            forBlock.f1.accept(this);
            // }
            forBlock.f2.accept(this);
        } else {
            if (this.needToExpandLoopsToBlock) {
                super.getTargetWriter().write(" { ");
            }
            this.loopManipulator.manipulateInner(n, primaryLoopID);
            n.f4.accept(this);
            if (this.needToExpandLoopsToBlock) {
                super.getTargetWriter().write(LINE_SEPARATOR);
                super.getTargetWriter().write("  }");
            }
        }
        this.loopManipulator.manipulateAfter(n, primaryLoopID);

        LocationList locationList = endLocateableLevel();

        createLoopingStatementAntPushIt(n.statementID,
                locationList,
                rootTerm,
                createLocation(keywordStartOffset, keywordEndOffset),
                CounterIDManager.generateLoopSubIDZero(primaryLoopID),
                CounterIDManager.generateLoopSubIDOne(primaryLoopID),
                CounterIDManager.generateLoopSubIDAbove(primaryLoopID),
                true);
    }

    /**
     * <PRE>
     * 
     * f0 -> "while"
     * f1 -> "("
     * f2 -> Expression()
     * f3 -> ")"
     * f4 -> Statement()
     * 
     * </PRE>
     */
    @Override
    public void visit(WhileStatement n) throws IOException {
        int keywordStartOffset = n.f0.startOffset;
        int keywordEndOffset = n.f0.endOffset;
        
        startLocateableLevel();

        String primaryLoopID = this.counterIDManager.nextLoopID();
        String conditionID = this.counterIDManager.nextConditionID();

        this.loopManipulator.manipulateBefore(n, primaryLoopID);
        // parse the Expression to a InstrBooleanTerm
        InstrBooleanTerm instrBooleanTerm = this.expressionParser.parse(n.f2);

        // instrument the InstrBooleanTerm and write the declaration for the bit
        // mask before the while ()
        ConditionManipualtionResult manipulationResult = this.conditionManipulator.
                manipulateAndDeclare(instrBooleanTerm,
                                     conditionID,
                                     createCoverableItem(conditionID),
                                     this.builder, this.sourceFile);
        handleConditionManipulatorMessage(manipulationResult.warningMessage,
                n.f0.startLine);

        // write the collected labelToken after manipulation
        n.labelToken.accept(this);
        // while
        n.f0.accept(this);
        // (
        n.f1.accept(this);

        // instead of writing the Expression, we write the boolean term,
        // that is possibly instrumented
        manipulationResult.instrumentedTerm.writeToTarget(super.getTargetWriter());

        // )
        n.f3.accept(this);

        // this is the sequence for the loop body
        pushNewStatementLevelToAttic();

        if (n.f4.f0.choice instanceof Block) {
            Block whileBlock = (Block) n.f4.f0.choice;

            // {
            whileBlock.f0.accept(this);
            this.loopManipulator.manipulateInner(n, primaryLoopID);
            // ( BlockStatement() )*
            whileBlock.f1.accept(this);
            // }
            whileBlock.f2.accept(this);
        } else {
            if (this.needToExpandLoopsToBlock) {
                super.getTargetWriter().write(" { ");
            }
            this.loopManipulator.manipulateInner(n, primaryLoopID);
            n.f4.accept(this);
            if (this.needToExpandLoopsToBlock) {
                super.getTargetWriter().write(LINE_SEPARATOR);
                super.getTargetWriter().write("  }");
            }
        }
        this.loopManipulator.manipulateAfter(n, primaryLoopID);

        LocationList locationList = endLocateableLevel();

        createLoopingStatementAntPushIt(n.statementID,
                locationList,
                manipulationResult.rootTermForMast,
                createLocation(keywordStartOffset, keywordEndOffset),
                CounterIDManager.generateLoopSubIDZero(primaryLoopID),
                CounterIDManager.generateLoopSubIDOne(primaryLoopID),
                CounterIDManager.generateLoopSubIDAbove(primaryLoopID),
                true);
    }

    /**
     * <PRE>
     * 
     * f0 -> "do"
     * f1 -> Statement()
     * f2 -> "while"
     * f3 -> "("
     * f4 -> Expression()
     * f5 -> ")"
     * f6 -> ";"
     * 
     * </PRE>
     */
    @Override
    public void visit(DoStatement n) throws IOException {
        int keywordStartOffset = n.f0.startOffset;
        int keywordEndOffset = n.f0.endOffset;
        
        startLocateableLevel();

        String primaryLoopID = this.counterIDManager.nextLoopID();
        String conditionID = this.counterIDManager.nextConditionID();

        this.loopManipulator.manipulateBefore(n, primaryLoopID);

        // parse the Expression to a InstrBooleanTerm
        InstrBooleanTerm instrBooleanTerm = this.expressionParser.parse(n.f4);

        // instrument the InstrBooleanTerm and write the declaration for the bit
        // mask before the do ()
        ConditionManipualtionResult manipulationResult = this.conditionManipulator.
                manipulateAndDeclare(instrBooleanTerm,
                                     conditionID,
                                     createCoverableItem(conditionID),
                                     this.builder, this.sourceFile);
        handleConditionManipulatorMessage(manipulationResult.warningMessage,
                n.f2.startLine);

        // write the collected labelToken after manipulation
        n.labelToken.accept(this);
        // do
        n.f0.accept(this);

        // this is the sequence for the loop body
        pushNewStatementLevelToAttic();

        if (n.f1.f0.choice instanceof Block) {
            Block doBlock = (Block) n.f1.f0.choice;

            // {
            doBlock.f0.accept(this);
            this.loopManipulator.manipulateInner(n, primaryLoopID);
            // ( BlockStatement() )*
            doBlock.f1.accept(this);
            // }
            doBlock.f2.accept(this);
        } else {
            if (this.needToExpandLoopsToBlock) {
                super.getTargetWriter().write(" { ");
            }
            this.loopManipulator.manipulateInner(n, primaryLoopID);
            n.f1.accept(this);
            if (this.needToExpandLoopsToBlock) {
                super.getTargetWriter().write(LINE_SEPARATOR);
                super.getTargetWriter().write("  }");
            }
        }

        // while ( Expression() ) ;
        n.f2.accept(this);
        n.f3.accept(this);


        // instead of writing the Expression, we write the boolean term,
        // that is possibly instrumented
        manipulationResult.instrumentedTerm.writeToTarget(super.getTargetWriter());

        n.f5.accept(this);
        n.f6.accept(this);

        this.loopManipulator.manipulateAfter(n, primaryLoopID);

        LocationList locationList = endLocateableLevel();

        createLoopingStatementAntPushIt(n.statementID,
                locationList,
                manipulationResult.rootTermForMast,
                createLocation(keywordStartOffset, keywordEndOffset),
                CounterIDManager.generateLoopSubIDZero(primaryLoopID),
                CounterIDManager.generateLoopSubIDOne(primaryLoopID),
                CounterIDManager.generateLoopSubIDAbove(primaryLoopID),
                false);
    }

    /**
     * <PRE>
     * f0 -> "synchronized"
     * f1 -> "("
     * f2 -> Expression()
     * f3 -> ")"
     * f4 -> Block()
     * </PRE>
     */
    @Override
    public void visit(SynchronizedStatement n) throws IOException {
        String syncStatementID = this.counterIDManager.nextSyncStatementID();
        
        n.syncID = syncStatementID;

        int keywordStartOffset = n.f0.startOffset;
        int keywordEndOffset = n.f3.endOffset;

        startLocateableLevel();
        
        this.syncStatementManipulator.manipulatePart1(n, syncStatementID);        
        // n.f0.accept(this); -> "synchronized"
        // n.f1.accept(this); -> "("
        n.f2.accept(this); // --> Expression()
        // n.f3.accept(this);
        this.syncStatementManipulator.manipulatePart2(n, syncStatementID);

        
        this.syncStatementManipulator.manipulateBefore(n, syncStatementID);
        
        this.syncStatementManipulator.manipulatePart3(n, syncStatementID);
        
        LocationList locationList = endLocateableLevel();
        
        org.codecover.model.mast.SynchronizedStatement syncStatement = 
            new org.codecover.model.mast.SynchronizedStatement(
                    locationList, createCoverableItem(n.syncID), createLocation(keywordStartOffset, keywordEndOffset), createCoverableItem(n.syncID + "-0"), createCoverableItem(n.syncID + "-1"), createCoverableItem(n.syncID + "-2"), questionMarkOperators);

        this.statementAttic.bottom().add(syncStatement);
        
        // pushNewStatementLevelToAttic();

        // {
        n.f4.f0.accept(this);
        this.syncStatementManipulator.manipulateInnerBefore(n, syncStatementID);
        // ( BlockStatement() )*
        n.f4.f1.accept(this);
        this.syncStatementManipulator.manipulateInnerAfter(n, syncStatementID);
        // }
        n.f4.f2.accept(this);
        
    }
    
    /**
     * <PRE>
     * f0 -> ConditionalOrExpression()
     * f1 -> [ "?" Expression() ":" Expression() ]
     * </PRE>
     */
    @Override
    public void visit(ConditionalExpression n) throws java.io.IOException {

       if(n.f1.node != null) {
           String qmoID = this.counterIDManager.nextQMOStatementID();
           
           n.conditionalExpressionID = qmoID;

           qmoManipulator.manipulateBefore(n.f0, qmoID);

           startLocateableLevel();
           
           n.f0.accept(this);

           LocationList locationListQMO = endLocateableLevel(); // only the BoolExpression           
           
           qmoManipulator.manipulateAfter(n.f0, qmoID);
                      
           NodeSequence qmoNodes = (NodeSequence)n.f1.node;           
           Node qmoChar1 = qmoNodes.nodes.elementAt(0);           
           Node qmoExpr1 = qmoNodes.nodes.elementAt(1);
           Node qmoChar2 = qmoNodes.nodes.elementAt(2);
           Node qmoExpr2 = qmoNodes.nodes.elementAt(3);
           
           // n.f1.accept(this);
           qmoChar1.accept(this); // "?"

           startLocateableLevel();
           qmoExpr1.accept(this);
           LocationList locationListExpr1 = endLocateableLevel();
           QuestionMarkOperatorExpression qmoe1 = new QuestionMarkOperatorExpression(locationListExpr1, createCoverableItem(n.conditionalExpressionID + "-0"));
           
           qmoChar2.accept(this); // ":"

           startLocateableLevel();
           qmoExpr2.accept(this);
           LocationList locationListExpr2 = endLocateableLevel();
           QuestionMarkOperatorExpression qmoe2 = new QuestionMarkOperatorExpression(locationListExpr2, createCoverableItem(n.conditionalExpressionID + "-1"));
           
                     
           QuestionMarkOperator newQMO = new QuestionMarkOperator(locationListQMO, createCoverableItem(n.conditionalExpressionID), qmoe1, qmoe2);
           
           this.questionMarkOperators.add(newQMO); // the QMOs are removed in the Statement Constructor

       } else {
           n.f0.accept(this); // there is no ?-Operator involved           
       }
       
    }
    
    /**
     * Manipulates the comment, if it starts with "// "
     *
     * @param n The special {@link NodeToken}.
     * @throws IOException
     * 
     * @see NodeToken#specialTokens
     * @see CommentManipulator
     */
    @Override
    protected void visitSpecial(NodeToken n) throws IOException {
        if (!this.commentManipulator.manipulate(n)) {
            super.visitSpecial(n);
        }
    }
}
