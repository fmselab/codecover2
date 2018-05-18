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
import java.io.Writer;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import org.codecover.instrumentation.HierarchyLevelContainer;
import org.codecover.instrumentation.booleanterms.InstrBasicBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrBooleanTerm;
import org.codecover.instrumentation.xampil.CounterIDProvider;
import org.codecover.instrumentation.xampil.HierarchyLevelTypes;
import org.codecover.instrumentation.xampil.XampilExpressionParser;
import org.codecover.instrumentation.xampil.manipulator.BranchManipulator;
import org.codecover.instrumentation.xampil.manipulator.ConditionManipulator;
import org.codecover.instrumentation.xampil.manipulator.LoopManipulator;
import org.codecover.instrumentation.xampil.manipulator.StatementManipulator;
import org.codecover.instrumentation.xampil.parser.InstrumentableItemCounter;
import org.codecover.instrumentation.xampil.parser.XampilParser;
import org.codecover.instrumentation.xampil.syntaxtree.AssignmentStatement;
import org.codecover.instrumentation.xampil.syntaxtree.CompilationUnit;
import org.codecover.instrumentation.xampil.syntaxtree.FileStatement;
import org.codecover.instrumentation.xampil.syntaxtree.IfStatement;
import org.codecover.instrumentation.xampil.syntaxtree.Node;
import org.codecover.instrumentation.xampil.syntaxtree.NodeListOptional;
import org.codecover.instrumentation.xampil.syntaxtree.NodeOptional;
import org.codecover.instrumentation.xampil.syntaxtree.NodeSequence;
import org.codecover.instrumentation.xampil.syntaxtree.NodeToken;
import org.codecover.instrumentation.xampil.syntaxtree.Program;
import org.codecover.instrumentation.xampil.syntaxtree.Statement;
import org.codecover.instrumentation.xampil.syntaxtree.SwitchStatement;
import org.codecover.instrumentation.xampil.syntaxtree.WhileStatement;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.BasicStatement;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.Branch;
import org.codecover.model.mast.ConditionalStatement;
import org.codecover.model.mast.CoverableItem;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.LocationList;
import org.codecover.model.mast.LoopingStatement;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.mast.StatementSequence;
import org.codecover.model.utils.Attic;

/**
 * The instrumentation visitor traverses the parsed syntax tree and adds counter to
 * the source code.
 *
 * @author Christoph Müller, Stefan Franke
 */
public class InstrumentationVisitor extends TreeDumper {

    /**
     * The name of the coverage log file.
     */
    private static final String CLF_NAME = "coverage-log.clf"; 
    
    private final InstrumentableItemCounter counter;

    private final MASTBuilder builder;

    private final SourceFile sourceFile;

    private final HierarchyLevelContainer hierarchyLevelContainer;

    private final String testSessionContainerUID;

    private final CounterIDProvider counterIDProvider;

    private StatementManipulator statementManipulator;

    private BranchManipulator branchManipulator;

    private LoopManipulator loopManipulator;

    private ConditionManipulator conditionManipulator;

    /**
     * Example of the usage:
     * <pre>
     *    &rarr; push new list for the PROGRAM unit
     * 1) (PROGRAM) {}
     * 2) (PROGRAM) {i := 1}
     *    &rarr; push new list for the IF
     * 3) (PROGRAM) {i := 1}
     *    (if)      {}
     * 4) (PROGRAM) {i := 1}
     *    (if)      {i := i + 1}
     * 5) (PROGRAM) {i := 1}
     *    (if)      {i := i + 1, a := a * i}
     *    &rarr; pop lowest list and create a StatementSequence for the IF branch
     * 6) (PROGRAM) {i := 1, {i := i + 1, a := a * i}}
     * 7) (PROGRAM) {i := 1, {i := i + 1, a := a * i}, FILE OVERWRITE "target.log" i}
     *    &rarr; pop lowest list and create a StatementSequence for the PROGRAM unit
     * </pre>
     */
    private Attic<List<org.codecover.model.mast.Statement>> statementAttic;

    /**
     * Constructor of a new {@link InstrumentationVisitor}.
     * 
     * @param writer
     *                target writer object
     * @param counter
     *                the {@link InstrumentableItemCounter} used during the
     *                parsing of
     *                {@link XampilParser#CompilationUnit(InstrumentableItemCounter)}
     * @param builder
     *                the MASTBuilder which creates the more abstract syntax
     *                tree objects
     * @param sourceFile
     *                the source code file
     * @param hierarchyLevelContainer
     *                This is the {@link HierarchyLevelContainer}, where all
     *                top level {@link HierarchyLevel}s of the source file can
     *                be added using
     *                {@link HierarchyLevelContainer#addHierarchyLevels(Collection, LinkedList)}.
     * @param testSessionContainerUID
     *                the test session container UID of the instrumented source
     *                code
     */
    public InstrumentationVisitor(PrintWriter writer,
                                  InstrumentableItemCounter counter,
                                  MASTBuilder builder,
                                  SourceFile sourceFile,
                                  HierarchyLevelContainer hierarchyLevelContainer,
                                  String testSessionContainerUID) {
        super(writer);
        this.counter = counter;
        this.builder = builder;
        this.sourceFile = sourceFile;
        this.hierarchyLevelContainer = hierarchyLevelContainer;
        this.testSessionContainerUID = testSessionContainerUID;
        this.counterIDProvider = new CounterIDProvider();
        this.statementAttic = new Attic<List<org.codecover.model.mast.Statement>>();
    }

    /**
     * Creates a {@link CoverableItem} using an ID an the
     * {@link #fullSourceFileName}.
     */
    private CoverableItem createCoverableItem(String id) {
        return this.builder.createCoverableItem(this.sourceFile.getFileName(), id);
    }

    private Location createLocation(int startOffset, int endOffset) {
        return this.builder.createLocation(this.sourceFile, startOffset, endOffset);
    }

    /**
     * Creates a {@link LocationList} out of a start and an end offset.<br>
     * <br>
     * If start and end offset are <code>-1</code>,
     * {@link MASTBuilder#createEmptyLocationList()} is returned.
     *
     * @param startOffset The start offset of the {@link Location}.
     * @param endOffset The end offset of the {@link Location}.
     * 
     * @return The {@link LocationList}.
     */
    private LocationList createLocationList(int startOffset, int endOffset) {
        if (startOffset == -1 && endOffset == -1) {
            return this.builder.createEmptyLocationList();
        }
        if (startOffset != -1 && endOffset != -1) {
            Location location = createLocation(startOffset, endOffset);
            List<Location> listOfLocations = Collections.<Location>singletonList(location);
            return this.builder.createLocationList(listOfLocations);
        }

        String message = "startOffset == -1 ^ endOffset == -1";
        Exception exception = new IllegalStateException(message);
        this.builder.getLogger().fatal(message, exception);

        // never reached cause fatal throws an Exception
        return null;
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

        return this.builder.createStatementSequence(
                this.builder.createLocationList(locationsOfSequence), 
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

    private void pushNewStatementLevelToAttic() {
        this.statementAttic.push(new LinkedList<org.codecover.model.mast.Statement>());
    }

    private void popTopLevelHieraryLevelsFromAttic(int start,
                                                   int end,
                                                   int headerStartOffset,
                                                   int headerEndOffset) {
        if (this.statementAttic.size() != 1) {
            String message = "this.statementAttic.size() != 1";
            Exception exception = new IllegalStateException(message);
            this.builder.getLogger().fatal(message, exception);
        }

        List<StatementSequence> programStatements = createStatementSequenceListFromAttic();
        HierarchyLevel programHL = this.builder.createHierarchyLevel(
                createLocationList(start, end),
                "PROGRAM",
                createLocationList(headerStartOffset, headerEndOffset),
                HierarchyLevelTypes.getProgramType(this.builder),
                Collections.<HierarchyLevel>emptyList(),
                programStatements);
        this.hierarchyLevelContainer.addHierarchyLevelToRoot(programHL);
    }

    /**
     * Gets the start and end offset of the statement. Then
     * a MAST {@link BasicStatement} is created and added to
     * {@link #statementAttic}.<br>
     * <br>
     * <b>The {@link Statement} has to visited before!</b>
     * 
     * @param statement
     *            A statement which will be put to the attic.
     * @param statementID The ID of the statement. Must be not <code>null</code>.
     */
    private void atticStatement(Node statement, String statementID) {
        if (statementID == null) {
            this.builder.getLogger().fatal("statementID == null");
        }
        int startOffset = StartOffset.getStartOffset(statement);
        int endOffset = super.getLastEndOffset();

        LocationList locationList = createLocationList(startOffset, endOffset);
        org.codecover.model.mast.Statement newStatement = this.builder
                .createBasicStatement(locationList,
                                      createCoverableItem(statementID),
                                      Collections.<RootTerm> emptySet());
        this.statementAttic.bottom().add(newStatement);
    }

    /**
     * Calls {@link #createStatementSequenceListFromAttic()}, creates an
     * explicit {@link Branch}.
     */
    private Branch createExplicitBranchFromAttic(String branchID,
                                                 int startOffset,
                                                 int endOffset,
                                                 int decisionStartOffset,
                                                 int decisionEndOffset) {
        LocationList locationList = createLocationList(startOffset, endOffset);
        LocationList locationListDecision = createLocationList(decisionStartOffset,
                decisionEndOffset);
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
            int startOffset,
            int endOffset,
            RootTerm rootTerm,
            List<Branch> branchList,
            int keywordStartOffset,
            int keywordEndOffset) {
        LocationList locationList = createLocationList(startOffset, endOffset);
        Location keywordLocation = createLocation(keywordStartOffset, keywordEndOffset);
        Set<RootTerm> setRootTerms;

        if (rootTerm == null) {
            setRootTerms = Collections.<RootTerm> emptySet();
        } else {
            setRootTerms = new HashSet<RootTerm>();
            setRootTerms.add(rootTerm);
        }

        ConditionalStatement conditionalStatement = this.builder.createConditionalStatement(
                locationList,
                createCoverableItem(statementID),
                setRootTerms,
                branchList,
                keywordLocation);
        this.statementAttic.bottom().add(conditionalStatement);
    }

    private void createLoopingStatementAntPushIt(String statementID,
            int startOffset,
            int endOffset,
            RootTerm rootTerm,
            int keywordStartOffset,
            int keywordEndOffset,
            String loopIDZero,
            String loopIDOnce,
            String loopIDAbove,
            boolean optionalBodyExecution) {
        LocationList locationList = createLocationList(startOffset, endOffset);
        Location keywordLocation = createLocation(keywordStartOffset, keywordEndOffset);
        StatementSequence statementSequence = createStatementSequenceFromAttic();
        Set<RootTerm> setRootTerms;

        if (rootTerm == null) {
            setRootTerms = Collections.<RootTerm> emptySet();
        } else {
            setRootTerms = new HashSet<RootTerm>();
            setRootTerms.add(rootTerm);
        }

        LoopingStatement loopingStatement = this.builder.createLoopingStatement(
                locationList,
                createCoverableItem(statementID),
                setRootTerms,
                statementSequence,
                keywordLocation,
                createCoverableItem(loopIDZero),
                createCoverableItem(loopIDOnce),
                createCoverableItem(loopIDAbove),
                optionalBodyExecution);
        this.statementAttic.bottom().add(loopingStatement);
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

    /**
     * <PRE>
     * f0 -> Declaration()
     * f1 -> Program()
     * f2 -> ( &lt;EOL&gt; )?
     * f3 -> &lt;EOF&gt;
     * </PRE>
     */
    @Override
    public void visit(CompilationUnit n) {
       int startOffset = 0;
       int endOffset;
       int headerStartOffset;
       int headerEndOffset;
       pushNewStatementLevelToAttic();

       // Declaration()
       n.f0.accept(this);
       this.statementManipulator.writeDeclarations(this.counter.getStatementCount());
       this.branchManipulator.writeDeclarations(this.counter.getBranchCount());
       this.conditionManipulator.writeDeclarations(this.counter);
       this.loopManipulator.writeDeclarations(this.counter.getLoopCount());

       headerStartOffset = StartOffset.getStartOffset(n.f1);
       // "PROGRAM".length == 7
       headerEndOffset = headerStartOffset + 7;
       // Program()
       n.f1.accept(this);
       // ( <EOL> )?
       n.f2.accept(this);
       // ( <EOF> )?
       n.f3.accept(this);

       endOffset = super.getLastEndOffset();
       popTopLevelHieraryLevelsFromAttic(startOffset, endOffset,
               headerStartOffset, headerEndOffset);

    }

    /**
     * <PRE>
     * f0 -> &lt;PROGRAM&gt;
     * f1 -> &lt;EOL&gt;
     * f2 -> ( Statement() )*
     * f3 -> &lt;ENDPROGRAM&gt;
     * </PRE>
     */
    @Override
    public void visit(Program n) {
       n.f0.accept(this);
       n.f1.accept(this);
       n.f2.accept(this);

       writeCoverageLogFileOutput();

       n.f3.accept(this);
    }
    
    /**
     * <PRE>
     * f0 -> AssignmentStatement()
     *       | IfStatement()
     *       | WhileStatement()
     *       | SwitchStatement()
     *       | FileStatement()
     * </PRE>
     */
    @Override
    public void visit(Statement n) {
        String statementID = this.counterIDProvider.nextStatementID();
        if (n.f0.choice instanceof AssignmentStatement ||
            n.f0.choice instanceof FileStatement) {
            n.f0.accept(this);
            // create a MAST Statement here
            // the statement has to be visited BEFORE
            atticStatement(n.f0.choice, statementID);
        } else if (n.f0.choice instanceof IfStatement) {
            IfStatement ifStatement = (IfStatement) n.f0.choice;
            ifStatement.statementID = statementID;
            ifStatement.accept(this);
        } else if (n.f0.choice instanceof WhileStatement) {
            WhileStatement whileStatement = (WhileStatement) n.f0.choice;
            whileStatement.statementID = statementID;
            whileStatement.accept(this);
        } else if (n.f0.choice instanceof SwitchStatement) {
            SwitchStatement switchStatement = (SwitchStatement) n.f0.choice;
            switchStatement.statementID = statementID;
            switchStatement.accept(this);
        }

        this.statementManipulator.manipulate(n, statementID);
    }

    /**
     * <PRE>
     * f0 -> &lt;IF&gt;
     * f1 -> Expression(basicBooleanCounter)
     * f2 -> &lt;THEN&gt;
     * f3 -> &lt;EOL&gt;
     * f4 -> ( Statement() )*
     * f5 -> ( &lt;ELSE&gt; &lt;EOL&gt; ( Statement() )* )?
     * f6 -> &lt;ENDIF&gt;
     * f7 -> &lt;EOL&gt;
     * </PRE>
     */
    @Override
    public void visit(IfStatement n) {
        final int startOffSet = n.f0.startOffset;
        final int endOffset;
        final int keywordStartOffset = startOffSet;
        final int keywordEndOffset = n.f0.endOffset;
        final int thenStartOffset;
        final int thenEndOffset;
        final int elseStartOffset;
        final int elseEndOffset;

        final String thenBranchID = this.counterIDProvider.nextBranchID();
        final String elseBranchID = this.counterIDProvider.nextBranchID();
        final String ifConditionID = this.counterIDProvider.nextConditionID();

        final Branch thenBranch;
        final Branch elseBranch;

        XampilExpressionParser xampilExpressionParser = new XampilExpressionParser();
        InstrBooleanTerm instrBooleanTerm = xampilExpressionParser.parse(n.f1);
        // create BooleanTerm BEFORE instrumenting
        BooleanTerm booleanTerm = instrBooleanTerm.toBooleanTerm(this.builder,
                this.sourceFile);
        RootTerm rootTerm = this.builder.createRootTerm(booleanTerm,
                createCoverableItem(ifConditionID));

        // Instrumenting the boolean term
        List<InstrBasicBooleanTerm> termList = new LinkedList<InstrBasicBooleanTerm>();
        instrBooleanTerm.getAllBasicBooleanTerms(termList);

        this.conditionManipulator.manipulate(ifConditionID, termList);
        // <IF>
        n.f0.accept(this);
        // Expression
        n.f1.accept(this);
        // <THEN>
        n.f2.accept(this);
        // <EOL>
        n.f3.accept(this);

        this.branchManipulator.manipulateIf(n, thenBranchID);
        if (n.f4.present()) {
            thenStartOffset = StartOffset.getStartOffset(n.f4);
        } else {
            thenStartOffset = -1;
        }
        pushNewStatementLevelToAttic();

        // ( Statement() )*
        n.f4.accept(this);
        // will be -1, if there was no NodeToken
        thenEndOffset = super.getLastEndOffset();

        // create the then branch
        thenBranch = createExplicitBranchFromAttic(thenBranchID,
                                                   thenStartOffset,
                                                   thenEndOffset,
                                                   -1, -1); 

        NodeOptional elseOption = n.f5;
        if (elseOption.present()) {
            // the else is present
            NodeSequence elseSequence = (NodeSequence) elseOption.node;
            // <ELSE>
            elseSequence.nodes.get(0).accept(this);
            // <EOL>
            elseSequence.nodes.get(1).accept(this);

            this.branchManipulator.manipulateElse(n, elseBranchID, false);

            NodeListOptional statementList = (NodeListOptional) elseSequence.nodes.get(2);
            if (statementList.present()) {
                elseStartOffset = StartOffset.getStartOffset(statementList);
            } else {
                elseStartOffset = -1;
            }
            pushNewStatementLevelToAttic();

            // ( Statement() )*
            statementList.accept(this);
            // will be -1, if there was no NodeToken
            elseEndOffset = super.getLastEndOffset();
            
            // create the explicit else branch
            elseBranch = createExplicitBranchFromAttic(elseBranchID,
                                                       elseStartOffset,
                                                       elseEndOffset,
                                                       -1, -1);
        } else {
            // there was no else branch -> create it
            this.branchManipulator.manipulateElse(n, elseBranchID, true);

            // create the implicit else branch
            elseBranch = createImplicitBranch(elseBranchID);
        }

        // <ENDIF>
        n.f6.accept(this);
        // <EOL>
        n.f7.accept(this);

        endOffset = super.getLastEndOffset();
        List<Branch> branchList = new Vector<Branch>(2);
        branchList.add(thenBranch);
        branchList.add(elseBranch);

        createConditionalStatementAndPushIt(n.statementID,
                                            startOffSet,
                                            endOffset,
                                            rootTerm,
                                            branchList,
                                            keywordStartOffset,
                                            keywordEndOffset);
    }

    /**
     * <PRE>
     * f0 -> &lt;SWITCH&gt;
     * f1 -> &lt;IDENTIFIER&gt;
     * f2 -> &lt;EOL&gt;
     * f3 -> ( &lt;CASE&gt; Expression(DUMMY_CONTAINER) &lt;COLON&gt; ( &lt;EOL&gt; )? ( Statement() )* &lt;ENDCASE&gt; &lt;EOL&gt; )+
     * f4 -> ( &lt;CASE_DEFAULT&gt; &lt;COLON&gt; ( &lt;EOL&gt; )? ( Statement() )* &lt;ENDCASE&gt; &lt;EOL&gt; )?
     * f5 -> &lt;ENDSWITCH&gt;
     * f6 -> &lt;EOL&gt;
     * </PRE>
     */
    @Override
    public void visit(SwitchStatement n) {
        final int startOffSet = n.f0.startOffset;
        final int endOffset;
        final int keywordStartOffset = startOffSet;
        final int keywordEndOffset = n.f0.endOffset;

        final List<Branch> branchList = new LinkedList<Branch>();

        n.f0.accept(this);
        n.f1.accept(this);
        n.f2.accept(this);
        for (Node thisCaseNode : n.f3.nodes) {
            final int branchStartOffset;
            final int branchEndOffset;
            final int decisionStartOffset;
            final int decisionEndOffset;
            final NodeSequence caseSequence = (NodeSequence) thisCaseNode;

            // <CASE>
            caseSequence.nodes.get(0).accept(this);
            // Expression(DUMMY_CONTAINER)
            caseSequence.nodes.get(1).accept(this);
            // <COLON>
            caseSequence.nodes.get(2).accept(this);
            
            decisionStartOffset = ((NodeToken) caseSequence.nodes.get(0)).startOffset;
            decisionEndOffset = super.getLastEndOffset();
            
            // ( <EOL> )?
            caseSequence.nodes.get(3).accept(this);
            
            String caseBranchID = this.counterIDProvider.nextBranchID();
            this.branchManipulator.manipulateCase(n, caseBranchID);

            NodeListOptional statementList = (NodeListOptional) caseSequence.nodes.get(4);
            if (statementList.present()) {
                branchStartOffset = StartOffset.getStartOffset(statementList);
            } else {
                branchStartOffset = -1;
            }
            pushNewStatementLevelToAttic();

            // ( Statement() )*
            statementList.accept(this);

            branchEndOffset = super.getLastEndOffset();

            branchList.add(createExplicitBranchFromAttic(caseBranchID,
                                                         branchStartOffset,
                                                         branchEndOffset,
                                                         decisionStartOffset,
                                                         decisionEndOffset));

            // <ENDCASE>
            caseSequence.nodes.get(5).accept(this);
            // <EOL>
            caseSequence.nodes.get(6).accept(this);
        }

        String defaultBranchID = this.counterIDProvider.nextBranchID();
        if (n.f4.present()) {
            final int branchStartOffset;
            final int branchEndOffset;
            final int decisionStartOffset;
            final int decisionEndOffset;
            final NodeSequence defaultSequence = (NodeSequence) n.f4.node;

            // <DEFAULT>
            defaultSequence.nodes.get(0).accept(this);
            // <COLON>
            defaultSequence.nodes.get(1).accept(this);

            decisionStartOffset = ((NodeToken) defaultSequence.nodes.get(0)).startOffset;
            decisionEndOffset = super.getLastEndOffset();

            // ( <EOL> )?
            defaultSequence.nodes.get(2).accept(this);

            this.branchManipulator.manipulateDefault(n, defaultBranchID, false);

            NodeListOptional statementList = (NodeListOptional) defaultSequence.nodes.get(3);
            if (statementList.present()) {
                branchStartOffset = StartOffset.getStartOffset(statementList);
            } else {
                branchStartOffset = -1;
            }
            pushNewStatementLevelToAttic();

            // ( Statement() )*
            statementList.accept(this);

            branchEndOffset = super.getLastEndOffset();

            branchList.add(createExplicitBranchFromAttic(defaultBranchID,
                                                         branchStartOffset,
                                                         branchEndOffset,
                                                         decisionStartOffset,
                                                         decisionEndOffset));
            // <ENDCASE>
            defaultSequence.nodes.get(4).accept(this);
            // <EOL>
            defaultSequence.nodes.get(5).accept(this);
        } else {
            this.branchManipulator.manipulateDefault(n, defaultBranchID, true);

            branchList.add(createImplicitBranch(defaultBranchID));
        }
        n.f5.accept(this);
        n.f6.accept(this);

        endOffset = super.getLastEndOffset();

        createConditionalStatementAndPushIt(n.statementID,
                startOffSet,
                endOffset,
                null,
                branchList,
                keywordStartOffset,
                keywordEndOffset);
    }

    /**
     * <PRE>
     * f0 -> &lt;WHILE&gt;
     * f1 -> Expression(basicBooleanCounter)
     * f2 -> &lt;DO&gt;
     * f3 -> &lt;EOL&gt;
     * f4 -> ( Statement() )*
     * f5 -> &lt;ENDWHILE&gt;
     * f6 -> &lt;EOL&gt;
     * </PRE>
     */
    @Override
    public void visit(WhileStatement n) {
        final int startOffSet = n.f0.startOffset;
        final int endOffset;
        final int keywordStartOffset = startOffSet;
        final int keywordEndOffset = n.f0.endOffset;

        final String whileLoopID = this.counterIDProvider.nextLoopID();
        final String whileConditionID = this.counterIDProvider.nextConditionID();

        XampilExpressionParser xampilExpressionParser = new XampilExpressionParser();
        InstrBooleanTerm instrBooleanTerm = xampilExpressionParser.parse(n.f1);
        // create BooleanTerm BEFORE instrumenting
        BooleanTerm booleanTerm = instrBooleanTerm.toBooleanTerm(this.builder,
                this.sourceFile);
        RootTerm rootTerm = this.builder.createRootTerm(booleanTerm,
                createCoverableItem(whileConditionID));

        List<InstrBasicBooleanTerm> termList = new LinkedList<InstrBasicBooleanTerm>();
        instrBooleanTerm.getAllBasicBooleanTerms(termList);
        this.conditionManipulator.manipulate(whileConditionID, termList);
        
        pushNewStatementLevelToAttic();

        this.loopManipulator.manipulateBeforeWhile(n, whileLoopID);
        
        // <WHILE>
        n.f0.accept(this);
        // Expression(basicBooleanCounter)
        n.f1.accept(this);
        // <DO>
        n.f2.accept(this);
        // <EOL>
        n.f3.accept(this);

        this.loopManipulator.manipulateInWhile(n, whileLoopID);

        // ( Statement() )*
        n.f4.accept(this);
        // <ENDWHILE>
        n.f5.accept(this);
        // <EOL>
        n.f6.accept(this);

        this.loopManipulator.manipulateAfterWhile(n, whileLoopID);

        endOffset = super.getLastEndOffset();
        createLoopingStatementAntPushIt(n.statementID,
                startOffSet,
                endOffset,
                rootTerm,
                keywordStartOffset,
                keywordEndOffset,
                CounterIDProvider.generateLoopSubIDZero(whileLoopID),
                CounterIDProvider.generateLoopSubIDOne(whileLoopID),
                CounterIDProvider.generateLoopSubIDAbove(whileLoopID),
                true);
    }

    /**
     * Writes the final FILE statement to the instrumented file by delegating
     * this to each manipulator.
     * @param testSessionContainerUUID The test session container UUID. 
     * @param targetWriter The writer to write the instrumentation to.
     * @param statementManipulator The used {@link StatementManipulator}.
     * @param branchManipulator The used {@link BranchManipulator}.
     * @param loopManipulator The used {@link LoopManipulator}.
     * @param conditionManipulator The used {@link ConditionManipulator}.
     */
    public void writeCoverageLogFileOutput() {
        PrintWriter targetWriter = super.getTargetWriter();
        writeFileStatement(targetWriter, true, "\"TEST_SESSION_CONTAINER \\\"" + this.testSessionContainerUID + "\\\"\"");
        writeFileStatement(targetWriter, false, "\"START_TEST_CASE \\\"Single Test Case\\\"\"");

        this.statementManipulator.writeCoverageLogFileOutput(this.counter.getStatementCount());
        this.branchManipulator.writeCoverageLogFileOutput(this.counter.getBranchCount());
        this.loopManipulator.writeCoverageLogFileOutput(this.counter.getLoopCount());
        this.conditionManipulator.writeCoverageLogFileOutput(this.counter);

        writeFileStatement(targetWriter, false, "\"END_TEST_CASE \\\"Single Test Case\\\"\"");
    }

    /**
     * Writes a statement:
     * <pre>
     * FILE OVERWRITE "coveragelog-file.clf" "message" 
     * </pre>
     * to the given <code>targetWriter</code>.
     * 
     * @param targetWriter The target {@link Writer}.
     * @param overwrite true &rarr; use "OVERWRITE"; false &rarr; use "APPEND"
     * @param message The message.
     */
    public static void writeFileStatement(PrintWriter targetWriter,
                                          boolean overwrite,
                                          String message) {
        targetWriter.write("    FILE ");
        if (overwrite) {
            targetWriter.write("OVERWRITE");
        } else {
            targetWriter.write("APPEND");
        }
        targetWriter.write(" \"" + CLF_NAME + "\" ");
        targetWriter.write(message);
        targetWriter.write(" + \"\\n\"\n");
    }
}