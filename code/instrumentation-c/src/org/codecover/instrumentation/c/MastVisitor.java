package org.codecover.instrumentation.c;

import org.codecover.instrumentation.HierarchyLevelContainer;
import org.codecover.instrumentation.c.adapter.CCNode;
import org.codecover.instrumentation.c.counter.CounterManager;
import org.codecover.instrumentation.c.syntaxtree.*;
import org.codecover.instrumentation.c.syntaxtree.Statement;
import org.codecover.instrumentation.c.visitor.DepthFirstVisitor;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.*;

import java.util.*;

public class MastVisitor extends DepthFirstVisitor {
    private Stack<List<org.codecover.model.mast.Statement>> statementStack = new Stack<List<org.codecover.model.mast.Statement>>();
    private Stack<List<HierarchyLevel>> hierarchyStack = new Stack<List<HierarchyLevel>>();
    private Set<QuestionMarkOperator> qmoSet = new HashSet<QuestionMarkOperator>();

    private MASTBuilder builder;
    private SourceFile sourceFile;
    private HierarchyLevelContainer rootContainer;
    private int lastEndOffset;

    private CounterManager cm;

    private final CExpressionParser expressionParser = new CExpressionParser();

    public MastVisitor(MASTBuilder builder, SourceFile sourceFile, HierarchyLevelContainer rootContainer, CounterManager cm) {
        this.builder = builder;
        this.sourceFile = sourceFile;
        this.rootContainer = rootContainer;
        this.cm = cm;
    }

    private void pushStatementLevel() {
        statementStack.push(new ArrayList<org.codecover.model.mast.Statement>());
    }

    private StatementSequence popStatementLevel() {
        List<org.codecover.model.mast.Statement> statementList = statementStack.pop();
        List<Location> locationsOfSequence = new Vector<Location>(statementList.size());
        for (org.codecover.model.mast.Statement thisStatement : statementList) {
            locationsOfSequence.addAll(thisStatement.getLocation().getLocations());
        }

        return builder.createStatementSequence(
                builder.createLocationList(locationsOfSequence),
                statementList);
    }

    private List<StatementSequence> createStatementSequenceList() {
        StatementSequence statementSequence = popStatementLevel();

        if (statementSequence.getStatements().isEmpty()) {
            return Collections.emptyList();
        }

        return Collections.singletonList(statementSequence);
    }

    private LocationList createLocationList(int startOffset, int endOffset) {
        if (startOffset == -1 && endOffset == -1) {
            return builder.createEmptyLocationList();
        }

        if (startOffset != -1 && endOffset != -1) {
            Location location = builder.createLocation(sourceFile, startOffset, endOffset);
            List<Location> listOfLocations = Collections.singletonList(location);
            return builder.createLocationList(listOfLocations);
        }

        String message = "startOffset == -1 ^ endOffset == -1";
        Exception exception = new IllegalStateException(message);
        builder.getLogger().fatal(message, exception);

        // never reached cause fatal throws an Exception
        return null;
    }

    private void pushHierarchy() {
        hierarchyStack.push(new ArrayList<HierarchyLevel>());
        pushStatementLevel();
    }

    private void popHierachy(HierarchyLevelType type, String name,
                             int start, int end,
                             int headerStart, int headerEnd) {
        List<StatementSequence> programStatements = createStatementSequenceList();
        HierarchyLevel level = builder.createHierarchyLevel(
                createLocationList(start, end),
                name,
                createLocationList(headerStart, headerEnd),
                type,
                hierarchyStack.pop(),
                programStatements);
        if(hierarchyStack.empty())
            rootContainer.addHierarchyLevelToRoot(level);
        else
            hierarchyStack.peek().add(level);
    }

    private CoverableItem createCoverableItem(String id) {
        return builder.createCoverableItem(sourceFile.getFileName(), id);
    }


    private void createBasicStatement(Node statement, String id) {
        int startOffset = BeginOffset.getStartOffset(statement);

        LocationList locationList = createLocationList(startOffset, lastEndOffset);
        org.codecover.model.mast.Statement newStatement = builder
                .createBasicStatement(locationList,
                        createCoverableItem(id),
                        Collections.<RootTerm> emptySet(), qmoSet);
        this.statementStack.peek().add(newStatement);
    }

    private Branch createBranch(String branchID,
                                                 int startOffset,
                                                 int endOffset,
                                                 int decisionStartOffset,
                                                 int decisionEndOffset,
                                                 boolean implicit) {
        LocationList locationList = createLocationList(startOffset, endOffset);
        LocationList locationListDecision = createLocationList(decisionStartOffset,
                decisionEndOffset);
        StatementSequence statementSequence = popStatementLevel();

        return builder.createBranch(locationList,
                createCoverableItem(branchID),
                implicit,
                locationListDecision,
                statementSequence);
    }

    private void createConditionalStatement(String statementID,
                                            int startOffset,
                                            int endOffset,
                                            RootTerm rootTerm,
                                            List<Branch> branchList,
                                            int keywordStartOffset,
                                            int keywordEndOffset) {
        LocationList locationList = createLocationList(startOffset, endOffset);
        Location keywordLocation = builder.createLocation(sourceFile, keywordStartOffset, keywordEndOffset);

        Set<RootTerm> setRootTerms;
        if (rootTerm == null) {
            setRootTerms = Collections.emptySet();
        } else {
            setRootTerms = new HashSet<RootTerm>();
            setRootTerms.add(rootTerm);
        }

        ConditionalStatement conditionalStatement = builder.createConditionalStatement(
                locationList,
                createCoverableItem(statementID),
                setRootTerms,
                branchList,
                keywordLocation, qmoSet);
        statementStack.peek().add(conditionalStatement);
    }

    public void createLoop(RootTerm rootTerm, int start, int end, int keywordStart, int keywordEnd, int stmtId, int loopId, boolean optionalBodyExecution) {
        Set<RootTerm> setRootTerms;
        if (rootTerm == null) {
            setRootTerms = Collections.emptySet();
        } else {
            setRootTerms = new HashSet<RootTerm>();
            setRootTerms.add(rootTerm);
        }

        LoopingStatement stmt = builder.createLoopingStatement(
                createLocationList(start, end),
                createCoverableItem(cm.stmtID(stmtId)),
                setRootTerms,
                popStatementLevel(),
                builder.createLocation(sourceFile, keywordStart, keywordEnd),
                createCoverableItem(cm.loopID(loopId++)),
                createCoverableItem(cm.loopID(loopId++)),
                createCoverableItem(cm.loopID(loopId++)),
                optionalBodyExecution,
                null
        );
        statementStack.peek().add(stmt);
    }

    ///////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////////


    @Override
    public void visit(org.codecover.instrumentation.c.syntaxtree.Statement n) {
        // We don't need ids for all kinds of statements
        // We want nice inscreasing IDs so we assign the ID before we visit the other nodes
        if(n.nodeChoice.which > 1)
            n.stmtID = cm.newStmtID();
        super.visit(n);
        if(n.nodeChoice.choice instanceof ExpressionStatement ||
                n.nodeChoice.choice instanceof JumpStatement) {
            createBasicStatement(n, cm.stmtID(n.stmtID));
        }
    }

    public void visit(FunctionDefinition n) {
        int headerStart = BeginOffset.getStartOffset(n);
        n.nodeOptional.accept(this);
        n.declarator.accept(this);
        n.nodeOptional1.accept(this);
        int headerEnd = lastEndOffset;
        int start = BeginOffset.getStartOffset(n.compoundStatement);
        pushHierarchy();
        n.compoundStatement.accept(this);
        int end = lastEndOffset;
        popHierachy(HierachyLevelTypes.getFunctionType(builder), Helper.findFunctionName(n),
                start, end, headerStart, headerEnd);
    }

    @Override
    public void visit(TranslationUnit n) {
        pushHierarchy();
        super.visit(n);
        popHierachy(HierachyLevelTypes.getSourceFileType(builder), sourceFile.getFileName(),
                0, Math.min(lastEndOffset, sourceFile.getContent().length()), -1, -1);
        // TODO find fix
        if(!qmoSet.isEmpty()) {
            String loc = "";
            for(QuestionMarkOperator op : qmoSet) {
                loc += op.getLocation().toString() + ", ";
            }
            System.out.println("Instrumenting conditional expressions in declarations " +
                    "without a following statement is not supported (" + loc + ")");
            qmoSet.clear();
        }
    }

    @Override
    public void visit(IfStatement n) {
        List<Branch> branchList = new ArrayList<Branch>(2);
        n.condID = cm.newCondID();
        n.branchID = cm.newBranchID();
        // another ID for the else part
        cm.newBranchID();

        n.nodeToken.accept(this);
        n.nodeToken1.accept(this);
        n.terms = expressionParser.visit(n.expression);
        RootTerm rootTerm = builder.createRootTerm(n.terms.toBooleanTerm(builder, sourceFile),
                createCoverableItem(cm.condID(n.condID)));
        n.expression.accept(this);
        n.nodeToken2.accept(this);

        pushStatementLevel();
        n.statement.accept(this);
        branchList.add(createBranch(cm.branchID(n.branchID),
                BeginOffset.getStartOffset(n.statement), lastEndOffset, -1, -1, false));

        pushStatementLevel();

        if(n.nodeOptional.present()) {
            n.nodeOptional.accept(this);
            NodeToken elseNode = (NodeToken) ((NodeSequence)n.nodeOptional.node).elementAt(0);
            branchList.add(createBranch(cm.branchID(n.branchID +1),
                    BeginOffset.getStartOffset(n.nodeOptional), lastEndOffset,
                    elseNode.beginOffset, elseNode.endOffset,
                    false));
        } else {
            branchList.add(createBranch(cm.branchID(n.branchID +1),
                    -1,-1, -1,-1, true));
        }

        createConditionalStatement(
                cm.stmtID(((Statement)n.getParent().getParent()).stmtID),
                n.nodeToken.beginOffset, lastEndOffset,
                rootTerm, branchList,
                n.nodeToken.beginOffset, n.nodeToken.endOffset
                );
    }

    private Stack<List<Branch>> branchesStack = new Stack<List<Branch>>();
    private Stack<Boolean> hasDefaultStack = new Stack<Boolean>();

    @Override
    public void visit(SwitchStatement n) {
        n.nodeToken.accept(this);
        n.nodeToken1.accept(this);
        n.expression.accept(this);
        n.nodeToken2.accept(this);
        hasDefaultStack.push(false);
        branchesStack.push(new ArrayList<Branch>());
        n.statement.accept(this);

        List<Branch> branches = branchesStack.pop();

        // Misuse the branchID field of the SwitchStatement so that we can pass the ID of the implicit default branch.
        if(!hasDefaultStack.pop()) {
            n.branchID = cm.newBranchID();
            // We need another statement level, so that the branch can pop one
            pushStatementLevel();
            branches.add(createBranch(cm.branchID(n.branchID),-1,-1,-1,-1, true));
        } else {
            n.branchID = -1;
        }

        createConditionalStatement(
                cm.stmtID(((Statement) n.getParent().getParent()).stmtID),
                n.nodeToken.beginOffset, lastEndOffset,
                null, branches,
                n.nodeToken.beginOffset, n.nodeToken.endOffset
        );
    }



    @Override
    public void visit(CaseStatement n) {
        n.branchID = cm.newBranchID();

        n.nodeToken.accept(this);
        n.constantExpression.accept(this);
        int decisionEnd = lastEndOffset;
        n.nodeToken1.accept(this);
        pushStatementLevel();
        n.statement.accept(this);

        branchesStack.peek().add(createBranch(cm.branchID(n.branchID),
                n.nodeToken.beginOffset, lastEndOffset, n.nodeToken.beginOffset, decisionEnd, false));
    }

    @Override
    public void visit(DefaultStatement n) {
        n.branchID = cm.newBranchID();

        n.nodeToken.accept(this);
        n.nodeToken1.accept(this);
        pushStatementLevel();
        n.statement.accept(this);

        branchesStack.peek().add(createBranch(cm.branchID(n.branchID),
                n.nodeToken.beginOffset, lastEndOffset,
                n.nodeToken.beginOffset, n.nodeToken.endOffset,
                false));
    }

    @Override
    public void visit(WhileStatement n) {
        n.nodeToken.accept(this);
        n.nodeToken1.accept(this);
        n.terms = expressionParser.visit(n.expression);
        n.expression.accept(this);
        n.nodeToken2.accept(this);
        pushStatementLevel();
        n.statement.accept(this);

        n.condID = cm.newCondID();
        n.loopID = cm.newloopID();
        // Stmt ID from the parent Statement
        n.stmtID = ((CCNode) n.getParent().getParent()).stmtID;

        RootTerm rootTerm = builder.createRootTerm(n.terms.toBooleanTerm(builder, sourceFile),
                createCoverableItem(cm.condID(n.condID)));

        createLoop(rootTerm,
                n.nodeToken.beginOffset, lastEndOffset,
                n.nodeToken.beginOffset, n.nodeToken.endOffset,
                n.stmtID, n.loopID, true);
    }

    @Override
    public void visit(DoStatement n) {
        n.nodeToken.accept(this);
        pushStatementLevel();
        n.statement.accept(this);
        n.nodeToken1.accept(this);
        n.nodeToken2.accept(this);
        n.terms = expressionParser.visit(n.expression);
        n.expression.accept(this);
        n.nodeToken3.accept(this);
        n.nodeToken4.accept(this);

        n.loopID = cm.newloopID();
        n.condID = cm.newCondID();
        // Stmt ID from the parent Statement
        n.stmtID = ((CCNode) n.getParent().getParent()).stmtID;

        RootTerm rootTerm = builder.createRootTerm(n.terms.toBooleanTerm(builder, sourceFile),
                createCoverableItem(cm.condID(n.condID)));

        createLoop(rootTerm,
                n.nodeToken.beginOffset, n.nodeToken4.endOffset,
                n.nodeToken.beginOffset, n.nodeToken.endOffset,
                n.stmtID, n.loopID, false);
    }

    @Override
    public void visit(ForStatement n) {
        n.nodeToken.accept(this);
        n.nodeToken1.accept(this);
        n.nodeChoice.accept(this);
        n.nodeToken2.accept(this);
        n.nodeOptional.accept(this);
        n.nodeToken3.accept(this);
        pushStatementLevel();
        n.statement.accept(this);

        n.loopID = cm.newloopID();
        n.condID = cm.newCondID();
        // Stmt ID from the parent Statement
        n.stmtID = ((CCNode) n.getParent().getParent()).stmtID;

        createLoop(null,
                n.nodeToken.beginOffset, lastEndOffset,
                n.nodeToken.beginOffset, n.nodeToken.endOffset,
                n.stmtID, n.loopID, true);
    }

    @Override
    public void visit(ConditionalExpression n) {
        if(inConstantExpression) {
            super.visit(n);
            return;
        }
        n.logicalORExpression.accept(this);

        if(n.nodeOptional.present()) {
            ConditionalExpressionRightSide r = (ConditionalExpressionRightSide) n.nodeOptional.node;
            r.qmoID = cm.newQmoID();
            r.nodeToken.accept(this);
            int exp1begin = BeginOffset.getStartOffset(r.expression);
            r.expression.accept(this);
            QuestionMarkOperatorExpression exp1 = new QuestionMarkOperatorExpression(
                    createLocationList(exp1begin, lastEndOffset),
                    createCoverableItem(cm.qmoID(r.qmoID) + "-0"));
            r.nodeToken1.accept(this);
            int exp2begin = BeginOffset.getStartOffset(r.conditionalExpression);
            r.conditionalExpression.accept(this);
            QuestionMarkOperatorExpression exp2 = new QuestionMarkOperatorExpression(
                    createLocationList(exp2begin, lastEndOffset),
                    createCoverableItem(cm.qmoID(r.qmoID) + "-1"));

            QuestionMarkOperator qmo = new QuestionMarkOperator(
                    createLocationList(r.nodeToken.beginOffset, r.nodeToken.endOffset),
                    createCoverableItem(cm.qmoID(r.qmoID)), exp1, exp2);
            qmoSet.add(qmo);
        }
    }

    boolean inConstantExpression = false;

    @Override
    public void visit(ConstantExpression n) {
        inConstantExpression = true;
        n.conditionalExpression.accept(this);
        inConstantExpression = false;
    }

    @Override
    public void visit(NodeToken n) {
        lastEndOffset = n.endOffset;
    }
}
