///////////////////////////////////////////////////////////////////////////////
//
// $Id: InstrumentationVisitorWithException.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 24.02.2007 17:30:00
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.visitor;

import java.io.IOException;
import java.io.Writer;
import java.util.Vector;

import org.gbt2.instrumentation.java15.counter.CounterIDManager;
import org.gbt2.instrumentation.java15.manipulators.BranchManipulator;
import org.gbt2.instrumentation.java15.manipulators.ConditionManipulator;
import org.gbt2.instrumentation.java15.manipulators.DummyBranchManipulator;
import org.gbt2.instrumentation.java15.manipulators.DummyConditionManipulator;
import org.gbt2.instrumentation.java15.manipulators.DummyLoopManipulator;
import org.gbt2.instrumentation.java15.manipulators.DummyStatementManipulator;
import org.gbt2.instrumentation.java15.manipulators.LoopManipulator;
import org.gbt2.instrumentation.java15.manipulators.Manipulator;
import org.gbt2.instrumentation.java15.manipulators.StatementManipulator;
import org.gbt2.instrumentation.java15.measurement.CoverageResultLog;
import org.gbt2.instrumentation.java15.syntaxtree.Block;
import org.gbt2.instrumentation.java15.syntaxtree.BlockStatement;
import org.gbt2.instrumentation.java15.syntaxtree.BreakStatement;
import org.gbt2.instrumentation.java15.syntaxtree.ClassOrInterfaceBody;
import org.gbt2.instrumentation.java15.syntaxtree.ClassOrInterfaceBodyDeclaration;
import org.gbt2.instrumentation.java15.syntaxtree.ClassOrInterfaceDeclaration;
import org.gbt2.instrumentation.java15.syntaxtree.ContinueStatement;
import org.gbt2.instrumentation.java15.syntaxtree.DoStatement;
import org.gbt2.instrumentation.java15.syntaxtree.EmptyStatement;
import org.gbt2.instrumentation.java15.syntaxtree.FieldDeclaration;
import org.gbt2.instrumentation.java15.syntaxtree.ForStatement;
import org.gbt2.instrumentation.java15.syntaxtree.FormalParameter;
import org.gbt2.instrumentation.java15.syntaxtree.IfStatement;
import org.gbt2.instrumentation.java15.syntaxtree.LocalVariableDeclaration;
import org.gbt2.instrumentation.java15.syntaxtree.Modifiers;
import org.gbt2.instrumentation.java15.syntaxtree.Node;
import org.gbt2.instrumentation.java15.syntaxtree.NodeChoice;
import org.gbt2.instrumentation.java15.syntaxtree.NodeListOptional;
import org.gbt2.instrumentation.java15.syntaxtree.NodeSequence;
import org.gbt2.instrumentation.java15.syntaxtree.PackageDeclaration;
import org.gbt2.instrumentation.java15.syntaxtree.ReturnStatement;
import org.gbt2.instrumentation.java15.syntaxtree.Statement;
import org.gbt2.instrumentation.java15.syntaxtree.StatementExpression;
import org.gbt2.instrumentation.java15.syntaxtree.SwitchLabel;
import org.gbt2.instrumentation.java15.syntaxtree.SwitchStatement;
import org.gbt2.instrumentation.java15.syntaxtree.TryStatement;
import org.gbt2.instrumentation.java15.syntaxtree.Type;
import org.gbt2.instrumentation.java15.syntaxtree.VariableDeclarator;
import org.gbt2.instrumentation.java15.syntaxtree.WhileStatement;

public class InstrumentationVisitorWithException extends TreeDumperWithException {
    /**
     * A class that is used as the {@link CoverageResultLog} when instumenting.
     */
    private Class<? extends CoverageResultLog> coverageResultLog;

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
     * basic boolean terms in if, while, do while, for and trinariy operator.
     */
    private ConditionManipulator conditionManipulator;

    /**
     * The {@link LoopManipulator} which is responsible for instrumenting
     * loops.
     */
    private LoopManipulator loopManipulator;

    /**
     * For every top level type declaration a {@link CounterIDManager} is used
     * to handle ID's for statements, branches and so on. It is responsible for
     * creating the inner class Gbt2CoverageCounter.
     */
    private CounterIDManager counterIDManager;

    /**
     * The name of the java package. This is parsed during the visitin.
     */
    private String packageName;
    
    /**
     * The class name of the current visiting.
     */
    private String topLevelClassName;
    
    /**
     * The full name of the class <b>packageName.className</b>
     */
    private String topLevelClassFullName;

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
     * Is the current visiting proceeding within a top level type declaration?
     * This might be a class or an interface directly in the source file.
     */
    private boolean inTopLevelTypeDeclaration;

    /**
     * Is the current visiting proceeding within a class or interface?
     * <ul>
     * <li>null &rarr; neither nor</li>
     * <li>false &rarr; we are in an interface</li>
     * <li>true &rarr; we are in a class</li>
     * </ul>
     */
    private Boolean inClass;

    /**
     * Constructs a new {@link InstrumentationVisitorWithException}.<br>
     * <br>
     * Some fields are initialized and DummyManipulators set.
     * 
     * @param writer
     *            This writer is delegated to the parent class
     *            {@link TreeDumperWithException}.
     * @param coverageResultLog
     *            A class that is used as the {@link CoverageResultLog} when
     *            instumenting.
     */
    public InstrumentationVisitorWithException(Writer writer,
            Class<? extends CoverageResultLog> coverageResultLog) {
        super(writer);
        this.coverageResultLog = coverageResultLog;
        this.statementManipulator = new DummyStatementManipulator();
        this.branchManipulator = new DummyBranchManipulator();
        this.conditionManipulator = new DummyConditionManipulator();
        this.loopManipulator = new DummyLoopManipulator();
        this.counterIDManager = null;
        this.packageName = null;
        this.topLevelClassName = null;
        this.inTopLevelTypeDeclaration = false;
        this.inClass = null;

        resetExpandToBlock();
    }

    private void resetExpandToBlock() {
        this.needToExpandBranchesToBlock = false;
        this.needToExpandBranchesToBlock |= this.statementManipulator
                .requiresBlockExpansionsForBranches();
        this.needToExpandBranchesToBlock |= this.branchManipulator
                .requiresBlockExpansionsForBranches();
        this.needToExpandBranchesToBlock |= this.conditionManipulator
                .requiresBlockExpansionsForBranches();
        this.needToExpandBranchesToBlock |= this.loopManipulator
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
    }

    /**
     * If the {@link StatementManipulator} needs block statements, we get it
     * here. See {@link Manipulator#requiresBlockExpansionsForBranches()} and
     * {@link Manipulator#requiresBlockExpansionsForLoops()}.
     * 
     * @param statementManipulator
     *            The statementManipulator to set.
     * 
     */
    public void setStatementManipulator(
            StatementManipulator statementManipulator) {
        this.statementManipulator = statementManipulator;
        this.statementManipulator.setWriter(super.originalWriter);

        resetExpandToBlock();
    }
    
    /**
     * If the {@link BranchManipulator} needs block statements, we get it
     * here. See {@link Manipulator#requiresBlockExpansionsForBranches()} and
     * {@link Manipulator#requiresBlockExpansionsForLoops()}.
     * 
     * @param branchManipulator
     *            The branchManipulator to set.
     * 
     */
    public void setBranchManipulator(
            BranchManipulator branchManipulator) {
        this.branchManipulator = branchManipulator;
        this.branchManipulator.setWriter(super.originalWriter);

        resetExpandToBlock();
    }
    
    /**
     * If the {@link ConditionManipulator} needs block statements, we get it
     * here. See {@link Manipulator#requiresBlockExpansionsForBranches()} and
     * {@link Manipulator#requiresBlockExpansionsForLoops()}.
     * 
     * @param conditionManipulator
     *            The conditionManipulator to set.
     * 
     */
    public void setConditionManipulator(
            ConditionManipulator conditionManipulator) {
        this.conditionManipulator = conditionManipulator;
        this.conditionManipulator.setWriter(super.originalWriter);
        
        resetExpandToBlock();
    }

    /**
     * If the {@link LoopManipulator} needs block statements, we get it here.
     * See {@link Manipulator#requiresBlockExpansionsForBranches()} and
     * {@link Manipulator#requiresBlockExpansionsForLoops()}.
     * 
     * @param loopManipulator
     *            The loopManipulator to set.
     * 
     */
    public void setLoopManipulator(LoopManipulator loopManipulator) {
        this.loopManipulator = loopManipulator;
        this.loopManipulator.setWriter(super.originalWriter);

        resetExpandToBlock();
    }

    // /////////////////////////////////////////////////////////////////////////
    //
    // All visit statements for instrumentation purpose
    //
    // /////////////////////////////////////////////////////////////////////////

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
        super.switchToTempMode();
        n.f2.accept(this);
        this.packageName = super.backFromTempMode().trim();

        super.visit(n);
    }

    /**
     * Handles, whether in a top level type declaration or not.
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
        this.inClass = new Boolean(n.f0.which == 0);

        if (!this.inTopLevelTypeDeclaration) {
            this.inTopLevelTypeDeclaration = true;
            this.topLevelClassName = n.f1.tokenImage;
            if (this.packageName != null) {
                this.topLevelClassFullName = this.packageName + "."
                + this.topLevelClassName;
            } else {
                this.topLevelClassFullName = this.topLevelClassName;
            }

            this.counterIDManager = new CounterIDManager(
                    this.topLevelClassFullName, super.originalWriter,
                    this.coverageResultLog);
            this.statementManipulator
                    .setCounterIDManager(this.counterIDManager);
            this.branchManipulator.setCounterIDManager(this.counterIDManager);
            this.conditionManipulator
                    .setCounterIDManager(this.counterIDManager);
            this.loopManipulator.setCounterIDManager(this.counterIDManager);
            n.isTopLevelTypeDeclaration = true;

            super.visit(n);

            this.counterIDManager = null;
            this.inTopLevelTypeDeclaration = false;
            this.topLevelClassName = null;
            this.topLevelClassFullName = null;
        } else {
            n.isTopLevelTypeDeclaration = false;
            super.visit(n);
        }

        this.inClass = null;
    }

    /**
     * Adds the inner counting class, if this ClassOrInterfaceBody is in a top
     * level type declaration.
     * 
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
        Node parent = n.getParent();
        if (parent instanceof ClassOrInterfaceDeclaration) {
            ClassOrInterfaceDeclaration coiDecl = (ClassOrInterfaceDeclaration) n
                    .getParent();
            if (coiDecl.isTopLevelTypeDeclaration) {
                n.f0.accept(this);
                this.counterIDManager.writeStaticBlock();

                n.f1.accept(this);

                super.targetNow.write(LINE_SEPERATOR);
                this.counterIDManager.writeInnerClass();

                n.f2.accept(this);
            } else {
                super.visit(n);
                
            }
        } else {
            super.visit(n);
        }
    }

    /**
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
        case 3: { // EmptyStatement
            n.statementID = this.counterIDManager.nextStatementID();
            super.visit(n);
            EmptyStatement emptyStatement = (EmptyStatement) n.f0.choice;
            this.statementManipulator.manipulate(emptyStatement, n.statementID);
            break StatementSwitch;
        }
        case 4: { // StatementExpression
            n.statementID = this.counterIDManager.nextStatementID();
            super.visit(n);
            NodeSequence nodeList = (NodeSequence) n.f0.choice;
            StatementExpression statementExpression = (StatementExpression) nodeList.nodes
                    .get(0);
            this.statementManipulator.manipulate(statementExpression,
                    n.statementID);
            break StatementSwitch;
        }
        case 5: { // SwitchStatement
            n.statementID = this.counterIDManager.nextStatementID();
            super.visit(n);
            break StatementSwitch;
        }
        case 6: { // IfStatement
            n.statementID = this.counterIDManager.nextStatementID();
            super.visit(n);
            break StatementSwitch;
        }
        case 7: { // WhileStatement
            n.statementID = this.counterIDManager.nextStatementID();
            super.visit(n);
            break StatementSwitch;
        }
        case 8: { // DoStatement
            n.statementID = this.counterIDManager.nextStatementID();
            super.visit(n);
            break StatementSwitch;
        }
        case 9: { // ForStatement
            n.statementID = this.counterIDManager.nextStatementID();
            super.visit(n);
            break StatementSwitch;
        }
        case 10: { // BreakStatement
            n.statementID = this.counterIDManager.nextStatementID();
            BreakStatement breakStatement = (BreakStatement) n.f0.choice;
            this.statementManipulator.manipulate(breakStatement, n.statementID);
            super.visit(n);
            break StatementSwitch;
        }
        case 11: { // ContinueStatement
            n.statementID = this.counterIDManager.nextStatementID();
            ContinueStatement continueStatement = (ContinueStatement) n.f0.choice;
            this.statementManipulator.manipulate(continueStatement,
                    n.statementID);
            super.visit(n);
            break StatementSwitch;
        }
        case 12: { // ReturnStatement
            n.statementID = this.counterIDManager.nextStatementID();
            ReturnStatement returnStatement = (ReturnStatement) n.f0.choice;
            if (!returnStatement.f1.present()) {
                // only if there is not return expression, return is
                // instrumentable
                this.statementManipulator.manipulate(returnStatement,
                        n.statementID);
            }
            super.visit(n);
            break StatementSwitch;
        }
        case 15: { // Try Statement
            n.statementID = this.counterIDManager.nextStatementID();
            super.visit(n);
            break StatementSwitch;
        }
        default: {
            super.visit(n);
            break StatementSwitch;
        }
        }
    }
    
    /**
     * Instrumentation of {@link LocalVariableDeclaration} using
     * {@link #statementManipulator}.
     * 
     * <PRE>
     * 
     * f0 -> LocalVariableDeclaration() ";" |
     *       Statement() |
     *       ClassOrInterfaceDeclaration()
     * 
     * </PRE>
     */
    @Override
    public void visit(BlockStatement n) throws IOException {
        if (n.f0.which == 0) {
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

            super.visit(n);

            if (foundExpression) {
                String statementID = this.counterIDManager.nextStatementID();
                this.statementManipulator.manipulate(locVarDeclaration, statementID);
            }
        } else {
            super.visit(n);
        }
    }

    /**
     * Instrumentation of {@link FieldDeclaration} using
     * {@link #statementManipulator}.
     * 
     * <PRE>
     * f0 -> Initializer() |
     *       Modifiers() ( ClassOrInterfaceDeclaration() |
     *                     EnumDeclaration() |
     *                     ConstructorDeclaration() |
     *                     FieldDeclaration() |
     *                     MethodDeclaration()
     *                   ) |
     *      ";"
     * </PRE>
     * 
     * @throws IOException
     */
    @Override
    public void visit(ClassOrInterfaceBodyDeclaration n) throws IOException {
        if (n.f0.which == 1) {
            // we are in Modifiers() (.. | ..)
            NodeSequence unknownDeclarationSequence = (NodeSequence) n.f0.choice;
            Modifiers modifiers = (Modifiers) unknownDeclarationSequence.nodes.get(0);
            NodeChoice unknownDeclarationChoice = (NodeChoice) unknownDeclarationSequence.nodes.get(1);
            
            if (unknownDeclarationChoice.which == 3) {
                // we have a FieldDeclaration success
                FieldDeclaration fieldDeclaration = (FieldDeclaration) unknownDeclarationChoice.choice;
                
                // we have to get to know, whether there are expressions and
                // assignments used here.
                VariableDeclarator variableDeclarator = fieldDeclaration.f1;
                boolean foundExpression = variableDeclarator.f1.present();

                if (!foundExpression && fieldDeclaration.f2.present()) {
                    Vector<Node> declarations = fieldDeclaration.f2.nodes;
                    for (Node thisNode : declarations) {
                        NodeSequence nodeSequence = (NodeSequence) thisNode;
                        variableDeclarator = (VariableDeclarator) nodeSequence.nodes.get(1);
                        foundExpression |= variableDeclarator.f1.present();
                    }
                }

                super.visit(n);

                if (foundExpression) {
                    String statementID = this.counterIDManager.nextStatementID();

                    // we have to get to know, whether there is a static
                    // modifier
                    super.switchToTempMode();
                    super.visit(modifiers);
                    String modifierString = super.backFromTempMode();
                    boolean isStaticPresent = modifierString.contains("static");

                    this.statementManipulator.manipulate(fieldDeclaration,
                            isStaticPresent, statementID);
                }
            } else {
                // not in FieldDeclaration
                super.visit(n);
            }
        } else {
            // not in Modifiers() (.. | ..)
            super.visit(n);
        }
    }

    /**
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
        String ifBranchID = this.counterIDManager.nextBranchID();
        String elseBranchID = this.counterIDManager.nextBranchID();

        n.f0.accept(this);
        n.f1.accept(this);
        n.f2.accept(this);
        n.f3.accept(this);
        
        // now we have to check the Statement()
        Statement ifStatement = n.f4;

        if (ifStatement.f0.choice instanceof Block) {
            // if it is a block statement, we have to call the branch
            // manipulator after the openening bracket
            Block ifBlock = (Block) ifStatement.f0.choice;

            // {
            ifBlock.f0.accept(this);
            super.targetNow.write(LINE_SEPERATOR);
            this.branchManipulator.manipulateIf(n, ifBranchID);
            // ( BlockStatement() )*
            ifBlock.f1.accept(this);
            // }
            super.targetNow.write(LINE_SEPERATOR);
            ifBlock.f2.accept(this);
        } else {
            // the if statement is not a block, possibly we have to expand it to
            // a block
            if (this.needToExpandBranchesToBlock) {
                super.targetNow.write(" {");
                super.targetNow.write(LINE_SEPERATOR);
                this.branchManipulator.manipulateIf(n, ifBranchID);
                ifStatement.accept(this);
                super.targetNow.write(LINE_SEPERATOR);
                super.targetNow.write("}");
            } else {
                // this should add nothing, cause otherwise we have a default
                // branch manipulator and needToExpandBranchesToBlock set to
                // false -> this would be very bad
                this.branchManipulator.manipulateIf(n, ifBranchID);
                ifStatement.accept(this);
            }
        } // ! (ifStatement.f0.choice instanceof Block)

        if (n.f5.present()) {
            // the else is present

            NodeSequence elseSequence = (NodeSequence) n.f5.node;
            // write the "else"
            elseSequence.nodes.get(0).accept(this);

            // now we have to check the Statement()
            Statement elseStatement = (Statement) elseSequence.nodes.get(1);

            if (elseStatement.f0.choice instanceof Block) {
                // if it is a block statement, we have to call the branch
                // manipulator after the openening bracket
                Block elseBlock = (Block) elseStatement.f0.choice;

                // {
                elseBlock.f0.accept(this);
                super.targetNow.write(LINE_SEPERATOR);
                this.branchManipulator.manipulateElse(n, elseBranchID, false);
                // ( BlockStatement() )*
                elseBlock.f1.accept(this);
                // }
                elseBlock.f2.accept(this);
            } else {
                // the else statement is not a block, possibly we have to expand
                // it to a block
                if (this.needToExpandBranchesToBlock) {
                    super.targetNow.write(" {");
                    super.targetNow.write(LINE_SEPERATOR);
                    this.branchManipulator.manipulateElse(n, elseBranchID, false);
                    elseStatement.accept(this);
                    super.targetNow.write(LINE_SEPERATOR);
                    super.targetNow.write("}");
                } else {
                    // this should add nothing, cause otherwise we have a default
                    // branch manipulator and needToExpandBranchesToBlock set to
                    // false -> this would be very bad
                    this.branchManipulator.manipulateElse(n, elseBranchID, false);
                    elseStatement.accept(this);
                }
            } // ! (elseStatement.f0.choice instanceof Block)
        } else {
            // there is no else branch -> maybe, we have to add one
            this.branchManipulator.manipulateElse(n, elseBranchID, true);
        }
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
        boolean isDefaultPresent = false;

        n.f0.accept(this);
        n.f1.accept(this);
        n.f2.accept(this);
        n.f3.accept(this);
        n.f4.accept(this);

        // we do not access n.f5 directly
        NodeListOptional branches = n.f5;
        for (Node thisBranch : branches.nodes) {
            NodeSequence branchSequence = (NodeSequence) thisBranch;
            SwitchLabel switchLabel = (SwitchLabel) branchSequence.nodes.get(0);
            NodeListOptional blockStatements = (NodeListOptional) branchSequence.nodes.get(1);
            
            switchLabel.accept(this);

            if (switchLabel.f0.which == 1) {
                // we found a "default" branch
                isDefaultPresent = true;
                String caseBranchID = this.counterIDManager.nextBranchID();
                this.branchManipulator.manipulateSwitchCase(switchLabel, caseBranchID);
            } else {
                String defaultBranchID = this.counterIDManager.nextBranchID();
                this.branchManipulator.manipulateSwitchDefault(n, defaultBranchID, false);
            }

            blockStatements.accept(this);
        }

        if (!isDefaultPresent) {
            String defaultBranchID = this.counterIDManager.nextBranchID();
            this.branchManipulator.manipulateSwitchDefault(n, defaultBranchID, true);
        }

        n.f6.accept(this);
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
        boolean isThrowablePresent = false;

        // this is the branchID for the succesfull finish of the try branch
        String branchID = this.counterIDManager.nextBranchID();
        String tryBranchID = this.counterIDManager.nextTryBranchID();

        // try
        n.f0.accept(this);
        // try Block()
        
        Block tryBlock = n.f1;
        
        // {
        tryBlock.f0.accept(this);
        this.branchManipulator.manipulateTry(n, branchID, tryBranchID);
        // ( BlockStatement() )*
        tryBlock.f1.accept(this);
        // }
        tryBlock.f2.accept(this);
        
        // we do not accept n.f2 directly - we do it catch by catch
        NodeListOptional catchBranches = n.f2;
        for (Node catchBranch : catchBranches.nodes) {
            NodeSequence catchSequence = (NodeSequence) catchBranch;
            Vector<Node> nodes = catchSequence.nodes;

            // catch
            nodes.get(0).accept(this);
            // (
            nodes.get(1).accept(this);

            // we want to get to know, whether a Throwable is catched
            FormalParameter formalParameter = (FormalParameter) nodes.get(2);
            Type catchType = formalParameter.f1;
            super.switchToTempMode();
            catchType.accept(this);
            String catchTypeName = super.backFromTempMode().trim();

            if (catchTypeName.equals("Throwable") || catchTypeName.equals("java.lang.Throwable")){
                isThrowablePresent = true;
            }

            formalParameter.accept(this);
            // )
            nodes.get(3).accept(this);

            Block catchBlock = (Block) nodes.get(4);
            String catchBranchId = this.counterIDManager.nextBranchID();
            
            // {
            catchBlock.f0.accept(this);
            this.branchManipulator.manipulateCatch(n, catchType, catchBranchId, tryBranchID);
            // ( BlockStatement() )*
            catchBlock.f1.accept(this);
            // }
            catchBlock.f2.accept(this);
        }

        // if there is no catch (Throwable ), we have to create one
        if (!isThrowablePresent) {
            String throwableBranchId = this.counterIDManager.nextBranchID();
            this.branchManipulator.manipulateAddThrowable(n, throwableBranchId, tryBranchID);
        }

        // we do not access the finally direct
        if (n.f3.present()) {
            NodeSequence finallySequence = (NodeSequence) n.f3.node;

            // finally
            finallySequence.nodes.get(0).accept(this);

            // the block
            Block finallyBlock = (Block) finallySequence.nodes.get(1);
            // {
            finallyBlock.f0.accept(this);
            this.branchManipulator.manipulateFinally(n, branchID, tryBranchID, false);
            // ( BlockStatement() )*
            finallyBlock.f1.accept(this);
            // }
            finallyBlock.f2.accept(this);
        } else {
            this.branchManipulator.manipulateFinally(n, branchID, tryBranchID, true);
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
        String primaryLoopID = this.counterIDManager.nextLoopID();
        
        this.loopManipulator.manipulateBefore(n, primaryLoopID);

        // for ( .. )
        n.f0.accept(this);
        n.f1.accept(this);
        n.f2.accept(this);
        n.f3.accept(this);

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
                super.targetNow.write(" { ");
            }
            this.loopManipulator.manipulateInner(n, primaryLoopID);
            n.f4.accept(this);
            if (this.needToExpandLoopsToBlock) {
                super.targetNow.write(LINE_SEPERATOR);
                super.targetNow.write("  }");
            }
        }
        this.loopManipulator.manipulateAfter(n, primaryLoopID);
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
        String primaryLoopID = this.counterIDManager.nextLoopID();

        this.loopManipulator.manipulateBefore(n, primaryLoopID);

        // while ( Expression() )
        n.f0.accept(this);
        n.f1.accept(this);
        n.f2.accept(this);
        n.f3.accept(this);

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
                super.targetNow.write(" { ");
            }
            this.loopManipulator.manipulateInner(n, primaryLoopID);
            n.f4.accept(this);
            if (this.needToExpandLoopsToBlock) {
                super.targetNow.write(LINE_SEPERATOR);
                super.targetNow.write("  }");
            }
        }
        this.loopManipulator.manipulateAfter(n, primaryLoopID);
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
        String primaryLoopID = this.counterIDManager.nextLoopID();
        
        this.loopManipulator.manipulateBefore(n, primaryLoopID);

        // do
        n.f0.accept(this);
        
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
                super.targetNow.write(" { ");
            }
            this.loopManipulator.manipulateInner(n, primaryLoopID);
            n.f1.accept(this);
            if (this.needToExpandLoopsToBlock) {
                super.targetNow.write(LINE_SEPERATOR);
                super.targetNow.write("  }");
            }
        }

        // while ( Expression() ) ;
        n.f2.accept(this);
        n.f3.accept(this);
        n.f4.accept(this);
        n.f5.accept(this);
        n.f6.accept(this);
        
        this.loopManipulator.manipulateAfter(n, primaryLoopID);
    }
}
