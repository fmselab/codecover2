///////////////////////////////////////////////////////////////////////////////
//
// $Id: InstrumentationVisitor.java 1 2007-12-12 17:37:26Z t-scheller $
// 
///////////////////////////////////////////////////////////////////////////////

/*
 * Packet: org.gbt2.instrumentation.cobol85.visitor
 * Datei:  InstrumentVisitor.java
 */
package org.gbt2.instrumentation.cobol85.visitor;

import java.io.Writer;
import java.util.Enumeration;
import java.util.Vector;

import org.gbt2.instrumentation.cobol85.manipulators.*;
import org.gbt2.instrumentation.cobol85.syntaxtree.*;

/**
 * This class does the real instrumentation work. It is a child of a deth first
 * visitor and overides methods for instrumentation.
 * 
 * @author Stefan Franke
 * @version 1.0 - 23.03.2007
 * 
 */
public class InstrumentationVisitor extends TreeDumper {

    private static final String END_TEST_CASE = "*>ENDTESTCASE";

    /**
     * Specifies the start test case comment string
     */
    public static final String START_TEST_CASE = "*>STARTTESTCASE";

    private BranchManipulator branchManipulator;

    private ConditionManipulator conditionManipulator;

    private Vector<CompilationUnit> editedCompilationUnits;

    private String fileName;

    private LoopManipulator loopManipulator;

    private String progName;

    private StatementManipulator statementManipulator;

    private StaticPartManipulator staticPartManipulator;

    /**
     * Constructor
     * 
     * @param writer
     *            target writer object
     */
    public InstrumentationVisitor(Writer writer) {
        super(writer);
        this.statementManipulator = new DummyStatementManipulator();
        this.branchManipulator = new DummyBranchManipulator();
        this.conditionManipulator = new DummyConditionManipulator();
        this.loopManipulator = new DummyLoopManipulator();
        this.staticPartManipulator = new DummyStaticPartManipulator();
        this.editedCompilationUnits = new Vector<CompilationUnit>();
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
                if (nToken.tokenImage.startsWith(START_TEST_CASE)) {
                    this.staticPartManipulator.replaceStartTestCase(nToken,
                            super.out);
                } else if (nToken.tokenImage.startsWith(END_TEST_CASE)) {
                    this.staticPartManipulator.replaceEndTestCase(super.out);
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
     * Writes the end of program section to output. End of program section
     * contains start and end test case paragraph, stop the program now
     * paragraph and write to disk paragraphs.
     * 
     * @param endProgramStatement
     *            token to be considered
     */
    @Override
    public void visit(EndProgramStatement endProgramStatement) {
        if (endProgramStatement.getParent().getParent() instanceof NodeOptional) {
            this.staticPartManipulator.generateWriteLogic(super.out);
            this.editedCompilationUnits
                    .add((CompilationUnit) endProgramStatement.getParent()
                            .getParent().getParent());
        }
        super.visit(endProgramStatement);
    }

    /**
     * Includes branch counter to evaluate-statements.
     * 
     * @param evaluateStatement
     *            token to be considered
     */
    @Override
    public void visit(EvaluateStatement evaluateStatement) {
        super.visit(evaluateStatement.f0);
        super.visit(evaluateStatement.f1);
        super.visit(evaluateStatement.f2);
        for (Node node : evaluateStatement.f3.nodes) {
            NodeSequence nodeSequence = (NodeSequence) node;
            /*
             * nodeSequence.nodes.get(0) is a NodeList object containing the
             * when-phrase
             */
            super.visit((NodeList) nodeSequence.nodes.get(0));
            this.branchManipulator.manipulate(super.out);
            /*
             * nodeSequence.nodes.get(1) is a NodeList object containing the
             * statement list of that when-phrase
             */
            super.visit((StatementList) nodeSequence.nodes.get(1));
        }
        NodeSequence nodeSequence = (NodeSequence) evaluateStatement.f4.node;
        if (nodeSequence != null) {
            /*
             * Source code contains when-other-branch.
             */
            super.visit((NodeToken) nodeSequence.nodes.get(0));
            super.visit((NodeToken) nodeSequence.nodes.get(1));
            this.branchManipulator.manipulate(super.out);
            super.visit((StatementList) nodeSequence.nodes.get(2));
        } else {
            this.branchManipulator.generateWhenOtherBranch(super.out);
        }
        if (evaluateStatement.f5.node != null) {
            /*
             * Source code contains end if keyword.
             */
            super.visit(evaluateStatement.f5);
        } else {
            /*
             * Source code does not contain end if keyword.
             */
            this.branchManipulator.generateEndEvaluate(super.out);
        }
    }

    /**
     * Includes branch and condition counter to if-statements.
     * 
     * @param ifStatement
     *            token to be considered
     */
    @Override
    public void visit(IfStatement ifStatement) {
        this.conditionManipulator.manipulate(super.out);
        super.visit(ifStatement.f0);
        super.visit(ifStatement.f1);
        super.visit(ifStatement.f2);
        this.branchManipulator.manipulate(super.out);
        if (ifStatement.f3.choice instanceof NodeList) {
            super.visit((NodeList) ifStatement.f3.choice);
        } else if (ifStatement.f3.choice instanceof NodeSequence) {
            super.visit((NodeSequence) ifStatement.f3.choice);
        }
        NodeSequence nodeSequence = (NodeSequence) ifStatement.f4.node;
        if (nodeSequence != null) {
            /*
             * Source code contains else-branch.
             * 
             * nodeSequence.nodes.get(0) is the NodeToken object containing the
             * else keyword
             */
            super.visit((NodeToken) nodeSequence.nodes.get(0));
            this.branchManipulator.manipulate(super.out);
            /*
             * nodeSequence.nodes.get(1) is a NodeChoice object containing the
             * else-branch-statements
             */
            NodeChoice nodeChoice = (NodeChoice) nodeSequence.nodes.get(1);
            if (nodeChoice.choice instanceof NodeList) {
                super.visit((NodeList) nodeChoice.choice);
            } else if (nodeChoice.choice instanceof NodeSequence) {
                super.visit((NodeSequence) nodeChoice.choice);
            }
        } else {
            this.branchManipulator.generateElseBranch(super.out);
        }
        if (ifStatement.f5.node != null) {
            /*
             * Source code contains end if keyword.
             */
            super.visit(ifStatement.f5);
        } else {
            /*
             * Source code does not contain end if keyword.
             */
            this.branchManipulator.generateEndIf(super.out);
        }
    }

    /**
     * Writes the end of program section to output, if end of file is reached
     * and end of program section is not yet writed. End of program section
     * contains start and end test case paragraph, stop the program now
     * paragraph and write to disk paragraphs.
     * 
     * @param nodeToken
     *            token to be considered
     */
    @Override
    public void visit(NodeToken nodeToken) {
        if (nodeToken.getParent() instanceof CompilationUnit
                && !this.editedCompilationUnits.contains(nodeToken.getParent())) {
            this.staticPartManipulator.generateWriteLogic(super.out);
            this.editedCompilationUnits.add((CompilationUnit) nodeToken
                    .getParent());
        }
        super.visit(nodeToken);
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
     * @param programUnit
     *            token to be considered
     */
    @Override
    public void visit(ProgramUnit programUnit) {
        super.visit(programUnit.f0);
        if (programUnit.f1.node != null) {
            /*
             * Source code contains environment division.
             */
            super.visit(programUnit.f1);
            this.staticPartManipulator.generateInputOutputSection(
                    this.fileName, super.out);
        } else {
            /*
             * Source code does not contain environment division.
             */
            this.staticPartManipulator.generateEnvironmentDivision(
                    this.fileName, super.out);
        }
        if (programUnit.f2.node != null) {
            /*
             * Source code contains data division.
             */
            super.visit(programUnit.f2);
            this.staticPartManipulator.generateFileDescription(this.progName,
                    super.out);
        } else {
            /*
             * Source code does not contain data division.
             */
            this.staticPartManipulator.generateDataDivision(this.progName,
                    super.out);
        }
        this.statementManipulator.generateStatementCounter(programUnit,
                super.out);
        this.branchManipulator.generateBranchCounter(programUnit, super.out);
        this.conditionManipulator.generateConditionCounter(programUnit,
                super.out);
        this.loopManipulator.generateLoopCounter(programUnit, super.out);
        this.staticPartManipulator
                .generateHorizontalLineInCoverageFile(super.out);
        this.staticPartManipulator.generateWorkingStorageSection(programUnit,
                super.out);
        this.loopManipulator.generateAuxiliaryLoopCounter(programUnit,
                super.out);
        super.visit(programUnit.f3);
    }
    
    @Override
    public void visit(SearchStatement searchStatement) {
        super.visit(searchStatement.f0);
        super.visit(searchStatement.f1);
        super.visit(searchStatement.f2);
        super.visit(searchStatement.f3);
        NodeSequence nodeSequence = (NodeSequence) searchStatement.f4.node;
        if (nodeSequence != null) {
            /*
             * Source code contains at-end-phrase.
             */
            super.visit((NodeOptional) nodeSequence.nodes.get(0));
            super.visit((NodeToken) nodeSequence.nodes.get(1));
            this.branchManipulator.manipulate(super.out);
            super.visit((StatementList) nodeSequence.nodes.get(2));
        }
        for (Node node : searchStatement.f5.nodes) {
            SearchPhrase searchPhrase = (SearchPhrase) node;
            super.visit(searchPhrase.f0);
            super.visit(searchPhrase.f1);
            this.branchManipulator.manipulate(super.out);
            NodeChoice nodeChoice = searchPhrase.f2;
            if (nodeChoice.choice instanceof StatementList) {
                super.visit((StatementList) nodeChoice.choice);
            } else if (nodeChoice.choice instanceof NodeSequence) {
                super.visit((NodeSequence) nodeChoice.choice);
            }
        }
        super.visit(searchStatement.f6);
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
        NodeChoice nodeChoice = (NodeChoice) statement.f0.choice;
        if (nodeChoice.choice instanceof StopStatement
                || nodeChoice.choice instanceof GobackStatement) {
            this.staticPartManipulator.replaceStopRun(nodeChoice, super.out);
        } else if (nodeChoice.choice instanceof CallStatement
                || nodeChoice.choice instanceof EvaluateStatement
                || nodeChoice.choice instanceof ExitProgramStatement
                || nodeChoice.choice instanceof IfStatement
                || nodeChoice.choice instanceof PerformStatement) {
            /*
             * If, evaluate and perform are not instrumented as statements. Exit
             * just stops a subprogram. For that, they are not instrumented.
             * Call is a harmful jumping statement.
             */
            super.visit(statement);
        } else if (nodeChoice.choice instanceof GotoStatement
                || nodeChoice.choice instanceof ContinueStatement) {
            /*
             * Goto and continue are harmless jumping statements. The counter
             * appears before the statement.
             */
            this.statementManipulator.manipulate(super.out);
            super.visit(statement);
        } else {
            /*
             * All other statements are normal statements. The counter appears
             * after the statement.
             */
            super.visit(statement);
            this.statementManipulator.manipulate(super.out);
        }
    }
    
    /**
     * Includes branch counter to statements which contains branches, e.g. 
     * add number1 to number2 on size error display "something".
     * 
     * @param statementList token to be considered
     */
    @Override
    public void visit(StatementList statementList) {
        Node parentStatement = statementList.getParent().getParent().getParent();
        if (parentStatement instanceof AddStatement
                || parentStatement instanceof CallStatement
                || parentStatement instanceof ComputeStatement
                || parentStatement instanceof DeleteStatement
                || parentStatement instanceof DivideStatement
                || parentStatement instanceof MultiplyStatement
                || parentStatement instanceof ReadStatement
                || parentStatement instanceof RewriteStatement
                || parentStatement instanceof StartStatement
                || parentStatement instanceof StringStatement
                || parentStatement instanceof SubtractStatement
                || parentStatement instanceof UnstringStatement
                || parentStatement instanceof WriteStatement) {
            this.branchManipulator.manipulate(super.out);
        }
        super.visit(statementList);
    }
    
}
