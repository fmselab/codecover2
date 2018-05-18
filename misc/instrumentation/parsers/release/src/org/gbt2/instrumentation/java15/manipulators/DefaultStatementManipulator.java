///////////////////////////////////////////////////////////////////////////////
//
// $Id: DefaultStatementManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 17:31:25
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.manipulators;

import java.io.IOException;
import java.io.Writer;

import org.gbt2.instrumentation.java15.syntaxtree.BreakStatement;
import org.gbt2.instrumentation.java15.syntaxtree.ContinueStatement;
import org.gbt2.instrumentation.java15.syntaxtree.EmptyStatement;
import org.gbt2.instrumentation.java15.syntaxtree.FieldDeclaration;
import org.gbt2.instrumentation.java15.syntaxtree.LocalVariableDeclaration;
import org.gbt2.instrumentation.java15.syntaxtree.ReturnStatement;
import org.gbt2.instrumentation.java15.syntaxtree.StatementExpression;

import static org.gbt2.instrumentation.java15.visitor.TreeDumperWithException.*;

/**
 * @author Christoph Müller, Stefan Franke
 * 
 */
public class DefaultStatementManipulator extends AbstractDefaultManipulator
        implements StatementManipulator {

    /**
     * Always true.
     * 
     * @return true;
     */
    public boolean requiresBlockExpansionsForBranches() {
        return true;
    }

    /**
     * Always true.
     * 
     * @return true;
     */
    public boolean requiresBlockExpansionsForLoops() {
        return true;
    }

    public void manipulate(StatementExpression n, String statementID)
            throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        super.getCounterIDManager().writeCounterIncrementingForStatement(
                statementID);
    }

    public void manipulate(EmptyStatement emptyStatement, String statementID)
            throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        super.getCounterIDManager().writeCounterIncrementingForStatement(
                statementID);
    }

    public void manipulate(BreakStatement breakStatement, String statementID)
            throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        super.getCounterIDManager().writeCounterIncrementingForStatement(
                statementID);
    }

    public void manipulate(ContinueStatement continueStatement,
            String statementID) throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        super.getCounterIDManager().writeCounterIncrementingForStatement(
                statementID);
    }

    public void manipulate(ReturnStatement returnStatement, String statementID)
            throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        super.getCounterIDManager().writeCounterIncrementingForStatement(
                statementID);
    }

    public void manipulate(LocalVariableDeclaration declaration,
            String statementID) throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        super.getCounterIDManager().writeCounterIncrementingForStatement(
                statementID);
    }

    public void manipulate(FieldDeclaration fieldDeclaration,
            boolean isStaticPresent, String statementID) throws IOException {
        Writer writer = super.getWriter();
        if (isStaticPresent) {
            writer.write(LINE_SEPERATOR + "  static {" + LINE_SEPERATOR + "    ");
        } else {
            writer.write(LINE_SEPERATOR + "  {" + LINE_SEPERATOR + "    ");
        }
        super.getCounterIDManager().writeCounterIncrementingForStatement(
                statementID);
        writer.write(LINE_SEPERATOR + "  }");
    }
}
