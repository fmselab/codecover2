///////////////////////////////////////////////////////////////////////////////
//
// $Id: DummyStatementManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 17:32:21
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.manipulators;

import org.gbt2.instrumentation.java15.syntaxtree.BreakStatement;
import org.gbt2.instrumentation.java15.syntaxtree.ContinueStatement;
import org.gbt2.instrumentation.java15.syntaxtree.EmptyStatement;
import org.gbt2.instrumentation.java15.syntaxtree.FieldDeclaration;
import org.gbt2.instrumentation.java15.syntaxtree.LocalVariableDeclaration;
import org.gbt2.instrumentation.java15.syntaxtree.ReturnStatement;
import org.gbt2.instrumentation.java15.syntaxtree.StatementExpression;

/**
 * @author Christoph Müller, Stefan Franke
 */
public class DummyStatementManipulator extends AbstractDummyManipulator
        implements StatementManipulator {

    public void manipulate(StatementExpression n, String statementID) {
        // do not add anything;
    }

    public void manipulate(EmptyStatement emptyStatement, String statementID) {
        // do not add anything;
    }

    public void manipulate(BreakStatement breakStatement, String statementID) {
        // do not add anything;
    }

    public void manipulate(ContinueStatement continueStatement,
            String statementID) {
        // do not add anything;
    }

    public void manipulate(ReturnStatement returnStatement, String statementID) {
        // do not add anything;
    }

    public void manipulate(LocalVariableDeclaration declaration,
            String statementID) {
        // do not add anything;
    }

    public void manipulate(FieldDeclaration fieldDeclaration,
            boolean isStaticPresent, String statementID) {
        // do not add anything;
    }
}
