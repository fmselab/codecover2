///////////////////////////////////////////////////////////////////////////////
//
// $Id: StatementManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 17:28:50
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.manipulators;

import java.io.IOException;

import org.gbt2.instrumentation.java15.syntaxtree.BreakStatement;
import org.gbt2.instrumentation.java15.syntaxtree.ContinueStatement;
import org.gbt2.instrumentation.java15.syntaxtree.EmptyStatement;
import org.gbt2.instrumentation.java15.syntaxtree.FieldDeclaration;
import org.gbt2.instrumentation.java15.syntaxtree.LocalVariableDeclaration;
import org.gbt2.instrumentation.java15.syntaxtree.ReturnStatement;
import org.gbt2.instrumentation.java15.syntaxtree.StatementExpression;
import org.gbt2.instrumentation.java15.visitor.InstrumentationVisitorWithException;

/**
 * This Manipulator is used for instrumentation of statements.<br>
 * </br> A object of this interface is called by the
 * {@link InstrumentationVisitorWithException}. This can either be an
 * {@link DummyStatementManipulator} or an {@link DefaultStatementManipulator}.
 * 
 * @author Christoph Müller, Stefan Franke
 * 
 * @see DummyStatementManipulator
 * @see DefaultStatementManipulator
 */
public interface StatementManipulator extends Manipulator {

    public void manipulate(StatementExpression n, String statementID)
            throws IOException;

    public void manipulate(EmptyStatement emptyStatement, String statementID)
            throws IOException;

    public void manipulate(BreakStatement breakStatement, String statementID)
            throws IOException;

    public void manipulate(ContinueStatement continueStatement,
            String statementID) throws IOException;

    public void manipulate(ReturnStatement returnStatement, String statementID)
            throws IOException;

    public void manipulate(LocalVariableDeclaration declaration,
            String statementID) throws IOException;

    public void manipulate(FieldDeclaration fieldDeclaration,
            boolean isStaticPresent, String statementID) throws IOException;
}
