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

package org.codecover.instrumentation.java15.manipulators;

import org.codecover.instrumentation.java15.syntaxtree.BreakStatement;
import org.codecover.instrumentation.java15.syntaxtree.ContinueStatement;
import org.codecover.instrumentation.java15.syntaxtree.DoStatement;
import org.codecover.instrumentation.java15.syntaxtree.EmptyStatement;
import org.codecover.instrumentation.java15.syntaxtree.ExplicitConstructorInvocation;
import org.codecover.instrumentation.java15.syntaxtree.FieldDeclaration;
import org.codecover.instrumentation.java15.syntaxtree.ForStatement;
import org.codecover.instrumentation.java15.syntaxtree.IfStatement;
import org.codecover.instrumentation.java15.syntaxtree.LocalVariableDeclaration;
import org.codecover.instrumentation.java15.syntaxtree.ReturnStatement;
import org.codecover.instrumentation.java15.syntaxtree.StatementExpression;
import org.codecover.instrumentation.java15.syntaxtree.SwitchStatement;
import org.codecover.instrumentation.java15.syntaxtree.ThrowStatement;
import org.codecover.instrumentation.java15.syntaxtree.TryStatement;
import org.codecover.instrumentation.java15.syntaxtree.WhileStatement;

/**
 * @author Christoph Müller, Stefan Franke
 * @version 1.0 ($Id: DummyStatementManipulator.java 18 2008-05-24 22:07:13Z ahija $)
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
    
    public void manipulate(ReturnStatement continueStatement,
            String statementID) {
        // do not add anything;
    }
    
    public void manipulate(ThrowStatement continueStatement,
            String statementID) {
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

    public void manipulate(SwitchStatement switchStatement, String statementID) {
        // do not add anything;
    }

    public void manipulate(WhileStatement switchStatement, String statementID) {
        // do not add anything;
    }

    public void manipulate(IfStatement whileStatement, String statementID) {
        // do not add anything;
    }

    public void manipulate(DoStatement whileStatement, String statementID) {
        // do not add anything;
    }

    public void manipulate(ForStatement doStatement, String statementID) {
        // do not add anything;
    }

    public void manipulate(TryStatement breakStatement, String statementID) {
        // do not add anything;
    }

    public void manipulate(ExplicitConstructorInvocation explicitConstructorInvocation,
            String statementID) {
        // do not add anything;
    }
}
