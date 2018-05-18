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

import java.io.IOException;

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
import org.codecover.instrumentation.java15.visitor.InstrumentationVisitor;

/**
 * This Manipulator is used for instrumentation of statements.<br>
 * </br> A object of this interface is called by the
 * {@link InstrumentationVisitor}. This can either be an
 * {@link DummyStatementManipulator} or an {@link LongStatementManipulator}.
 * 
 * @author Christoph Müller, Stefan Franke
 * @version 1.0 ($Id: StatementManipulator.java 18 2008-05-24 22:07:13Z ahija $)
 * 
 * @see DummyStatementManipulator
 * @see ArrayStatementManipulator
 */
@SuppressWarnings("all")
public interface StatementManipulator extends Manipulator {

    public void manipulate(StatementExpression n, String statementID)
            throws IOException;

    public void manipulate(EmptyStatement emptyStatement, String statementID)
            throws IOException;

    public void manipulate(BreakStatement breakStatement, String statementID)
            throws IOException;

    public void manipulate(ContinueStatement continueStatement,
            String statementID) throws IOException;
    
    public void manipulate(ReturnStatement continueStatement,
            String statementID) throws IOException;
    
    public void manipulate(ThrowStatement continueStatement,
            String statementID) throws IOException;

    public void manipulate(LocalVariableDeclaration declaration,
            String statementID) throws IOException;

    public void manipulate(FieldDeclaration fieldDeclaration,
            boolean isStaticPresent, String statementID) throws IOException;

    public void manipulate(SwitchStatement switchStatement, String statementID)
            throws IOException;

    public void manipulate(WhileStatement switchStatement, String statementID)
            throws IOException;

    public void manipulate(IfStatement whileStatement, String statementID)
            throws IOException;

    public void manipulate(DoStatement whileStatement, String statementID)
            throws IOException;

    public void manipulate(ForStatement doStatement, String statementID)
            throws IOException;

    public void manipulate(TryStatement breakStatement, String statementID)
            throws IOException;

    public void manipulate(ExplicitConstructorInvocation explicitConstructorInvocation,
            String statementID) throws IOException;
}
