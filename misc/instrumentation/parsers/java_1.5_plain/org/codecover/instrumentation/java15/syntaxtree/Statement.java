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

//
// Generated by JTB 1.3.2
//

package org.codecover.instrumentation.java15.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * f0 -> LabeledStatement()
 *       | AssertStatement()
 *       | Block()
 *       | EmptyStatement()
 *       | StatementExpression() ";"
 *       | SwitchStatement()
 *       | IfStatement()
 *       | WhileStatement()
 *       | DoStatement()
 *       | ForStatement()
 *       | BreakStatement()
 *       | ContinueStatement()
 *       | ReturnStatement()
 *       | ThrowStatement()
 *       | SynchronizedStatement()
 *       | TryStatement()
 * </PRE>
 */
@SuppressWarnings("all")
public class Statement implements Node {
   public String statementID = null;
   public NodeSequence labelToken = NodeSequence.EMPTY_INSTANCE;
   private Node parent;
   public NodeChoice f0;

   public Statement(NodeChoice n0) {
      f0 = n0;
      if ( f0 != null ) f0.setParent(this);
   }

   public void accept(org.codecover.instrumentation.java15.visitor.Visitor v) {
      v.visit(this);
   }
   public void accept(org.codecover.instrumentation.java15.visitor.VisitorWithException v) throws java.io.IOException {
      v.visit(this);
   }
   public <R,A> R accept(org.codecover.instrumentation.java15.visitor.GJVisitor<R,A> v, A argu) {
      return v.visit(this,argu);
   }
   public <R> R accept(org.codecover.instrumentation.java15.visitor.GJNoArguVisitor<R> v) {
      return v.visit(this);
   }
   public <A> void accept(org.codecover.instrumentation.java15.visitor.GJVoidVisitor<A> v, A argu) {
      v.visit(this,argu);
   }
   public void setParent(Node n) { parent = n; }
   public Node getParent()       { return parent; }
   /** for debugging purposes */
   @Override
   public String toString() {
      return org.codecover.instrumentation.java15.visitor.TreeSourceFileImageDumper.convertToString(this);
   }
}
