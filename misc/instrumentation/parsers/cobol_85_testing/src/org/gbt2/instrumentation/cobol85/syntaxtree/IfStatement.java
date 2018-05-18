﻿///////////////////////////////////////////////////////////////////////////////
//
// $Id: IfStatement.java 1 2007-12-12 17:37:26Z t-scheller $
// 
///////////////////////////////////////////////////////////////////////////////

//
// Generated by JTB 1.3.2
//

package org.gbt2.instrumentation.cobol85.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * f0 -> &lt;IF&gt;
 * f1 -> Condition()
 * f2 -> [ &lt;THEN&gt; ]
 * f3 -> ( ( Statement() )+ | &lt;NEXT&gt; &lt;SENTENCE&gt; )
 * f4 -> [ &lt;ELSE&gt; ( ( Statement() )+ | &lt;NEXT&gt; &lt;SENTENCE&gt; ) ]
 * f5 -> [ &lt;END_IF&gt; ]
 * </PRE>
 */
public class IfStatement implements Node {
   private Node parent;
   public NodeToken f0;
   public Condition f1;
   public NodeOptional f2;
   public NodeChoice f3;
   public NodeOptional f4;
   public NodeOptional f5;

   public IfStatement(NodeToken n0, Condition n1, NodeOptional n2, NodeChoice n3, NodeOptional n4, NodeOptional n5) {
      f0 = n0;
      if ( f0 != null ) f0.setParent(this);
      f1 = n1;
      if ( f1 != null ) f1.setParent(this);
      f2 = n2;
      if ( f2 != null ) f2.setParent(this);
      f3 = n3;
      if ( f3 != null ) f3.setParent(this);
      f4 = n4;
      if ( f4 != null ) f4.setParent(this);
      f5 = n5;
      if ( f5 != null ) f5.setParent(this);
   }

   public IfStatement(Condition n0, NodeOptional n1, NodeChoice n2, NodeOptional n3, NodeOptional n4) {
      f0 = new NodeToken("if");
      if ( f0 != null ) f0.setParent(this);
      f1 = n0;
      if ( f1 != null ) f1.setParent(this);
      f2 = n1;
      if ( f2 != null ) f2.setParent(this);
      f3 = n2;
      if ( f3 != null ) f3.setParent(this);
      f4 = n3;
      if ( f4 != null ) f4.setParent(this);
      f5 = n4;
      if ( f5 != null ) f5.setParent(this);
   }

   public void accept(org.gbt2.instrumentation.cobol85.visitor.Visitor v) {
      v.visit(this);
   }
   public <R,A> R accept(org.gbt2.instrumentation.cobol85.visitor.GJVisitor<R,A> v, A argu) {
      return v.visit(this,argu);
   }
   public <R> R accept(org.gbt2.instrumentation.cobol85.visitor.GJNoArguVisitor<R> v) {
      return v.visit(this);
   }
   public <A> void accept(org.gbt2.instrumentation.cobol85.visitor.GJVoidVisitor<A> v, A argu) {
      v.visit(this,argu);
   }
   public void setParent(Node n) { parent = n; }
   public Node getParent()       { return parent; }
}

