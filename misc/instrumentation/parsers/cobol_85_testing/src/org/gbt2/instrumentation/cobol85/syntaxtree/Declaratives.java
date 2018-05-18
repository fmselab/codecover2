﻿///////////////////////////////////////////////////////////////////////////////
//
// $Id: Declaratives.java 1 2007-12-12 17:37:26Z t-scheller $
// 
///////////////////////////////////////////////////////////////////////////////

//
// Generated by JTB 1.3.2
//

package org.gbt2.instrumentation.cobol85.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * f0 -> &lt;DECLARATIVES&gt;
 * f1 -> &lt;DOT&gt;
 * f2 -> ( SectionHeader() &lt;DOT&gt; UseStatement() &lt;DOT&gt; Paragraphs() )+
 * f3 -> &lt;END&gt;
 * f4 -> &lt;DECLARATIVES&gt;
 * f5 -> &lt;DOT&gt;
 * </PRE>
 */
public class Declaratives implements Node {
   private Node parent;
   public NodeToken f0;
   public NodeToken f1;
   public NodeList f2;
   public NodeToken f3;
   public NodeToken f4;
   public NodeToken f5;

   public Declaratives(NodeToken n0, NodeToken n1, NodeList n2, NodeToken n3, NodeToken n4, NodeToken n5) {
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

   public Declaratives(NodeToken n0, NodeList n1, NodeToken n2) {
      f0 = new NodeToken("declaratives");
      if ( f0 != null ) f0.setParent(this);
      f1 = n0;
      if ( f1 != null ) f1.setParent(this);
      f2 = n1;
      if ( f2 != null ) f2.setParent(this);
      f3 = new NodeToken("end");
      if ( f3 != null ) f3.setParent(this);
      f4 = new NodeToken("declaratives");
      if ( f4 != null ) f4.setParent(this);
      f5 = n2;
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

