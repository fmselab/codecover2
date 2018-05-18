﻿///////////////////////////////////////////////////////////////////////////////
//
// $Id: SQLSelectForUpdate.java 1 2007-12-12 17:37:26Z t-scheller $
// 
///////////////////////////////////////////////////////////////////////////////

//
// Generated by JTB 1.3.2
//

package org.gbt2.instrumentation.cobol85.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * f0 -> &lt;FOR&gt;
 * f1 -> &lt;UPDATE&gt;
 * f2 -> &lt;OF&gt;
 * f3 -> ( SQLIdentifier() )+
 * </PRE>
 */
public class SQLSelectForUpdate implements Node {
   private Node parent;
   public NodeToken f0;
   public NodeToken f1;
   public NodeToken f2;
   public NodeList f3;

   public SQLSelectForUpdate(NodeToken n0, NodeToken n1, NodeToken n2, NodeList n3) {
      f0 = n0;
      if ( f0 != null ) f0.setParent(this);
      f1 = n1;
      if ( f1 != null ) f1.setParent(this);
      f2 = n2;
      if ( f2 != null ) f2.setParent(this);
      f3 = n3;
      if ( f3 != null ) f3.setParent(this);
   }

   public SQLSelectForUpdate(NodeList n0) {
      f0 = new NodeToken("for");
      if ( f0 != null ) f0.setParent(this);
      f1 = new NodeToken("update");
      if ( f1 != null ) f1.setParent(this);
      f2 = new NodeToken("of");
      if ( f2 != null ) f2.setParent(this);
      f3 = n0;
      if ( f3 != null ) f3.setParent(this);
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
