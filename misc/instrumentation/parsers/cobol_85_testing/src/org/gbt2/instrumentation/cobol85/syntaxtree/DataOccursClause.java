﻿///////////////////////////////////////////////////////////////////////////////
//
// $Id: DataOccursClause.java 1 2007-12-12 17:37:26Z t-scheller $
// 
///////////////////////////////////////////////////////////////////////////////

//
// Generated by JTB 1.3.2
//

package org.gbt2.instrumentation.cobol85.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * f0 -> &lt;OCCURS&gt;
 * f1 -> [ IntegerConstant() &lt;TO&gt; ]
 * f2 -> IntegerConstant()
 * f3 -> [ &lt;TIMES&gt; ]
 * f4 -> [ &lt;DEPENDING&gt; [ &lt;ON&gt; ] QualifiedDataName() ]
 * f5 -> ( ( &lt;ASCENDING&gt; | &lt;DESCENDING&gt; ) [ &lt;KEY&gt; ] [ &lt;IS&gt; ] ( QualifiedDataName() )+ )*
 * f6 -> [ &lt;INDEXED&gt; [ &lt;BY&gt; ] ( IndexName() )+ ]
 * </PRE>
 */
public class DataOccursClause implements Node {
   private Node parent;
   public NodeToken f0;
   public NodeOptional f1;
   public IntegerConstant f2;
   public NodeOptional f3;
   public NodeOptional f4;
   public NodeListOptional f5;
   public NodeOptional f6;

   public DataOccursClause(NodeToken n0, NodeOptional n1, IntegerConstant n2, NodeOptional n3, NodeOptional n4, NodeListOptional n5, NodeOptional n6) {
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
      f6 = n6;
      if ( f6 != null ) f6.setParent(this);
   }

   public DataOccursClause(NodeOptional n0, IntegerConstant n1, NodeOptional n2, NodeOptional n3, NodeListOptional n4, NodeOptional n5) {
      f0 = new NodeToken("occurs");
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
      f6 = n5;
      if ( f6 != null ) f6.setParent(this);
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

