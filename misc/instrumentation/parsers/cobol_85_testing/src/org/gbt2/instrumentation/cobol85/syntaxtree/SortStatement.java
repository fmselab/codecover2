﻿///////////////////////////////////////////////////////////////////////////////
//
// $Id: SortStatement.java 1 2007-12-12 17:37:26Z t-scheller $
// 
///////////////////////////////////////////////////////////////////////////////

//
// Generated by JTB 1.3.2
//

package org.gbt2.instrumentation.cobol85.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * f0 -> &lt;SORT&gt;
 * f1 -> FileName()
 * f2 -> ( [ &lt;ON&gt; ] ( &lt;ASCENDING&gt; | &lt;DESCENDING&gt; ) [ &lt;KEY&gt; ] ( QualifiedDataName() )+ )+
 * f3 -> [ [ &lt;WITH&gt; ] &lt;DUPLICATES&gt; [ &lt;IN&gt; ] [ &lt;ORDER&gt; ] ]
 * f4 -> [ [ &lt;COLLATING&gt; ] &lt;SEQUENCE&gt; [ &lt;IS&gt; ] AlphabetName() ]
 * f5 -> ( &lt;USING&gt; ( FileName() )+ | &lt;INPUT&gt; &lt;PROCEDURE&gt; [ &lt;IS&gt; ] ProcedureName() [ ( &lt;THROUGH&gt; | &lt;THRU&gt; ) ProcedureName() ] )
 * f6 -> ( &lt;GIVING&gt; ( FileName() )+ | &lt;OUTPUT&gt; &lt;PROCEDURE&gt; [ &lt;IS&gt; ] ProcedureName() [ ( &lt;THROUGH&gt; | &lt;THRU&gt; ) ProcedureName() ] )
 * </PRE>
 */
public class SortStatement implements Node {
   private Node parent;
   public NodeToken f0;
   public FileName f1;
   public NodeList f2;
   public NodeOptional f3;
   public NodeOptional f4;
   public NodeChoice f5;
   public NodeChoice f6;

   public SortStatement(NodeToken n0, FileName n1, NodeList n2, NodeOptional n3, NodeOptional n4, NodeChoice n5, NodeChoice n6) {
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

   public SortStatement(FileName n0, NodeList n1, NodeOptional n2, NodeOptional n3, NodeChoice n4, NodeChoice n5) {
      f0 = new NodeToken("sort");
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

