﻿///////////////////////////////////////////////////////////////////////////////
//
// $Id: SpecialNamesParagraphStatusPhrase.java 1 2007-12-12 17:37:26Z t-scheller $
// 
///////////////////////////////////////////////////////////////////////////////

//
// Generated by JTB 1.3.2
//

package org.gbt2.instrumentation.cobol85.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * f0 -> ( &lt;ON&gt; [ &lt;STATUS&gt; ] [ &lt;IS&gt; ] Condition() [ &lt;OFF&gt; [ &lt;STATUS&gt; ] [ &lt;IS&gt; ] Condition() ] | &lt;OFF&gt; [ &lt;STATUS&gt; ] [ &lt;IS&gt; ] Condition() [ &lt;ON&gt; [ &lt;STATUS&gt; ] [ &lt;IS&gt; ] Condition() ] )
 * </PRE>
 */
public class SpecialNamesParagraphStatusPhrase implements Node {
   private Node parent;
   public NodeChoice f0;

   public SpecialNamesParagraphStatusPhrase(NodeChoice n0) {
      f0 = n0;
      if ( f0 != null ) f0.setParent(this);
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

