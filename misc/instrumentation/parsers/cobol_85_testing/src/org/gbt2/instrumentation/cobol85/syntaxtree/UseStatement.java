﻿///////////////////////////////////////////////////////////////////////////////
//
// $Id: UseStatement.java 1 2007-12-12 17:37:26Z t-scheller $
// 
///////////////////////////////////////////////////////////////////////////////

//
// Generated by JTB 1.3.2
//

package org.gbt2.instrumentation.cobol85.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * f0 -> &lt;USE&gt;
 * f1 -> ( [ &lt;FOR&gt; ] &lt;DEBUGGING&gt; [ &lt;ON&gt; ] ( ( ProcedureName() )+ | &lt;ALL&gt; &lt;PROCEDURES&gt; ) | [ &lt;GLOBAL&gt; ] &lt;AFTER&gt; [ &lt;STANDARD&gt; ] ( ( &lt;EXCEPTION&gt; | &lt;ERROR&gt; ) | [ ( &lt;BEGINNING&gt; | &lt;ENDING&gt; ) ] [ ( &lt;FILE&gt; | &lt;REEL&gt; | &lt;UNIT&gt; ) ] &lt;LABEL&gt; ) &lt;PROCEDURE&gt; [ &lt;ON&gt; ] ( ( FileName() )+ | &lt;INPUT&gt; | &lt;OUTPUT&gt; | &lt;I_O&gt; | &lt;EXTEND&gt; ) )
 * </PRE>
 */
public class UseStatement implements Node {
   private Node parent;
   public NodeToken f0;
   public NodeChoice f1;

   public UseStatement(NodeToken n0, NodeChoice n1) {
      f0 = n0;
      if ( f0 != null ) f0.setParent(this);
      f1 = n1;
      if ( f1 != null ) f1.setParent(this);
   }

   public UseStatement(NodeChoice n0) {
      f0 = new NodeToken("use");
      if ( f0 != null ) f0.setParent(this);
      f1 = n0;
      if ( f1 != null ) f1.setParent(this);
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

