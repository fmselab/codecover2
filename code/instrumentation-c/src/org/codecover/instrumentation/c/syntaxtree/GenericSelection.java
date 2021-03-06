//
// Generated by JTB 1.3.2
//

package org.codecover.instrumentation.c.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeToken -> &lt;GENERIC&gt;
 * nodeToken1 -> "("
 * assignmentExpression -> AssignmentExpression()
 * nodeToken2 -> ","
 * genericAssocList -> GenericAssocList()
 * nodeToken3 -> ")"
 * </PRE>
 */
public class GenericSelection extends org.codecover.instrumentation.c.adapter.CCNode implements Node {
   private Node parent;
   public NodeToken nodeToken;
   public NodeToken nodeToken1;
   public AssignmentExpression assignmentExpression;
   public NodeToken nodeToken2;
   public GenericAssocList genericAssocList;
   public NodeToken nodeToken3;

   public GenericSelection(NodeToken n0, NodeToken n1, AssignmentExpression n2, NodeToken n3, GenericAssocList n4, NodeToken n5) {
      nodeToken = n0;
      if ( nodeToken != null ) nodeToken.setParent(this);
      nodeToken1 = n1;
      if ( nodeToken1 != null ) nodeToken1.setParent(this);
      assignmentExpression = n2;
      if ( assignmentExpression != null ) assignmentExpression.setParent(this);
      nodeToken2 = n3;
      if ( nodeToken2 != null ) nodeToken2.setParent(this);
      genericAssocList = n4;
      if ( genericAssocList != null ) genericAssocList.setParent(this);
      nodeToken3 = n5;
      if ( nodeToken3 != null ) nodeToken3.setParent(this);
   }

   public GenericSelection(AssignmentExpression n0, GenericAssocList n1) {
      nodeToken = new NodeToken("_Generic");
      if ( nodeToken != null ) nodeToken.setParent(this);
      nodeToken1 = new NodeToken("(");
      if ( nodeToken1 != null ) nodeToken1.setParent(this);
      assignmentExpression = n0;
      if ( assignmentExpression != null ) assignmentExpression.setParent(this);
      nodeToken2 = new NodeToken(",");
      if ( nodeToken2 != null ) nodeToken2.setParent(this);
      genericAssocList = n1;
      if ( genericAssocList != null ) genericAssocList.setParent(this);
      nodeToken3 = new NodeToken(")");
      if ( nodeToken3 != null ) nodeToken3.setParent(this);
   }

   public void accept(org.codecover.instrumentation.c.visitor.Visitor v) {
      v.visit(this);
   }
   public <R,A> R accept(org.codecover.instrumentation.c.visitor.GJVisitor<R,A> v, A argu) {
      return v.visit(this,argu);
   }
   public <R> R accept(org.codecover.instrumentation.c.visitor.GJNoArguVisitor<R> v) {
      return v.visit(this);
   }
   public <A> void accept(org.codecover.instrumentation.c.visitor.GJVoidVisitor<A> v, A argu) {
      v.visit(this,argu);
   }
   public void setParent(Node n) { parent = n; }
   public Node getParent()       { return parent; }
}

