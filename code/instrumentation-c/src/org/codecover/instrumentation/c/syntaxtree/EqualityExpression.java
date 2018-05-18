//
// Generated by JTB 1.3.2
//

package org.codecover.instrumentation.c.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * relationalExpression -> RelationalExpression()
 * nodeOptional -> [ ( &lt;EQ: "=="&gt; | &lt;NE: "!="&gt; ) EqualityExpression() ]
 * </PRE>
 */
public class EqualityExpression extends org.codecover.instrumentation.c.adapter.CCNode implements Node {
   private Node parent;
   public RelationalExpression relationalExpression;
   public NodeOptional nodeOptional;

   public EqualityExpression(RelationalExpression n0, NodeOptional n1) {
      relationalExpression = n0;
      if ( relationalExpression != null ) relationalExpression.setParent(this);
      nodeOptional = n1;
      if ( nodeOptional != null ) nodeOptional.setParent(this);
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
