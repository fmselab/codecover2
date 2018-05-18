//
// Generated by JTB 1.3.2
//

package org.codecover.instrumentation.c.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeToken -> &lt;FOR&gt;
 * nodeToken1 -> "("
 * nodeChoice -> ( Declaration() [ Expression() ] | [ Expression() ] ";" [ Expression() ] )
 * nodeToken2 -> ";"
 * nodeOptional -> [ Expression() ]
 * nodeToken3 -> ")"
 * statement -> Statement()
 * </PRE>
 */
public class ForStatement extends org.codecover.instrumentation.c.adapter.CCNode implements Node {
   private Node parent;
   public NodeToken nodeToken;
   public NodeToken nodeToken1;
   public NodeChoice nodeChoice;
   public NodeToken nodeToken2;
   public NodeOptional nodeOptional;
   public NodeToken nodeToken3;
   public Statement statement;

   public ForStatement(NodeToken n0, NodeToken n1, NodeChoice n2, NodeToken n3, NodeOptional n4, NodeToken n5, Statement n6) {
      nodeToken = n0;
      if ( nodeToken != null ) nodeToken.setParent(this);
      nodeToken1 = n1;
      if ( nodeToken1 != null ) nodeToken1.setParent(this);
      nodeChoice = n2;
      if ( nodeChoice != null ) nodeChoice.setParent(this);
      nodeToken2 = n3;
      if ( nodeToken2 != null ) nodeToken2.setParent(this);
      nodeOptional = n4;
      if ( nodeOptional != null ) nodeOptional.setParent(this);
      nodeToken3 = n5;
      if ( nodeToken3 != null ) nodeToken3.setParent(this);
      statement = n6;
      if ( statement != null ) statement.setParent(this);
   }

   public ForStatement(NodeChoice n0, NodeOptional n1, Statement n2) {
      nodeToken = new NodeToken("for");
      if ( nodeToken != null ) nodeToken.setParent(this);
      nodeToken1 = new NodeToken("(");
      if ( nodeToken1 != null ) nodeToken1.setParent(this);
      nodeChoice = n0;
      if ( nodeChoice != null ) nodeChoice.setParent(this);
      nodeToken2 = new NodeToken(";");
      if ( nodeToken2 != null ) nodeToken2.setParent(this);
      nodeOptional = n1;
      if ( nodeOptional != null ) nodeOptional.setParent(this);
      nodeToken3 = new NodeToken(")");
      if ( nodeToken3 != null ) nodeToken3.setParent(this);
      statement = n2;
      if ( statement != null ) statement.setParent(this);
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
