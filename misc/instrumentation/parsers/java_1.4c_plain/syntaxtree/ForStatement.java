//
// Generated by JTB 1.3.2
//

package syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * f0 -> "for"
 * f1 -> "("
 * f2 -> [ ForInit() ]
 * f3 -> ";"
 * f4 -> [ Expression() ]
 * f5 -> ";"
 * f6 -> [ ForUpdate() ]
 * f7 -> ")"
 * f8 -> Statement()
 * </PRE>
 */
public class ForStatement implements Node {
   private Node parent;
   public NodeToken f0;
   public NodeToken f1;
   public NodeOptional f2;
   public NodeToken f3;
   public NodeOptional f4;
   public NodeToken f5;
   public NodeOptional f6;
   public NodeToken f7;
   public Statement f8;

   public ForStatement(NodeToken n0, NodeToken n1, NodeOptional n2, NodeToken n3, NodeOptional n4, NodeToken n5, NodeOptional n6, NodeToken n7, Statement n8) {
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
      f7 = n7;
      if ( f7 != null ) f7.setParent(this);
      f8 = n8;
      if ( f8 != null ) f8.setParent(this);
   }

   public ForStatement(NodeOptional n0, NodeOptional n1, NodeOptional n2, Statement n3) {
      f0 = new NodeToken("for");
      if ( f0 != null ) f0.setParent(this);
      f1 = new NodeToken("(");
      if ( f1 != null ) f1.setParent(this);
      f2 = n0;
      if ( f2 != null ) f2.setParent(this);
      f3 = new NodeToken(";");
      if ( f3 != null ) f3.setParent(this);
      f4 = n1;
      if ( f4 != null ) f4.setParent(this);
      f5 = new NodeToken(";");
      if ( f5 != null ) f5.setParent(this);
      f6 = n2;
      if ( f6 != null ) f6.setParent(this);
      f7 = new NodeToken(")");
      if ( f7 != null ) f7.setParent(this);
      f8 = n3;
      if ( f8 != null ) f8.setParent(this);
   }

   public void accept(visitor.Visitor v) {
      v.visit(this);
   }
   public <R,A> R accept(visitor.GJVisitor<R,A> v, A argu) {
      return v.visit(this,argu);
   }
   public <R> R accept(visitor.GJNoArguVisitor<R> v) {
      return v.visit(this);
   }
   public <A> void accept(visitor.GJVoidVisitor<A> v, A argu) {
      v.visit(this,argu);
   }
   public void setParent(Node n) { parent = n; }
   public Node getParent()       { return parent; }
}

