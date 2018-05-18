//
// Generated by JTB 1.3.2
//

package syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * f0 -> &lt;IDENTIFIER&gt;
 * f1 -> ":"
 * f2 -> Statement()
 * </PRE>
 */
public class LabeledStatement implements Node {
   private Node parent;
   public NodeToken f0;
   public NodeToken f1;
   public Statement f2;

   public LabeledStatement(NodeToken n0, NodeToken n1, Statement n2) {
      f0 = n0;
      if ( f0 != null ) f0.setParent(this);
      f1 = n1;
      if ( f1 != null ) f1.setParent(this);
      f2 = n2;
      if ( f2 != null ) f2.setParent(this);
   }

   public LabeledStatement(NodeToken n0, Statement n1) {
      f0 = n0;
      if ( f0 != null ) f0.setParent(this);
      f1 = new NodeToken(":");
      if ( f1 != null ) f1.setParent(this);
      f2 = n1;
      if ( f2 != null ) f2.setParent(this);
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

