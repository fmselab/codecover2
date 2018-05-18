//
// Generated by JTB 1.3.2
//

package syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * f0 -> ( "static" | "abstract" | "final" | "public" | "protected" | "private" | "strictfp" )*
 * f1 -> UnmodifiedInterfaceDeclaration()
 * </PRE>
 */
public class NestedInterfaceDeclaration implements Node {
   private Node parent;
   public NodeListOptional f0;
   public UnmodifiedInterfaceDeclaration f1;

   public NestedInterfaceDeclaration(NodeListOptional n0, UnmodifiedInterfaceDeclaration n1) {
      f0 = n0;
      if ( f0 != null ) f0.setParent(this);
      f1 = n1;
      if ( f1 != null ) f1.setParent(this);
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

