//
// Generated by JTB 1.3.2
//

package org.codecover.instrumentation.xampil.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * f0 -> &lt;FILE&gt;
 * f1 -> ( &lt;OVERWRITE&gt; | &lt;APPEND&gt; )
 * f2 -> ( &lt;STRING_LITERAL&gt; | &lt;IDENTIFIER&gt; )
 * f3 -> Expression(DUMMY_CONTAINER)
 * f4 -> &lt;EOL&gt;
 * </PRE>
 */
public class FileStatement implements Node {
   private Node parent;
   public NodeToken f0;
   public NodeChoice f1;
   public NodeChoice f2;
   public Expression f3;
   public NodeToken f4;

   public FileStatement(NodeToken n0, NodeChoice n1, NodeChoice n2, Expression n3, NodeToken n4) {
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
   }

   public FileStatement(NodeChoice n0, NodeChoice n1, Expression n2, NodeToken n3) {
      f0 = new NodeToken("FILE");
      if ( f0 != null ) f0.setParent(this);
      f1 = n0;
      if ( f1 != null ) f1.setParent(this);
      f2 = n1;
      if ( f2 != null ) f2.setParent(this);
      f3 = n2;
      if ( f3 != null ) f3.setParent(this);
      f4 = n3;
      if ( f4 != null ) f4.setParent(this);
   }

   public void accept(org.codecover.instrumentation.xampil.visitor.Visitor v) {
      v.visit(this);
   }
   public <R,A> R accept(org.codecover.instrumentation.xampil.visitor.GJVisitor<R,A> v, A argu) {
      return v.visit(this,argu);
   }
   public <R> R accept(org.codecover.instrumentation.xampil.visitor.GJNoArguVisitor<R> v) {
      return v.visit(this);
   }
   public <A> void accept(org.codecover.instrumentation.xampil.visitor.GJVoidVisitor<A> v, A argu) {
      v.visit(this,argu);
   }
   public void setParent(Node n) { parent = n; }
   public Node getParent()       { return parent; }
}
