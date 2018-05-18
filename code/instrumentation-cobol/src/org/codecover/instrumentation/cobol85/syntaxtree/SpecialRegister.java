/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

//
// Generated by JTB 1.3.2
//

package org.codecover.instrumentation.cobol85.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * f0 -> ( &lt;ADDRESS&gt; &lt;OF&gt; DataName() | &lt;DEBUG_ITEM&gt; | &lt;LENGTH&gt; &lt;OF&gt; Identifier() | &lt;RETURN_CODE&gt; | &lt;SHIFT_OUT&gt; | &lt;SHIFT_IN&gt; | &lt;SORT_CONTROL&gt; | &lt;SORT_CORE_SIZE&gt; | &lt;SORT_FILE_SIZE&gt; | &lt;SORT_MESSAGE&gt; | &lt;SORT_MODE_SIZE&gt; | &lt;SORT_RETURN&gt; | &lt;TALLY&gt; | &lt;WHEN_COMPILED&gt; )
 * </PRE>
 */
public class SpecialRegister implements Node {
   private Node parent;
   public NodeChoice f0;

   public SpecialRegister(NodeChoice n0) {
      f0 = n0;
      if ( f0 != null ) f0.setParent(this);
   }

   public void accept(org.codecover.instrumentation.cobol85.visitor.Visitor v) {
      v.visit(this);
   }
   public <R,A> R accept(org.codecover.instrumentation.cobol85.visitor.GJVisitor<R,A> v, A argu) {
      return v.visit(this,argu);
   }
   public <R> R accept(org.codecover.instrumentation.cobol85.visitor.GJNoArguVisitor<R> v) {
      return v.visit(this);
   }
   public <A> void accept(org.codecover.instrumentation.cobol85.visitor.GJVoidVisitor<A> v, A argu) {
      v.visit(this,argu);
   }
   public void setParent(Node n) { parent = n; }
   public Node getParent()       { return parent; }
}
