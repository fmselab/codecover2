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

import java.util.*;
import org.codecover.instrumentation.cobol85.NodeCounter;
import org.codecover.instrumentation.cobol85.visitor.InstrumentationVisitor;
/**
 * Represents a single token in the grammar.  If the "-tk" option
 * is used, also contains a Vector of preceding special tokens.
 *
 * @author Generated by JTB 1.3.2
 * @author Christoph Müller / Stefan Franke:<br>
 * added position handling in
 *
 * @version 1.0 ($Id: NodeToken.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
@SuppressWarnings("all")
public class NodeToken implements Node {
   public NodeToken(String s) {
      this(s, -1, -1, -1, -1, -1, -1, -1);    }

   public NodeToken(String s, int kind, int beginLine, int beginColumn, int endLine, int endColumn, int beginPosition, int endPosition) {
      tokenImage = s;
      specialTokens = null;
      this.kind = kind;
      this.beginLine = beginLine;
      this.beginColumn = beginColumn;
      this.startOffset = beginPosition;
      this.endLine = endLine;
      this.endColumn = endColumn;
      this.endOffset = endPosition;
   }

   public NodeToken getSpecialAt(int i) {
      if ( specialTokens == null )
         throw new java.util.NoSuchElementException("No specials in token");
      return specialTokens.elementAt(i);
   }

   public int numSpecials() {
      if ( specialTokens == null ) return 0;
      return specialTokens.size();
   }

   public void addSpecial(NodeToken s) {
      if ( specialTokens == null ) specialTokens = new Vector<NodeToken>();
      specialTokens.addElement(s);

      // informs the node counter that a start test case comment was found in the
      // source code
      if (s.tokenImage.equals(InstrumentationVisitor.START_TEST_CASE)) {
          nodeCounter.setStartTestCase();
      }

      s.setParent(this);
   }

   public void trimSpecials() {
      if ( specialTokens == null ) return;
      specialTokens.trimToSize();
   }

   public String toString()     { return tokenImage; }

   public String withSpecials() {
      if ( specialTokens == null )
          return tokenImage;

      StringBuilder buf = new StringBuilder();

       for ( Enumeration<NodeToken> e = specialTokens.elements(); e.hasMoreElements(); )
          buf.append(e.nextElement().toString());

       buf.append(tokenImage);
       return buf.toString();
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

   private Node parent;
   public String tokenImage;

   // Stores a list of NodeTokens
   public Vector<NodeToken> specialTokens;

   // -1 for these ints means no position info is available.
   public int beginLine, beginColumn, endLine, endColumn, startOffset, endOffset;

   // Equal to the JavaCC token "kind" integer.
   // -1 if not available.
   public int kind;
   private static NodeCounter nodeCounter = NodeCounter.getInstance();
}

