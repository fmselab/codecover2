﻿///////////////////////////////////////////////////////////////////////////////
//
// $Id: MethodDeclarator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 24.02.2007 17:30:00
//
///////////////////////////////////////////////////////////////////////////////

//
// Generated by JTB 1.3.2
//

package org.gbt2.instrumentation.java15.syntaxtree;

import java.io.IOException;

import org.gbt2.instrumentation.java15.visitor.GJNoArguVisitor;
import org.gbt2.instrumentation.java15.visitor.GJVisitor;
import org.gbt2.instrumentation.java15.visitor.GJVoidVisitor;
import org.gbt2.instrumentation.java15.visitor.Visitor;
import org.gbt2.instrumentation.java15.visitor.VisitorWithException;

/**
 * Grammar production:
 * 
 * <PRE>
 * 
 * f0 -> &lt;IDENTIFIER&gt; f1 -> FormalParameters() f2 -> ( "[" "]" )*
 * 
 * </PRE>
 */
public class MethodDeclarator implements Node {
    private Node parent;

    public NodeToken f0;

    public FormalParameters f1;

    public NodeListOptional f2;

    public MethodDeclarator(NodeToken n0, FormalParameters n1,
            NodeListOptional n2) {
        f0 = n0;
        if (f0 != null)
            f0.setParent(this);
        f1 = n1;
        if (f1 != null)
            f1.setParent(this);
        f2 = n2;
        if (f2 != null)
            f2.setParent(this);
    }

    public void accept(VisitorWithException v) throws IOException {
        v.visit(this);
    }

    public void accept(Visitor v) {
        v.visit(this);
    }

    public <R, A> R accept(GJVisitor<R, A> v, A argu) {
        return v.visit(this, argu);
    }

    public <R> R accept(GJNoArguVisitor<R> v) {
        return v.visit(this);
    }

    public <A> void accept(GJVoidVisitor<A> v, A argu) {
        v.visit(this, argu);
    }

    public void setParent(Node n) {
        parent = n;
    }

    public Node getParent() {
        return parent;
    }
}
