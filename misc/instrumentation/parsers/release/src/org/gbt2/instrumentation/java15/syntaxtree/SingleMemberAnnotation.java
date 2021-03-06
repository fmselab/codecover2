﻿///////////////////////////////////////////////////////////////////////////////
//
// $Id: SingleMemberAnnotation.java 1 2007-12-12 17:37:26Z t-scheller $
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
 * f0 -> "@" f1 -> Name() f2 -> "(" f3 -> MemberValue() f4 -> ")"
 * 
 * </PRE>
 */
public class SingleMemberAnnotation implements Node {
    private Node parent;

    public NodeToken f0;

    public Name f1;

    public NodeToken f2;

    public MemberValue f3;

    public NodeToken f4;

    public SingleMemberAnnotation(NodeToken n0, Name n1, NodeToken n2,
            MemberValue n3, NodeToken n4) {
        f0 = n0;
        if (f0 != null)
            f0.setParent(this);
        f1 = n1;
        if (f1 != null)
            f1.setParent(this);
        f2 = n2;
        if (f2 != null)
            f2.setParent(this);
        f3 = n3;
        if (f3 != null)
            f3.setParent(this);
        f4 = n4;
        if (f4 != null)
            f4.setParent(this);
    }

    public SingleMemberAnnotation(Name n0, MemberValue n1) {
        f0 = new NodeToken("@");
        if (f0 != null)
            f0.setParent(this);
        f1 = n0;
        if (f1 != null)
            f1.setParent(this);
        f2 = new NodeToken("(");
        if (f2 != null)
            f2.setParent(this);
        f3 = n1;
        if (f3 != null)
            f3.setParent(this);
        f4 = new NodeToken(")");
        if (f4 != null)
            f4.setParent(this);
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
