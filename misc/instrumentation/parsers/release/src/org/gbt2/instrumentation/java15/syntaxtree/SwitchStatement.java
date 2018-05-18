﻿///////////////////////////////////////////////////////////////////////////////
//
// $Id: SwitchStatement.java 1 2007-12-12 17:37:26Z t-scheller $
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
 * f0 -> "switch" f1 -> "(" f2 -> Expression() f3 -> ")" f4 -> "{" f5 -> (
 * SwitchLabel() ( BlockStatement() )* )* f6 -> "}"
 * 
 * </PRE>
 */
public class SwitchStatement implements Node {
    private Node parent;

    public NodeToken f0;

    public NodeToken f1;

    public Expression f2;

    public NodeToken f3;

    public NodeToken f4;

    public NodeListOptional f5;

    public NodeToken f6;

    public SwitchStatement(NodeToken n0, NodeToken n1, Expression n2,
            NodeToken n3, NodeToken n4, NodeListOptional n5, NodeToken n6) {
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
        f5 = n5;
        if (f5 != null)
            f5.setParent(this);
        f6 = n6;
        if (f6 != null)
            f6.setParent(this);
    }

    public SwitchStatement(Expression n0, NodeListOptional n1) {
        f0 = new NodeToken("switch");
        if (f0 != null)
            f0.setParent(this);
        f1 = new NodeToken("(");
        if (f1 != null)
            f1.setParent(this);
        f2 = n0;
        if (f2 != null)
            f2.setParent(this);
        f3 = new NodeToken(")");
        if (f3 != null)
            f3.setParent(this);
        f4 = new NodeToken("{");
        if (f4 != null)
            f4.setParent(this);
        f5 = n1;
        if (f5 != null)
            f5.setParent(this);
        f6 = new NodeToken("}");
        if (f6 != null)
            f6.setParent(this);
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
