﻿///////////////////////////////////////////////////////////////////////////////
//
// $Id: NodeOptional.java 1 2007-12-12 17:37:26Z t-scheller $
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
 * Represents an grammar optional node, e.g. ( A )? or [ A ]
 */
public class NodeOptional implements Node {
    public NodeOptional() {
        node = null;
    }

    public NodeOptional(Node n) {
        addNode(n);
    }

    public void addNode(Node n) {
        if (node != null) // Oh oh!
            throw new Error("Attempt to set optional node twice");

        node = n;
        n.setParent(this);
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

    public boolean present() {
        return node != null;
    }

    public void setParent(Node n) {
        parent = n;
    }

    public Node getParent() {
        return parent;
    }

    private Node parent;

    public Node node;
}
