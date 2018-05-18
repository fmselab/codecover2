﻿///////////////////////////////////////////////////////////////////////////////
//
// $Id: NodeSequence.java 1 2007-12-12 17:37:26Z t-scheller $
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
import java.util.Enumeration;
import java.util.Vector;

import org.gbt2.instrumentation.java15.visitor.GJNoArguVisitor;
import org.gbt2.instrumentation.java15.visitor.GJVisitor;
import org.gbt2.instrumentation.java15.visitor.GJVoidVisitor;
import org.gbt2.instrumentation.java15.visitor.Visitor;
import org.gbt2.instrumentation.java15.visitor.VisitorWithException;

/**
 * Represents a sequence of nodes nested within a choice, list, optional list,
 * or optional, e.g. ( A B )+ or [ C D E ]
 */
public class NodeSequence implements NodeListInterface {
    public NodeSequence(int n) {
        nodes = new Vector<Node>(n);
    }

    public NodeSequence(Node firstNode) {
        nodes = new Vector<Node>();
        addNode(firstNode);
    }

    public void addNode(Node n) {
        nodes.addElement(n);
        n.setParent(this);
    }

    public Node elementAt(int i) {
        return nodes.elementAt(i);
    }

    public Enumeration<Node> elements() {
        return nodes.elements();
    }

    public int size() {
        return nodes.size();
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

    private Node parent;

    public Vector<Node> nodes;
}
