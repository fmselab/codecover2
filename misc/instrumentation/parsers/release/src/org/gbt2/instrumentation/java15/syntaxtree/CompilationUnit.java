﻿///////////////////////////////////////////////////////////////////////////////
//
// $Id: CompilationUnit.java 1 2007-12-12 17:37:26Z t-scheller $
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
 * f0 -> [ PackageDeclaration() ] f1 -> ( ImportDeclaration() ) f2 -> (
 * TypeDeclaration() ) f3 -> ( &lt;"\u001a"&gt; )? f4 -> ( &lt;STUFF_TO_IGNORE:
 * ~[]&gt; )? f5 -> &lt;EOF&gt;
 * 
 * </PRE>
 */
public class CompilationUnit implements Node {
    private Node parent;

    public NodeOptional f0;

    public NodeListOptional f1;

    public NodeListOptional f2;

    public NodeOptional f3;

    public NodeOptional f4;

    public NodeToken f5;

    public CompilationUnit(NodeOptional n0, NodeListOptional n1,
            NodeListOptional n2, NodeOptional n3, NodeOptional n4, NodeToken n5) {
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
    }

    public CompilationUnit(NodeOptional n0, NodeListOptional n1,
            NodeListOptional n2, NodeOptional n3, NodeOptional n4) {
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
        f5 = new NodeToken("");
        if (f5 != null)
            f5.setParent(this);
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
