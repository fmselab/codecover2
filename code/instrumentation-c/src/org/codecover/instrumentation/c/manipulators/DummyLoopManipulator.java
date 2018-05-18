package org.codecover.instrumentation.c.manipulators;

import org.codecover.instrumentation.c.adapter.CCNode;

import java.io.PrintWriter;

public class DummyLoopManipulator implements LoopManipulator {
    @Override
    public void writeForwardDeclaration(PrintWriter out) {
    }

    @Override
    public void visitBefore(PrintWriter out, CCNode n) {
    }

    @Override
    public void visit(PrintWriter out, CCNode n) {
    }

    @Override
    public void visitAfter(PrintWriter out, CCNode n) {
    }
}
