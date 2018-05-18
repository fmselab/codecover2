package org.codecover.instrumentation.c.manipulators;

import org.codecover.instrumentation.c.adapter.CCNode;

import java.io.PrintWriter;

public class DummyBranchManipulator implements BranchManipulator {
    @Override
    public void writeForwardDeclaration(PrintWriter out) {
    }

    @Override
    public void visit(PrintWriter out, CCNode n) {
    }

    @Override
    public void visitElse(PrintWriter out, CCNode n) {
    }
}
