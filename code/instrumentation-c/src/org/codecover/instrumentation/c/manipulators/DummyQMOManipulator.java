package org.codecover.instrumentation.c.manipulators;

import org.codecover.instrumentation.c.adapter.CCNode;

import java.io.PrintWriter;

public class DummyQMOManipulator implements QMOManipulator {
    @Override
    public void writeForwardDeclaration(PrintWriter out) {
    }

    @Override
    public void visitFirst(PrintWriter out, CCNode n) {
    }

    @Override
    public void visitSecond(PrintWriter out, CCNode n) {
    }
}
