package org.codecover.instrumentation.c.manipulators;

import org.codecover.instrumentation.c.adapter.CCNode;

import java.io.PrintWriter;

public interface QMOManipulator {
    void writeForwardDeclaration(PrintWriter out);

    void visitFirst(PrintWriter out, CCNode n);
    void visitSecond(PrintWriter out, CCNode n);
}
