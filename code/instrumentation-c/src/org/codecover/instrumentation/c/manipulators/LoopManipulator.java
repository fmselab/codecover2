package org.codecover.instrumentation.c.manipulators;

import org.codecover.instrumentation.c.adapter.CCNode;

import java.io.PrintWriter;

public interface LoopManipulator {
    public void writeForwardDeclaration(PrintWriter out);

    public void visitBefore(PrintWriter out, CCNode n);

    public void visit(PrintWriter out, CCNode n);

    public void visitAfter(PrintWriter out, CCNode n);
}
