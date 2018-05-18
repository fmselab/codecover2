package org.codecover.instrumentation.c.manipulators;

import org.codecover.instrumentation.c.adapter.CCNode;
import org.codecover.instrumentation.c.syntaxtree.IfStatement;

import java.io.PrintWriter;

public interface BranchManipulator {
    public void writeForwardDeclaration(PrintWriter out);

    public void visit(PrintWriter out, CCNode n);

    public void visitElse(PrintWriter out, CCNode n);
}
