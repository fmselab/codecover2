package org.codecover.instrumentation.c.manipulators;

import org.codecover.instrumentation.c.adapter.CCNode;
import org.codecover.instrumentation.c.syntaxtree.Statement;

import java.io.PrintWriter;

public interface StatementManipulator {
    void visit(PrintWriter out, Statement n);

    void writeForwardDeclaration(PrintWriter out);
}
