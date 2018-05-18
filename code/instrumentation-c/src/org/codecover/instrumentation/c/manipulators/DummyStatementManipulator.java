package org.codecover.instrumentation.c.manipulators;

import org.codecover.instrumentation.c.syntaxtree.Statement;

import java.io.PrintWriter;

public class DummyStatementManipulator implements StatementManipulator {
    @Override
    public void visit(PrintWriter out, Statement n) {
    }

    @Override
    public void writeForwardDeclaration(PrintWriter out) {
    }
}
