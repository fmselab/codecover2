package org.codecover.instrumentation.c.manipulators;

import org.codecover.instrumentation.c.adapter.CCNode;
import org.codecover.instrumentation.c.counter.CounterManager;

import java.io.PrintWriter;

public class DefaultBranchManipulator implements BranchManipulator {
    private CounterManager cm;

    public DefaultBranchManipulator(CounterManager cm) {
        this.cm = cm;
    }

    @Override
    public void writeForwardDeclaration(PrintWriter out) {
        out.format("extern int %s[];\n", cm.branchVarName());
    }

    @Override
    public void visit(PrintWriter out, CCNode n) {
        out.format("%s[%d]++;\n", cm.branchVarName(), n.branchID);
    }

    @Override
    public void visitElse(PrintWriter out, CCNode n) {
        out.format("%s[%d]++;\n", cm.branchVarName(), n.branchID + 1);
    }
}
