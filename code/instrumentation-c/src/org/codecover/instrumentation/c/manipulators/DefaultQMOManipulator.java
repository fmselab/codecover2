package org.codecover.instrumentation.c.manipulators;

import org.codecover.instrumentation.c.adapter.CCNode;
import org.codecover.instrumentation.c.counter.CounterManager;

import java.io.PrintWriter;

public class DefaultQMOManipulator implements QMOManipulator {
    private final CounterManager cm;

    public DefaultQMOManipulator(CounterManager cm) {
        this.cm = cm;
    }

    @Override
    public void writeForwardDeclaration(PrintWriter out) {
        out.format("extern int %s[];\n", cm.qmoVarName());
    }

    @Override
    public void visitFirst(PrintWriter out, CCNode n) {
        if(n.qmoID == -1)
            return;
        out.format("%s[%d]++,", cm.qmoVarName(), n.qmoID * 2);
    }

    @Override
    public void visitSecond(PrintWriter out, CCNode n) {
        if(n.qmoID == -1)
            return;
        out.format("%s[%d]++,", cm.qmoVarName(), n.qmoID * 2 + 1);
    }
}
