package org.codecover.instrumentation.c.manipulators;

import org.codecover.instrumentation.booleanterms.InstrBooleanTerm;
import org.codecover.instrumentation.c.adapter.CCNode;

import java.io.PrintWriter;

public interface ConditionManipulator {
    public void writeForwardDeclaration(PrintWriter out);

    public InstrBooleanTerm visit(PrintWriter out, CCNode n);
}
