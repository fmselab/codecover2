package org.codecover.instrumentation.c.manipulators;

import org.codecover.instrumentation.booleanterms.InstrBooleanTerm;
import org.codecover.instrumentation.c.CExpressionParser;
import org.codecover.instrumentation.c.adapter.CCNode;

import java.io.PrintWriter;

public class DummyConditionManipulator implements ConditionManipulator {
    @Override
    public void writeForwardDeclaration(PrintWriter out) {
    }

    @Override
    public InstrBooleanTerm visit(PrintWriter out, CCNode n) {
        return n.terms;
    }
}
