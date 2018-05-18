package org.codecover.instrumentation.c.manipulators;

import org.codecover.instrumentation.booleanterms.InstrBasicBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrBracketTerm;
import org.codecover.instrumentation.booleanterms.InstrOperatorTerm;
import org.codecover.instrumentation.c.CBooleanExpressions;
import org.codecover.instrumentation.c.adapter.CCNode;
import org.codecover.instrumentation.c.counter.CounterManager;

import java.io.PrintWriter;
import java.util.ArrayList;

public class DefaultConditionManipulator implements ConditionManipulator {
    private CounterManager cm;

    int sizeofChar = 8;

    public DefaultConditionManipulator(CounterManager cm) {
        this.cm = cm;
    }

    @Override
    public void writeForwardDeclaration(PrintWriter out) {
        out.println(org.codecover.instrumentation.c.Helper.getCondDefinitions());
        out.format("extern %s %s[];\n", org.codecover.instrumentation.c.Helper.getCondTypeName(), cm.condVarName());
    }

    @Override
    public InstrBooleanTerm visit(PrintWriter out, CCNode n) {
        InstrBooleanTerm root = n.terms;

        ArrayList<InstrBasicBooleanTerm> basicTerms = new ArrayList<InstrBasicBooleanTerm>();
        root.getAllBasicBooleanTerms(basicTerms);

        String helper = cm.condVarName()  + "tmp" + n.condID;

        int numBytes = (basicTerms.size()*2 / 8 + 1);

        out.println("unsigned char " + helper + "[" + numBytes + "];");

        for(int i = 0;i < numBytes; i++) {
            out.println(helper + "[" + i + "] = 0;");
        }

        visitBasicTerms(helper, basicTerms);

        // We need 2 variables per term
        return addIncrementation(root, helper, cm.condVarName() + "+" + n.condID, basicTerms.size() * 2);
    }

    private InstrBooleanTerm addIncrementation(InstrBooleanTerm root,
                                               String helper,
                                               String counter,
                                               int numValues) {
        InstrBooleanTerm writeT = new InstrBasicBooleanTerm(
                String.format("(CodeCover_ConditionAdd(%s,%s,%d), 1)", counter, helper, numValues) ,-1 ,-1);
        InstrBooleanTerm writeF = new InstrBasicBooleanTerm(
                String.format("(CodeCover_ConditionAdd(%s,%s,%d), 0)", counter, helper, numValues) ,-1 ,-1);

        InstrBooleanTerm t = new InstrOperatorTerm(new InstrBracketTerm(root),
                CBooleanExpressions.andOperator, writeT, -1, -1);

        return new InstrOperatorTerm(new InstrBracketTerm(t), CBooleanExpressions.orOperator, writeF, -1, -1);
    }

    private void visitBasicTerms(String helper, ArrayList<InstrBasicBooleanTerm> terms) {
        int i = 0;
        for(InstrBasicBooleanTerm term : terms) {
            term.modifyImage(String.format(
                    "(%1$s[%3$d] |= %2$d, (%6$s) && (%1$s[%5$d] |= %4$d))",
                    helper, 1<<i%sizeofChar, i++ / sizeofChar, 1<<i%sizeofChar, i++ / sizeofChar,
                    term.termToString()));
        }
    }
}
