package org.codecover.instrumentation.c.adapter;

import org.codecover.instrumentation.booleanterms.InstrBooleanTerm;

public class CCNode {
    public int beginOffset, endOffset;

    public int stmtID = -1;
    public int branchID = -1;
    public int loopID = -1;
    public int condID = -1;
    public int qmoID = -1;

    public InstrBooleanTerm terms;
}
