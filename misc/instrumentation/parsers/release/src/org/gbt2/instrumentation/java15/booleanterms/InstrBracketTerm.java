///////////////////////////////////////////////////////////////////////////////
//
// $Id: InstrBracketTerm.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 10.04.2007 17:37:14
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.booleanterms;

import java.util.Queue;

/**
 * This class extends the {@link InstrBooleanTerm} and represents an inner
 * {@link InstrBooleanTerm} that is wrapped by brackets ().<br>
 * <br>
 * The method {@link #termToString()} adds brackets to the String of the inner
 * {@link InstrBooleanTerm}. The method {@link #getAllBasicBooleanTerms(Queue)} just
 * delegates the call.
 * 
 * @author Christoph Müller
 */
public class InstrBracketTerm extends InstrBooleanTerm {
    private InstrBooleanTerm innerTerm;

    /**
     * Creates a new {@link InstrBracketTerm} by giving the inner
     * {@link InstrBooleanTerm}.
     * 
     * @param innerTerm
     *            The inner {@link InstrBooleanTerm}.
     */
    public InstrBracketTerm(InstrBooleanTerm innerTerm) {
        this.innerTerm = innerTerm;
    }

    @Override
    public String termToString() {
        return "(" + this.innerTerm.toString() + ")";
    }

    @Override
    public void getAllBasicBooleanTerms(Queue<InstrBasicBooleanTerm> termQueue) {
        this.innerTerm.getAllBasicBooleanTerms(termQueue);
    }
}
