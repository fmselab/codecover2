/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.instrumentation.booleanterms;

import java.io.IOException;
import java.io.Writer;
import java.util.List;

import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.SourceFile;

/**
 * This class extends the {@link InstrBooleanTerm} and represents an inner
 * {@link InstrBooleanTerm} that is wrapped by brackets ().<br>
 * <br>
 * The method {@link #termToString()} adds brackets to the String of the inner
 * {@link InstrBooleanTerm}. The method {@link #getAllBasicBooleanTerms(List)}
 * just delegates the call.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: InstrBracketTerm.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InstrBracketTerm extends InstrBooleanTerm {
    private InstrBooleanTerm innerTerm;

    /**
     * Creates a new {@link InstrBracketTerm} by giving the inner
     * {@link InstrBooleanTerm}.
     * 
     * @param innerTerm
     *            The inner {@link InstrBooleanTerm}.
     *            
     * @throws NullPointerException
     *             if the innerTerm is <code>null</code>
     */
    public InstrBracketTerm(InstrBooleanTerm innerTerm) {
        if (innerTerm == null) {
            throw new NullPointerException("innerTerm == null");
        }

        this.innerTerm = innerTerm;
    }

    /**
     * Return the inner term.
     * 
     * @return The inner {@link InstrBooleanTerm}.
     */
    public InstrBooleanTerm getInnerTerm() {
        return this.innerTerm;
    }

    @Override
    public String termToString() {
        return "(" + this.innerTerm.toString() + ")";
    }

    @Override
    public void writeToTarget(Writer target) throws IOException {
        target.write('(');
        this.innerTerm.writeToTarget(target);
        target.write(')');
    }

    @Override
    public void getAllBasicBooleanTerms(List<InstrBasicBooleanTerm> termList) {
        this.innerTerm.getAllBasicBooleanTerms(termList);
    }

    @Override
    public BooleanTerm toBooleanTerm(MASTBuilder builder, SourceFile sourceFile) {
        return this.innerTerm.toBooleanTerm(builder, sourceFile);
    }

    @Override
    public void access(InstrBooleanVisitor visitor) {
        visitor.visit(this);
    }
}
