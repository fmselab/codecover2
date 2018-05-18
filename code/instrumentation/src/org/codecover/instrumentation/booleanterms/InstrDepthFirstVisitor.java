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

/**
 * A DepthFirstVisitor for {@link InstrBooleanVisitor}.<br>
 * <br>
 * The tree is traversed until the {@link InstrBasicBooleanTerm}s are reached.
 * 
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: InstrDepthFirstVisitor.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InstrDepthFirstVisitor implements InstrBooleanVisitor {

    public void visit(InstrBasicBooleanTerm term) {
        // Leaf reached
    }

    public void visit(InstrBracketTerm term) {
        term.getInnerTerm().access(this);
    }

    public void visit(InstrOperatorTerm term) {
        for (InstrBooleanTerm thisTerm : term.getOperands()) {
            thisTerm.access(this);
        }

        visit(term.getOperator());
    }

    public void visit(InstrBooleanOperator operator) {
        // Operator reached
    }
}
