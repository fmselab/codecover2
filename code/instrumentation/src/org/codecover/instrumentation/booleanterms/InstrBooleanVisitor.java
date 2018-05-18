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
 * This is a visitor for the boolean terms of the instrumenter.<br>
 * <br>
 * It follows the visitor pattern.
 * 
 * @author Christoh Müller
 *
 * @version 1.0 ($Id: InstrBooleanVisitor.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface InstrBooleanVisitor {

    /**
     * Visit a {@link InstrBasicBooleanTerm}.
     * 
     * @param term the {@link InstrBasicBooleanTerm}.
     */
    public void visit(InstrBasicBooleanTerm term);

    /**
     * Visit a {@link InstrBracketTerm}.
     * 
     * @param term the {@link InstrBracketTerm}.
     */
    public void visit(InstrBracketTerm term);

    /**
     * Visit a {@link InstrOperatorTerm}.
     * 
     * @param term the {@link InstrOperatorTerm}.
     */
    public void visit(InstrOperatorTerm term);

    /**
     * Visit a {@link InstrBooleanOperator}.
     * 
     * @param operator the {@link InstrBooleanOperator}.
     */
    public void visit(InstrBooleanOperator operator);
}