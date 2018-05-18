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
 * This is a representative of a boolean term of the syntaxtree.<br>
 * <br>
 * It is similar to the BooleanTerm of the MAST.<br>
 * This class is abstract - implementations of this abstract class are:
 * <ul>
 * <li>{@link InstrBasicBooleanTerm}</li>
 * <li>{@link InstrOperatorTerm}</li>
 * <li>{@link InstrBracketTerm}</li>
 * </ul>
 * <br>
 * A BooleanTerm can be transformed to a String {@link #termToString()},
 * {@link #toString()}. All {@link InstrBasicBooleanTerm}s can be grabbed by
 * using the method {@link #getAllBasicBooleanTerms(List)}. These methods are
 * all handled recursively through the whole boolean tree. Its leafs are the
 * {@link InstrBasicBooleanTerm} and the nodes are mainly created by
 * {@link InstrOperatorTerm}.<br>
 * A {@link InstrBooleanTerm} can be created by parsing an Expression using a
 * ExpressionParser.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: InstrBooleanTerm.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public abstract class InstrBooleanTerm {
    /**
     * Transforms the {@link InstrBooleanTerm} to a String.<br>
     * <br>
     * This String will be equivalent to the parsed Expression.
     * 
     * @return The {@link InstrBooleanTerm} as a String.
     */
    public abstract String termToString();

    /**
     * Same as {@link #termToString()}.
     * 
     * @return Same as {@link #termToString()}.
     * @see #termToString()
     */
    @Override
    public String toString() {
        return termToString();
    }

    /**
     * Transforms the boolean term to a String and writes it's content to the
     * target {@link Writer}.<br>
     * <br>
     * Instead of creating this String hierarchically, each boolean term can
     * also put its image directl< to the writer and avoid expensive String
     * concatenations.
     * 
     * @param target
     *            The target {@link Writer}.
     * 
     * @throws IOException
     *             If there occur errors when using {@link Writer#write(String)}.
     */
    public abstract void writeToTarget(Writer target) throws IOException;

    /**
     * This method collects all {@link InstrBasicBooleanTerm}s of this
     * {@link InstrBooleanTerm} and its children.<br>
     * <br>
     * This object and all children are ordered to add all
     * {@link InstrBasicBooleanTerm}s to the given Queue. Therefor this Queue
     * is exprected to be not null and empty.
     * 
     * @param termList
     *            The List, where all {@link InstrBasicBooleanTerm}s are
     *            added.
     */
    public abstract void getAllBasicBooleanTerms(
            List<InstrBasicBooleanTerm> termList);

    /**
     * Transforms this representation of a boolean term into a
     * {@link BooleanTerm} of the MAST.
     * 
     * @param builder
     *            The MASTBuilder to use for creation methods.
     * @param sourceFile
     *            The source file to use for location creation.
     * 
     * @return A {@link BooleanTerm} of the MAST.
     */
    public abstract BooleanTerm toBooleanTerm(MASTBuilder builder,
            SourceFile sourceFile);

    /**
     * Just call: <code>visitor.visit(this);</code>.
     * 
     * @param visitor
     *          A {@link InstrBooleanVisitor} who wants to traverse the boolean
     *          tree.
     */
    public abstract void access(InstrBooleanVisitor visitor);
}
