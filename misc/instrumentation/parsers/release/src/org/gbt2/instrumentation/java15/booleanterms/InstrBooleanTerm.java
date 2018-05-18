///////////////////////////////////////////////////////////////////////////////
//
// $Id: InstrBooleanTerm.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 09.04.2007 16:27:18
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.booleanterms;

import java.util.Queue;

import org.gbt2.instrumentation.java15.syntaxtree.Expression;

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
 * using the method {@link #getAllBasicBooleanTerms(Queue)}. These methods are all
 * handled recursively through the whole boolean tree. Its leafs are the
 * {@link InstrBasicBooleanTerm} and the nodes are mainly created by
 * {@link InstrOperatorTerm}.<br>
 * A {@link InstrBooleanTerm} can be created by parsing an {@link Expression}
 * using {@link InstrExpressionParser#parse(Expression)}.
 * 
 * @author Christoph Müller
 */
public abstract class InstrBooleanTerm {
    /**
     * Transforms the {@link InstrBooleanTerm} to a String.<br>
     * <br>
     * This String will be equivalent to the parsed {@link Expression}.
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
     * This method collects all {@link InstrBasicBooleanTerm}s of this
     * {@link InstrBooleanTerm} and its children.<br>
     * <br>
     * This object and all children are ordered to add all
     * {@link InstrBasicBooleanTerm}s to the given Queue. Therefor this Queue
     * is exprected to be not null and empty.
     * 
     * @param termQueue
     *            The Queue, where all {@link InstrBasicBooleanTerm}s are
     *            added.
     */
    public abstract void getAllBasicBooleanTerms(
            Queue<InstrBasicBooleanTerm> termQueue);
}
