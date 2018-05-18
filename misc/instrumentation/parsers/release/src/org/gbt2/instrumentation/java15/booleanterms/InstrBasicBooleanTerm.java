///////////////////////////////////////////////////////////////////////////////
//
// $Id: InstrBasicBooleanTerm.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 09.04.2007 16:28:28
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.booleanterms;

import java.util.Queue;

import org.gbt2.instrumentation.java15.syntaxtree.RelationalExpression;

/**
 * This class extends the {@link InstrBooleanTerm} and represents a Basic
 * Boolean Term.<br>
 * <br>
 * A {@link InstrBasicBooleanTerm} is meant to be the leaf of the boolean term
 * tree, that can not be subdivided into smaller {@link InstrBasicBooleanTerm}s.<br>
 * Therefore a {@link InstrBasicBooleanTerm} might be a
 * {@link RelationalExpression} of the syntaxtree.<br>
 * The method {@link #termToString()} just returns the image of the element of
 * the syntaxtree. The method {@link #getAllBasicBooleanTerms(Queue)} adds this
 * object to the Queue.
 * 
 * @author Christoph Müller
 * @see InstrBooleanTerm
 */
public class InstrBasicBooleanTerm extends InstrBooleanTerm {
    private String image;

    /**
     * Creates a new {@link InstrBasicBooleanTerm} from the given image.
     * 
     * @param image
     *            The image of the {@link InstrBasicBooleanTerm}&mdash;e.g.<br>
     *            "<code>vector.size() > 10</code>".
     */
    public InstrBasicBooleanTerm(String image) {
        this.image = image;
    }

    @Override
    public String termToString() {
        return this.image;
    }

    @Override
    public void getAllBasicBooleanTerms(Queue<InstrBasicBooleanTerm> termQueue) {
        termQueue.add(this);
    }
}
