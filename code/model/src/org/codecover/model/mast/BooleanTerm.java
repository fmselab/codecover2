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

package org.codecover.model.mast;

/**
 * A BooleanTerm represents a boolean term which ist constructed of basic
 * boolean terms and boolean operators. A BooleanTerm can be a BasicBooleanTerm
 * or a OperatorTerm.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: BooleanTerm.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public abstract class BooleanTerm extends AbstractLocatableMetaDataObject {

    private final int basicBooleanTerms;

    BooleanTerm(LocationList location, int basicBooleanTerms) {
        super(location);
        this.basicBooleanTerms = basicBooleanTerms;
    }

    /**
     * Gets the number of {@link BasicBooleanTerm}s below this
     * {@link BooleanTerm}
     * 
     * @return the number of {@link BasicBooleanTerm}s
     */
    public int getBasicBooleanTerms() {
        return this.basicBooleanTerms;
    }

    /**
     * The {@link org.codecover.model.mast.BooleanTerm.Visitor Visitor} to be
     * used for all {@link BooleanTerm}s
     * 
     * @author Steffen Kieß, Markus Wittlinger
     * @version 1.0 ($Id: BooleanTerm.java 1 2007-12-12 17:37:26Z t-scheller $)
     * 
     */
    public static interface Visitor {
        /**
         * Visits a {@link BasicBooleanTerm}
         * 
         * @param term
         *            the given {@link BasicBooleanTerm}
         */
        void visit(BasicBooleanTerm term);

        /**
         * Visits an {@link OperatorTerm}
         * 
         * @param term
         *            the given {@link OperatorTerm}
         */
        void visit(OperatorTerm term);
    }

    /**
     * A default, empty implementation of the
     * {@link org.codecover.model.mast.BooleanTerm.Visitor Visitor}
     * 
     * @author Steffen Kieß, Markus Wittlinger
     * @version 1.0 ($Id: BooleanTerm.java 1 2007-12-12 17:37:26Z t-scheller $)
     * 
     */
    public static class DefaultVisitor implements Visitor {
        /**
         * (non-Javadoc)
         * 
         * @see org.codecover.model.mast.BooleanTerm.Visitor#visit(org.codecover.model.mast.BasicBooleanTerm)
         */
        public void visit(BasicBooleanTerm term) {
            // Do nothing.
        }

        /**
         * (non-Javadoc)
         * 
         * @see org.codecover.model.mast.BooleanTerm.Visitor#visit(org.codecover.model.mast.OperatorTerm)
         */
        public void visit(OperatorTerm term) {
            // Do nothing.
        }
    }

    /**
     * Operations on the subclass of {@link BooleanTerm} can be performed here.
     * 
     * @param pre
     *            the
     *            {@link org.codecover.model.mast.BooleanTerm.Visitor Visitor}
     *            to be called before any operations are preformed.
     * @param post
     *            the
     *            {@link org.codecover.model.mast.BooleanTerm.Visitor Visitor}
     *            to be called after any operations were preformed.
     */
    public abstract void accept(Visitor pre, Visitor post);
}
