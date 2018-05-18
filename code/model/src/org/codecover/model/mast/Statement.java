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

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.codecover.model.utils.CollectionUtil;

/**
 * A Statement is a basic or a complex statement. Every Statement either has the
 * type BasicStatement or on of the types derived of ComplexStatement,
 * ConditionalStatement and LoopingStatement. A Statement has a list of
 * RootTerms which appear in the statement and a CoverableItem which will be
 * covered when the Statement is executed.
 *
 * @author Steffen Kieß
 * @version 1.0 ($Id: Statement.java 93 2013-02-22 13:19:14Z hanikesn $)
 */
public abstract class Statement extends AbstractLocatableMetaDataObject {
    private final CoverableItem coverableItem;

    private final Set<RootTerm> terms;
    private final Set<QuestionMarkOperator> questionMarkOperators;

    Statement(LocationList location, CoverableItem coverableItem,
            Set<RootTerm> terms, Set<QuestionMarkOperator> questionMarkOperators) {
        super(location);
        
        if (coverableItem == null) {
            throw new NullPointerException("coverableItem == null");
        }
        
        this.coverableItem = coverableItem;
        this.terms = terms == null ? new HashSet<RootTerm>() : CollectionUtil.copy(terms);
        this.questionMarkOperators = questionMarkOperators == null ? new HashSet<QuestionMarkOperator>() : CollectionUtil.copy(questionMarkOperators);
        
        // the QMO are copied, now clean the QMO
        if(questionMarkOperators != null)
            questionMarkOperators.clear();
    }

    /**
     * @return the coverableItem
     */
    public CoverableItem getCoverableItem() {
        return this.coverableItem;
    }

    /**
     * @return the terms
     */
    public Set<RootTerm> getTerms() {
        return this.terms;
    }

    /**
     * @return the questionMarkOperators
     */
    public Set<QuestionMarkOperator> getQuestionMarkOperators() {
        return this.questionMarkOperators;
    }

    /**
     * The {@link org.codecover.model.mast.Statement.Visitor Visitor} to be used
     * for all {@link Statement}s
     *
     * @author Steffen Kieß, Markus Wittlinger
     * @version 1.0 ($Id: Statement.java 93 2013-02-22 13:19:14Z hanikesn $)
     */
    public static interface Visitor {
        /**
         * Visits a {@link BasicStatement}
         *
         * @param statement
         *            the given {@link BasicStatement}
         */
        void visit(BasicStatement statement);

        /**
         * Visits a {@link ConditionalStatement}
         *
         * @param statement
         *            the given {@link ConditionalStatement}
         */
        void visit(ConditionalStatement statement);

        /**
         * Visits a {@link SynchronizedStatement}
         *
         * @param statement
         *            the given {@link SynchronizedStatement}
         */
        void visit(SynchronizedStatement statement);

        /**
         * Visits a {@link LoopingStatement}
         *
         * @param statement
         *            the given {@link LoopingStatement}
         */
        void visit(LoopingStatement statement);

        /**
         * Visits a {@link StatementSequence}
         *
         * @param sequence
         *            the given {@link StatementSequence}
         */
        void visit(StatementSequence sequence);

        /**
         * Visits a {@link Branch}
         *
         * @param branch
         *            the given {@link Branch}
         */
        void visit(Branch branch);
    }

    /**
     * A default, empty implementation of the
     * {@link org.codecover.model.mast.Statement.Visitor Visitor}
     *
     * @author Steffen Kieß, Markus Wittlinger
     * @version 1.0 ($Id: Statement.java 93 2013-02-22 13:19:14Z hanikesn $)
     */
    public static class DefaultVisitor implements Visitor {
        /**
         * (non-Javadoc)
         *
         * @see org.codecover.model.mast.Statement.Visitor#visit(org.codecover.model.mast.BasicStatement)
         */
        public void visit(BasicStatement statement) {
            // Do nothing.
        }

        /**
         * (non-Javadoc)
         *
         * @see org.codecover.model.mast.Statement.Visitor#visit(org.codecover.model.mast.ConditionalStatement)
         */
        public void visit(ConditionalStatement statement) {
            // Do nothing.
        }

        /**
         * (non-Javadoc)
         *
         * @see org.codecover.model.mast.Statement.Visitor#visit(org.codecover.model.mast.LoopingStatement)
         */
        public void visit(LoopingStatement statement) {
            // Do nothing.
        }

        /**
         * (non-Javadoc)
         *
         * @see org.codecover.model.mast.Statement.Visitor#visit(org.codecover.model.mast.SynchronizedStatement)
         */
        public void visit(SynchronizedStatement statement) {
            // Do nothing.
        }

        /**
         * (non-Javadoc)
         *
         * @see org.codecover.model.mast.Statement.Visitor#visit(org.codecover.model.mast.StatementSequence)
         */
        public void visit(StatementSequence sequence) {
            // Do nothing.
        }

        /**
         * (non-Javadoc)
         *
         * @see org.codecover.model.mast.Statement.Visitor#visit(org.codecover.model.mast.Branch)
         */
        public void visit(Branch branch) {
            // Do nothing.
        }
    }

    /**
     * Calls the
     * {@link RootTerm#accept(org.codecover.model.mast.RootTerm.Visitor, org.codecover.model.mast.RootTerm.Visitor, org.codecover.model.mast.BooleanTerm.Visitor, org.codecover.model.mast.BooleanTerm.Visitor)}
     * for each {@link RootTerm} in this {@link Statement}
     *
     * @param pre
     *            the {@link Visitor} to be called before any operations are
     *            performed
     * @param post
     *            the {@link Visitor} to be called after any operations were
     *            performed
     * @param rootTermPre
     *            the {@link org.codecover.model.mast.RootTerm.Visitor Visitor}
     *            to be used in accepting the {@link RootTerm}
     * @param rootTermPost
     *            the {@link org.codecover.model.mast.RootTerm.Visitor Visitor}
     *            to be used in accepting the {@link RootTerm}
     * @param termPre
     *            the
     *            {@link org.codecover.model.mast.BooleanTerm.Visitor Visitor}
     *            to be used in accepting the {@link RootTerm}
     * @param termPost
     *            the
     *            {@link org.codecover.model.mast.BooleanTerm.Visitor Visitor}
     *            to be used in accepting the {@link RootTerm}
     */
    public void accept(Visitor pre, Visitor post, RootTerm.Visitor rootTermPre,
            RootTerm.Visitor rootTermPost, BooleanTerm.Visitor termPre,
            BooleanTerm.Visitor termPost, QuestionMarkOperator.Visitor qmoVisitor) {
        if (rootTermPre != null || rootTermPost != null || termPre != null
                || termPost != null) {
            for (RootTerm term : getTerms()) {
                term.accept(rootTermPre, rootTermPost, termPre, termPost);
            }
        }
        
        if(qmoVisitor != null) {
            for (QuestionMarkOperator qmo : getQuestionMarkOperators()) {
            	qmo.accept(qmoVisitor);
            }
        }
    }

}
