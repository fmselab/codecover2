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

import java.util.*;

import org.codecover.model.mast.Statement.Visitor;

/**
 * A LoopingStatement is a statement which has a body which can be executed a
 * number of times not known at compile time. It has a StatementSequence
 * representing the body, the LocationList of the keyword of the statement (for
 * the purpose of coloring the source code), a boolean tag indicating whether
 * the body also can be executed zero times and three CoverableItems covered
 * when the body is executed zero times, one time or more often.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: LoopingStatement.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public final class LoopingStatement extends ComplexStatement {
    private final StatementSequence body;

    private final CoverableItem neverExecutedItem;

    private final CoverableItem onceExecutedItem;

    private final CoverableItem multipleExecutedItem;

    private final boolean optionalBodyExecution;

    LoopingStatement(LocationList location, CoverableItem coverableItem,
            Set<RootTerm> terms, StatementSequence body, Location keyword,
            CoverableItem neverExecutedItem, CoverableItem onceExecutedItem,
            CoverableItem multipleExecutedItem, boolean optionalBodyExecution, Set<QuestionMarkOperator> questionMarkOperators) {
        super(location, keyword, coverableItem, terms, questionMarkOperators);

        if (body == null) {
            throw new NullPointerException("body == null");
        }
        if (neverExecutedItem == null) {
            throw new NullPointerException("neverExecutedItem == null");
        }
        if (onceExecutedItem == null) {
            throw new NullPointerException("onceExecutedItem == null");
        }
        if (multipleExecutedItem == null) {
            throw new NullPointerException("multipleExecutedItem == null");
        }

        this.body = body;
        this.neverExecutedItem = neverExecutedItem;
        this.onceExecutedItem = onceExecutedItem;
        this.multipleExecutedItem = multipleExecutedItem;
        this.optionalBodyExecution = optionalBodyExecution;
    }

    /**
     * @return the body
     */
    public StatementSequence getBody() {
        return this.body;
    }

    /**
     * @return the multipleExecutedItem
     */
    public CoverableItem getMultipleExecutedItem() {
        return this.multipleExecutedItem;
    }

    /**
     * @return the neverExecutedItem
     */
    public CoverableItem getNeverExecutedItem() {
        return this.neverExecutedItem;
    }

    /**
     * @return the onceExecutedItem
     */
    public CoverableItem getOnceExecutedItem() {
        return this.onceExecutedItem;
    }

    /**
     * @return the optionalBodyExecution
     */
    public boolean isOptionalBodyExecution() {
        return this.optionalBodyExecution;
    }

    /**
     * Returns a hash code value for the object. This method is supported for
     * the benefit of hashtables such as those provided by
     * <code>java.util.Hashtable</code>.
     * 
     * @return a hash code value for this object.
     */
    @Override
    public int hashCode() {
        int result = getBody().getStatements().size();
        for (Location location : getLocation().getLocations()) {
            result += location.getStartOffset();
            result += location.getEndOffset();
            result += location.getFile().hashCode();
            result += location.getFile().getContent().hashCode();
        }

        return result;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.model.mast.Statement#accept(org.codecover.model.mast.Statement.Visitor,
     *      org.codecover.model.mast.Statement.Visitor,
     *      org.codecover.model.mast.RootTerm.Visitor,
     *      org.codecover.model.mast.RootTerm.Visitor,
     *      org.codecover.model.mast.BooleanTerm.Visitor,
     *      org.codecover.model.mast.BooleanTerm.Visitor)
     */
    @Override
    public void accept(Visitor pre, Visitor post, RootTerm.Visitor rootTermPre,
            RootTerm.Visitor rootTermPost, BooleanTerm.Visitor termPre,
            BooleanTerm.Visitor termPost, QuestionMarkOperator.Visitor qmoVisitor) {
        if (pre != null) {
            pre.visit(this);
        }
        super.accept(pre, post, rootTermPre, rootTermPost, termPre, termPost, null);
        this.body.accept(pre, post, rootTermPre, rootTermPost, termPre,
                termPost, qmoVisitor);
        if (post != null) {
            post.visit(this);
        }
    }
}
