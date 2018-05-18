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

import java.util.List;

import org.codecover.model.utils.CollectionUtil;

/**
 * A StatementSequence is a list of Statements.
 * 
 * @author Markus Wittlinger, Steffen Kieß
 * @version 1.0 ($Id: StatementSequence.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public class StatementSequence extends AbstractLocatableMetaDataObject {
    private final List<Statement> statements;

    StatementSequence(LocationList location, List<Statement> statements) {
        super(location);

        if (statements == null) {
            throw new NullPointerException("statements == null");
        }

        this.statements = CollectionUtil.copy(statements);
    }

    /**
     * @return the statements
     */
    public List<Statement> getStatements() {
        return this.statements;
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
        int result = getStatements().size();
        for (Location location : getLocation().getLocations()) {
            result += location.getStartOffset();
            result += location.getEndOffset();
            result += location.getFile().hashCode();
            result += location.getFile().getContent().hashCode();
        }

        return result;
    }

    /**
     * Operations on the {@link StatementSequence} can be performed here.
     * 
     * @param pre
     *            the {@link org.codecover.model.mast.Statement.Visitor Visitor}
     *            to be called before any operations are performed
     * @param post
     *            the {@link org.codecover.model.mast.Statement.Visitor Visitor}
     *            to be called after any operations were performed
     * @param rootTermPre
     *            the {@link org.codecover.model.mast.RootTerm.Visitor Visitor}
     *            to be used in accepting the {@link StatementSequence}
     * @param rootTermPost
     *            the {@link org.codecover.model.mast.RootTerm.Visitor Visitor}
     *            to be used in accepting the {@link StatementSequence}
     * @param termPre
     *            the
     *            {@link org.codecover.model.mast.BooleanTerm.Visitor Visitor}
     *            to be used in accepting the {@link StatementSequence}
     * @param termPost
     *            the
     *            {@link org.codecover.model.mast.BooleanTerm.Visitor Visitor}
     *            to be used in accepting the {@link StatementSequence}
     */
    public void accept(Statement.Visitor pre, Statement.Visitor post,
            RootTerm.Visitor rootTermPre, RootTerm.Visitor rootTermPost,
            BooleanTerm.Visitor termPre, BooleanTerm.Visitor termPost, 
            QuestionMarkOperator.Visitor qmaVisitor) {
        if (pre != null) {
            pre.visit(this);
        }
        for (Statement statement : getStatements()) {
            statement.accept(pre, post, rootTermPre, rootTermPost, termPre,
                    termPost, qmaVisitor);
        }
        if (post != null) {
            post.visit(this);
        }
    }
}
