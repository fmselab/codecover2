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
 * A Branch is a branch which can be taken in a conditonal statement. It
 * consists of a StatementSequence, a CoverableItem which is covered when the
 * branch is executed, a tag which says that this branch does not appear
 * explicitly in the source code (e.g. a else branch when there is no else
 * keyword for a if statement or the default branch of a select statement where
 * there is no default: block) and optionally the LocationList of the conditon
 * whether this branch is taken (for the purpose of coloring the source code).
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: Branch.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public final class Branch extends AbstractLocatableMetaDataObject {
    private final CoverableItem coverableItem;

    private final StatementSequence sequence;

    private final boolean implicit;

    private final LocationList decision;

    Branch(LocationList location, CoverableItem coverableItem,
            boolean implicit, LocationList decision, StatementSequence sequence) {
        super(location);

        if (decision == null) {
            throw new NullPointerException("decision == null");
        }

        if (coverableItem == null) {
            throw new NullPointerException("coverableItem == null");
        }

        if (sequence == null) {
            throw new NullPointerException("sequence == null");
        }
        
        this.coverableItem = coverableItem;
        this.implicit = implicit;
        this.decision = decision;
        this.sequence = sequence;
    }

    /**
     * @return the condition
     */
    public LocationList getDecision() {
        return this.decision;
    }

    /**
     * @return the coverableItem
     */
    public CoverableItem getCoverableItem() {
        return this.coverableItem;
    }

    /**
     * @return the implicit
     */
    public boolean isImplicit() {
        return this.implicit;
    }

    /**
     * @return the sequence
     */
    public StatementSequence getSequence() {
        return this.sequence;
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
        int result = getSequence().getStatements().size();
        for (Location location : getLocation().getLocations()) {
            result += location.getStartOffset();
            result += location.getEndOffset();
            result += location.getFile().hashCode();
            result += location.getFile().getContent().hashCode();
        }

        return result;
    }

    /**
     * Operations on the {@link Branch} can be performed here.
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
            BooleanTerm.Visitor termPre, BooleanTerm.Visitor termPost) {
        if (pre != null) {
            pre.visit(this);
        }

        getSequence().accept(pre, post, rootTermPre, rootTermPost, termPre,
                termPost, null);

        if (post != null) {
            post.visit(this);
        }
    }
}
