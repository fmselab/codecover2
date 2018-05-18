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

/**
 * A BasicStatement is a statement which contains no other statements.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: BasicStatement.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public final class BasicStatement extends Statement {

    BasicStatement(LocationList location, CoverableItem coverableItem,
            Set<RootTerm> terms, Set<QuestionMarkOperator> questionMarkOperators) {
        super(location, coverableItem, terms, questionMarkOperators);
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
        int result = 0;
        for (Location location : getLocation().getLocations()) {
            result += location.getStartOffset();
            result += location.getEndOffset();
            result += location.getFile().hashCode();
            result += getCoverableItem().getId().hashCode();
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
            BooleanTerm.Visitor termPost, QuestionMarkOperator.Visitor qmaVisitor) {
        if (pre != null) {
            pre.visit(this);
        }
        super.accept(pre, post, rootTermPre, rootTermPost, termPre, termPost, qmaVisitor);
        if (post != null) {
            post.visit(this);
        }
    }
}
