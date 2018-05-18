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
 * A ComplexStatement is a statement which can contain other statements and is
 * either a ConditionalStatement or a LoopingStatement.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: ComplexStatement.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public abstract class ComplexStatement extends Statement {
    private final Location keyword;

    protected ComplexStatement(LocationList location, Location keyword,
            CoverableItem coverableItem, Set<RootTerm> terms, Set<QuestionMarkOperator> questionMarkOperators) {
        super(location, coverableItem, terms, questionMarkOperators);

        if (keyword == null) {
            throw new NullPointerException("keyword == null");
        }

        this.keyword = keyword;
    }

    /**
     * @return the keyword
     */
    public Location getKeyword() {
        return this.keyword;
    }
}
