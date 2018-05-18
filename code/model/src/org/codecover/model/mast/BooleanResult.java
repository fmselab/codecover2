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
 * A BooleanResult is an enum which describes the result of the evaluation of a
 * boolean term. The value NOT_EVALUATED is used if a subterm was not evaluated,
 * e.g. because of the short circuit behaviour of an operator.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: BooleanResult.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public enum BooleanResult {
    /**
     * Represents the {@link Boolean} value {@link Boolean#FALSE}
     */
    FALSE,
    /**
     * Represents the {@link Boolean} value {@link Boolean#TRUE}
     */
    TRUE,
    /**
     * This value is used if a subterm was not evaluated, e.g. because of the
     * short circuit behaviour of an operator
     */
    NOT_EVALUATED, ;

}
