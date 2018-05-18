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

package org.codecover.model.utils.criteria;

/**
 * This is an {@link Criterion} describing LoopCoverage.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: LoopCoverage.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class LoopCoverage extends AbstractCriterion {

    private static final LoopCoverage instance = new LoopCoverage();

    private final String INVOCATION_METHOD = "getInstance";

    /**
     * The prefix of loop ID's:<br>
     * <code>{@value #ID_PREFIX}</code>
     */
    public static final String ID_PREFIX = "L";

    /**
     * The suffix of the loop sub ID for zero:<br>
     * <code>{@value #ID_SUFFIX_ZERO}</code>
     */
    public static final String ID_SUFFIX_ZERO = "-0";

    /**
     * The suffix of the loop sub ID for one:<br>
     * <code>{@value #ID_SUFFIX_ONE}</code>
     */
    public static final String ID_SUFFIX_ONE = "-1";

    /**
     * The suffix of the loop sub ID for more than one:<br>
     * <code>{@value #ID_SUFFIX_ABOVE}</code>
     */
    public static final String ID_SUFFIX_ABOVE = "-2";

    /** the String "<b>LoopCoverage</b>" */
    public static final String NAME = "LoopCoverage";

    /**
     * @return The single instance of LoopCoverage;
     */
    public static LoopCoverage getInstance() {
        return instance;
    }

    private LoopCoverage() {
        super(NAME, "org.codecover.model.utils.criteria.LoopCoverage", "org.codecover");
    }
}
