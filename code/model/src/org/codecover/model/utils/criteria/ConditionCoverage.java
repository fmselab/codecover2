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
 * This is an {@link Criterion} describing ConditionCoverage.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: ConditionCoverage.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ConditionCoverage extends AbstractCriterion {
    private static final ConditionCoverage instance = new ConditionCoverage();

    private final String INVOCATION_METHOD = "getInstance";

    /**
     * The prefix of condition ID's:<br>
     * <code>{@value #ID_PREFIX}</code>
     */
    public static final String ID_PREFIX = "C";

    /**
     * the String "<b>ConditionCoverage</b>"
     */
    public static final String NAME = "ConditionCoverage";

    /**
     * @return The single instance of ConditionCoverage;
     */
    public static ConditionCoverage getInstance() {
        return instance;
    }

    private ConditionCoverage() {
        super(NAME, "org.codecover.model.utils.criteria.ConditionCoverage", "org.codecover");
    }
}
