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
 * This is an {@link Criterion} describing StatementCoverage.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: StatementCoverage.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class StatementCoverage extends AbstractCriterion {

    private static final StatementCoverage instance = new StatementCoverage();
    
    private final String INVOCATION_METHOD = "getInstance";

    /**
     * The prefix of statement ID's:<br>
     * <code>{@value #ID_PREFIX}</code>
     */
    public static final String ID_PREFIX = "S";

    /**
     * the String "<b>StatementCoverage</b>"
     */
    public static final String NAME = "StatementCoverage";

    /**
     * @return The single instance of StatementCoverage;
     */
    public static StatementCoverage getInstance() {
        return instance;
    }

    private StatementCoverage() {
        super(NAME, "org.codecover.model.utils.criteria.StatementCoverage", "org.codecover");
    }

}
