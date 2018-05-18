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

package org.codecover.metrics.correlation;

import java.util.*;

import org.codecover.model.*;
import org.codecover.model.mast.*;
import org.codecover.model.utils.criteria.Criterion;

/**
 * This class calculates the correlation of the {@link Statement}s.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: StatementCorrelation.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public class StatementCorrelation extends AbstractCorrelationMetric {

    private static final String NAME = "Statement Correlation";

    private static final String DESCRIPTION = "";

    private static StatementCorrelation instance;

    /**
     * This method returns an instance of {@link StatementCorrelation}
     * 
     * @return the instance of {@link StatementCorrelation}
     */
    public static StatementCorrelation getInstance() {
        if (instance == null) {
            instance = new StatementCorrelation();
        }

        return instance;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.metrics.correlation.CorrelationMetric#calculateCorrelation(java.util.List)
     */
    public CorrelationResult calculateCorrelation(List<TestCase> testCases) {
        if (testCases.size() == 0) {
            throw new IllegalArgumentException("testCases.size() == 0");
        }

        if (!checkTestCases(testCases)) {
            throw new IllegalArgumentException("Not all test cases have the same test session container");
        }

        TestSessionContainer tsc = testCases.get(0)
                                            .getTestSession()
                                            .getTestSessionContainer();

        // Collect only the coverable items of the statements, for use in the
        // calculations.
        final Set<CoverableItem> coverableItemSet = new HashSet<CoverableItem>();
        tsc.getCode().accept(null, null, new Statement.DefaultVisitor() {
            private void add(CoverableItem item) {
                if (item != null) {
                    coverableItemSet.add(item);
                }
            }

            @Override
            public void visit(BasicStatement statement) {
                add(statement.getCoverableItem());
            }

            @Override
            public void visit(ConditionalStatement statement) {
                add(statement.getCoverableItem());
            }

            @Override
            public void visit(LoopingStatement statement) {
                add(statement.getCoverableItem());
            }
        }, null, null, null, null, null, null);

        return super.calculateCorrelation(coverableItemSet, testCases);
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.metrics.Metric#getDescription()
     */
    public String getDescription() {
        return DESCRIPTION;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.metrics.Metric#getName()
     */
    public String getName() {
        return NAME;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.metrics.Metric#getRequiredCriteria()
     */
    public Set<Criterion> getRequiredCriteria() {
        Set<Criterion> criteria = new TreeSet<Criterion>();

        criteria.add(org.codecover.model.utils.criteria.StatementCoverage.getInstance());

        return criteria;
    }
}
