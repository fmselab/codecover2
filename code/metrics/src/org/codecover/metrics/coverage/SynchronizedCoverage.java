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

package org.codecover.metrics.coverage;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;

import org.codecover.model.TestCase;
import org.codecover.model.mast.BasicStatement;
import org.codecover.model.mast.Branch;
import org.codecover.model.mast.ConditionalStatement;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.LoopingStatement;
import org.codecover.model.mast.QuestionMarkOperator;
import org.codecover.model.mast.QuestionMarkOperatorExpression;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.Statement;
import org.codecover.model.mast.StatementSequence;
import org.codecover.model.mast.SynchronizedStatement;
import org.codecover.model.utils.criteria.Criterion;

/**
 * This class would need to override getCoverage(testCases, statement).
 *
 * @author RS
 * @version 1.0 ($Id: StatementCoverage.java 64 2009-09-28 15:11:11Z ahija $)
 */
public class SynchronizedCoverage extends AbstractCoverageMetric {

    /**
     * @author RS
     * @version 1.0 ($Id: StatementCoverage.java 64 2009-09-28 15:11:11Z ahija $)
     */
    public static interface SynchronizedHint extends Hint {
        /**
         * Gets the number of executions
         * @return the number of executions
         */
        public long getNumberOfExecutions();
    }

    private static final class SynchronizedHintImpl implements SynchronizedHint {
        private long numberOfExecutions;

        /**
         * Constructor.
         *
         * @param numberOfExecutions
         *            the number of executions.
         */
        public SynchronizedHintImpl(long numberOfExecutions) {
            if (numberOfExecutions < 0) {
                throw new IllegalArgumentException("numberOfExecutions < 0");
            }

            this.numberOfExecutions = numberOfExecutions;
        }

        public long getNumberOfExecutions() {
            return this.numberOfExecutions;
        }
    }

    private static final String CACHING_KEY = "SynchronizedCoverage";

    private static final String DESCRIPTION = "";

    private static final String NAME = "Synchronized Coverage";

    private static SynchronizedCoverage instance = new SynchronizedCoverage();

    private SynchronizedCoverage() {
        super(CACHING_KEY);
    }

    /**
     * This method returns an instance of StatementCoverage.
     *
     * @return instance of StatementCoverage.
     */
    public static SynchronizedCoverage getInstance() {
        return instance;
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
        return Collections.<Criterion>singleton(
            org.codecover.model.utils.criteria.SynchronizedStatementCoverage.getInstance());
    }


    /**
     * Calculates the
     * {@link org.codecover.model.utils.criteria.SynchronizedStatementCoverage}
     *
    */
    public CoverageResult getCoverageLocal(Collection<TestCase> testCases, Statement statement) {
    	    	
    	if(!(statement instanceof SynchronizedStatement)) return  CoverageResult.NULL;
    		
    	SynchronizedStatement synchronizedStatement = (SynchronizedStatement)statement;
    	
    	int synchronized0 = 0;
    	int synchronized1 = 0;
    	int synchronized2 = 0;
    	
    	// a QuestionMarkOperator is covered, when both Expressions are covered
        for (TestCase testCase : testCases) {
            if (testCase.getCoverageCount(synchronizedStatement.getCoverableItem(0)) > 0) {
            	synchronized0 = 1;
            }
            if (testCase.getCoverageCount(synchronizedStatement.getCoverableItem(1)) > 0) {
            	synchronized1 = 1;
            }

            if (testCase.getCoverageCount(synchronizedStatement.getCoverableItem(2)) > 0) {
            	synchronized2 = 1;
            }
            
            if(synchronized0 == 1 && synchronized1 == 1 && synchronized2 == 1) break;
        }

        return new CoverageResult(synchronized0 + synchronized1 + synchronized2, 3);
    }


    public Set<Hint> getHints(Collection<TestCase> testCases, SynchronizedStatement synchronizedStatement) {
  
    	int executions = 0;
        for (final TestCase testCase : testCases) {
            executions += testCase.getCoverageCount(synchronizedStatement.getCoverableItem());
        }

        final Hint hint = new SynchronizedHintImpl(executions);
        return Collections.singleton(hint);
    }


}
