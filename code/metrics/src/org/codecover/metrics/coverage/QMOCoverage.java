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
import org.codecover.model.utils.criteria.Criterion;

/**
 * This class would need to override getCoverage(testCases, statement).
 *
 * @author RS
 * @version 1.0 ($Id: StatementCoverage.java 64 2009-09-28 15:11:11Z ahija $)
 */
public class QMOCoverage extends AbstractCoverageMetric {

    /**
     * @author RS
     * @version 1.0 ($Id: StatementCoverage.java 64 2009-09-28 15:11:11Z ahija $)
     */
    public static interface QMOHint extends Hint {
        /**
         * Gets the number of executions
         * @return the number of executions
         */
        public long getNumberOfExecutions();
    }

    private static final class QMOHintImpl implements QMOHint {
        private long numberOfExecutions;

        /**
         * Constructor.
         *
         * @param numberOfExecutions
         *            the number of executions.
         */
        public QMOHintImpl(long numberOfExecutions) {
            if (numberOfExecutions < 0) {
                throw new IllegalArgumentException("numberOfExecutions < 0");
            }

            this.numberOfExecutions = numberOfExecutions;
        }

        public long getNumberOfExecutions() {
            return this.numberOfExecutions;
        }
    }

    private static final String CACHING_KEY = "QMOCoverage";

    private static final String DESCRIPTION = "";

    private static final String NAME = "?-Operator Coverage";

    private static QMOCoverage instance = new QMOCoverage();

    private QMOCoverage() {
        super(CACHING_KEY);
    }

    /**
     * This method returns an instance of StatementCoverage.
     *
     * @return instance of StatementCoverage.
     */
    public static QMOCoverage getInstance() {
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
            org.codecover.model.utils.criteria.QMOCoverage.getInstance());
    }


    /**
     * Calculates the
     * {@link org.codecover.model.utils.criteria.QMOCoverage}
     *
    */
    public CoverageResult getCoverageLocal(Collection<TestCase> testCases, QuestionMarkOperator qmo) {
    	
    	QuestionMarkOperatorExpression expr1 = qmo.getQuestionMarkOperatorExpression1();
    	QuestionMarkOperatorExpression expr2 = qmo.getQuestionMarkOperatorExpression2();
    	
    	int expr1Covered = 0;
    	int expr2Covered = 0;
    	
    	// a QuestionMarkOperator is covered, when both Expressions are covered
        for (TestCase testCase : testCases) {
            if ((testCase.getCoverageCount(expr1.getCoverableItem())) > 0) {
                expr1Covered = 1;
            }
            if ((testCase.getCoverageCount(expr2.getCoverableItem())) > 0) {
                expr2Covered = 1;
            }
            
            if(expr1Covered == 1 && expr2Covered == 1) break;
        }

        return new CoverageResult(expr1Covered + expr2Covered, 2);
    }

    public CoverageResult getCoverageLocal(Collection<TestCase> testCases, QuestionMarkOperatorExpression qmoe) {
    	// a QuestionMarkOperator is covered, when both Expressions are covered
        for (TestCase testCase : testCases) {
            if ((testCase.getCoverageCount(qmoe.getCoverableItem())) > 0) {
                return new CoverageResult(1, 1);
            }
        }

        return new CoverageResult(0, 1);
    }
    
    public Set<Hint> getHints(Collection<TestCase> testCases, QuestionMarkOperator qmo) {
  
    	int executions = 0;
        for (final TestCase testCase : testCases) {
            executions += testCase.getCoverageCount(qmo.getCoverableItem());
        }

        final Hint hint = new QMOHintImpl(executions);
        return Collections.singleton(hint);
    }


}
