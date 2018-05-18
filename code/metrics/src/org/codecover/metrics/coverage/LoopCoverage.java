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
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.Statement;
import org.codecover.model.mast.StatementSequence;
import org.codecover.model.mast.SynchronizedStatement;
import org.codecover.model.utils.criteria.Criterion;

/**
 * This class would need to override getCoverage(testCases, statement).
 *
 * @author Markus Wittlinger, Tilmann Scheller
 * @version 1.0 ($Id: LoopCoverage.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public class LoopCoverage extends AbstractCoverageMetric {

    /**
     * Hint how to reach full coverage of a loop.
     *
     * @author Steffen Kieß
     * @version 1.0 ($Id: LoopCoverage.java 69 2010-01-27 19:31:18Z schmidberger $)
     */
    public static interface LoopHint extends EnglishTextHint {

        /**
         * @return true iff body can be executed 0 times in a run
         */
        public boolean needZeroTimes();

        /**
         * @return true iff body was executed 0 times in a run
         */
        public boolean wasExecutedZeroTimes();

        /**
         * @return true iff body was executed 1 time in a run
         */
        public boolean wasExecutedOnce();

        /**
         * @return true iff body was executed > 1 time in a run
         */
        public boolean wasExecutedMultipleTimes();
    }

    private static final class LoopHintImpl implements LoopHint {
        private final boolean needZero;
        private final boolean zero;
        private final boolean once;
        private final boolean multiple;

        /**
         * Create Hint telling which coverable items are not covered.
         *
         * @param needZero
         * true iff body can be executed 0 times
         * @param zero
         * true iff body was executed 0 times
         * @param once
         * true iff body was executed 1 time
         * @param multiple
         * true iff body was executed > 1 time
         */
        public LoopHintImpl(boolean needZero, boolean zero, boolean once, boolean multiple) {
            if ((!needZero || zero) && once && multiple) {
                throw new IllegalArgumentException();
            }

            this.needZero = needZero;
            this.zero = zero;
            this.once = once;
            this.multiple = multiple;
        }

        public boolean needZeroTimes() {
            return this.needZero;
        }

        public boolean wasExecutedZeroTimes() {
            return this.zero;
        }

        public boolean wasExecutedOnce() {
            return this.once;
        }

        public boolean wasExecutedMultipleTimes() {
            return this.multiple;
        }

        public String toEnglishText() {
            String note = "Missing execution(s): ";
            if (this.needZero && !this.zero) {
                note += "ZERO ";
            }
            if (!this.once) {
                note += "ONE ";
            }
            if (!this.multiple) {
                note += "MANY ";
            }
            note += "time(s).";
            return note;
        }
    }

    private static final String CACHING_KEY = "LoopCoverage";

    private static final String NAME = "Loop Coverage";

    private static final String DESCRIPTION = "";

    private static LoopCoverage instance = new LoopCoverage();

    private LoopCoverage() {
        super(CACHING_KEY);
    }

    /**
     * This method returns an instance of LoopCoverage.
     *
     * @return instance of LoopCoverage.
     */
    public static LoopCoverage getInstance() {
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
            org.codecover.model.utils.criteria.LoopCoverage.getInstance());
    }

    /**
     * Calculates the {@link org.codecover.model.utils.criteria.LoopCoverage}
     *
     * @see org.codecover.metrics.coverage.AbstractCoverageMetric#getCoverage(java.util.List,
     *      org.codecover.model.mast.Statement)
     */
    public CoverageResult getCoverageLocal(Collection<TestCase> testCases,
            Statement statement) {
        if(statement instanceof LoopingStatement) {
            LoopingStatement loopStatement = (LoopingStatement) statement;

            int totalItems = 0;
            int coveredItems = 0;

            // handle coverable items which are specific for loop coverage
            // never executed
            if (loopStatement.isOptionalBodyExecution()) {
                totalItems = totalItems + 1;

                // check whether the item has been covered in one of the test
                // cases
                for (TestCase testCase : testCases) {
                    if ((testCase.getCoverageCount(loopStatement.getNeverExecutedItem())) > 0) {
                        coveredItems++;
                        break;
                    }
                }
            }

            // executed once
            totalItems = totalItems + 1;
            // check whether the item has been covered in one of the test cases
            for (TestCase testCase : testCases) {
                if ((testCase.getCoverageCount(loopStatement.getOnceExecutedItem())) > 0) {
                    coveredItems = coveredItems + 1;
                    break;
                }
            }

            // executed multiple times
            totalItems = totalItems + 1;
            // check whether the item has been covered in one of the test cases
            for (TestCase testCase : testCases) {
                if ((testCase.getCoverageCount(loopStatement.getMultipleExecutedItem())) > 0) {
                    coveredItems = coveredItems + 1;
                    break;
                }
            }

            return new CoverageResult(coveredItems, totalItems);
        }
        return CoverageResult.NULL;
    }

    public Set<Hint> getHints(Collection<TestCase> testCases,
                              Statement statement) {
        if (statement instanceof ConditionalStatement) {
            return noHints;
        } else if (statement instanceof LoopingStatement) {
            LoopingStatement loop = (LoopingStatement) statement;
            boolean execNever = false;
            boolean execOnce  = false;
            boolean execMulti = false;
            for (TestCase testCase : testCases) {
                if (testCase.getCoverageCount(loop.getNeverExecutedItem())
                    > 0) {
                    execNever = true;
                }
                if (testCase.getCoverageCount(loop.getOnceExecutedItem())
                    > 0) {
                    execOnce = true;
                }
                if (testCase.getCoverageCount(loop.getMultipleExecutedItem()) > 0) {
                    execMulti = true;
                }
            }
            if ((loop.isOptionalBodyExecution() && !execNever) || !execOnce || !execMulti) {
                Hint hint = new LoopHintImpl(loop.isOptionalBodyExecution(), execNever, execOnce, execMulti);
                return Collections.singleton(hint);
            } else {
                return noHints;
            }
        } else if (statement instanceof BasicStatement) {
            return noHints;
        } else if (statement instanceof SynchronizedStatement) {
            return noHints;
        } else {
            throw new RuntimeException();
        }
    }
}
