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

import static org.codecover.model.TestCase.TEST_CASE_BY_COVERAGE_COMPARATOR;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import org.codecover.metrics.coverage.CoverageMetric.Hint;
import org.codecover.model.TestCase;
import org.codecover.model.mast.BasicBooleanTerm;
import org.codecover.model.mast.BasicStatement;
import org.codecover.model.mast.Branch;
import org.codecover.model.mast.ConditionalStatement;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.LoopingStatement;
import org.codecover.model.mast.OperatorTerm;
import org.codecover.model.mast.QuestionMarkOperator;
import org.codecover.model.mast.QuestionMarkOperatorExpression;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.Statement;
import org.codecover.model.mast.StatementSequence;
import org.codecover.model.mast.SynchronizedStatement;

/**
 * This abstract class implements the CoverageMetric. It provides a default
 * implementation for all methods declared in CoverageMetric. Classes
 * subclassing AbstractCoverageMetric need only override those methods, whose
 * implementation here differs from behaviour they require. It also provides
 * caching functionality for the coverage results calculated for
 * HierarchyLevels. Every calculation done solely with the methods of this class
 * will always return zero coverage. But they provide a way to traverse the AST.
 * To implement e.g. statement coverage, it would be necessary to override
 * getCoverage(testCases, statement) and check, if a basic statement is given
 * and return the true coverage. But for the traversal of the AST one can call
 * super.getCoverage(testCases, statement).
 *
 * @author Markus Wittlinger, Tilmann Scheller, Christoph Müller
 * @version 1.1 ($Id: AbstractCoverageMetric.java 73 2010-07-30 19:42:18Z schmidberger $)
 */
public abstract class AbstractCoverageMetric implements CoverageMetric {

    private static final String CACHINGKEY_HIERARCHYLEVEL = ".getCoverage_HierarchyLevel";

    private final String cachingKeyName;

    /**
     * A empty {@link CoverageMetric.Hint} set.
     */
    protected static final Set<Hint> noHints = Collections.<Hint> emptySet();

    public static final class SumVisitor implements PostMetricVisitor {
        private int coveredItems = 0;

        private int totalItems = 0;

        /**
         * Adds a {@link CoverageResult}
         *
         * @param result
         *            the given result to add.
         */
        public void add(CoverageResult result) {
            this.coveredItems += result.getCoveredItems();
            this.totalItems += result.getTotalItems();
        }

        /**
         * Creates a {@link CoverageResult}.
         *
         * @return the created {@link CoverageResult}.
         */
        public CoverageResult createResult() {
            if (this.coveredItems > this.totalItems || this.coveredItems < 0
                    || this.totalItems < 0) {
                // We might get here thanks to Java's ingenious (= nonexistent)
                // overflow handling.
                throw new RuntimeException();
            }

            if (this.totalItems == 0) {
                return CoverageResult.NULL;
            } else {
                return new CoverageResult(this.coveredItems, this.totalItems);
            }
        }

        public void visit(HierarchyLevel statement, CoverageResult result,
                Set<Hint> hints) {
            add(result);
        }

        public void visit(BasicStatement statement, CoverageResult result,
                Set<Hint> hints) {
            add(result);
        }

        public void visit(ConditionalStatement statement,
                CoverageResult result, Set<Hint> hints) {
            add(result);
        }

        public void visit(LoopingStatement statement, CoverageResult result,
                Set<Hint> hints) {
            add(result);
        }

        public void visit(StatementSequence statement, CoverageResult result,
                Set<Hint> hints) {
            add(result);
        }

        public void visit(Branch statement, CoverageResult result,
                Set<Hint> hints) {
            add(result);
        }

        public void visit(RootTerm term, CoverageResult result, Set<Hint> hints) {
            add(result);
        }

        public void visit(BasicBooleanTerm term, RootTerm rootTerm,
                CoverageResult result, Set<Hint> hints) {
            add(result);
        }

        public void visit(OperatorTerm term, RootTerm rootTerm,
                CoverageResult result, Set<Hint> hints) {
            add(result);
        }

		public void visit(QuestionMarkOperator qmo, CoverageResult result,
				Set<Hint> hints) {
            add(result);			
		}

		public void visit(SynchronizedStatement synchronizedStatement,
				CoverageResult result, Set<Hint> hints) {
            add(result);						
		}

		public void visit(QuestionMarkOperatorExpression qmoe,
				CoverageResult result, Set<Hint> hints) {
            add(result);									
		}
    }

    private static class SumWithParentVisitor implements PrePostMetricVisitor {

        private Map<HierarchyLevel, CoverageResult> coverageResultMap;

        public SumWithParentVisitor(Map<HierarchyLevel, CoverageResult> coverageResultMap) {
            this.coverageResultMap = coverageResultMap;
        }

        /** we use int[], because CoverageResult is immutable. */
        private Stack<int[]> intermediateResultsStack = new Stack<int[]>();

        private int[] currentIntermediateResult;

        private void incrementIntermediate(CoverageResult coverageResult) {
            this.currentIntermediateResult[0] += coverageResult.getCoveredItems();
            this.currentIntermediateResult[1] += coverageResult.getTotalItems();
        }

        /** Pre visit! */
        public void visit(HierarchyLevel level) {
            // push a new int[]
            this.currentIntermediateResult = new int[2];
            this.intermediateResultsStack.push(this.currentIntermediateResult);
        }

        /** Post visit. */
        public void visit(HierarchyLevel level, CoverageResult result, Set<Hint> hints) {
            // local coverage
            incrementIntermediate(result);

            // we are at the end of the hierarchy level
            CoverageResult itsOverAllCoverageResult = new CoverageResult(this.currentIntermediateResult[0],
                    this.currentIntermediateResult[1]);
            this.coverageResultMap.put(level, itsOverAllCoverageResult);

            // pop the stack
            this.intermediateResultsStack.pop();
            if (!this.intermediateResultsStack.isEmpty()) {
                this.currentIntermediateResult = this.intermediateResultsStack.peek();
                incrementIntermediate(itsOverAllCoverageResult);
            } else {
                this.currentIntermediateResult = null;
            }
        }

        public void visit(BasicStatement statement, CoverageResult result,
                Set<Hint> hints) {
            incrementIntermediate(result);
        }

        public void visit(ConditionalStatement statement,
                CoverageResult result, Set<Hint> hints) {
            incrementIntermediate(result);
        }

        public void visit(LoopingStatement statement, CoverageResult result,
                Set<Hint> hints) {
            incrementIntermediate(result);
        }

        public void visit(StatementSequence statement, CoverageResult result,
                Set<Hint> hints) {
            incrementIntermediate(result);
        }

        public void visit(Branch statement, CoverageResult result,
                Set<Hint> hints) {
            incrementIntermediate(result);
        }

        public void visit(RootTerm term, CoverageResult result, Set<Hint> hints) {
            incrementIntermediate(result);
        }

        public void visit(BasicBooleanTerm term, RootTerm rootTerm,
                CoverageResult result, Set<Hint> hints) {
            incrementIntermediate(result);
        }

        public void visit(OperatorTerm term, RootTerm rootTerm,
                CoverageResult result, Set<Hint> hints) {
            incrementIntermediate(result);
        }

		public void visit(QuestionMarkOperator qmo, CoverageResult result,
				Set<Hint> hints) {
            incrementIntermediate(result);			
		}

		public void visit(SynchronizedStatement synchronizedStatement,
				CoverageResult result, Set<Hint> hints) {
            incrementIntermediate(result);						
		}

		public void visit(QuestionMarkOperatorExpression qmoe,
				CoverageResult result, Set<Hint> hints) {
            incrementIntermediate(result);						
			
		}
    };


    /**
     * This method performs the actual calculation of the coverage for
     * HierarchyLevels. It calls getCoverage(testCases, statements) for every
     * StatementSequence it contains. It unifies the the list of results into a
     * single result and returns it.
     *
     * @param testCases
     *            The list containing TestCases, whose combined coverage is
     *            calculated here.
     * @param level
     *            The HierarchyLevel which is the entry point into the AST.
     * @return the unifed {@link CoverageResult} encompassing all
     *         {@link StatementSequence}s and child {@link HierarchyLevel}s of
     *         this {@link HierarchyLevel}
     */
    private CoverageResult getUncachedCoverage(Collection<TestCase> testCases,
            HierarchyLevel level) {
        SumVisitor visitor = new SumVisitor();

        // We mustn't simply do a
        // accept(testCases, level, visitor);
        // because this would not cache the sublevels

        for (StatementSequence sequence : level.getSequences()) {
            accept(testCases, sequence, visitor);
        }

        // sum up coverage of all children
        for (HierarchyLevel child : level.getChildren()) {
            visitor.add(getCoverage(testCases, child));
        }

        visitor.add(getCoverageLocal(testCases, level));

        return visitor.createResult();
    }

    /**
     * This method checks first, if the given statement is a BasicStatement. Was
     * that the case, a coverage of zero is returned. If not, it calls itself
     * recursively. Also getCoverage(testCases, term) is called for every root
     * term the statement contains.
     *
     * @param testCases
     *            The list containing TestCases, whose combined coverage is
     *            calculated here.
     * @param statement
     *            the {@link Statement} whose coverage is to be determined
     * @return the {@link CoverageResult} of the {@link Statement}
     */
    public CoverageResult getCoverage(Collection<TestCase> testCases,
            Statement statement) {
        SumVisitor visitor = new SumVisitor();

        accept(testCases, statement, visitor);

        return visitor.createResult();
    }

    /**
     * This method calls getCoverage(testCases, statement) for all the
     * Statements the StatementSequence contains. It unifies the the list of
     * results into a single result and returns it.
     *
     * @param testCases
     *            The list containing TestCases, whose combined coverage is
     *            calculated here.
     * @param statements
     *            the statements, whose coverage is to be determined
     * @return the unified {@link CoverageResult} of all {@link Statement}s in
     *         the {@link StatementSequence}
     */
    public final CoverageResult getCoverage(Collection<TestCase> testCases,
            StatementSequence statements) {
        SumVisitor visitor = new SumVisitor();

        accept(testCases, statements, visitor);

        return visitor.createResult();
    }

    /**
     * This method always returns zero coverage.
     *
     * @param testCases
     *            The list containing TestCases, whose combined coverage is
     *            calculated here.
     * @param term
     *            the {@link RootTerm}, whose coverage is to de determined
     * @return {@link CoverageResult#NULL}
     */
    public final CoverageResult getCoverage(Collection<TestCase> testCases,
            RootTerm term) {
        SumVisitor visitor = new SumVisitor();

        accept(testCases, term, visitor);

        return visitor.createResult();
    }

    /**
     * This method first checks, if the coverage of the given TestCases and the
     * given HierarchyLevel has been calculated and stored in the meta data
     * before. If that is the case, the stored results are returned. If not, the
     * protected method getUncachedCoverage(testCases, level) is called and its
     * results are stored in the meta data and then returned.
     *
     * @param testCases
     *            The list containing TestCases, whose combined coverage is
     *            calculated here.
     * @param level
     *            The HierarchyLevel which is the entry point into the AST.
     * @return the unified {@link CoverageResult} encompassing all
     *         {@link StatementSequence}s and child {@link HierarchyLevel}s of
     *         this {@link HierarchyLevel}
     */
    public final CoverageResult getCoverage(Collection<TestCase> testCases,
            HierarchyLevel level) {
        CoverageResult result = null;

        if (testCases.size() == 1) {
            TestCase firstTestCase = testCases.iterator().next();

            // use cache only for single test cases
            Object cacheEntry = firstTestCase.getMetaData(
                    this.cachingKeyName + CACHINGKEY_HIERARCHYLEVEL
                            + level.getId());

            if (cacheEntry == null) {
                // calculate coverage and store result in cache
                result = getUncachedCoverage(testCases, level);
                firstTestCase.setMetaData(
                        this.cachingKeyName + CACHINGKEY_HIERARCHYLEVEL
                                + level.getId(), result);
            } else {
                // return coverage result fetched from cache
                result = (CoverageResult) cacheEntry;
            }
        } else {
            // no caching for lists of test cases
            result = getUncachedCoverage(testCases, level);
        }

        return result;
    }

    /**
     * This method calls getCoverage(testCases, statements) for every
     * StatementSequence it contains. It unifies the the list of results into a
     * single result and returns it.
     *
     * @param testCases
     *            The list containing TestCases, whose combined coverage is
     *            calculated here.
     * @param branch
     *            the branch, whose coverage is to be determined
     * @return the {@link CoverageResult} of the {@link StatementSequence} of
     *         the {@link Branch}
     */
    public final CoverageResult getCoverage(Collection<TestCase> testCases,
            Branch branch) {
        SumVisitor visitor = new SumVisitor();

        accept(testCases, branch, visitor);

        return visitor.createResult();
    }

     /**
     * Contructor, used to assure, that a cachingKeyName is set.
     *
     * @param cachingKeyName
     *            The key used for the storage of the results of coverage
     *            calculations on HierarchyLevels. It must be unique to ensure,
     *            that results are not stored incorrectly.
     */
    protected AbstractCoverageMetric(String cachingKeyName) {
        this.cachingKeyName = cachingKeyName;
    }

    public void accept(Collection<TestCase> testCases, HierarchyLevel level,
            PreMetricVisitor pre, PostMetricVisitor post) {
        pre.visit(level);

        for (StatementSequence sequence : level.getSequences()) {
            accept(testCases, sequence, post);
        }

        // sum up coverage of all children
        for (HierarchyLevel child : level.getChildren()) {
            accept(testCases, child, pre, post);
        }
        // delegate local coverage measurement to subclass
        CoverageResult result = getCoverageLocal(testCases, level);
        Set<Hint> hints = getHints(testCases, level);

        // we tell about the coverage even if its empty
        post.visit(level, result, hints);
    }

    public void accept(Collection<TestCase> testCases, Branch branch, PostMetricVisitor post) {
        accept(testCases, branch.getSequence(), post);
        // delegate local coverage measurement to subclass
        CoverageResult result = getCoverageLocal(testCases, branch);
        Set<Hint> hints = getHints(testCases, branch);

        // we tell about the coverage even if its empty
        post.visit(branch, result, hints);
    }

    
    public void accept(Collection<TestCase> testCases, QuestionMarkOperator qmo, PostMetricVisitor post) {
        // delegate local coverage measurement to subclass
        CoverageResult result = getCoverageLocal(testCases, qmo);
        Set<Hint> hints = getHints(testCases, qmo);  


        // we tell about the coverage even if its empty
        post.visit(qmo, result, hints);
        
        post.visit(qmo.getQuestionMarkOperatorExpression1(), getCoverageLocal(testCases, qmo.getQuestionMarkOperatorExpression1()), hints);
        post.visit(qmo.getQuestionMarkOperatorExpression2(), getCoverageLocal(testCases, qmo.getQuestionMarkOperatorExpression2()), hints);
        
    }
    
    
    public void accept(Collection<TestCase> testCases, SynchronizedStatement synchronizedStatement, PostMetricVisitor post) {
        // delegate local coverage measurement to subclass
        CoverageResult result = getCoverageLocal(testCases, synchronizedStatement);
        Set<Hint> hints = getHints(testCases, synchronizedStatement);  


        // we tell about the coverage even if its empty
        post.visit(synchronizedStatement, result, hints);
    }
    
    public void accept(Collection<TestCase> testCases, StatementSequence statements,
            PostMetricVisitor post) {
        for (Statement statement : statements.getStatements()) {
            accept(testCases, statement, post);
        }
        // delegate local coverage measurement to subclass
        CoverageResult result = getCoverageLocal(testCases, statements);
        Set<Hint> hints = getHints(testCases, statements);

        // we tell about the coverage even if its empty
        post.visit(statements, result, hints);
    }

    public void accept(Collection<TestCase> testCases, Statement statement,
            PostMetricVisitor post) {
        for (RootTerm term : statement.getTerms()) {
            accept(testCases, term, post);
        }

        for (QuestionMarkOperator qmo : statement.getQuestionMarkOperators()) {
            accept(testCases, qmo, post);
        }

        if (statement instanceof ConditionalStatement) {
            List<Branch> branches = ((ConditionalStatement) statement)
                    .getBranches();

            for (Branch branch : branches) {
                accept(testCases, branch, post);
            }

            // delegate local coverage measurement to subclass
            CoverageResult result = getCoverageLocal(testCases, statement);
            Set<Hint> hints = getHints(testCases, statement);

            // we tell about the coverage even if its empty
            post.visit((ConditionalStatement) statement, result, hints);
        } else if (statement instanceof LoopingStatement) {
            accept(testCases, ((LoopingStatement) statement).getBody(), post);

            // delegate local coverage measurement to subclass
            CoverageResult result = getCoverageLocal(testCases, statement);
            Set<Hint> hints = getHints(testCases, statement);

            // we tell about the coverage even if its empty
            post.visit((LoopingStatement) statement, result, hints);
        } else if (statement instanceof BasicStatement) {

            // delegate local coverage measurement to subclass
            CoverageResult result = getCoverageLocal(testCases, statement);
            Set<Hint> hints = getHints(testCases, statement);

            // we tell about the coverage even if its empty
            post.visit((BasicStatement) statement, result, hints);
        } else if (statement instanceof SynchronizedStatement) {

            // delegate local coverage measurement to subclass
            CoverageResult result = getCoverageLocal(testCases, statement);
            Set<Hint> hints = getHints(testCases, statement);

            // we tell about the coverage even if its empty
            post.visit((SynchronizedStatement) statement, result, hints);
        } else {
            throw new RuntimeException();
        }
    }

    public void accept(Collection<TestCase> testCases, RootTerm term, PostMetricVisitor post) {
        // TermCoverage has to override this in order to call
        // the visit(BooleanTerm, ...) methods.

        // delegate local coverage measurement to subclass
        CoverageResult result = getCoverageLocal(testCases, term);
        Set<Hint> hints = getHints(testCases, term);

        // we tell about the coverage even if its empty
        post.visit(term, result, hints);
    }

    /**
     * This helper method efficiently gets all {@link CoverageResult} mapped by {@link HierarchyLevel} for a
     * {@link CoverageMetric} and a set of {@link TestCase}.
     * <p>
     * Instead of getting these entries using multiple {@link #getCoverage(Collection, HierarchyLevel)} calls
     * this method uses a stack of {@link CoverageResult} to propagate coverage to the parent too.
     */
    public static Map<HierarchyLevel, CoverageResult> calculateCoverageForAllLevels(
            CoverageMetric coverageMetric, Collection<TestCase> testCases, HierarchyLevel level) {

        // we sort the test cases by decreasing coverage count in order that the first test case has the
        // most coverage
        // this increases efficiency because in the search, whether a coverable is covered, all test cases
        // are asked; if the first test case can answer the question, the others need not be asked any more
        List<TestCase> sortedTestCases = new ArrayList<TestCase>(testCases);
        Collections.sort(sortedTestCases, TEST_CASE_BY_COVERAGE_COMPARATOR);

        Map<HierarchyLevel, CoverageResult> coverageResultMap = new HashMap<HierarchyLevel, CoverageResult>();

        SumWithParentVisitor sumWithParentVisitor = new SumWithParentVisitor(coverageResultMap);

        coverageMetric.accept(sortedTestCases, level, sumWithParentVisitor, sumWithParentVisitor);

        return coverageResultMap;
    }
    
    
    /**
     * Dummy implementations; 
     */
    public CoverageResult getCoverageLocal(Collection<TestCase> testCases, RootTerm term) {
        return CoverageResult.NULL;
    }

    public CoverageResult getCoverageLocal(Collection<TestCase> testCases,
                                           StatementSequence statements) {
        return CoverageResult.NULL;
    }

    public CoverageResult getCoverageLocal(Collection<TestCase> testCases,
                                           HierarchyLevel level) {
        return CoverageResult.NULL;
    }

    public CoverageResult getCoverageLocal(Collection<TestCase> testCases, Branch branch) {
        return CoverageResult.NULL;
    }

    
    public CoverageResult getCoverageLocal(Collection<TestCase> testCases,
            Statement statement) {
        return CoverageResult.NULL;
    }

    public CoverageResult getCoverageLocal(Collection<TestCase> testCases,
    		QuestionMarkOperator qmo) {
    	return CoverageResult.NULL;
    }

    public CoverageResult getCoverageLocal(Collection<TestCase> testCases,
    		QuestionMarkOperatorExpression qmo) {
    	return CoverageResult.NULL;
    }

    
    public Set<Hint> getHints(Collection<TestCase> testCases,
                              Statement statement) {
        return noHints;
    }

    public Set<Hint> getHints(Collection<TestCase> testCases,
                              RootTerm term) {
        return noHints;
    }

    public Set<Hint> getHints(Collection<TestCase> testCases, StatementSequence statements) {
        return noHints;
    }

    public Set<Hint> getHints(Collection<TestCase> testCases, HierarchyLevel level) {
        return noHints;
    }

    public Set<Hint> getHints(Collection<TestCase> testCases, Branch branch) {
        return noHints;
    }    
    public Set<Hint> getHints(Collection<TestCase> testCases, QuestionMarkOperator qmo) {
        return noHints;
    }    
}
