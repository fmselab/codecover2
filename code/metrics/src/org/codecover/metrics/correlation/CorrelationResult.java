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

import org.codecover.model.TestCase;
import org.codecover.model.mast.CoverableItem;

/**
 * This class represents the result of a correlation calculation.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: CorrelationResult.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CorrelationResult {
    private final Map<TestCase, Integer> coverableItemCounts;

    private final Map<Set<TestCase>, Integer> intersectionCounts;

    /**
     * Constructor
     * 
     * @param coverableItemCounts
     * @param intersectionCounts
     */
    public CorrelationResult(final Map<TestCase, Integer> coverableItemCounts,
            final Map<Set<TestCase>, Integer> intersectionCounts) {
        this.coverableItemCounts = coverableItemCounts;
        this.intersectionCounts = intersectionCounts;
    }

    /**
     * Gets the correlation of the firstTestCase to the secondTestCase.
     * <p>
     * A result of 1.0 would indicate, that the all the coverage the second test
     * case performs, is also performed by the first test case.
     * <p>
     * A result of 0.5 would indicate, that the first test case performs only
     * half of the coverage that the second test case performs.
     * 
     * @param firstTestCase
     * @param secondTestCase
     * @return a value between <code>0.0</code> and <code>1.0</code>
     */
    public double getCorrelation(TestCase firstTestCase, TestCase secondTestCase) {
        if (firstTestCase == null) {
            throw new NullPointerException("firstTestCase == null");
        }
        if (secondTestCase == null) {
            throw new NullPointerException("secondTestCase == null");
        }

        Integer firstTestCaseTotal = this.coverableItemCounts
                .get(firstTestCase);

        if (firstTestCaseTotal == null) {
            throw new IllegalArgumentException(
                    "firstTestCase was not part of correlation computation");
        }

        Integer secondTestCaseTotal = this.coverableItemCounts
                .get(secondTestCase);

        if (secondTestCaseTotal == null) {
            throw new IllegalArgumentException(
                    "secondTestCase was not part of correlation computation");
        }

        // If the two test cases are equal, the correlation is 1.0
        if (firstTestCase.equals(secondTestCase)) {
            return 1.0;
        }

        if (secondTestCaseTotal == 0) {
            return 1.0;
        }

        Set<TestCase> testCaseSet = new HashSet<TestCase>();
        testCaseSet.add(firstTestCase);
        testCaseSet.add(secondTestCase);

        Integer intersectionCount = this.intersectionCounts.get(testCaseSet);
        // intersectionCount should never be null, since the test cases were
        // used in the computation of the correlation
        if (intersectionCount == null) {
            throw new RuntimeException(
                    "Both testcases were used in the computation,"
                            + " but no intersection information was saved! This is *bad*");
        }

        return ((double) intersectionCount) / ((double) secondTestCaseTotal);
    }

    /**
     * Gets the total number of {@link CoverableItem}s that were used in the
     * computation of the correlation
     * 
     * @param testCase
     *            the {@link TestCase} whose number of {@link CoverableItem}s
     *            is desired
     * @return the total number of {@link CoverableItem}s
     */
    public int getCoverableItemCount(TestCase testCase) {
        if (testCase == null) {
            throw new NullPointerException("testCase == null");
        }

        Integer result = this.coverableItemCounts.get(testCase);

        if (result == null) {
            throw new IllegalArgumentException("Testcase was not part of "
                    + "correlation computation");
        }

        return result.intValue();
    }

    /**
     * Gets the amount of shared {@link CoverableItem}s of the given set of
     * {@link TestCase}s
     * 
     * @param testCases
     *            the {@link Set} of {@link TestCase}s
     * @return the amount of shared {@link CoverableItem}s
     */
    public int getSharedCoverableItemCount(Set<TestCase> testCases) {
        List<TestCase> cases = new Vector<TestCase>(testCases);
        boolean areAllEqual = true;

        for (int i = 0; i < cases.size() - 1; i++) {
            TestCase case1 = cases.get(i);
            TestCase case2 = cases.get(i + 1);

            if (!case1.equals(case2)) {
                areAllEqual = false;
                break;
            }
        }

        if (areAllEqual && testCases.size() != 0) {
            return getCoverableItemCount(cases.get(0));
        }

        Integer intersectionCount = this.intersectionCounts.get(testCases);

        if (intersectionCount != null) {
            return intersectionCount.intValue();
        }
        return 0;
    }
}
