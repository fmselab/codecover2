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

/**
 * Class which provides the distinct metrics with core functionality to compute
 * correlation.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: AbstractCorrelationMetric.java 1912 2007-09-20 12:42:47Z
 *          wittlims $)
 */
public abstract class AbstractCorrelationMetric implements CorrelationMetric {

    /**
     * This method can be used by subclasses, which operate on the data of
     * {@link TestCase#getCoverageData()}. The correlation between the given
     * {@link TestCase}s is calculated, but only those coverable items, that
     * are given, are used in the calculation. This allows the subclasses to
     * prefilter the calculation.
     * 
     * @param coverableItems
     *            the set of {@link CoverableItem}s
     * @param testCases
     *            the list of {@link TestCase}s
     * @return the {@link CorrelationResult} holding the calculated correlation.
     */
    protected static CorrelationResult calculateCorrelation(
            Set<CoverableItem> coverableItems, List<TestCase> testCases) {
        final Set<CoverableItem> mainSet = new HashSet<CoverableItem>(
                coverableItems);

        final Map<TestCase, Integer> totalCoverableItemCount = new HashMap<TestCase, Integer>();
        final Map<Set<TestCase>, Integer> intersectionCoverableItemCount = new HashMap<Set<TestCase>, Integer>();

        for (int i = 0; i < testCases.size(); i++) {
            TestCase firstTestCase = testCases.get(i);

            // Create a new set out of the coverable items contained in the
            // testcase
            Set<CoverableItem> firstSet = new HashSet<CoverableItem>(
                    firstTestCase.getCoverageData().keySet());
            // And intersect it with the given set of all the valid coverable
            // items
            firstSet.retainAll(mainSet);

            totalCoverableItemCount.put(firstTestCase, firstSet.size());

            for (int a = i + 1; a < testCases.size(); a++) {
                TestCase secondTestCase = testCases.get(a);

                Set<CoverableItem> secondSet = new HashSet<CoverableItem>(
                        secondTestCase.getCoverageData().keySet());
                // Again intersect to get only the valid coverable items
                secondSet.retainAll(mainSet);
                // And keep only the items that are present in both sets of
                // coverable items of both test cases
                secondSet.retainAll(firstSet);

                Set<TestCase> testCaseSet = new HashSet<TestCase>();
                testCaseSet.add(firstTestCase);
                testCaseSet.add(secondTestCase);

                intersectionCoverableItemCount.put(testCaseSet, secondSet
                        .size());
            }
        }

        return new CorrelationResult(totalCoverableItemCount,
                intersectionCoverableItemCount);
    }

    /**
     * This method can be used by subclasses, which operate on the data of
     * {@link TestCase#getAssignmentsCount(RootTerm)}. The correlation between
     * the given {@link TestCase}s is calculated, but only those coverable
     * items, that are given, are used in the calculation. This allows the
     * subclasses to prefilter the calculation.
     * 
     * @param coverableItems
     *            the set of {@link CoverableItem}s
     * @param testCases
     *            the list of {@link TestCase}s
     * @return the {@link CorrelationResult} holding the calculated correlation.
     */
    protected static CorrelationResult calculateCorrelationWithAssignments(
            Set<CoverableItem> coverableItems, List<TestCase> testCases) {
        final Set<CoverableItem> mainSet = new HashSet<CoverableItem>(
                coverableItems);

        final Map<TestCase, Integer> totalCoverableItemCount = new HashMap<TestCase, Integer>();
        final Map<Set<TestCase>, Integer> intersectionCoverableItemCount = new HashMap<Set<TestCase>, Integer>();

        for (int i = 0; i < testCases.size(); i++) {
            TestCase firstTestCase = testCases.get(i);

            /*
             * Create a new set out of the coverable items contained in the
             * test case
             */
            Set<CoverableItem> firstSet = new HashSet<CoverableItem>(
                    firstTestCase.getAssignmentsMap().keySet());
            /*
             * And intersect it with the given set of all the valid coverable
             * items
             */
            firstSet.retainAll(mainSet);

            int amountFirstTestCase = 0;
            /*
             * Calculate the total amount of coverable items from the amount of
             * booleanAssignments in the test cases
             */

            for (CoverableItem item : firstSet) {
                BooleanAssignmentMap map = firstTestCase.getAssignmentsMap()
                        .get(item);
                amountFirstTestCase += map.getEvaluatedAssignments().size();
            }

            totalCoverableItemCount.put(firstTestCase, amountFirstTestCase);

            for (int a = i + 1; a < testCases.size(); a++) {
                TestCase secondTestCase = testCases.get(a);

                Set<CoverableItem> secondSet = new HashSet<CoverableItem>(
                        secondTestCase.getAssignmentsMap().keySet());
                /* Again intersect to get only the valid coverable items */
                secondSet.retainAll(mainSet);
                /*
                 * And keep only the items that are present in both sets of
                 * coverable items of both test cases
                 */
                secondSet.retainAll(firstSet);

                int amountIntersection = 0;
                /*
                 * Calculate the intersected amount of coverableItems from the
                 * amount of booleanAssignments in the test cases.
                 */
                for (CoverableItem item : secondSet) {
                    BooleanAssignmentMap map1 = firstTestCase
                            .getAssignmentsMap().get(item);
                    BooleanAssignmentMap map2 = secondTestCase
                            .getAssignmentsMap().get(item);

                    Set<BooleanAssignment> sharedAssignments = new HashSet<BooleanAssignment>(
                            map1.getEvaluatedAssignments());
                    sharedAssignments.retainAll(map2.getEvaluatedAssignments());

                    amountIntersection += sharedAssignments.size();
                }

                Set<TestCase> testCaseSet = new HashSet<TestCase>();
                testCaseSet.add(firstTestCase);
                testCaseSet.add(secondTestCase);

                intersectionCoverableItemCount.put(testCaseSet,
                        amountIntersection);
            }
        }

        return new CorrelationResult(totalCoverableItemCount,
                intersectionCoverableItemCount);
    }

    /**
     * Checks, if all the given {@link TestCase}s share the same
     * {@link TestSessionContainer}
     * 
     * @param testCases
     *            the {@link TestCase}s to check
     * @return true &rarr; all {@link TestCase}s share the same
     *         {@link TestSessionContainer} <br>
     *         false &rarr; they don't.
     */
    protected static boolean checkTestCases(List<TestCase> testCases) {
        /* Check, if all the test cases share the same test session container */
        for (int i = 0; i < testCases.size() - 1; i++) {
            if (!testCases.get(i).getTestSession().getTestSessionContainer()
                    .equals(
                            testCases.get(i + 1).getTestSession()
                                    .getTestSessionContainer())) {
                return false;
            }
        }
        return true;
    }
}
