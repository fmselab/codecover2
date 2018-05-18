/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieأں, Johannes Langauf,                         *
 *                    Christoph Marian Mأ¼ller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.model;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.codecover.model.exceptions.MergeException;
import org.codecover.model.exceptions.NameAlreadyUsedException;
import org.codecover.model.mast.BooleanAssignment;
import org.codecover.model.mast.BooleanAssignmentMap;
import org.codecover.model.mast.CoverableItem;
import org.codecover.model.mast.Internal;
import org.codecover.model.mast.MetaDataObject;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.utils.ChangeType;
import org.codecover.model.utils.CollectionUtil;
import org.codecover.model.utils.IntComparator;
import org.codecover.model.utils.Pair;

/**
 * TestCase represents a test case. It contains a name, a comment, the date when
 * the test case was created and the coverage results. The method delete() can
 * be used to delete the TestCase. The method getCoverageCount(...) returns the
 * coverage data for an AST element. The methods setObjectMetaData(...) and
 * getObjectMetaData(...) can set and get meta data for specific AST elements
 * for this test case. This meta data might be e.g. cached coverage metrics.
 *
 * @author Markus Wittlinger, Tilmann Scheller
 * @version 1.0 ($Id: TestCase.java 72 2010-07-15 07:25:29Z schmidberger $)
 */

public class TestCase extends AbstractMetaDataProvider {
    private final TestSession testSession;

    private String name;

    private final Date date;

    private String comment;

    private final Map<CoverableItem, Long> coverageData;

    private final Map<String, Map<Long, Object>> objectMetaData = new HashMap<String, Map<Long, Object>>();

    private final Map<CoverableItem, BooleanAssignmentMap> assignments;

    private Map<RootTerm, Map<BooleanAssignment, Boolean>> cachedAssignments = new HashMap<RootTerm, Map<BooleanAssignment, Boolean>>();

    private double statementRedundancy;
    private double branchRedundancy;
    private double condRedundancy;
    private double loopRedundancy;
    private double totalRedundancy;

    private int statementCoveredItem;
    private int branchCoveredItem;
    private int condCoveredItem;
    private int loopCoveredItem;
    private int totalCoveredItem;

    /**
     * Is true iff delete() has been called successfully.
     */
    private boolean deleted = false;

    /**
     * Is true iff delete() has been started. (You cannot modify the TestCase
     * then.)
     */
    private boolean deleting = false;

    private final Object lock = new Object();

    @Override
    protected final void assertNotDeleted() {
        if (this.deleted) {
            throw new IllegalStateException("This TestCase has been deleted.");
        }
    }

    private void assertNotDeleting() {
        assertNotDeleted();
        if (this.deleting) {
            throw new IllegalStateException("This TestCase is being deleted.");
        }
    }

    /**
     * Contructor
     *
     * @param testSession
     *            the test session containing this instance
     * @param date
     *            the date this test session was created
     * @param coverageData
     *            the measured coverage associated with this test case
     * @param name
     *            the name of the test case
     * @param comment
     *            the comment associated with this test case
     */
    TestCase(TestSession testSession, Date date,
            Map<CoverableItem, Long> coverageData,
            Map<CoverableItem, BooleanAssignmentMap> assignments,
            final String name, String comment) {
        if (testSession == null) {
            throw new NullPointerException("testSession == null");
        }
        if (name == null) {
            throw new NullPointerException("name == null");
        }
        if (comment == null) {
            throw new NullPointerException("comment == null");
        }
        if (date == null) {
            throw new NullPointerException("date == null");
        }
        if (coverageData == null) {
            throw new NullPointerException("coverageData == null");
        }
        if (assignments == null) {
            throw new NullPointerException("assignments == null");
        }

        this.date = (Date) date.clone();
        this.testSession = testSession;
        this.name = name;
        this.comment = comment;
        this.coverageData = CollectionUtil.copy(coverageData);
        this.assignments = CollectionUtil.copy(assignments);
        
        if (!getTestSession().getTestSessionContainer().getCoverableItems()
                .containsAll(coverageData.keySet())) {
            final Set<CoverableItem> items = getTestSession()
                    .getTestSessionContainer().getCoverableItems();
            for (CoverableItem item : coverageData.keySet()) {
                if (!items.contains(item)) {
                    throw new IllegalArgumentException("CoverableItem " + item
                            + " is in coverageData but not in the MAST");
                }
            }
            // Should never be reached
            throw new RuntimeException();
        }

        // Check whether all assignments have the correct length
        final Map<CoverableItem, RootTerm> rootTerms = getTestSession()
                .getTestSessionContainer().getRootTerms();
        for (Map.Entry<CoverableItem, BooleanAssignmentMap> entry : this.assignments
                .entrySet()) {

            if (!rootTerms.containsKey(entry.getKey())) {
                throw new IllegalArgumentException(
                        "assignments contains CoverableItem '"
                                + entry.getKey()
                                + "' not contained in this TestSessionContainer");
            } else {
                RootTerm rootTerm = rootTerms.get(entry.getKey());
                if (rootTerm.getTerm().getBasicBooleanTerms() != entry
                        .getValue().getLength()) {
                    throw new IllegalArgumentException(
                            "rootTerm.getTerm().getBasicBooleanTerms() ("
                                    + rootTerm.getTerm().getBasicBooleanTerms()
                                    + ") != entry.getValue().getLength() ("
                                    + entry.getValue().getLength()
                                    + ") for the RootTerm with the Prefix '"
                                    + rootTerm.getCoverableItem().getPrefix()
                                    + "' and the ID '"
                                    + rootTerm.getCoverableItem().getId()
                                    + "' in the test case '" + name
                                    + "' in the test session '"
                                    + getTestSession().getName() + "'");
                }
            }

        }
    }

    /**
     * Returns the measured coverage of the given coverable item during the run
     * of this testcase.
     *
     * @param item
     *            the coverable item, whose coverage is sought.
     * @return the number of times the given coverable item was covered.
     */
    public long getCoverageCount(CoverableItem item) {
        if (item == null) {
            throw new NullPointerException("item == null");
        }

        //assertNotDeleted();

        final Long result = this.coverageData.get(item);
        if (result == null) {
            return 0;
        } else {
            return result;
        }
    }

    /**
     * Associates a given object as metadata with a given {@link MetaDataObject}
     * under a given key
     *
     * @param name
     *            the key used in storing the object
     * @param metaDataObject
     *            the {@link MetaDataObject} the metadata is associated with
     * @param value
     *            the object to be stored as metadata
     */
    public void setObjectMetaData(String name, MetaDataObject metaDataObject,
            Object value) {
        // We still allow setting metadata while deleting
        assertNotDeleted();

        if (name == null) {
            throw new NullPointerException("name == null");
        }

        if (metaDataObject == null) {
            throw new NullPointerException("metaDataObject == null");
        }

        final long id = Internal.getMetaDataId(metaDataObject.getMetaData());

        synchronized (this.objectMetaData) {
            Map<Long, Object> map = this.objectMetaData.get(name);
            if (map == null) {
                map = new HashMap<Long, Object>();
                this.objectMetaData.put(name, map);
            }
            map.put(id, value);
        }
    }

    /**
     * Associates a given object as metadata with a given {@link MetaDataObject}
     * under a given key
     *
     * @param name
     *            the key used to retrieve the object
     * @param metaDataObject
     *            the {@link MetaDataObject} the metadata is associated with
     * @return the retrieved object or <code>null</code>, if no object was
     *         associated with the given {@link MetaDataObject} under the given
     *         key.
     */
    public Object getObjectMetaData(String name, MetaDataObject metaDataObject) {
        //assertNotDeleted();

        if (name == null) {
            throw new NullPointerException("name == null");
        }

        if (metaDataObject == null) {
            throw new NullPointerException("metaDataObject == null");
        }

        final long id = Internal.getMetaDataId(metaDataObject.getMetaData());

        synchronized (this.objectMetaData) {
            Map<Long, Object> map = this.objectMetaData.get(name);
            if (map == null) {
                return null;
            }
            return map.get(id);
        }
    }

    /**
     * Delete this {@link TestCase} from its enclosing {@link TestSession}. Any
     * further method call to this TestCase will cause a
     * {@link IllegalStateException} to be thrown. Any attempt to modify the
     * content of this TestCase during the removal (e.g. in listeners) will also
     * cause a {@link IllegalStateException}.
     */
    public void delete() {
        synchronized (this.lock) {
            assertNotDeleting();

            this.deleting = true;

            synchronized (getTestSession().lock) {
                this.testSession.removeTestCase(this);
            }
        }

        // the test case was removed, send change event
        notifyChangeListener(ChangeType.REMOVE);
        this.testSession.notifyChangeListener(ChangeType.CHANGE);

        synchronized (this.lock) {
            this.deleted = true;
        }
    }

    /**
     * @return the comment
     */
    public String getComment() {
        //assertNotDeleted();

        synchronized (this.lock) {
            return this.comment;
        }
    }

    /**
     * @param comment
     *            the comment to set
     */
    public void setComment(String comment) {
        if (comment == null) {
            throw new NullPointerException("comment == null");
        }

        assertNotDeleting();

        synchronized (this.lock) {
            this.comment = comment;
        }
        notifyChangeListener(ChangeType.CHANGE);
    }

    /**
     * @return the date
     */
    public Date getDate() {
        //assertNotDeleted();

        return (Date) this.date.clone();
    }

    /**
     * @return the Statement Redundancy
     */
    public Double getSatementRedundancy() {
        return this.statementRedundancy;
    }

    /**
     * @return the Branch Redundancy
     */
    public Double getBranchRedundancy() {
    	return this.branchRedundancy;
    }

    /**
     * @return the Condition Redundancy
     */
    public Double getCondRedundancy() {
    	return this.condRedundancy;
    }

    /**
     * @return the Loop Redundancy
     */
    public Double getLoopRedundancy() {
    	return this.loopRedundancy;
    }

    /**
     * @return the Total Redundancy
     */
    public Double getTotalRedundancy() {
    	return this.totalRedundancy;
    }

    /**
     * @return the Statement Covered Item
     */
    public Integer getStatementCoveredItem() {
    	return this.statementCoveredItem;
    }

    /**
     * @return the Branch Covered Item
     */
    public Integer getBranchCoveredItem() {
    	return this.branchCoveredItem;
    }

    /**
     * @return the Condition Covered Item
     */
    public Integer getCondCoveredItem() {
    	return this.condCoveredItem;
    }

    /**
     * @return the Loop Covered Item
     */
    public Integer getLoopCoveredItem() {
    	return this.loopCoveredItem;
    }

    /**
     * @return the Total Covered Item
     */
    public Integer getTotalCoveredItem() {
    	return this.totalCoveredItem;
    }

    /**
     * @param d
     *            the StatementRedundancy to set
     */
    public void setSatementRedundancy(Double d) {
        this.statementRedundancy = d;
    }

    /**
     * @param d
     *            the BranchRedundancy to set
     */
    public void setBranchRedundancy(Double d) {
    	this.branchRedundancy = d;
    }

    /**
     * @param d
     *            the CondRedundancy to set
     */
    public void setCondRedundancy(Double d) {
    	this.condRedundancy = d;
    }

    /**
     * @param d
     *            the LoopRedundancy to set
     */
    public void setLoopRedundancy(Double d) {
    	this.loopRedundancy = d;
    }

    /**
     * @param d
     *            the TotalRedundancy to set
     */
    public void setTotalRedundancy(Double d) {
    	this.totalRedundancy = d;
    }

    /**
     * @param i
     *            the StatementCoveredItem to set
     */
    public void setSatementCoveredItem(Integer i) {
    	this.statementCoveredItem = i;
    }

    /**
     * @param i
     *            the BranchCoveredItem to set
     */
    public void setBranchCoveredItem(Integer i) {
    	this.branchCoveredItem = i;
    }

    /**
     * @param i
     *            the CondCoveredItem to set
     */
    public void setCondCoveredItem(Integer i) {
    	this.condCoveredItem = i;
    }

    /**
     * @param i
     *            the LoopCoveredItem to set
     */
    public void setLoopCoveredItem(Integer i) {
    	this.loopCoveredItem = i;
    }

    /**
     * @param i
     *            the TotalCoveredItem to set
     */
    public void setTotalCoveredItem(Integer i) {
    	this.totalCoveredItem = i;
    }

    /**
     * @return the name
     */
    public String getName() {
        //assertNotDeleted();

        synchronized (this.lock) {
            return this.name;
        }
    }

    /**
     * @param name
     *            the name to set
     * @throws NameAlreadyUsedException
     *             thrown, if a test case with the given name already exisits
     */
    public void setName(String name) throws NameAlreadyUsedException {
        if (name == null) {
            throw new NullPointerException("name == null");
        }

        synchronized (this.lock) {
            assertNotDeleting();

            synchronized (getTestSession().lock) {
                if (getTestSession().getTestCaseWithName(name) != null) {
                    throw new NameAlreadyUsedException(
                            "A test case with the name \"" + name
                                    + "\" already exists");
                }

                this.name = name;
            }
        }
        notifyChangeListener(ChangeType.CHANGE);
    }

    /**
     * @return the test session
     */
    public TestSession getTestSession() {
        //assertNotDeleted();

        return this.testSession;
    }

    void notifyChangeListener(ChangeType type) {
        getTestSession().getTestSessionContainer().testCaseEvent.emitChanged(
                type, this);
    }

    /**
     * Returns the assignments of the given term which occured during the run of
     * this testcase and the number of times, they were found.
     *
     * @param term
     *            the RootTerm of whom to get the assignments
     * @return a Map containing all the assignments as well as the number of
     *         execution
     */
    public BooleanAssignmentMap getAssignmentsCount(RootTerm term) {
        //assertNotDeleted();

        if (term == null) {
            throw new NullPointerException("term == null");
        }

        final BooleanAssignmentMap result = this.assignments.get(term
                .getCoverableItem());
        if (result == null) {
            return BooleanAssignmentMap.createEmptyMap(term.getTerm()
                    .getBasicBooleanTerms());
        } else {
            // This check should not be needed, but it is fast (O(1)) and
            // might safe us from a lot of trouble
            if (term.getTerm().getBasicBooleanTerms() != result.getLength()) {
                throw new RuntimeException("Inconsistent data. This is *bad*.");
            }
            return result;
        }
    }

    /**
     * Returns the assignments of the given term which occured during the run of
     * this testcase and the result of the expression under each assignment.
     *
     * @param term
     *            the RootTerm of whom to get the assignments
     * @return a Map containing all the assignments as well as the resulting
     *         boolean value
     */
    public Map<BooleanAssignment, Boolean> getAssignments(RootTerm term) {
        if (term == null) {
            throw new NullPointerException("term == null");
        }

        //assertNotDeleted();

        Map<BooleanAssignment, Boolean> result;
        synchronized (this.cachedAssignments) {
            result = this.cachedAssignments.get(term);
        }

        if (result == null) {
            result = new HashMap<BooleanAssignment, Boolean>();
            for (BooleanAssignment booleanAssignment : getAssignmentsCount(term)
                    .getData().keySet()) {
                Boolean assignmentResult = term
                        .getAssignmentResult(booleanAssignment);

                if (assignmentResult == null) {
                    // This should not happen. This means that there is an
                    // assignment in the map which cannot occur (according
                    // to the static data.)
                    // TODO: Check this on the TestCase construction
                    // throw new RuntimeException("Illegal assignment in map");
                	
                	// RS, 12.07.2010 Problems in termcovergae using a expression like false && a()
                } else {
                    result.put(booleanAssignment, assignmentResult);                	
                }

            }
            result = Collections.unmodifiableMap(result);
            synchronized (this.cachedAssignments) {
                // Note: Two threads might compute the results for the same
                // term separately, but we don't care (the second put will
                // override the first result)
                this.cachedAssignments.put(term, result);
            }
        }

        return result;
    }

    Map<String, Map<Long, Object>> getObjectMetaDataMapEntries() {
        //assertNotDeleted();

        synchronized (this.objectMetaData) {
            // We have to return a copy due to threading issues
            return CollectionUtil.copy(this.objectMetaData);
        }
    }

    /**
     * Gets the map of {@link CoverableItem}s and {@link Long}s
     *
     * @return the map holding the {@link CoverableItem}s with their number of
     *         occurrences
     */
    public Map<CoverableItem, Long> getCoverageData() {
        //assertNotDeleted();

        return this.coverageData;
    }

    /**
     * Gets the map of {@link CoverableItem}s and {@link BooleanAssignmentMap}s
     *
     * @return the map holding the {@link BooleanAssignment}s per
     *         {@link CoverableItem} with their number of occurences
     */
    public Map<CoverableItem, BooleanAssignmentMap> getAssignmentsMap() {
        //assertNotDeleted();

        return this.assignments;
    }

    /**
     * Merges the coverage of a number of {@link TestCase}s into a single temporary test case that
     * is not persisted in this model.
     * <p>
     * The temporary test case can be used to represent the coverage of a collection of test cases and
     * intended to be thrown away afterwards.
     *
     * @param testCases
     *            the given collection of test cases to be merged. This collection may not be empty.
     * @return the merged test case
     * @throws MergeException
     */
    public static TestCase mergeCoverageToTemporaryTestCase(Collection<TestCase> testCases) throws MergeException {
        Pair<Map<CoverableItem, Long>, Map<CoverableItem, BooleanAssignmentMap>> mergedCoverage =
            TestCase.mergeTestCasesCoverage(testCases);

        return new TestCase(testCases.iterator().next().testSession, new Date(), mergedCoverage.first,
                mergedCoverage.second, "Temporary Test Case", "Temporary test case only!");
    }

    /**
     * Merges the coverage of a number of {@link TestCase}s into a single coverage.
     * <p>
     *
     * @param testCases
     *            the given collection of test cases to be merged. This collection may not be empty.
     * @return the merged coverage
     * @throws MergeException
     */
    public static Pair<Map<CoverableItem, Long>, Map<CoverableItem, BooleanAssignmentMap>>
            mergeTestCasesCoverage(Collection<TestCase> testCases) throws MergeException {
        if (testCases == null) {
            throw new NullPointerException("testCases == null");
        }

        if (testCases.isEmpty()) {
            throw new IllegalArgumentException("testCases.isEmpty()");
        }

        Map<CoverableItem, Long> coverageData = new HashMap<CoverableItem, Long>();
        Map<CoverableItem, BooleanAssignmentMap> assignments = new HashMap<CoverableItem, BooleanAssignmentMap>();

        for (TestCase testCase : testCases) {
            // Unify the coverage data of the given test cases
            for (Entry<CoverableItem, Long> entry : testCase.getCoverageData().entrySet()) {
                final Long existingValue = coverageData.get(entry.getKey());
                // Check if the key points already to a number of executions, if
                // so add the new number to old one and then put the value in
                // the map.

                if (existingValue == null) {
                    coverageData.put(entry.getKey(), entry.getValue());
                } else {
                    // There was already something mapped, so the already
                    // existing and the current value must be unified.
                    coverageData.put(entry.getKey(), existingValue + entry.getValue());
                }
            }

            // Unify the assignment data of the given test cases
            for (Entry<CoverableItem, BooleanAssignmentMap> entry : testCase.getAssignmentsMap().entrySet()) {
                final BooleanAssignmentMap existingValue = assignments.get(entry.getKey());
                // If no value was saved under the current key, just put the new
                // value in.
                if (existingValue == null) {
                    assignments.put(entry.getKey(), entry.getValue());
                } else {
                    // There was already something mapped, so the already
                    // existing and the current value must be unified.
                    assignments.put(entry.getKey(), BooleanAssignmentMap.merge(
                            existingValue, entry.getValue()));
                }
            }
        }

        return new Pair<Map<CoverableItem,Long>, Map<CoverableItem,BooleanAssignmentMap>>(
                coverageData, assignments);
    }

    /** A comparator that sorts test cases after decreasing coverable item count. */
    public static Comparator<TestCase> TEST_CASE_BY_COVERAGE_COMPARATOR = new Comparator<TestCase>() {
        public int compare(TestCase o1, TestCase o2) {
            // we mix o1 and o2 in order to achieve decreased sorting
            return IntComparator.compare(o2.coverageData.size() + o2.assignments.size(),
                    o1.coverageData.size() + o1.assignments.size());
        }
    };
}
