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

package org.codecover.model;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.codecover.model.exceptions.NameAlreadyUsedException;
import org.codecover.model.mast.BooleanAssignmentMap;
import org.codecover.model.mast.CoverableItem;
import org.codecover.model.utils.ChangeType;
import org.codecover.model.utils.CollectionUtil;
import org.codecover.model.utils.NameReoccurenceHelper;

/**
 * TestSession represents a test session. It contains a name, a comment, the
 * date when the test session was created and the TestCases belonging to this
 * TestSession. The method delete() can be used to delete the TestSession. The
 * method createTestCase(...) can be used to create a new TestCase.
 *
 * @author Markus Wittlinger
 * @version 1.0 ($Id: TestSession.java 68 2009-10-02 06:14:02Z ahija $)
 */
public class TestSession extends AbstractMetaDataProvider {
    private String name;

    private final Date date;

    private String comment;

    private final List<TestCase> testCases = new Vector<TestCase>();

    private final TestSessionContainer testSessionContainer;

    final Object lock = new Object();

    /**
     * Is true iff delete() has been called successfully.
     */
    private boolean deleted = false;

    /**
     * Is true iff delete() has been started. (You cannot create any new
     * TestCases or modify the TestSession then.)
     */
    private boolean deleting = false;

    @Override
    protected void assertNotDeleted() {
        if (this.deleted) {
            throw new IllegalStateException(
                    "This TestSession has been deleted.");
        }
    }

    private void assertNotDeleting() {
        assertNotDeleted();
        if (this.deleting) {
            throw new IllegalStateException(
                    "This TestSession is being deleted.");
        }
    }

    /**
     * Delete this TestSession and all {@link TestCase}s in it. Any further
     * method call to this TestSession or its {@link TestCase}s will cause a
     * {@link IllegalStateException} to be thrown. Any attempt to modify the
     * content of this TestSession during the removal (e.g. in listeners) will
     * also cause a {@link IllegalStateException}.
     */
    public void delete() {
        synchronized (this.lock) {
            assertNotDeleting();

            this.deleting = true;
        }

        for (TestCase testCase : getTestCases()) {
            testCase.delete();
        }

        if (this.testCases.size() != 0) {
            // This should not happen since deleting is true and createTestCase
            // should work anymore
            throw new RuntimeException();
        }

        synchronized (this.lock) {
            synchronized (getTestSessionContainer().lock) {
                this.testSessionContainer.removeTestSession(this);
            }
        }

        notifyChangeListener(ChangeType.REMOVE);
        this.testSessionContainer.notifyChangeListener(ChangeType.CHANGE);

        synchronized (this.lock) {
            this.deleted = true;
        }
    }

    /**
     * Constructor
     *
     * @param testSessionContainer
     *            the {@link TestSessionContainer} containing this instance
     * @param date
     *            the date this test session was created
     */
    TestSession(TestSessionContainer testSessionContainer, Date date) {
        this(testSessionContainer, date, "", "");
    }

    /**
     * Constructor
     *
     * @param testSessionContainer
     *            the {@link TestSessionContainer} containing this instance
     * @param date
     *            the date this test session was created
     * @param name
     *            the name of the test session
     * @param comment
     *            the comment associated with this test session
     */
    TestSession(TestSessionContainer testSessionContainer, Date date,
            String name, String comment) {
        if (testSessionContainer == null) {
            throw new NullPointerException("testSessionContainer == null");
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

        this.testSessionContainer = testSessionContainer;
        this.date = (Date) date.clone();
        this.name = name;
        this.comment = comment;
    }

    /**
     * Gets an unmodifiable list of all the {@link TestCase}s associated with
     * this {@link TestSession}
     *
     * @return the list of {@link TestCase}s
     */
    public List<TestCase> getTestCases() {
        //assertNotDeleted();

        synchronized (this.lock) {
            return CollectionUtil.copy(this.testCases);
        }
    }

    /**
     * Gets the {@link TestCase} with the given name.
     *
     * @param name
     *            the name of the {@link TestCase}
     * @return the {@link TestCase} or <code>null</code>, if no such
     *         {@link TestCase} exists.
     */
    public TestCase getTestCaseWithName(String name) {
        if (name == null) {
            throw new NullPointerException("name == null");
        }

        TestCase testCase = null;

        synchronized (this.lock) {
            for (TestCase currentTestCase : getTestCases()) {
                if (currentTestCase.getName().equals(name)) {
                    testCase = currentTestCase;
                    break;
                }
            }
        }

        return testCase;
    }

    /**
     * Gets a list of the names of all {@link TestCase}s in this
     * {@link TestSession}
     *
     * @return the list of {@link TestCase} names
     */
    public List<String> getTestCaseNames() {
        //assertNotDeleted();

        final List<String> testCaseNames;

        synchronized (this.lock) {
            testCaseNames = new Vector<String>();

            for (TestCase testCase : getTestCases()) {
                testCaseNames.add(testCase.getName());
            }
        }

        return testCaseNames;
    }

    /**
     * Gets whether or not this {@link TestSession} contains a {@link TestCase}
     * with the given name
     *
     * @param name
     *            the name of the {@link TestCase}
     * @return <code>true</code>, if the {@link TestSession} contains a
     *         {@link TestCase} with the name, <code>false</code>, if not.
     */
    public boolean containsTestCaseWithName(String name) {
        return (getTestCaseWithName(name) != null);
    }

    /**
     * Copys the given {@link TestCase} into this {@link TestSession}, if is
     * not already present.
     *
     * @param testCase
     *            the given {@link TestCase}
     * @see TestSession#copyTestCaseIntoTestSession(TestCase, String)
     */
    public void copyTestCaseIntoTestSession(TestCase testCase) {
        if (testCase == null) {
            throw new NullPointerException("testCase == null");
        }

        assertNotDeleting();

        copyTestCaseIntoTestSession(testCase, testCase.getName());
    }

    /**
     * Copys the given {@link TestCase} into this {@link TestSession}, if is
     * not already present. The {@link TestCase} will receive the given new
     * name.
     *
     * @param testCase
     *            the given {@link TestCase}
     * @param newTestCaseName
     *            the new name under which the {@link TestCase} is to be saved.
     */
    public void copyTestCaseIntoTestSession(TestCase testCase,
            String newTestCaseName) {
        synchronized (this.lock) {
            assertNotDeleting();

            if (!getTestCases().contains(testCase)) {
                this.createTestCase(newTestCaseName, testCase.getComment(),
                        testCase.getDate(), testCase.getCoverageData(),
                        testCase.getAssignmentsMap());
            }
        }
    }

    /**
     * Creates a test case and adds it to this test session.
     *
     * @param name
     *            the name of the test case
     * @param comment
     *            the comment associated with this test case
     * @param date
     *            the date this test case was created
     * @param coverageData
     *            the measured coverage associated with this test case
     * @param assignments
     *            the measured assignments of the conditions contained in the
     *            root terms of the MAST
     * @return the created test session
     */
    public TestCase createTestCase(String name, String comment, Date date,
            Map<CoverableItem, Long> coverageData,
            Map<CoverableItem, BooleanAssignmentMap> assignments) {
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

        final TestCase testCase;

        synchronized (this.lock) {
            assertNotDeleting();

            // A name identifies a test case, if the same name exists in this
            // test
            // session, the test case name is expanded to %test case name%
            // (%number
            // of reoccurences%)
            String newName = NameReoccurenceHelper.escapeName(
                    getTestCaseNames(), name);

            testCase = new TestCase(this, date, coverageData, assignments,
                    newName, comment);

            this.testCases.add(testCase);
        }

        testCase.notifyChangeListener(ChangeType.ADD);
        notifyChangeListener(ChangeType.CHANGE);

        return testCase;
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

            synchronized (getTestSessionContainer().lock) {
                if (getTestSessionContainer().getTestSessionWithName(name) != null) {
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
     * @return the date
     */
    public Date getDate() {
        //assertNotDeleted();

        return (Date) this.date.clone();
    }

    /**
     * Gets the {@link TestSessionContainer}
     *
     * @return the {@link TestSessionContainer}
     */
    public TestSessionContainer getTestSessionContainer() {
        //assertNotDeleted();

        return this.testSessionContainer;
    }

    /**
     * Removes the given test case from this test session This method should
     * only be called by the {\link TestCase} class.
     *
     * @param testCase
     *            the to be removed test case
     * @return true, if this test session contained the test case
     */
    boolean removeTestCase(TestCase testCase) {
        if (testCase == null) {
            throw new NullPointerException("testCase == null");
        }

        return this.testCases.remove(testCase);
    }

    void notifyChangeListener(ChangeType type) {
        getTestSessionContainer().testSessionEvent.emitChanged(type, this);
    }

}
