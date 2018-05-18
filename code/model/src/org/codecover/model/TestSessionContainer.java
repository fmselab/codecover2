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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.exceptions.FileLoadIOException;
import org.codecover.model.exceptions.FileLoadParseException;
import org.codecover.model.exceptions.FileSaveException;
import org.codecover.model.exceptions.FileSaveIOException;
import org.codecover.model.exceptions.MergeException;
import org.codecover.model.extensions.PluginManager;
import org.codecover.model.mast.BasicStatement;
import org.codecover.model.mast.BooleanAssignmentMap;
import org.codecover.model.mast.Branch;
import org.codecover.model.mast.ConditionalStatement;
import org.codecover.model.mast.CoverableItem;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.LoopingStatement;
import org.codecover.model.mast.QuestionMarkOperator;
import org.codecover.model.mast.SynchronizedStatement;
import org.codecover.model.mast.QuestionMarkOperatorExpression;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.mast.Statement;
import org.codecover.model.utils.ChangeEvent;
import org.codecover.model.utils.ChangeListener;
import org.codecover.model.utils.ChangeType;
import org.codecover.model.utils.CollectionUtil;
import org.codecover.model.utils.ListenerHandle;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.NameReoccurenceHelper;
import org.codecover.model.utils.Pair;
import org.codecover.model.utils.criteria.Criterion;
import org.xml.sax.SAXException;

/**
 * A {@link TestSessionContainer} contains the MAST, as well as the list of all
 * original sourcefiles, the criteria, with which the original sourcefiles were
 * instrumented and the {@link TestSession}s and {@link TestCase}s, which
 * contain the measured coverage during a run of the instrumented system.
 *
 * @author Markus Wittlinger
 * @version 1.0 ($Id: TestSessionContainer.java 70 2010-04-02 09:20:58Z schmidberger $)
 */
public class TestSessionContainer {

    private final List<TestSession> testSessions = new Vector<TestSession>();

    private final List<SourceFile> files;

    private final Date date;

    private final Set<Criterion> criteria;

    private final String id;

    private File lastFileLocation;

    private final Logger logger;

    private final HierarchyLevel code;

    final ChangeEvent<TestSessionContainer> testSessionContainerEvent = new ChangeEvent<TestSessionContainer>();

    final ChangeEvent<TestSession> testSessionEvent = new ChangeEvent<TestSession>();

    final ChangeEvent<TestCase> testCaseEvent = new ChangeEvent<TestCase>();

    private final Map<CoverableItem, RootTerm> rootTerms;
    private final Map<CoverableItem, QuestionMarkOperator> questionMarkOperators;
    private final Map<CoverableItem, QuestionMarkOperatorExpression> questionMarkOperatorExpressions;

    // The root terms' coverable items are not in this set
    private final Set<CoverableItem> coverableItems;

    final Object lock = new Object();

    private volatile Map<HierarchyLevel, HierarchyLevel> parentMap;

    /**
     * Constructor
     *
     * @param code
     *            the top hierarchy level of the code
     * @param files
     *            the list of source files represented in this
     *            {@link TestSessionContainer}
     * @param date
     *            the date this {@link TestSessionContainer} was created.
     * @param logger
     *            the logger to be used
     * @param criteria
     *            the list of {@link Criterion}s
     * @param id
     *            the id of the {@link TestSessionContainer}
     */
    public TestSessionContainer(HierarchyLevel code, Logger logger,
            List<SourceFile> files, Set<Criterion> criteria, String id,
            Date date) {
        if (logger == null) {
            throw new NullPointerException("logger == null");
        }
        if (files == null) {
            throw new NullPointerException("files == null");
        }
        if (criteria == null) {
            throw new NullPointerException("criteria == null");
        }
        if (id == null) {
            throw new NullPointerException("id == null");
        }
        if (date == null) {
            throw new NullPointerException("date == null");
        }

        this.logger = logger;
        this.date = (Date) date.clone();
        this.id = id;
        this.code = code;
        this.criteria = CollectionUtil.copy(criteria);
        this.files = CollectionUtil.copy(files);

        final Map<CoverableItem, RootTerm> terms = new HashMap<CoverableItem, RootTerm>();
        if (code != null) {
            code.accept(null, null, null, null, new RootTerm.DefaultVisitor() {
                @Override
                public void visit(RootTerm term) {
                    terms.put(term.getCoverableItem(), term);
                }
            }, null, null, null, null);
        }
        this.rootTerms = Collections.unmodifiableMap(terms);
        
        final Map<CoverableItem, QuestionMarkOperator> questionMarkOperators = new TreeMap<CoverableItem, QuestionMarkOperator>();
        final Map<CoverableItem, QuestionMarkOperatorExpression> questionMarkOperatorExpressions = new TreeMap<CoverableItem, QuestionMarkOperatorExpression>();
        
        final Set<CoverableItem> items = new TreeSet<CoverableItem>();
        if (code != null) {
           
            code.accept(null, null, new Statement.DefaultVisitor() {
            	
            	private void add(Statement statement) {
            		
            		for(QuestionMarkOperator questionMarkOperator : statement.getQuestionMarkOperators()) {
            		
	                  	QuestionMarkOperatorExpression expr1 = questionMarkOperator.getQuestionMarkOperatorExpression1();
	                	QuestionMarkOperatorExpression expr2 = questionMarkOperator.getQuestionMarkOperatorExpression2();
	                	
	                	items.add(questionMarkOperator.getCoverableItem());
	                	items.add(expr1.getCoverableItem());
	                	items.add(expr2.getCoverableItem());
            		}
            		
            	}
            	
                private void add(CoverableItem item) {
                    if (item != null) {
                        items.add(item);
                    }
                }

                @Override
                public void visit(BasicStatement statement) {
                    add(statement.getCoverableItem());
                    add(statement);
                }

                @Override
                public void visit(ConditionalStatement statement) {
                    add(statement.getCoverableItem());
                    add(statement);
                }

                @Override
                public void visit(LoopingStatement statement) {
                    add(statement.getCoverableItem());
                    add(statement.getNeverExecutedItem());
                    add(statement.getOnceExecutedItem());
                    add(statement.getMultipleExecutedItem());
                    add(statement);
                }

                @Override
                public void visit(SynchronizedStatement statement) {
                	add(statement.getCoverableItem());
                	add(statement.getCoverableItem(0));
                	add(statement.getCoverableItem(1));
                	add(statement.getCoverableItem(2));
                    add(statement);
                }
                
                @Override
                public void visit(Branch branch) {
                    add(branch.getCoverableItem());
                }
            }, null, null, null, null, null, null);
        }
        
        this.questionMarkOperators = Collections.unmodifiableMap(questionMarkOperators);
        this.questionMarkOperatorExpressions = Collections.unmodifiableMap(questionMarkOperatorExpressions);

        this.coverableItems = Collections.unmodifiableSet(items);
        
    }

    /**
     * Loads a file with the given name and creates a
     * {@link TestSessionContainer} with the data it contains
     *
     * @param logger
     *            the logger to be used
     * @param builder
     *            the {@link MASTBuilder} to be used in creating the MAST
     *            elements
     * @param filename
     *            the name of the source file
     * @param pluginManager
     *            the {@link PluginManager} to handle to plugin parts of
     *            CodeCover
     * @return the created {@link TestSessionContainer}
     * @throws FileLoadException
     */
    public static TestSessionContainer load(PluginManager pluginManager,
            Logger logger, MASTBuilder builder, String filename)
            throws FileLoadException {
        return load(pluginManager, logger, builder, new File(filename));
    }

    /**
     * Loads a given file and creates a {@link TestSessionContainer} with the
     * data it contains
     *
     * @param logger
     *            the logger to be used
     * @param builder
     *            the {@link MASTBuilder} to be used in creating the MAST
     *            elements
     * @param file
     *            the source file
     * @param pluginManager
     *            the {@link PluginManager} to handle to plugin parts of
     *            CodeCover
     * @return the created {@link TestSessionContainer}
     * @throws FileLoadException
     */
    public static TestSessionContainer load(PluginManager pluginManager,
            Logger logger, MASTBuilder builder, File file)
            throws FileLoadException {
        if (pluginManager == null) {
            throw new NullPointerException("pluginManager == null");
        }
        if (logger == null) {
            throw new NullPointerException("logger == null");
        }
        if (builder == null) {
            throw new NullPointerException("builder == null");
        }
        if (file == null) {
            throw new NullPointerException("file == null");
        }

        XMLReaderBase readerBase = new XMLReader1_0_SAX(logger, builder,
                pluginManager);

        return load(logger, getInputStream(file), readerBase);
    }

    private static InputStream getInputStream(File file)
            throws FileLoadIOException {
        File absoluteFile = file.getAbsoluteFile();

        try {
            return new FileInputStream(absoluteFile);
        } catch (FileNotFoundException e) {
            throw new FileLoadIOException("A FileLoadIOException"
                    + " has occured", e);
        }
    }

    private static TestSessionContainer load(Logger logger,
            InputStream inputStream, XMLReaderBase readerBase)
            throws FileLoadException {
        try {
            SAXFileReader.parse(inputStream, readerBase);
            TestSessionContainer testSessionContainer = readerBase
                    .getTestSessionContainer();

            if (testSessionContainer == null) {
                throw new FileLoadParseException(
                        "The given file is not a valid test session container!");
            }

            return testSessionContainer;
        } catch (ParserConfigurationException e) {
            throw new RuntimeException("XMLParser configuration error", e);
        } catch (IOException e) {
            throw new FileLoadIOException("A FileLoadIOException"
                    + " has occurred", e);
        } catch (SAXException e) {
            throw new FileLoadParseException("A FileLoadParseException"
                    + " has occurred", e);
        } catch (NullPointerException e) {
            // This probably means an element or an attribute is missing in
            // the XML file
            throw new FileLoadParseException("A FileLoadParseException"
                    + " has occurred", e);
        } catch (IllegalArgumentException e) {
            // This probably means some data in the XML file is inconsistent
            throw new FileLoadParseException("A FileLoadParseException"
                    + " has occurred", e);
        }
    }

    /**
     * Loads the given file reading only the infos about the
     * {@link TestSessionContainer}, the {@link TestSession}s and the
     * {@link TestCase}s it contains.
     * <p>
     * This method is intended to be used for scenarios in which the mast is not
     * required.
     *
     * @param logger
     *            the logger to be used
     * @param builder
     *            the {@link MASTBuilder} to be used in creating the MAST
     *            elements
     * @param file
     *            the source file
     * @param pluginManager
     *            the {@link PluginManager} to handle to plugin parts of
     *            CodeCover
     * @return the created {@link TestSessionContainer}
     * @throws FileLoadException
     */
    public static TestSessionContainer loadInfoOnly(
            PluginManager pluginManager, Logger logger, MASTBuilder builder,
            File file) throws FileLoadException {
        if (pluginManager == null) {
            throw new NullPointerException("pluginManager == null");
        }
        if (logger == null) {
            throw new NullPointerException("logger == null");
        }
        if (builder == null) {
            throw new NullPointerException("builder == null");
        }
        if (file == null) {
            throw new NullPointerException("file == null");
        }

        XMLReaderBase readerBase = new XMLReader1_0_SAXInfoOnly(logger, builder);

        return load(logger, getInputStream(file), readerBase);
    }

    /**
     * Loads a file with the given name reading only the infos about the
     * {@link TestSessionContainer}, the {@link TestSession}s and the
     * {@link TestCase}s it contains.
     * <p>
     * This method is intended to be used for scenarios in which the mast is not
     * required.
     *
     * @param logger
     *            the logger to be used
     * @param builder
     *            the {@link MASTBuilder} to be used in creating the MAST
     *            elements
     * @param filename
     *            the name of the source file
     * @param pluginManager
     *            the {@link PluginManager} to handle to plugin parts of
     *            CodeCover
     * @return the created {@link TestSessionContainer}
     * @throws FileLoadException
     */
    public static TestSessionContainer loadInfoOnly(
            PluginManager pluginManager, Logger logger, MASTBuilder builder,
            String filename) throws FileLoadException {
        return loadInfoOnly(pluginManager, logger, builder, new File(filename));
    }

    @Override
    public String toString() {
        return String.format("%s (%2$td.%2$tm.%2$tY %2$tH:%2$tM:%2$tS)",
                getId(), getDate());
    }

    /**
     * @return a map for all {@link RootTerm}s appearing in the code from the
     *         RootTerm CoverableItem IDs to the RootTerms.
     */
    public Map<CoverableItem, RootTerm> getRootTerms() {
        return this.rootTerms;
    }

    /**
     * @return all {@link CoverableItem}s appearing in the code except those in
     *         a {@link RootTerm} RootTerm
     */
    public Set<CoverableItem> getCoverableItems() {
        return this.coverableItems;
    }

    /**
     * Saves this {@link TestSessionContainer} to the last location it was saved
     * to.
     * <p>
     * Note: Will throw a NullPointerException, if the
     * {@link TestSessionContainer} was not saved before, or was loaded using a
     * {@link InputStream}.
     *
     * @throws FileSaveException
     */
    public void save() throws FileSaveException {
        final File location;

        synchronized (this.lock) {
            location = this.lastFileLocation;
        }

        if (location == null) {
            throw new NullPointerException("lastFileLocation == null");
        }

        save(location);
    }

    /**
     * Saves this {@link TestSessionContainer} to the location specified by the
     * filename
     *
     * @param filename
     *            the name of the target file
     * @throws FileSaveException
     */
    public void save(String filename) throws FileSaveException {
        save(new File(filename));
    }

    /**
     * Saves this {@link TestSessionContainer} to the specified file
     *
     * @param file
     *            the target file
     * @throws FileSaveException
     */
    public void save(File file) throws FileSaveException {
        File absoluteFile = file.getAbsoluteFile();

        // TODO: produce 1.0 files for now

        // XMLWriter2_0_SAX fileWriter = new XMLWriter2_0_SAX(getLogger());
        // XMLWriter1_0_DOM fileWriter = new XMLWriter1_0_DOM(getLogger());
        XMLWriter1_0_SAX fileWriter = new XMLWriter1_0_SAX(getLogger());

        try {
            // To avert corrupting any older files, the database is first saved
            // to a temporary file, and then, baring any mishaps, renamed into
            // the destination file
            File tmpFile = File.createTempFile("codecover", null, absoluteFile
                    .getParentFile());
            try {
                SAXFileWriter.write(this, tmpFile, fileWriter);
                // fileWriter.writeFile(tmpFile, this);

                // No exceptions were thrown, so we can rename the temp file to
                // the
                // destination file
                if (tmpFile.renameTo(absoluteFile)) {
                    tmpFile = null;
                } else {
                    // Rename didn't work, probably we're on an OS where we
                    // have to delete the target file first
                    final boolean deleteResult = absoluteFile.delete();

                    // When delete fails, we ignore this (it might fail because
                    // the target file doesn't exist)

                    if (tmpFile.renameTo(absoluteFile)) {
                        tmpFile = null;
                    } else {
                        if (deleteResult) {
                            // This is *bad*. We've deleted the old file but
                            // couldn't rename the temp file.
                            final String message = "Could not rename temp file "
                                    + tmpFile + " to " + absoluteFile;
                            // Set tmpFile to null so that at least the tmpfile
                            // still exists
                            tmpFile = null;
                            throw new IOException(message);
                        } else { // delete didn't work
                            throw new IOException("Could not delete "
                                    + absoluteFile);
                        }
                    }
                }
            } finally {
                if (tmpFile != null) {
                    if (!tmpFile.delete()) {
                        throw new IOException("Could not delete tmpfile "
                                + tmpFile);
                    }
                }
            }
        } catch (IOException e) {
            throw new FileSaveIOException("A FileSaveIOException"
                    + " has occurred", e);
        } catch (TransformerException e) {
            throw new RuntimeException("A TransformerException"
                    + " has occurred", e);
            // } catch (ParserConfigurationException e) {
            // should not happen
            // throw new RuntimeException(
            // "A ParserConfigurationException has occurred", e);
        }

        synchronized (this.lock) {
            this.lastFileLocation = absoluteFile;
        }
    }

    /**
     * Adds a ChangeListener for this {@link TestSessionContainer}.
     *
     * @param listener
     *            the given listener
     * @return an instance of a ListenerHandle, which removes the listener from
     *         the {@link TestSessionContainer}, when it is disposed.
     */
    public ListenerHandle addTestSessionContainerListener(
            ChangeListener<TestSessionContainer> listener) {
        return this.testSessionContainerEvent.addListener(listener);
    }

    /**
     * Adds a ChangeListener for the test sessions in this
     * {@link TestSessionContainer}.
     *
     * @param listener
     *            the given listener
     * @return an instance of a ListenerHandle, which removes the listener from
     *         the {@link TestSessionContainer}, when it is disposed.
     */
    public ListenerHandle addTestSessionListener(
            ChangeListener<TestSession> listener) {
        return this.testSessionEvent.addListener(listener);
    }

    /**
     * Adds a ChangeListener for the test cases in this
     * {@link TestSessionContainer}.
     *
     * @param listener
     *            the given listener
     * @return an instance of a ListenerHandle, which removes the listener from
     *         the {@link TestSessionContainer}, when it is disposed.
     */
    public ListenerHandle addTestCaseListener(ChangeListener<TestCase> listener) {
        return this.testCaseEvent.addListener(listener);
    }

    /**
     * Creates a testsession and adds it to this {@link TestSessionContainer}.
     *
     * @param name
     *            the name of the test session
     * @param comment
     *            the comment associated with this test tession
     * @param date
     *            the date this test session was created
     * @return the created test session
     */
    public TestSession createTestSession(String name, String comment, Date date) {
        if (name == null) {
            throw new NullPointerException("name == null");
        }
        if (comment == null) {
            throw new NullPointerException("comment == null");
        }
        if (date == null) {
            throw new NullPointerException("date == null");
        }

        final TestSession session;

        synchronized (this.lock) {
            // A name identifies a test session, if the same name exists in this
            // codebase, the test session name is expanded to %test session
            // name%
            // (%number of reoccurences%)
            String newName = NameReoccurenceHelper.escapeName(
                    getTestSessionNames(), name);

            session = new TestSession(this, date, newName, comment);

            this.testSessions.add(session);
        }

        session.notifyChangeListener(ChangeType.ADD);

        // List of test sessions has been changed, send change event.
        notifyChangeListener(ChangeType.CHANGE);

        return session;
    }

    /**
     * @return the top hierarchy level
     */
    public HierarchyLevel getCode() {
        return this.code;
    }

    /**
     * Gets the {@link Logger} associated with this {@link TestSessionContainer}
     *
     * @return the {@link Logger}
     */

    public Logger getLogger() {
        return this.logger;
    }

    /**
     * Gets the id of the {@link TestSessionContainer}
     *
     * @return the id of the {@link TestSessionContainer}
     */
    public String getId() {
        return this.id;
    }

    /**
     * @return the date
     */
    public Date getDate() {
        return (Date) this.date.clone();
    }

    /**
     * @return the criteria used for instrumentation
     */
    public Set<Criterion> getCriteria() {
        return this.criteria;
    }

    /**
     * @return the files
     */
    public List<SourceFile> getFiles() {
        return this.files;
    }

    /**
     * Gets an unmodifiable list of all the {@link TestSession}s associated
     * with this {@link TestSessionContainer}
     *
     * @return the list of {@link TestSession}s
     */
    public List<TestSession> getTestSessions() {
        synchronized (this.lock) {
            return CollectionUtil.copy(this.testSessions);
        }
    }

    /**
     * Gets the {@link TestSession} with the given name.
     *
     * @param name
     *            the name of the {@link TestSession}
     * @return the {@link TestSession} or <code>null</code>, if no such
     *         {@link TestSession} exists.
     */
    public TestSession getTestSessionWithName(String name) {
        if (name == null) {
            throw new NullPointerException("name == null");
        }

        TestSession testSession = null;

        synchronized (this.lock) {
            for (TestSession currentTestSession : getTestSessions()) {

                if (currentTestSession.getName().equals(name)) {
                    testSession = currentTestSession;
                    break;
                }
            }
        }

        return testSession;
    }

    /**
     * Gets a list of the names of all {@link TestSession}s in this
     * {@link TestSessionContainer}
     *
     * @return the list of {@link TestSession} names
     */
    public List<String> getTestSessionNames() {
        List<String> testSessionNames = new Vector<String>();

        synchronized (this.lock) {
            for (TestSession testSession : getTestSessions()) {
                testSessionNames.add(testSession.getName());
            }
        }

        return testSessionNames;
    }

    /**
     * Gets whether or not this {@link TestSessionContainer} contains a
     * {@link TestSession} with the given name
     *
     * @param name
     *            the name of the {@link TestSession}
     * @return <code>true</code>, if the {@link TestSessionContainer}
     *         contains a {@link TestSession} with the name, <code>false</code>,
     *         if not.
     */
    public boolean containsTestSessionWithName(String name) {
        if (name == null) {
            throw new NullPointerException("name == null");
        }

        return (getTestSessionWithName(name) != null);
    }

    /**
     * Merges a collection of {@link TestCase}s into one single {@link TestCase} with a given new name and new
     * comment. All coverage data and assignment data the test cases contained is combined. The original
     * {@link TestCase}s remain untouched.
     * <p>
     * All the test cases must have the same test session. The resulting test case will be in this test
     * session.
     * <p>
     * No meta data will carried over into the new test case.
     *
     * @param testCases
     *            the given collection of test cases to be merged. This collection may not be empty.
     * @param newName
     *            the name of the merged test case
     * @param newComment
     *            the comment of the merged test case
     * @return the merged {@link TestCase}
     * @throws MergeException
     */
    public TestCase mergeTestCases(Collection<TestCase> testCases, String newName, String newComment)
            throws MergeException {
        if (newName == null) {
            throw new NullPointerException("newName == null");
        }

        if (newComment == null) {
            throw new NullPointerException("newComment == null");
        }

        // Check, if all the test cases share the same test session.
        TestSession testSessionOfFirst = testCases.iterator().next().getTestSession();
        for (TestCase testCase : testCases) {
            if (!testCase.getTestSession().equals(testSessionOfFirst)) {
                throw new MergeException("test sessions do not match: " + testCase.getTestSession() + " vs. "
                        + testSessionOfFirst);
            }
        }

        Pair<Map<CoverableItem, Long>, Map<CoverableItem, BooleanAssignmentMap>> mergedCoverage =
            TestCase.mergeTestCasesCoverage(testCases);

        // Create the test case with the merged data and the current date.
        TestCase newTestCase = testSessionOfFirst.createTestCase(newName, newComment,
                new Date(), mergedCoverage.first, mergedCoverage.second);

        return newTestCase;
    }

    /**
     * Merges a list of {@link TestSession}s into one single
     * {@link TestSession}, containing all the {@link TestCase}s of the
     * original {@link TestSession}s. The original {@link TestSession}s will
     * remain untouched.
     * <p>
     * If two {@link TestCase}s from differing {@link TestSession}s would
     * share the same name, their name will be modified and include the name of
     * the originating {@link TestSession}.
     * <p>
     * E.g.: test case name (session name 1), test case name (session name 2).
     * <p>
     * No metadata will carried over into the new test session.
     * <p>
     * All {@link TestCase}s have to belong to this TestSessionContainer.
     * <p>
     * The new {@link TestSession} will belong to this TestSessionContainer,
     * i.e. calling
     * {@link TestSession#getTestSessionContainer() getTestSessionContainer()}
     * will return the TestSessionContainer you call this method on.
     *
     * @param testSessions
     *            the given list of {@link TestSession}s to be merged. This
     *            list may not be emtpy.
     * @param newName
     *            the name of the merged {@link TestSession}.
     * @param newComment
     *            the comment of the merged {@link TestSession}.
     * @return the merged {@link TestSession}
     * @throws MergeException
     */
    public TestSession mergeTestSessions(List<TestSession> testSessions,
            String newName, String newComment) throws MergeException {
        if (testSessions == null) {
            throw new NullPointerException("testSessions == null");
        }

        if (testSessions.size() == 0) {
            throw new IllegalArgumentException("testSessions.size() == 0");
        }

        if (newName == null) {
            throw new NullPointerException("newName == null");
        }

        if (newComment == null) {
            throw new NullPointerException("newComment == null");
        }

        for (int i = 0; i < testSessions.size() - 1; i++) {
            TestSessionContainer tsc1 = testSessions.get(i)
                    .getTestSessionContainer();
            TestSessionContainer tsc2 = testSessions.get(i + 1)
                    .getTestSessionContainer();
            if (!tsc1.isCompatible(tsc2)) {
                throw new MergeException(
                        "test session containers are incompatible");
            }
        }

        TestSession newTestSession = createTestSession(newName, newComment,
                new Date());

        // Collect all testcases of all test session, that are to be merged.
        List<TestCase> allTestCases = new Vector<TestCase>();

        for (TestSession testSession : testSessions) {
            allTestCases.addAll(testSession.getTestCases());
        }

        Map<TestCase, String> replacementNames = new HashMap<TestCase, String>();

        // This check, if some test cases share the same name, and - if so -
        // saves the replacement name for later usage.
        // TODO: This should not be O(n²)
        // TODO: Does this always work? I (Steffen) don't think so.
        // TODO: Take care of what happens when s.o. changes the test case names
        // while this method is running
        for (int i = 0; i < allTestCases.size(); i++) {
            TestCase firstTestCase = allTestCases.get(i);

            for (int a = i + 1; a < allTestCases.size(); a++) {

                TestCase secondTestCase = allTestCases.get(a);

                if (firstTestCase.getName().equals(secondTestCase.getName())) {

                    replacementNames.put(firstTestCase,
                            getTestCaseReplacementName(firstTestCase));

                    replacementNames.put(secondTestCase,
                            getTestCaseReplacementName(secondTestCase));
                }
            }
        }

        for (TestCase testCase : allTestCases) {
            // if a replacement name was put under the name of the current test
            // case, use the replacement name.
            String replacementName = replacementNames.get(testCase);
            String testCaseName = (replacementName != null ? replacementName
                    : testCase.getName());
            // Create new test case in the merged session, with the data of the
            // current test case
            newTestSession.createTestCase(testCaseName, testCase.getComment(),
                    testCase.getDate(), testCase.getCoverageData(), testCase
                            .getAssignmentsMap());
        }

        return newTestSession;
    }

    /**
     * Adds the name of the test session to the name of the given test case and
     * returns it. The test case is not modified.
     *
     * @param testCase
     *            the given testcase.
     * @return the replacement name in the form of
     *         <code>test case name (test session name)</code>
     */
    private static String getTestCaseReplacementName(TestCase testCase) {
        String name = testCase.getName() + " ("
                + testCase.getTestSession().getName() + ")";
        return name;
    }

    /**
     * Removes the given test session from this {@link TestSessionContainer}
     * This method should only be called by the {\link TestSession} class.
     *
     * @param testSession
     *            the to be removed test session
     * @return true, if this {@link TestSessionContainer} contained the test
     *         session
     */
    boolean removeTestSession(TestSession testSession) {
        if (testSession == null) {
            throw new NullPointerException("testSession == null");
        }

        return this.testSessions.remove(testSession);
    }

    /**
     * Compares the ids of the testSessionContainer
     *
     * @param testSessionContainer
     *            the {@link TestSessionContainer} to be compared
     * @return <code>true</code>, if the ids of the
     *         {@link TestSessionContainer}s match, <code>false</code> if
     *         they do not
     */
    public boolean isCompatible(TestSessionContainer testSessionContainer) {
        if (testSessionContainer == null) {
            throw new NullPointerException("testSessionContainer == null");
        }

        return this.getId().equals(testSessionContainer.getId());
    }

    /**
     * Gets the parent of the given {@link HierarchyLevel} <br>
     * FIXME: What should be returned, when the topLevel is given?
     *
     * @param hierarchyLevel
     *            the {@link HierarchyLevel}, whose parent is desired
     * @return the parent of the {@link HierarchyLevel}
     * @throws IllegalArgumentException
     *             when the given {@link HierarchyLevel} was not part of this
     *             {@link TestSessionContainer}
     */
    public HierarchyLevel getParentOfHierarchyLevel(
            HierarchyLevel hierarchyLevel) {
        if (hierarchyLevel == null) {
            throw new NullPointerException("hierarchyLevel == null");
        }

        if (this.parentMap == null) {
            final Map<HierarchyLevel, HierarchyLevel> newParentMap = new HashMap<HierarchyLevel, HierarchyLevel>();
            this.code.accept(new HierarchyLevel.DefaultVisitor() {
                @Override
                public void visit(HierarchyLevel hierarchyLevel) {
                    // Put for every child in this hierarchylevel the parent
                    for (HierarchyLevel childLevel : hierarchyLevel
                            .getChildren()) {
                        newParentMap.put(childLevel, hierarchyLevel);
                    }
                }
            }, null, null, null, null, null, null, null, null);
            // This might override another parentMap created in another thread,
            // but we don't care.
            // Since parentMap is volatile it always will be null or point
            // to a valid Map
            this.parentMap = newParentMap;
        }

        HierarchyLevel parentHierarchyLevel = this.parentMap
                .get(hierarchyLevel);

        // No entry for the given HierarchyLevel existed (and it is not the top
        // level hierarchy level), so throw Exception
        if (parentHierarchyLevel == null && !hierarchyLevel.equals(getCode())) {
            throw new IllegalArgumentException("The given HierarchyLevel "
                    + "is not part of this TestSessionContainer");
        }

        return parentHierarchyLevel;
    }

    void notifyChangeListener(ChangeType type) {
        this.testSessionContainerEvent.emitChanged(type, this);
    }
}
