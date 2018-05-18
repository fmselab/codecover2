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
import java.util.*;
import java.util.Map.Entry;

import org.codecover.model.exceptions.*;
import org.codecover.model.extensions.*;
import org.codecover.model.mast.*;
import org.codecover.model.utils.LogLevel;
import org.codecover.model.utils.SimpleLogger;
import org.codecover.model.utils.criteria.*;

/**
 * JUnit Testcase of Database class
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: DatabaseTest.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public class DatabaseTest extends junit.framework.TestCase {
    protected MASTBuilder builder;

    protected TestSessionContainer testSessionContainer;

    protected SimpleLogger logger;

    private RootTerm ifRootTerm;

    private RootTerm whileRootTerm;

    private BasicBooleanTerm barTerm;

    private BasicBooleanTerm somethingTerm;

    private BasicBooleanTerm nothingTerm;

    /**
     * @throws java.lang.Exception
     */
    @Override
    protected void setUp() throws Exception {

        this.logger = new SimpleLogger(LogLevel.INFO);

        this.builder = new MASTBuilder(this.logger);

        Vector<Statement> statementList;
        Set<RootTerm> terms;
        List<Branch> branches;
        Vector<BooleanTerm> operands;
        Vector<StatementSequence> sequenceList;
        Vector<HierarchyLevel> children;
        Vector<SourceFile> sourceFiles;
        int counter = 0;

        SourceFile sourceFile = this.builder
                .createSourceFile(
                        "Example.java",
                        "public class TestClass {\r public static void main(String[] args) {\r    if (something || nothing) {\r      foo();\r    } else {\r      bar();\r    }\r    \r    while (!bar()) {\r      i++;\r      j++;\r    }\r  }\r}");

        // while
        LocationList incrementILocationList = this.createLocationList(
                this.builder, sourceFile, 176, 180);
        BasicStatement incrementIStatement = this.builder.createBasicStatement(
                incrementILocationList, this.builder.createCoverableItem(
                        "TestClass", "S1"), new HashSet<RootTerm>(), new HashSet<QuestionMarkOperator>());

        LocationList incrementJLocationList = this.createLocationList(
                this.builder, sourceFile, 187, 191);
        BasicStatement incrementJStatement = this.builder.createBasicStatement(
                incrementJLocationList, this.builder.createCoverableItem(
                        "TestClass", "S2"), new HashSet<RootTerm>(), new HashSet<QuestionMarkOperator>());

        statementList = new Vector<Statement>();
        statementList.addElement(incrementIStatement);
        statementList.addElement(incrementJStatement);

        LocationList whileBodyLocationList = this.createLocationList(
                this.builder, sourceFile, 176, 192);

        StatementSequence whileBody = this.builder.createStatementSequence(
                whileBodyLocationList, statementList);

        LocationList whileLocationList = this.createLocationList(this.builder,
                sourceFile, 153, 201);

        Location whileKeywordLocation = this.builder.createLocation(sourceFile,
                153, 158);

        LocationList barTermLocationList = this.createLocationList(
                this.builder, sourceFile, 161, 166);

        BasicBooleanTerm barTerm = this.builder
                .createBasicBooleanTerm(barTermLocationList);
        this.barTerm = barTerm;

        operands = new Vector<BooleanTerm>();
        operands.addElement(barTerm);

        BooleanOperator notOperator = createNotOperator(this.builder);

        LocationList notOperatorTermLocationList = this.createLocationList(
                this.builder, sourceFile, 160, 161);

        OperatorTerm notOperatorTerm = this.builder.createOperatorTerm(
                notOperatorTermLocationList, notOperator, operands);

        RootTerm whileExpression = this.builder.createRootTerm(notOperatorTerm,
                this.builder.createCoverableItem("TestClass", "C1"));

        // ////////////////////////////////////////////////////// //
        // save the whileExpression in member variable for testing purpose
        this.whileRootTerm = whileExpression;
        // ////////////////////////////////////////////////////// //

        terms = new HashSet<RootTerm>();
        terms.add(whileExpression);
        

        LoopingStatement whileStatement = this.builder.createLoopingStatement(
                whileLocationList, this.builder.createCoverableItem(
                        "TestClass", "S3"), terms, whileBody,
                whileKeywordLocation, this.builder.createCoverableItem(
                        "TestClass", "L-0"), this.builder.createCoverableItem(
                        "TestClass", "L-1"), this.builder.createCoverableItem(
                        "TestClass", "L-2"), true, new HashSet<QuestionMarkOperator>());
        // while

        // if
        // ifBranch
        LocationList fooStatementLocationList = this.createLocationList(
                this.builder, sourceFile, 105, 111);
        BasicStatement fooStatement = this.builder.createBasicStatement(
                fooStatementLocationList, this.builder.createCoverableItem(
                        "TestClass", "S4"), new HashSet<RootTerm>(), new HashSet<QuestionMarkOperator>());

        statementList = new Vector<Statement>();
        statementList.addElement(fooStatement);

        LocationList fooStatementSequenceLocationList = this
                .createLocationList(this.builder, sourceFile, 105, 111);
        StatementSequence fooStatementSequence = this.builder
                .createStatementSequence(fooStatementSequenceLocationList,
                        statementList);

        LocationList ifBranchLocationList = this.createLocationList(
                this.builder, sourceFile, 97, 117);
        LocationList ifBranchDecisionList = this.createLocationList(
                this.builder, sourceFile, 71, 73);
        Branch ifBranch = this.builder.createBranch(ifBranchLocationList,
                this.builder.createCoverableItem("TestClass", "B1"), false,
                ifBranchDecisionList, fooStatementSequence);
        // ifBranch

        // elseBranch
        LocationList barStatementLocationList = this.createLocationList(
                this.builder, sourceFile, 131, 137);
        BasicStatement barStatement = this.builder.createBasicStatement(
                barStatementLocationList, this.builder.createCoverableItem(
                        "TestClass", "S5"), new HashSet<RootTerm>(), new HashSet<QuestionMarkOperator>());

        statementList = new Vector<Statement>();
        statementList.addElement(barStatement);

        LocationList barStatementSequenceLocationList = this
                .createLocationList(this.builder, sourceFile, 131, 137);
        StatementSequence barStatementSequence = this.builder
                .createStatementSequence(barStatementSequenceLocationList,
                        statementList);

        LocationList elseBranchLocationList = this.createLocationList(
                this.builder, sourceFile, 123, 143);
        LocationList elseBranchDecisionList = this.createLocationList(
                this.builder, sourceFile, 118, 123);
        Branch elseBranch = this.builder.createBranch(elseBranchLocationList,
                this.builder.createCoverableItem("TestClass", "B2"), false,
                elseBranchDecisionList, barStatementSequence);
        // elseBranch

        branches = new Vector<Branch>();
        branches.add(ifBranch);
        branches.add(elseBranch);

        LocationList somethingTermLocationList = this.createLocationList(
                this.builder, sourceFile, 75, 84);
        BasicBooleanTerm somethingTerm = this.builder
                .createBasicBooleanTerm(somethingTermLocationList);
        this.somethingTerm = somethingTerm;

        LocationList nothingTermLocationList = this.createLocationList(
                this.builder, sourceFile, 88, 95);
        BasicBooleanTerm nothingTerm = this.builder
                .createBasicBooleanTerm(nothingTermLocationList);
        this.nothingTerm = nothingTerm;

        operands = new Vector<BooleanTerm>();
        operands.addElement(somethingTerm);
        operands.addElement(nothingTerm);

        LocationList operatorTermLocationList = this.createLocationList(
                this.builder, sourceFile, 85, 87);

        BooleanOperator orOperator = createShortCircuitOr(this.builder);

        OperatorTerm operatorTerm = this.builder.createOperatorTerm(
                operatorTermLocationList, orOperator, operands);

        RootTerm ifTerm = this.builder.createRootTerm(operatorTerm,
                this.builder.createCoverableItem("TestClass", "C2"));

        // ////////////////////////////////////////////////////// //
        // save the ifTerm in member variable for testing purpose
        this.ifRootTerm = ifTerm;
        // ////////////////////////////////////////////////////// //

        terms = new HashSet<RootTerm>();
        terms.add(ifTerm);

        LocationList ifStatementLocationList = this.createLocationList(
                this.builder, sourceFile, 71, 143);

        Location ifStatementKeywordLocation = this.builder.createLocation(
                sourceFile, 71, 73);

        Set<QuestionMarkOperator> questionMarkOperators = new HashSet<QuestionMarkOperator>();
        
        ConditionalStatement ifStatement = this.builder
                .createConditionalStatement(ifStatementLocationList,
                        this.builder.createCoverableItem("TestClass", "S6"),
                        terms, branches, ifStatementKeywordLocation, questionMarkOperators);
        // if

        statementList = new Vector<Statement>();
        statementList.addElement(ifStatement);
        statementList.addElement(whileStatement);

        LocationList mainSequenceLocationList = this.createLocationList(
                this.builder, sourceFile, 71, 197);
        StatementSequence mainSequence = this.builder.createStatementSequence(
                mainSequenceLocationList, statementList);

        HierarchyLevelType methodType = this.builder.createHierarchyLevelType(
                "method", "method");

        sequenceList = new Vector<StatementSequence>();
        sequenceList.addElement(mainSequence);

        LocationList mainMethodLocationList = this.createLocationList(
                this.builder, sourceFile, 26, 202);
        LocationList mainMethodHeader = this.createLocationList(this.builder,
                sourceFile, 26, 64);
        HierarchyLevel mainMethod = this.builder.createHierarchyLevel(
                mainMethodLocationList, "main method", mainMethodHeader,
                methodType, new Vector<HierarchyLevel>(), sequenceList,
                ++counter + "");

        HierarchyLevelType classType = this.builder.createHierarchyLevelType(
                "class", "class");

        LocationList testClassObjectLocationList = this.createLocationList(
                this.builder, sourceFile, 0, 203);
        LocationList testClassObjectHeader = this.createLocationList(
                this.builder, sourceFile, 0, 22);
        // testClassObjectHeader = database.createEmptyLocationList();

        children = new Vector<HierarchyLevel>();
        children.addElement(mainMethod);

        HierarchyLevel testClassObject = this.builder.createHierarchyLevel(
                testClassObjectLocationList, "Test Class",
                testClassObjectHeader, classType, children,
                new Vector<StatementSequence>(), ++counter + "");

        sourceFiles = new Vector<SourceFile>();
        sourceFiles.addElement(sourceFile);

        final Set<Criterion> criteria = new HashSet<Criterion>();
        criteria.add(LoopCoverage.getInstance());
        criteria.add(ConditionCoverage.getInstance());
        criteria.add(StatementCoverage.getInstance());
        criteria.add(BranchCoverage.getInstance());

        // Create the code base, with the class as top level mast element.
        this.testSessionContainer = new TestSessionContainer(testClassObject,
                this.logger, sourceFiles, criteria, UUID.randomUUID()
                        .toString(), new Date());

        for (int k = 0; k < 3; k++) {

            TestSession testSession = this.testSessionContainer
                    .createTestSession("TestSession" + k, "42", new Date());

            for (int i = 0; i < 6; i++) {
                Map<CoverableItem, Long> coverageData = new HashMap<CoverableItem, Long>();

                long coverage;

                // if (i % 2 == 0) {
                // coverage = 0;
                // } else {
                coverage = 1;
                // }

                for (CoverableItem coverableItem : this.testSessionContainer
                        .getCoverableItems()) {
                    if (coverableItem.getId().equals("S1")) {
                        switch (i) {
                            case 0:
                                coverageData.put(coverableItem, new Long(1));
                                break;
                            case 1:
                                break;
                            case 2:
                                coverageData.put(coverableItem, new Long(1));
                                break;
                            case 3:
                                break;
                            case 4:
                                break;
                            case 5:
                                coverageData.put(coverableItem, new Long(1));
                                break;

                        }
                    } else if (coverableItem.getId().equals("S2")) {
                        switch (i) {
                            case 0:
                                coverageData.put(coverableItem, new Long(1));
                                break;
                            case 1:
                                coverageData.put(coverableItem, new Long(1));
                                break;
                            case 2:
                                coverageData.put(coverableItem, new Long(1));
                                break;
                            case 3:
                                coverageData.put(coverableItem, new Long(1));
                                break;
                            case 4:
                                coverageData.put(coverableItem, new Long(1));
                                break;
                            case 5:
                                coverageData.put(coverableItem, new Long(1));
                                break;

                        }
                    } else if (coverableItem.getId().equals("S3")) {
                        switch (i) {
                            case 0:
                                break;
                            case 1:
                                coverageData.put(coverableItem, new Long(1));
                                break;
                            case 2:
                                coverageData.put(coverableItem, new Long(1));
                                break;
                            case 3:
                                break;
                            case 4:
                                coverageData.put(coverableItem, new Long(1));
                                break;
                            case 5:
                                coverageData.put(coverableItem, new Long(1));
                                break;

                        }
                    } else if (coverableItem.getId().equals("S4")) {
                        switch (i) {
                            case 0:
                                coverageData.put(coverableItem, new Long(1));
                                break;
                            case 1:
                                coverageData.put(coverableItem, new Long(1));
                                break;
                            case 2:
                                break;
                            case 3:
                                coverageData.put(coverableItem, new Long(1));
                                break;
                            case 4:
                                break;
                            case 5:
                                coverageData.put(coverableItem, new Long(1));
                                break;

                        }
                    } else if (coverableItem.getId().equals("S5")) {
                        switch (i) {
                            case 0:
                                coverageData.put(coverableItem, new Long(1));
                                break;
                            case 1:
                                break;
                            case 2:
                                break;
                            case 3:
                                break;
                            case 4:
                                break;
                            case 5:
                                break;

                        }
                    } else {
                        coverageData.put(coverableItem, coverage);
                    }

                }

                Map<CoverableItem, BooleanAssignmentMap> assignments = new HashMap<CoverableItem, BooleanAssignmentMap>();
                Map<BooleanAssignment, Long> subMap = new HashMap<BooleanAssignment, Long>();

                List<BooleanResult> resultList;
                BooleanAssignment booleanAssignment;

                // set assignments of if condition "if (something || nothing)"
                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.FALSE);
                resultList.add(BooleanResult.FALSE);

                booleanAssignment = new BooleanAssignment(resultList);

                subMap.put(booleanAssignment, (long) ++counter);

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.TRUE);
                resultList.add(BooleanResult.NOT_EVALUATED);

                booleanAssignment = new BooleanAssignment(resultList);

                subMap.put(booleanAssignment, (long) ++counter);

                assignments.put(this.ifRootTerm.getCoverableItem(),
                        new BooleanAssignmentMap(2, subMap));
                // end assignment of if condition

                // set assignments of while condition "while (bar())"
                subMap = new HashMap<BooleanAssignment, Long>();

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.TRUE);
                booleanAssignment = new BooleanAssignment(resultList);

                subMap.put(booleanAssignment, (long) ++counter);

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.FALSE);
                booleanAssignment = new BooleanAssignment(resultList);

                subMap.put(booleanAssignment, (long) ++counter);

                assignments.put(this.whileRootTerm.getCoverableItem(),
                        new BooleanAssignmentMap(1, subMap));
                // end assignment of while condition

                String name = "";
                switch (i) {
                    case 0:
                        name = "e1245";
                        break;
                    case 1:
                        name = "c234";
                        break;
                    case 2:
                        name = "d123";
                        break;
                    case 3:
                        name = "f24";
                        break;
                    case 4:
                        name = "a23";
                        break;
                    case 5:
                        name = "b1234";
                        break;
                    default:
                        name = "TestCase" + i;
                }

                org.codecover.model.TestCase testCase = testSession
                        .createTestCase(name, "" + 42 * i, new Date(),
                                coverageData, assignments);
                testCase.setObjectMetaData("ObjectMetaData" + i,
                        testClassObject, 42l);
                testCase.setObjectMetaData("ObjectMetaData" + i % 2, ifBranch,
                        42);

                testCase.setMetaData("MetaData" + i, "testData");
                testCase.setMetaData("MetaData" + i % 4, "dataTest");
            }

            List<String> list = new Vector<String>();
            list.add("42");
            list.add("23");
            list.add("47");
            list.add("666");

            testSession.setMetaData("MetaData" + k, list);
        }
    }

    private BooleanOperator createShortCircuitOr(MASTBuilder database) {
        Vector<BooleanResult> results;
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();

        results = new Vector<BooleanResult>();
        results.addElement(BooleanResult.TRUE);
        results.addElement(BooleanResult.NOT_EVALUATED);

        possibleAssignments.put(new BooleanAssignment(results), Boolean.TRUE);

        results = new Vector<BooleanResult>();
        results.addElement(BooleanResult.FALSE);
        results.addElement(BooleanResult.FALSE);

        possibleAssignments.put(new BooleanAssignment(results), Boolean.FALSE);

        results = new Vector<BooleanResult>();
        results.addElement(BooleanResult.FALSE);
        results.addElement(BooleanResult.TRUE);

        possibleAssignments.put(new BooleanAssignment(results), Boolean.TRUE);

        BooleanOperator orOperator = database.createBooleanOperator(2,
                possibleAssignments, "shortCircuitOr");

        return orOperator;
    }

    private BooleanOperator createNotOperator(MASTBuilder database) {
        Vector<BooleanResult> results;
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();

        results = new Vector<BooleanResult>();
        results.addElement(BooleanResult.TRUE);

        possibleAssignments.put(new BooleanAssignment(results), Boolean.FALSE);

        results = new Vector<BooleanResult>();
        results.addElement(BooleanResult.FALSE);

        possibleAssignments.put(new BooleanAssignment(results), Boolean.TRUE);

        BooleanOperator orOperator = database.createBooleanOperator(1,
                possibleAssignments, "not");

        return orOperator;
    }

    private LocationList createLocationList(MASTBuilder database,
            SourceFile sourceFile, int start, int end) {
        Location location = database.createLocation(sourceFile, start, end);
        Vector<Location> locList = new Vector<Location>();
        locList.addElement(location);
        return database.createLocationList(locList);
    }

    /**
     * @throws java.lang.Exception
     */
    @Override
    protected void tearDown() throws Exception {
        // Do nothing.
    }

    /**
     * 
     */
    public void testDatabase() {
        new File("test").mkdir();

        for (TestSession session : this.testSessionContainer.getTestSessions()) {
            for (TestCase testCase : session.getTestCases()) {
                testCase.getAssignments(this.ifRootTerm);
            }
        }

        String filename = "test/databasetest.xml";
        try {
            this.testSessionContainer.save(filename);
        } catch (FileSaveException e) {
            this.logger.fatal("An error occured during saving", e);
        }

        MASTBuilder mastBuilder = new MASTBuilder(this.logger);
        TestSessionContainer newTestSessionContainer;
        try {
            newTestSessionContainer = TestSessionContainer.load(PluginManager
                    .create(), this.logger, mastBuilder,
                    "test/databasetest.xml");

            try {
                newTestSessionContainer.save("test/databasetestResaved.xml");

                // Test overriding the old file
                newTestSessionContainer.save();
            } catch (FileSaveException e) {
                this.logger.fatal("An error occured during saving", e);
            }
        } catch (FileLoadException e) {
            this.logger.fatal("An error occured during loading", e);
        }

    }

    /**
     * Check if the locations are correct
     */
    public void testLocations() {
        traverseHierarchyLevel(this.testSessionContainer.getCode());
    }

    private void traverseHierarchyLevel(HierarchyLevel hierarchyLevel) {
        printLocation(hierarchyLevel.getName(), hierarchyLevel.getLocation());
        printLocation(hierarchyLevel.getName(), hierarchyLevel.getHeader());

        for (StatementSequence sequence : hierarchyLevel.getSequences()) {
            traverseStatementSequence(sequence);
        }

        for (HierarchyLevel level : hierarchyLevel.getChildren()) {
            traverseHierarchyLevel(level);
        }
    }

    private void traverseStatementSequence(StatementSequence sequence) {
        printLocation("StatementSequence", sequence.getLocation());

        for (Statement statement : sequence.getStatements()) {
            traverseStatement(statement);
        }
    }

    private void traverseStatement(Statement statement) {
        for (RootTerm term : statement.getTerms())
            traverseRootTerm(term);

        if (statement instanceof BasicStatement) {
            traverseBasicStatement((BasicStatement) statement);
        } else if (statement instanceof ComplexStatement) {
            traverseComplexStatement((ComplexStatement) statement);
        }
    }

    private void traverseRootTerm(RootTerm term) {
        traverseBooleanTerm(term.getTerm());
    }

    private void traverseBooleanTerm(BooleanTerm term) {
        if (term instanceof BasicBooleanTerm) {
            BasicBooleanTerm basicBooleanTerm = (BasicBooleanTerm) term;
            printLocation("BasicBooleanTerm", basicBooleanTerm.getLocation());
        } else if (term instanceof OperatorTerm) {
            OperatorTerm operatorTerm = (OperatorTerm) term;
            printLocation("OperatorTerm", operatorTerm.getLocation());

            for (BooleanTerm booleanTerm : operatorTerm.getOperands()) {
                traverseBooleanTerm(booleanTerm);
            }
        }
    }

    private void traverseComplexStatement(ComplexStatement statement) {
        List<Location> list = new Vector<Location>();
        list.add(statement.getKeyword());
        printLocation("ComplexStatement", this.builder.createLocationList(list));
        if (statement instanceof ConditionalStatement) {
            traverseConditionalStatement((ConditionalStatement) statement);
        } else if (statement instanceof LoopingStatement) {
            traverseLoopingStatement((LoopingStatement) statement);
        }
    }

    private void traverseLoopingStatement(LoopingStatement statement) {
        printLocation("LoopingStatement", statement.getLocation());
        traverseStatementSequence(statement.getBody());
    }

    private void traverseConditionalStatement(ConditionalStatement statement) {
        printLocation("ConditionalStatement", statement.getLocation());

        for (Branch branch : statement.getBranches()) {
            printLocation("Branch", branch.getLocation());

            printLocation("Branch", branch.getDecision());

            traverseStatementSequence(branch.getSequence());
        }
    }

    private void traverseBasicStatement(BasicStatement statement) {
        printLocation("BasicStatement", statement.getLocation());
    }

    private void printLocation(String name, LocationList location) {
        String message = name + ":\n";

        for (Location loc : location.getLocations()) {
            message += loc.getContent() + " ";
        }

        while (message.endsWith(" ")) {
            message = message.substring(0, message.length() - 1);
        }

        // System.out.println(message);
    }

    /**
     * 
     * 
     */
    public void testIfRootTerm() {
        List<BooleanResult> list;

        BooleanAssignment[] assignments = new BooleanAssignment[3];

        list = new Vector<BooleanResult>();

        list.add(BooleanResult.FALSE);
        list.add(BooleanResult.TRUE);

        assignments[0] = new BooleanAssignment(list);

        list = new Vector<BooleanResult>();

        list.add(BooleanResult.FALSE);
        list.add(BooleanResult.FALSE);

        assignments[1] = new BooleanAssignment(list);

        list = new Vector<BooleanResult>();

        list.add(BooleanResult.TRUE);
        list.add(BooleanResult.NOT_EVALUATED);

        assignments[2] = new BooleanAssignment(list);

        assertEquals(this.ifRootTerm.getTerm().getBasicBooleanTerms(), 2);

        assertEquals(this.ifRootTerm.getAssignmentResult(assignments[0])
                .booleanValue(), true);
        assertEquals(this.ifRootTerm.getAssignmentResult(assignments[1])
                .booleanValue(), false);
        assertEquals(this.ifRootTerm.getAssignmentResult(assignments[2])
                .booleanValue(), true);

    }

    /**
     * 
     * 
     */
    public void testWhileRootTerm() {
        List<BooleanResult> list;

        BooleanAssignment[] assignments = new BooleanAssignment[3];

        list = new Vector<BooleanResult>();

        list.add(BooleanResult.TRUE);

        assignments[0] = new BooleanAssignment(list);

        list = new Vector<BooleanResult>();

        list.add(BooleanResult.FALSE);

        assignments[1] = new BooleanAssignment(list);

        list = new Vector<BooleanResult>();

        list.add(BooleanResult.NOT_EVALUATED);

        assignments[2] = new BooleanAssignment(list);

        assertEquals(this.whileRootTerm.getTerm().getBasicBooleanTerms(), 1);

        assertEquals(this.whileRootTerm.getAssignmentResult(assignments[0])
                .booleanValue(), false);
        assertEquals(this.whileRootTerm.getAssignmentResult(assignments[1])
                .booleanValue(), true);
        assertNull(this.whileRootTerm.getAssignmentResult(assignments[2]));
    }

    /**
     * @throws MergeException
     */
    public void testMerge() throws MergeException {
        Long preCount = 0L;
        Long postCount = 0L;

        for (TestSession testSession : this.testSessionContainer
                .getTestSessions()) {
            for (TestCase testCase : testSession.getTestCases()) {
                for (Entry<BooleanAssignment, Long> entry : testCase
                        .getAssignmentsCount(this.ifRootTerm).getData()
                        .entrySet()) {
                    if (entry.getKey().getResults().contains(
                            BooleanResult.NOT_EVALUATED)) {
                        preCount += entry.getValue();
                    }
                }
            }
            List<TestCase> testCases = testSession.getTestCases();
            this.testSessionContainer.mergeTestCases(testCases,
                    "Merged Test Case", "Comment");
            for (TestCase testCase : testCases) {
                testCase.delete();
            }
        }
        List<TestSession> sessions = this.testSessionContainer
                .getTestSessions();
        this.testSessionContainer.mergeTestSessions(sessions,
                "Merged Test Session", "Test Session Comment");
        for (TestSession session : sessions) {
            session.delete();
        }

        new File("test").mkdir();

        String filename = "test/databasetestMerge.xml";
        try {
            this.testSessionContainer.save(filename);
        } catch (FileSaveException e) {
            this.logger.fatal("An error occured during saving", e);
        }

        for (TestSession testSession : this.testSessionContainer
                .getTestSessions()) {
            for (TestCase testCase : testSession.getTestCases()) {
                for (Entry<BooleanAssignment, Long> entry : testCase
                        .getAssignmentsCount(this.ifRootTerm).getData()
                        .entrySet()) {
                    if (entry.getKey().getResults().contains(
                            BooleanResult.NOT_EVALUATED)) {
                        postCount += entry.getValue();
                    }
                }
            }
        }

        // System.out.println("preCount: " + preCount);
        // System.out.println("postCount: " + postCount);

        assertTrue(preCount.equals(postCount));
    }

    /**
     * 
     * 
     */
    public void testRootTermPos() {
        assertTrue(this.whileRootTerm.getPositionOfTerm(this.barTerm) == 0);
        assertTrue(this.whileRootTerm.getTermAtPosition(0) == this.barTerm);
        assertTrue(this.ifRootTerm.getPositionOfTerm(this.somethingTerm) == 0);
        assertTrue(this.ifRootTerm.getPositionOfTerm(this.nothingTerm) == 1);
        assertTrue(this.ifRootTerm.getTermAtPosition(0) == this.somethingTerm);
        assertTrue(this.ifRootTerm.getTermAtPosition(1) == this.nothingTerm);
    }

}
