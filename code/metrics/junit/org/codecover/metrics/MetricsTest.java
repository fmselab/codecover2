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

package org.codecover.metrics;

import java.util.*;

import org.codecover.metrics.coverage.BranchCoverage;
import org.codecover.metrics.coverage.CoverageMetric;
import org.codecover.metrics.coverage.CoverageResult;
import org.codecover.metrics.coverage.LoopCoverage;
import org.codecover.metrics.coverage.StatementCoverage;
import org.codecover.metrics.coverage.TermCoverage;
import org.codecover.model.*;
import org.codecover.model.mast.*;
import org.codecover.model.extensions.*;
import org.codecover.model.utils.SimpleLogger;
import org.codecover.model.utils.criteria.Criterion;

/**
 * Tests for metrics component
 * 
 * @author Tilmann Scheller
 * @version 1.0 ($Id: MetricsTest.java 71 2010-04-14 18:28:46Z schmidberger $)
 */
public class MetricsTest extends junit.framework.TestCase {
    
    protected PluginManager pluginManager = PluginManager.create();
    
    protected MASTBuilder builder;

    protected TestSessionContainer testSessionContainer;

    protected SimpleLogger logger;

    RootTerm ifTerm;

    RootTerm whileExpression;

    HierarchyLevel testClassObject;

    Branch ifBranch;

    int counter = 0;

    BasicStatement incrementJStatement;

    private BasicBooleanTerm somethingTerm;

    private BasicBooleanTerm nothingTerm;

    /**
     * @throws java.lang.Exception
     */
    @Override
    protected void setUp() throws Exception {

        this.logger = new SimpleLogger();

        this.builder = new MASTBuilder(logger);

        Vector<Statement> statementList;
        Set<RootTerm> terms;
        List<Branch> branches;
        Vector<BooleanTerm> operands;
        Vector<StatementSequence> sequenceList;
        Vector<HierarchyLevel> children;
        Vector<SourceFile> sourceFiles;

        SourceFile sourceFile = builder
                .createSourceFile(
                        "Example.java",
                        "public class TestClass {\r public static void main(String[] args) {\r    if (something || nothing) {\r      foo();\r    } else {\r      bar();\r    }\r    \r    while (!bar()) {\r      i++;\r      j++;\r    }\r  }\r}");

        // while
        LocationList incrementILocationList = this.createLocationList(builder,
                sourceFile, 176, 180);
        BasicStatement incrementIStatement = builder.createBasicStatement(
                incrementILocationList, builder.createCoverableItem("TestClass", ++counter
                                + ""), new HashSet<RootTerm>(), new HashSet<QuestionMarkOperator>());

        LocationList incrementJLocationList = this.createLocationList(builder,
                sourceFile, 187, 191);
        incrementJStatement = builder.createBasicStatement(
                incrementJLocationList, builder.createCoverableItem("TestClass", ++counter
                                + ""), new HashSet<RootTerm>(), new HashSet<QuestionMarkOperator>());

        statementList = new Vector<Statement>();
        statementList.addElement(incrementIStatement);
        statementList.addElement(incrementJStatement);

        LocationList whileBodyLocationList = this.createLocationList(builder,
                sourceFile, 176, 192);

        StatementSequence whileBody = builder.createStatementSequence(
                whileBodyLocationList, statementList);

        LocationList whileLocationList = this.createLocationList(builder,
                sourceFile, 153, 201);

        Location whileKeywordLocation = builder.createLocation(sourceFile, 153,
                158);

        LocationList barTermLocationList = this.createLocationList(builder,
                sourceFile, 161, 166);

        BasicBooleanTerm barTerm = builder
                .createBasicBooleanTerm(barTermLocationList);

        operands = new Vector<BooleanTerm>();
        operands.addElement(barTerm);

        BooleanOperator notOperator = createNotOperator(builder);

        LocationList notOperatorTermLocationList = this.createLocationList(
                builder, sourceFile, 160, 161);

        OperatorTerm notOperatorTerm = builder.createOperatorTerm(
                notOperatorTermLocationList, notOperator, operands);

        whileExpression = builder.createRootTerm(notOperatorTerm, builder
                .createCoverableItem("TestClass", ++counter + ""));

        terms = new HashSet<RootTerm>();
        terms.add(whileExpression);

        LoopingStatement whileStatement = builder.createLoopingStatement(
                whileLocationList, builder.createCoverableItem("TestClass",
                        ++counter + ""), terms, whileBody, whileKeywordLocation,
                builder.createCoverableItem("TestClass", ++counter + ""),
                builder.createCoverableItem("TestClass", ++counter + ""),
                builder.createCoverableItem("TestClass", ++counter + ""), true, new HashSet<QuestionMarkOperator>());
        // while

        // if
        // ifBranch
        LocationList fooStatementLocationList = this.createLocationList(
                builder, sourceFile, 105, 111);
        BasicStatement fooStatement = builder.createBasicStatement(
                fooStatementLocationList, builder.createCoverableItem("TestClass", ++counter
                                + ""), new HashSet<RootTerm>(), new HashSet<QuestionMarkOperator>());

        statementList = new Vector<Statement>();
        statementList.addElement(fooStatement);

        LocationList fooStatementSequenceLocationList = this
                .createLocationList(builder, sourceFile, 105, 111);
        StatementSequence fooStatementSequence = builder
                .createStatementSequence(fooStatementSequenceLocationList,
                        statementList);

        LocationList ifBranchLocationList = this.createLocationList(builder,
                sourceFile, 97, 117);
        LocationList ifBranchDecisionList = this.createLocationList(builder,
                sourceFile, 71, 73);
        ifBranch = builder.createBranch(ifBranchLocationList, builder
                .createCoverableItem("TestClass", ++counter + ""), false,
                ifBranchDecisionList, fooStatementSequence);
        // ifBranch

        // elseBranch
        LocationList barStatementLocationList = this.createLocationList(
                builder, sourceFile, 131, 137);
        BasicStatement barStatement = builder.createBasicStatement(
                barStatementLocationList, builder.createCoverableItem("TestClass", ++counter
                                + ""), new HashSet<RootTerm>(), new HashSet<QuestionMarkOperator>());

        statementList = new Vector<Statement>();
        statementList.addElement(barStatement);

        LocationList barStatementSequenceLocationList = this
                .createLocationList(builder, sourceFile, 131, 137);
        StatementSequence barStatementSequence = builder
                .createStatementSequence(barStatementSequenceLocationList,
                        statementList);

        LocationList elseBranchLocationList = this.createLocationList(builder,
                sourceFile, 123, 143);
        LocationList elseBranchDecisionList = this.createLocationList(builder,
                sourceFile, 118, 123);
        Branch elseBranch = builder.createBranch(elseBranchLocationList,
                builder.createCoverableItem("TestClass", ++counter + ""),
                false, elseBranchDecisionList, barStatementSequence);
        // elseBranch

        branches = new Vector<Branch>();
        branches.add(ifBranch);
        branches.add(elseBranch);

        LocationList somethingTermLocationList = this.createLocationList(
                builder, sourceFile, 75, 84);
        BasicBooleanTerm somethingTerm = builder
                .createBasicBooleanTerm(somethingTermLocationList);
        this.somethingTerm = somethingTerm;

        LocationList nothingTermLocationList = this.createLocationList(builder,
                sourceFile, 88, 95);
        BasicBooleanTerm nothingTerm = builder
                .createBasicBooleanTerm(nothingTermLocationList);
        this.nothingTerm = nothingTerm;

        operands = new Vector<BooleanTerm>();
        operands.addElement(somethingTerm);
        operands.addElement(nothingTerm);

        LocationList operatorTermLocationList = this.createLocationList(
                builder, sourceFile, 85, 87);

        BooleanOperator orOperator = createShortCircuitOr(builder);

        OperatorTerm operatorTerm = builder.createOperatorTerm(
                operatorTermLocationList, orOperator, operands);

        ifTerm = builder.createRootTerm(operatorTerm, builder
                .createCoverableItem("TestClass", ++counter + ""));

        terms = new HashSet<RootTerm>();
        terms.add(ifTerm);

        LocationList ifStatementLocationList = this.createLocationList(builder,
                sourceFile, 71, 143);

        Location ifStatementKeywordLocation = builder.createLocation(
                sourceFile, 71, 73);

        Set<QuestionMarkOperator> questionMarkOperators = new HashSet<QuestionMarkOperator>();
        
        ConditionalStatement ifStatement = builder.createConditionalStatement(
                ifStatementLocationList, builder.createCoverableItem("TestClass", ++counter
                                + ""), terms, branches,
                ifStatementKeywordLocation, questionMarkOperators);
        // if

        statementList = new Vector<Statement>();
        statementList.addElement(ifStatement);
        statementList.addElement(whileStatement);

        LocationList mainSequenceLocationList = this.createLocationList(
                builder, sourceFile, 71, 197);
        StatementSequence mainSequence = builder.createStatementSequence(
                mainSequenceLocationList, statementList);

        HierarchyLevelType methodType = builder.createHierarchyLevelType(
                "method", "method");

        sequenceList = new Vector<StatementSequence>();
        sequenceList.addElement(mainSequence);

        LocationList mainMethodLocationList = this.createLocationList(builder,
                sourceFile, 26, 202);
        LocationList mainMethodHeader = this.createLocationList(builder,
                sourceFile, 26, 64);
        HierarchyLevel mainMethod = builder.createHierarchyLevel(
                mainMethodLocationList, "main method", mainMethodHeader,
                methodType, new Vector<HierarchyLevel>(), sequenceList);

        HierarchyLevelType classType = builder.createHierarchyLevelType(
                "class", "class");

        LocationList testClassObjectLocationList = this.createLocationList(
                builder, sourceFile, 0, 203);
        LocationList testClassObjectHeader = this.createLocationList(builder,
                sourceFile, 0, 22);
        // testClassObjectHeader = database.createEmptyLocationList();

        children = new Vector<HierarchyLevel>();
        children.addElement(mainMethod);

        testClassObject = builder.createHierarchyLevel(
                testClassObjectLocationList, "Test Class",
                testClassObjectHeader, classType, children,
                new Vector<StatementSequence>());

        sourceFiles = new Vector<SourceFile>();
        sourceFiles.addElement(sourceFile);

        final Set<Criterion> criteria = new HashSet<Criterion>();
        criteria.add(org.codecover.model.utils.criteria.LoopCoverage
                .getInstance());
        criteria.add(org.codecover.model.utils.criteria.ConditionCoverage
                .getInstance());

        // Create the code base, with the class as top level mast element.
        testSessionContainer = new TestSessionContainer(testClassObject,
                logger, sourceFiles, criteria, UUID.randomUUID().toString(),
                new Date());
    }

    private LocationList createLocationList(MASTBuilder database,
            SourceFile sourceFile, int start, int end) {
        Location location = database.createLocation(sourceFile, start, end);
        Vector<Location> locList = new Vector<Location>();
        locList.addElement(location);
        return database.createLocationList(locList);
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

    private BooleanOperator createOr(MASTBuilder database) {
        Vector<BooleanResult> results;
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();

        results = new Vector<BooleanResult>();
        results.addElement(BooleanResult.TRUE);
        results.addElement(BooleanResult.FALSE);

        possibleAssignments.put(new BooleanAssignment(results), Boolean.TRUE);

        results = new Vector<BooleanResult>();
        results.addElement(BooleanResult.TRUE);
        results.addElement(BooleanResult.TRUE);

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
                possibleAssignments, "Or");

        return orOperator;
    }

    /**
     * 
     * 
     */
    public void testAllMetricsNoCoverage() {
        // create test sessions with test cases and no coverage
        for (int k = 0; k < 3; k++) {

            TestSession testSession = testSessionContainer.createTestSession(
                    "TestSession" + k, "42", new Date());

            for (int i = 0; i < 4; i++) {
                Map<CoverableItem, Long> coverageData = new HashMap<CoverableItem, Long>();

                for (CoverableItem coverableItem : testSessionContainer
                        .getCoverableItems()) {
                    coverageData.put(coverableItem, 0L);
                }

                Map<CoverableItem, BooleanAssignmentMap> assignments = new HashMap<CoverableItem, BooleanAssignmentMap>();
                Map<BooleanAssignment, Long> subMap = new HashMap<BooleanAssignment, Long>();

                // set assignments of if condition "if (something || nothing)"

                // end assignment of if condition
                assignments.put(ifTerm.getCoverableItem(),
                        new BooleanAssignmentMap(2, subMap));

                // set assignments of while condition "while (bar())"
                subMap = new HashMap<BooleanAssignment, Long>();

                assignments.put(whileExpression.getCoverableItem(),
                        new BooleanAssignmentMap(1, subMap));
                // end assignment of while condition

                testSession.createTestCase("TestCase" + i, "42", new Date(),
                        coverageData, assignments);
            }
        }

        // check whether all coverage metrics return 0 coverage
        for (Metric metric : MetricProvider.getAvailabeMetrics(pluginManager, logger)) {
            if (metric instanceof CoverageMetric) {
                for (TestSession testSession : testSessionContainer
                        .getTestSessions()) {
                    CoverageResult result;
                    result = ((CoverageMetric) metric).getCoverage(testSession
                            .getTestCases(), testSessionContainer.getCode());
                    assertTrue(result.getCoveredItems() == 0);
                }
            }
        }

    }

    /**
     * 
     * 
     */
    public void testAllMetricsFullCoverage() {
        // create test sessions with test cases and full coverage
        for (int k = 0; k < 3; k++) {

            TestSession testSession = testSessionContainer.createTestSession(
                    "TestSession" + k, "42", new Date());

            for (int i = 0; i < 4; i++) {
                Map<CoverableItem, Long> coverageData = new HashMap<CoverableItem, Long>();

                for (CoverableItem coverableItem : testSessionContainer
                        .getCoverableItems()) {
                    coverageData.put(coverableItem, 1L);
                }

                Map<CoverableItem, BooleanAssignmentMap> assignments = new HashMap<CoverableItem, BooleanAssignmentMap>();
                Map<BooleanAssignment, Long> subMap = new HashMap<BooleanAssignment, Long>();

                List<BooleanResult> resultList;
                BooleanAssignment booleanAssignment;

                // set assignments of if condition "if (something || nothing)"
                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.TRUE);
                resultList.add(BooleanResult.NOT_EVALUATED);
                booleanAssignment = new BooleanAssignment(resultList);
                subMap.put(booleanAssignment, 1l);

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.FALSE);
                resultList.add(BooleanResult.FALSE);
                booleanAssignment = new BooleanAssignment(resultList);
                subMap.put(booleanAssignment, 1l);

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.FALSE);
                resultList.add(BooleanResult.TRUE);
                booleanAssignment = new BooleanAssignment(resultList);
                subMap.put(booleanAssignment, 1l);

                assignments.put(ifTerm.getCoverableItem(),
                        new BooleanAssignmentMap(2, subMap));
                // end assignment of if condition

                // set assignments of while condition "while (bar())"
                subMap = new HashMap<BooleanAssignment, Long>();

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.TRUE);
                booleanAssignment = new BooleanAssignment(resultList);

                subMap.put(booleanAssignment, 1l);

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.FALSE);
                booleanAssignment = new BooleanAssignment(resultList);

                subMap.put(booleanAssignment, 1l);

                assignments.put(whileExpression.getCoverableItem(),
                        new BooleanAssignmentMap(1, subMap));
                // end assignment of while condition

                testSession.createTestCase("TestCase" + i, "42", new Date(),
                        coverageData, assignments);
            }
        }

        // check whether all coverage metrics return full coverage
        for (Metric metric : MetricProvider.getAvailabeMetrics(pluginManager, logger)) {
            if (metric instanceof CoverageMetric) {
                for (TestSession testSession : testSessionContainer
                        .getTestSessions()) {
                    CoverageResult result;
                    result = ((CoverageMetric) metric).getCoverage(testSession
                            .getTestCases(), testSessionContainer.getCode());
                    assertTrue(result.getCoveredItems() == result
                            .getTotalItems());
                }
            }
        }

    }

    /**
     * 
     * 
     */
    public void testStrictConditionCoverage() {
        // create test sessions with test cases and full coverage

        // same like the example MAST except that the if has 3 basic boolean
        // terms
        this.logger = new SimpleLogger();

        Vector<Statement> statementList;
        Set<RootTerm> terms;
        List<Branch> branches;
        Vector<BooleanTerm> operands;
        Vector<StatementSequence> sequenceList;
        Vector<HierarchyLevel> children;
        Vector<SourceFile> sourceFiles;
        int counter = 0;

        SourceFile sourceFile = builder
                .createSourceFile(
                        "Example.java",
                        "public class TestClass {\r public static void main(String[] args) {\r    if (something || nothing) {\r      foo();\r    } else {\r      bar();\r    }\r    \r    while (!bar()) {\r      i++;\r      j++;\r    }\r  }\r}");

        // while
        LocationList incrementILocationList = this.createLocationList(builder,
                sourceFile, 176, 180);
        BasicStatement incrementIStatement = builder.createBasicStatement(
                incrementILocationList, builder.createCoverableItem("TestClass", ++counter
                                + ""), new HashSet<RootTerm>(), new HashSet<QuestionMarkOperator>());

        LocationList incrementJLocationList = this.createLocationList(builder,
                sourceFile, 187, 191);
        BasicStatement incrementJStatement = builder.createBasicStatement(
                incrementJLocationList, builder.createCoverableItem("TestClass", ++counter
                                + ""), new HashSet<RootTerm>(), new HashSet<QuestionMarkOperator>());

        statementList = new Vector<Statement>();
        statementList.addElement(incrementIStatement);
        statementList.addElement(incrementJStatement);

        LocationList whileBodyLocationList = this.createLocationList(builder,
                sourceFile, 176, 192);

        StatementSequence whileBody = builder.createStatementSequence(
                whileBodyLocationList, statementList);

        LocationList whileLocationList = this.createLocationList(builder,
                sourceFile, 153, 201);

        Location whileKeywordLocation = builder.createLocation(sourceFile, 153,
                158);

        LocationList barTermLocationList = this.createLocationList(builder,
                sourceFile, 161, 166);

        BasicBooleanTerm barTerm = builder
                .createBasicBooleanTerm(barTermLocationList);

        operands = new Vector<BooleanTerm>();
        operands.addElement(barTerm);

        BooleanOperator notOperator = createNotOperator(builder);

        LocationList notOperatorTermLocationList = this.createLocationList(
                builder, sourceFile, 160, 161);

        OperatorTerm notOperatorTerm = builder.createOperatorTerm(
                notOperatorTermLocationList, notOperator, operands);

        whileExpression = builder.createRootTerm(notOperatorTerm, builder
                .createCoverableItem("TestClass", ++counter + ""));

        terms = new HashSet<RootTerm>();
        terms.add(whileExpression);

        LoopingStatement whileStatement = builder.createLoopingStatement(
                whileLocationList, builder.createCoverableItem("TestClass",
                        ++counter + ""), terms, whileBody, whileKeywordLocation,
                builder.createCoverableItem("TestClass", ++counter + ""),
                builder.createCoverableItem("TestClass", ++counter + ""),
                builder.createCoverableItem("TestClass", ++counter + ""), true, new HashSet<QuestionMarkOperator>());
        // while

        // if
        // ifBranch
        LocationList fooStatementLocationList = this.createLocationList(
                builder, sourceFile, 105, 111);
        BasicStatement fooStatement = builder.createBasicStatement(
                fooStatementLocationList, builder.createCoverableItem("TestClass", ++counter
                                + ""), new HashSet<RootTerm>(), new HashSet<QuestionMarkOperator>());

        statementList = new Vector<Statement>();
        statementList.addElement(fooStatement);

        LocationList fooStatementSequenceLocationList = this
                .createLocationList(builder, sourceFile, 105, 111);
        StatementSequence fooStatementSequence = builder
                .createStatementSequence(fooStatementSequenceLocationList,
                        statementList);

        LocationList ifBranchLocationList = this.createLocationList(builder,
                sourceFile, 97, 117);
        LocationList ifBranchDecisionList = this.createLocationList(builder,
                sourceFile, 71, 73);
        Branch ifBranch = builder.createBranch(ifBranchLocationList, builder
                .createCoverableItem("TestClass", ++counter + ""), false,
                ifBranchDecisionList, fooStatementSequence);
        // ifBranch

        // elseBranch
        LocationList barStatementLocationList = this.createLocationList(
                builder, sourceFile, 131, 137);
        BasicStatement barStatement = builder.createBasicStatement(
                barStatementLocationList, builder.createCoverableItem("TestClass", ++counter
                                + ""), new HashSet<RootTerm>(), new HashSet<QuestionMarkOperator>());

        statementList = new Vector<Statement>();
        statementList.addElement(barStatement);

        LocationList barStatementSequenceLocationList = this
                .createLocationList(builder, sourceFile, 131, 137);
        StatementSequence barStatementSequence = builder
                .createStatementSequence(barStatementSequenceLocationList,
                        statementList);

        LocationList elseBranchLocationList = this.createLocationList(builder,
                sourceFile, 123, 143);
        LocationList elseBranchDecisionList = this.createLocationList(builder,
                sourceFile, 118, 123);
        Branch elseBranch = builder.createBranch(elseBranchLocationList,
                builder.createCoverableItem("TestClass", ++counter + ""),
                false, elseBranchDecisionList, barStatementSequence);
        // elseBranch

        branches = new Vector<Branch>();
        branches.add(ifBranch);
        branches.add(elseBranch);

        LocationList somethingTermLocationList = this.createLocationList(
                builder, sourceFile, 75, 84);
        BasicBooleanTerm somethingTerm = builder
                .createBasicBooleanTerm(somethingTermLocationList);

        LocationList nothingTermLocationList = this.createLocationList(builder,
                sourceFile, 88, 95);
        BasicBooleanTerm nothingTerm = builder
                .createBasicBooleanTerm(nothingTermLocationList);

        LocationList op3TermLocationList = this.createLocationList(builder,
                sourceFile, 88, 95);
        BasicBooleanTerm op3Term = builder
                .createBasicBooleanTerm(op3TermLocationList);

        operands = new Vector<BooleanTerm>();
        operands.addElement(somethingTerm);
        operands.addElement(nothingTerm);

        LocationList operatorTermLocationList = this.createLocationList(
                builder, sourceFile, 85, 87);

        BooleanOperator orOperator = createOr(builder);

        OperatorTerm operatorTerm = builder.createOperatorTerm(
                operatorTermLocationList, orOperator, operands);

        operands = new Vector<BooleanTerm>();
        operands.addElement(operatorTerm);
        operands.addElement(op3Term);

        LocationList operator2TermLocationList = this.createLocationList(
                builder, sourceFile, 85, 87);
        OperatorTerm operator2Term = builder.createOperatorTerm(
                operator2TermLocationList, orOperator, operands);

        ifTerm = builder.createRootTerm(operator2Term, builder
                .createCoverableItem("TestClass", ++counter + ""));

        terms = new HashSet<RootTerm>();
        terms.add(ifTerm);

        LocationList ifStatementLocationList = this.createLocationList(builder,
                sourceFile, 71, 143);

        Location ifStatementKeywordLocation = builder.createLocation(
                sourceFile, 71, 73);

        Set<QuestionMarkOperator> questionMarkOperators = new HashSet<QuestionMarkOperator>();
        
        ConditionalStatement ifStatement = builder.createConditionalStatement(
                ifStatementLocationList, builder.createCoverableItem("TestClass", ++counter
                                + ""), terms, branches,
                ifStatementKeywordLocation, questionMarkOperators);
        // if

        statementList = new Vector<Statement>();
        statementList.addElement(ifStatement);
        statementList.addElement(whileStatement);

        LocationList mainSequenceLocationList = this.createLocationList(
                builder, sourceFile, 71, 197);
        StatementSequence mainSequence = builder.createStatementSequence(
                mainSequenceLocationList, statementList);

        HierarchyLevelType methodType = builder.createHierarchyLevelType(
                "method", "method");

        sequenceList = new Vector<StatementSequence>();
        sequenceList.addElement(mainSequence);

        LocationList mainMethodLocationList = this.createLocationList(builder,
                sourceFile, 26, 202);
        LocationList mainMethodHeader = this.createLocationList(builder,
                sourceFile, 26, 64);
        HierarchyLevel mainMethod = builder.createHierarchyLevel(
                mainMethodLocationList, "main method", mainMethodHeader,
                methodType, new Vector<HierarchyLevel>(), sequenceList);

        HierarchyLevelType classType = builder.createHierarchyLevelType(
                "class", "class");

        LocationList testClassObjectLocationList = this.createLocationList(
                builder, sourceFile, 0, 203);
        LocationList testClassObjectHeader = this.createLocationList(builder,
                sourceFile, 0, 22);
        // testClassObjectHeader = database.createEmptyLocationList();

        children = new Vector<HierarchyLevel>();
        children.addElement(mainMethod);

        testClassObject = builder.createHierarchyLevel(
                testClassObjectLocationList, "Test Class",
                testClassObjectHeader, classType, children,
                new Vector<StatementSequence>());

        sourceFiles = new Vector<SourceFile>();
        sourceFiles.addElement(sourceFile);

        final Set<Criterion> criteria = new HashSet<Criterion>();
        criteria.add(org.codecover.model.utils.criteria.LoopCoverage
                .getInstance());
        criteria.add(org.codecover.model.utils.criteria.ConditionCoverage
                .getInstance());

        // Create the code base, with the class as top level mast element.
        testSessionContainer = new TestSessionContainer(testClassObject,
                logger, sourceFiles, criteria,

                UUID.randomUUID().toString(), new Date());

        // create test sessions with test cases and full coverage
        for (int k = 0; k < 3; k++) {

            TestSession testSession = testSessionContainer.createTestSession(
                    "TestSession" + k, "42", new Date());

            for (int i = 0; i < 4; i++) {
                Map<CoverableItem, Long> coverageData = new HashMap<CoverableItem, Long>();

                for (CoverableItem coverableItem : testSessionContainer
                        .getCoverableItems()) {
                    coverageData.put(coverableItem, 1L);
                }

                Map<CoverableItem, BooleanAssignmentMap> assignments = new HashMap<CoverableItem, BooleanAssignmentMap>();
                Map<BooleanAssignment, Long> subMap = new HashMap<BooleanAssignment, Long>();

                List<BooleanResult> resultList;
                BooleanAssignment booleanAssignment;

                // set assignments of if condition "if (something || nothing)"
                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.FALSE);
                resultList.add(BooleanResult.FALSE);
                resultList.add(BooleanResult.FALSE);
                booleanAssignment = new BooleanAssignment(resultList);
                subMap.put(booleanAssignment, 1l);

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.TRUE);
                resultList.add(BooleanResult.FALSE);
                resultList.add(BooleanResult.FALSE);
                booleanAssignment = new BooleanAssignment(resultList);
                subMap.put(booleanAssignment, 1l);

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.FALSE);
                resultList.add(BooleanResult.TRUE);
                resultList.add(BooleanResult.FALSE);
                booleanAssignment = new BooleanAssignment(resultList);
                subMap.put(booleanAssignment, 1l);

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.FALSE);
                resultList.add(BooleanResult.FALSE);
                resultList.add(BooleanResult.TRUE);
                booleanAssignment = new BooleanAssignment(resultList);
                subMap.put(booleanAssignment, 1l);

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.FALSE);
                resultList.add(BooleanResult.TRUE);
                resultList.add(BooleanResult.TRUE);
                booleanAssignment = new BooleanAssignment(resultList);
                subMap.put(booleanAssignment, 1l);

                assignments.put(ifTerm.getCoverableItem(),
                        new BooleanAssignmentMap(3, subMap));
                // end assignment of if condition

                // set assignments of while condition "while (bar())"
                subMap = new HashMap<BooleanAssignment, Long>();

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.TRUE);
                booleanAssignment = new BooleanAssignment(resultList);

                subMap.put(booleanAssignment, 1l);

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.FALSE);
                booleanAssignment = new BooleanAssignment(resultList);

                subMap.put(booleanAssignment, 1l);

                assignments.put(whileExpression.getCoverableItem(),
                        new BooleanAssignmentMap(1, subMap));
                // end assignment of while condition

                testSession.createTestCase("TestCase" + i, "42", new Date(),
                        coverageData, assignments);
            }
        }

        // check whether all coverage metrics return full coverage
        for (Metric metric : MetricProvider.getAvailabeMetrics(pluginManager, logger)) {
            if (metric instanceof CoverageMetric) {
                for (TestSession testSession : testSessionContainer
                        .getTestSessions()) {
                    CoverageResult result;
                    result = ((CoverageMetric) metric).getCoverage(testSession
                            .getTestCases(), testSessionContainer.getCode());
                    assertTrue(result.getCoveredItems() == result
                            .getTotalItems());
                }
            }
        }

        TermCoverage metric = TermCoverage.getInstance();

        for (TestSession testSession : testSessionContainer.getTestSessions()) {
        	CoverageResult result;
            result = metric.getCoverage(testSession.getTestCases(), ifTerm);
            // assertTrue(result.size() >= 2);
            result = metric.getCoverage(testSession.getTestCases(), ifTerm);
            // assertTrue(result.size() >= 2);
            result = metric.getCoverage(testSession.getTestCases(), ifTerm);
            // assertTrue(result.size() >= 2);
        }

    }

    /**
     * 
     * 
     */
    public void testCoverageResult() {
        boolean hit = false;
        try {
            new CoverageResult(-1, -1);
        } catch (IllegalArgumentException e) {
            hit = true;
        }
        assertTrue(hit);
    }

    /**
     * 
     * 
     */
    public void testMetricsProvider() {
        Set<Criterion> criteria = new HashSet<Criterion>();
        criteria.add(org.codecover.model.utils.criteria.StatementCoverage
                .getInstance());
        Set<Metric> result;
        result = MetricProvider.getAvailabeMetrics(pluginManager, logger, criteria);
        assertTrue(result.size() >= 1);

        Set<Metric> metrics = MetricProvider.getAvailabeMetrics(pluginManager, logger);

        assertTrue(metrics.contains(StatementCoverage.getInstance()));
        assertTrue(metrics.contains(BranchCoverage.getInstance()));
        assertTrue(metrics.contains(LoopCoverage.getInstance()));
        assertTrue(metrics.contains(TermCoverage.getInstance()));

        //TODO: this is false: assertTrue(metrics.size() == 4);
    }

    /**
     * 
     * 
     */
    public void testCaching() {
        // create one test session with one test case and full coverage

        TestSession testSession = testSessionContainer.createTestSession(
                "TestSession", "42", new Date());

        Map<CoverableItem, Long> coverageData = new HashMap<CoverableItem, Long>();

        for (CoverableItem coverableItem : testSessionContainer
                .getCoverableItems()) {
            coverageData.put(coverableItem, 1L);
        }

        Map<CoverableItem, BooleanAssignmentMap> assignments = new HashMap<CoverableItem, BooleanAssignmentMap>();
        Map<BooleanAssignment, Long> subMap = new HashMap<BooleanAssignment, Long>();

        List<BooleanResult> resultList;
        BooleanAssignment booleanAssignment;

        // set assignments of if condition "if (something || nothing)"
        resultList = new Vector<BooleanResult>();
        resultList.add(BooleanResult.TRUE);
        resultList.add(BooleanResult.NOT_EVALUATED);
        booleanAssignment = new BooleanAssignment(resultList);
        subMap.put(booleanAssignment, 1l);

        resultList = new Vector<BooleanResult>();
        resultList.add(BooleanResult.FALSE);
        resultList.add(BooleanResult.FALSE);
        booleanAssignment = new BooleanAssignment(resultList);
        subMap.put(booleanAssignment, 1l);

        resultList = new Vector<BooleanResult>();
        resultList.add(BooleanResult.FALSE);
        resultList.add(BooleanResult.TRUE);
        booleanAssignment = new BooleanAssignment(resultList);
        subMap.put(booleanAssignment, 1l);

        assignments.put(ifTerm.getCoverableItem(), new BooleanAssignmentMap(2,
                subMap));
        // end assignment of if condition

        // set assignments of while condition "while (bar())"
        subMap = new HashMap<BooleanAssignment, Long>();

        resultList = new Vector<BooleanResult>();
        resultList.add(BooleanResult.TRUE);
        booleanAssignment = new BooleanAssignment(resultList);

        subMap.put(booleanAssignment, 1l);

        resultList = new Vector<BooleanResult>();
        resultList.add(BooleanResult.FALSE);
        booleanAssignment = new BooleanAssignment(resultList);

        subMap.put(booleanAssignment, 1l);

        assignments.put(whileExpression.getCoverableItem(),
                new BooleanAssignmentMap(1, subMap));
        // end assignment of while condition

        testSession.createTestCase("TestCase", "42", new Date(), coverageData,
                assignments);

        // check whether all coverage metrics return full coverage
        for (Metric metric : MetricProvider.getAvailabeMetrics(pluginManager, logger)) {
            if (metric instanceof CoverageMetric) {
                for (TestSession currSession : testSessionContainer
                        .getTestSessions()) {
                    CoverageResult result;
                    result = ((CoverageMetric) metric).getCoverage(currSession
                            .getTestCases(), testSessionContainer.getCode());
                    assertTrue(result.getCoveredItems() == result
                            .getTotalItems());
                }
            }
        }
        // now get the values directly from the cache
        // check whether all coverage metrics return full coverage
        for (Metric metric : MetricProvider.getAvailabeMetrics(pluginManager, logger)) {
            if (metric instanceof CoverageMetric) {
                for (TestSession currSession : testSessionContainer
                        .getTestSessions()) {
                    CoverageResult result;
                    result = ((CoverageMetric) metric).getCoverage(currSession
                            .getTestCases(), testSessionContainer.getCode());
                    assertTrue(result.getCoveredItems() == result
                            .getTotalItems());
                }
            }
        }
    }

    /**
     * 
     * 
     */
    public void testSetterGetter() {
        // no need to test setter and getter
        // this test case only ensures that setters and getters
        // are covered
        for (Metric metric : MetricProvider.getAvailabeMetrics(pluginManager, logger)) {
            metric.getName();
            metric.getDescription();
        }
    }

    /**
     * 
     * 
     */
    public void testPartialStrictConditionCoverage() {
        // create one test session with one test case and partial condition
        // coverage

        TestSession testSession = testSessionContainer.createTestSession(
                "TestSession", "42", new Date());

        Map<CoverableItem, Long> coverageData = new HashMap<CoverableItem, Long>();

        for (CoverableItem coverableItem : testSessionContainer
                .getCoverableItems()) {
            coverageData.put(coverableItem, 1L);
        }

        Map<CoverableItem, BooleanAssignmentMap> assignments = new HashMap<CoverableItem, BooleanAssignmentMap>();
        Map<BooleanAssignment, Long> subMap = new HashMap<BooleanAssignment, Long>();

        List<BooleanResult> resultList;
        BooleanAssignment booleanAssignment;

        // set assignments of if condition "if (something || nothing)"
        resultList = new Vector<BooleanResult>();
        resultList.add(BooleanResult.TRUE);
        resultList.add(BooleanResult.NOT_EVALUATED);
        booleanAssignment = new BooleanAssignment(resultList);
        subMap.put(booleanAssignment, 1l);

        resultList = new Vector<BooleanResult>();
        resultList.add(BooleanResult.FALSE);
        resultList.add(BooleanResult.FALSE);
        booleanAssignment = new BooleanAssignment(resultList);
        subMap.put(booleanAssignment, 1l);

        assignments.put(ifTerm.getCoverableItem(), new BooleanAssignmentMap(2,
                subMap));
        // end assignment of if condition

        // set assignments of while condition "while (bar())"
        subMap = new HashMap<BooleanAssignment, Long>();

        resultList = new Vector<BooleanResult>();
        resultList.add(BooleanResult.TRUE);
        booleanAssignment = new BooleanAssignment(resultList);

        subMap.put(booleanAssignment, 1l);

        resultList = new Vector<BooleanResult>();
        resultList.add(BooleanResult.FALSE);
        booleanAssignment = new BooleanAssignment(resultList);

        subMap.put(booleanAssignment, 1l);

        assignments.put(whileExpression.getCoverableItem(),
                new BooleanAssignmentMap(1, subMap));
        // end assignment of while condition

        testSession.createTestCase("TestCase", "42", new Date(), coverageData,
                assignments);

        // check whether all coverage metrics return full coverage
        for (Metric metric : MetricProvider.getAvailabeMetrics(pluginManager, logger)) {
            if (metric instanceof CoverageMetric) {
                for (TestSession currSession : testSessionContainer
                        .getTestSessions()) {
                    CoverageResult result;
                    result = ((CoverageMetric) metric).getCoverage(currSession
                            .getTestCases(), testSessionContainer.getCode());
                    if (metric instanceof TermCoverage) {
                        assertTrue(result.getCoveredItems() == 2);
                        assertTrue(result.getTotalItems() == 3);
                    } else {
                        assertTrue(result.getCoveredItems() == result
                                .getTotalItems());
                    }
                }
            }
        }

        TermCoverage metric = TermCoverage.getInstance();

        for (TestSession currSession : testSessionContainer.getTestSessions()) {
        	CoverageResult result;
            result = metric.getCoverage(currSession.getTestCases(), ifTerm);
            // assertTrue(result.size() >= 2);
                    
            result = metric.getCoverage(currSession.getTestCases(), ifTerm);
            // assertTrue(result.size() == 0);
        }

    }
}
