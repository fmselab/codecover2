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

package org.codecover;

import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.BooleanAssignment;
import org.codecover.model.mast.BooleanOperator;
import org.codecover.model.mast.BooleanResult;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.LocationList;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.utils.SimpleLogger;

/**
 * JUnit Testcase of Database class (taken from org.codecover.model.DatabaseTest
 * r907)
 * 
 * @author Johannes Langauf
 * @version 1.0 ($Id: DatabaseExample.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DatabaseExample {
    private TestSessionContainer testSessionContainer;

    protected MASTBuilder builder;

    protected SimpleLogger logger;

    private RootTerm ifRootTerm;

    private RootTerm whileRootTerm;

    /**
     * Constructor
     */
    public DatabaseExample() {
        setUp();
    }

    /**
     * @return the database
     */
    public TestSessionContainer getTestSessionContainer() {
        return this.testSessionContainer;
    }

    /**
     * @return the ifRootTerm
     */
    public RootTerm getIfRootTerm() {
        return this.ifRootTerm;
    }

    /**
     * @return the logger
     */
    public SimpleLogger getLogger() {
        return this.logger;
    }

    /**
     * @return the whileRootTerm
     */
    public RootTerm getWhileRootTerm() {
        return this.whileRootTerm;
    }

    /**
     * @author Markus Wittlinger
     */
    private void setUp() {

        /* 
        
        this.logger = new SimpleLogger();

        this.builder = new MASTBuilder(logger);

        Vector<Statement> statementList;
        Set<RootTerm> terms;
        List<Branch> branches;
        Vector<BooleanTerm> operands;
        Vector<StatementSequence> sequenceList;
        Vector<HierarchyLevel> children;
        Vector<SourceFile> sourceFiles;
        int counter = 0;

        SourceFile sourceFile = builder.createSourceFile("Example.java",
                                                         "public class TestClass {\r public static void main(String[] args) {\r    if (something || nothing) {\r      foo();\r    } else {\r      bar();\r    }\r    \r    while (!bar()) {\r      i++;\r      j++;\r    }\r  }\r}");

        // while
        LocationList incrementILocationList = this.createLocationList(builder,
                                                                      sourceFile,
                                                                      176,
                                                                      180);
        BasicStatement incrementIStatement = builder.createBasicStatement(incrementILocationList,
                                                                          builder.createCoverableItem(++counter
                                                                                  + ""),
                                                                          new HashSet<RootTerm>());

        LocationList incrementJLocationList = this.createLocationList(builder,
                                                                      sourceFile,
                                                                      187,
                                                                      191);
        BasicStatement incrementJStatement = builder.createBasicStatement(incrementJLocationList,
                                                                          builder.createCoverableItem(++counter
                                                                                  + ""),
                                                                          new HashSet<RootTerm>());

        statementList = new Vector<Statement>();
        statementList.addElement(incrementIStatement);
        statementList.addElement(incrementJStatement);

        LocationList whileBodyLocationList = this.createLocationList(builder,
                                                                     sourceFile,
                                                                     176,
                                                                     192);

        StatementSequence whileBody = builder.createStatementSequence(whileBodyLocationList,
                                                                      statementList);

        LocationList whileLocationList = this.createLocationList(builder,
                                                                 sourceFile,
                                                                 153,
                                                                 201);

        Location whileKeywordLocation = builder.createLocation(sourceFile,
                                                               153,
                                                               158);

        LocationList barTermLocationList = this.createLocationList(builder,
                                                                   sourceFile,
                                                                   161,
                                                                   166);

        BasicBooleanTerm barTerm = builder.createBasicBooleanTerm(barTermLocationList);

        operands = new Vector<BooleanTerm>();
        operands.addElement(barTerm);

        BooleanOperator notOperator = createNotOperator(builder);

        LocationList notOperatorTermLocationList = this.createLocationList(builder,
                                                                           sourceFile,
                                                                           160,
                                                                           161);

        OperatorTerm notOperatorTerm = builder.createOperatorTerm(notOperatorTermLocationList,
                                                                  notOperator,
                                                                  operands);

        RootTerm whileExpression = builder.createRootTerm(notOperatorTerm,
                                                          builder.createCoverableItem(++counter
                                                                  + ""));

        // ////////////////////////////////////////////////////// //
        // save the whileExpression in member variable for testing purpose
        this.whileRootTerm = whileExpression;
        // ////////////////////////////////////////////////////// //

        terms = new HashSet<RootTerm>();
        terms.add(whileExpression);

        LoopingStatement whileStatement = builder.createLoopingStatement(whileLocationList,
                                                                         builder.createCoverableItem(++counter
                                                                                 + ""),
                                                                         terms,
                                                                         whileBody,
                                                                         whileKeywordLocation,
                                                                         builder.createCoverableItem(++counter
                                                                                 + ""),
                                                                         builder.createCoverableItem(++counter
                                                                                 + ""),
                                                                         builder.createCoverableItem(++counter
                                                                                 + ""),
                                                                         true);
        // while

        // if
        // ifBranch
        LocationList fooStatementLocationList = this.createLocationList(builder,
                                                                        sourceFile,
                                                                        105,
                                                                        111);
        BasicStatement fooStatement = builder.createBasicStatement(fooStatementLocationList,
                                                                   builder.createCoverableItem(++counter
                                                                           + ""),
                                                                   new HashSet<RootTerm>());

        statementList = new Vector<Statement>();
        statementList.addElement(fooStatement);

        LocationList fooStatementSequenceLocationList = this.createLocationList(builder,
                                                                                sourceFile,
                                                                                105,
                                                                                111);
        StatementSequence fooStatementSequence = builder.createStatementSequence(fooStatementSequenceLocationList,
                                                                                 statementList);

        LocationList ifBranchLocationList = this.createLocationList(builder,
                                                                    sourceFile,
                                                                    97,
                                                                    117);
        Branch ifBranch = builder.createBranch(ifBranchLocationList,
                                               builder.createCoverableItem(++counter
                                                       + ""),
                                               false,
                                               builder.createLocation(sourceFile,
                                                                      71,
                                                                      73),
                                               fooStatementSequence);
        // ifBranch

        // elseBranch
        LocationList barStatementLocationList = this.createLocationList(builder,
                                                                        sourceFile,
                                                                        131,
                                                                        137);
        BasicStatement barStatement = builder.createBasicStatement(barStatementLocationList,
                                                                   builder.createCoverableItem(++counter
                                                                           + ""),
                                                                   new HashSet<RootTerm>());

        statementList = new Vector<Statement>();
        statementList.addElement(barStatement);

        LocationList barStatementSequenceLocationList = this.createLocationList(builder,
                                                                                sourceFile,
                                                                                131,
                                                                                137);
        StatementSequence barStatementSequence = builder.createStatementSequence(barStatementSequenceLocationList,
                                                                                 statementList);

        LocationList elseBranchLocationList = this.createLocationList(builder,
                                                                      sourceFile,
                                                                      123,
                                                                      143);
        Branch elseBranch = builder.createBranch(elseBranchLocationList,
                                                 builder.createCoverableItem(++counter
                                                         + ""),
                                                 false,
                                                 builder.createLocation(sourceFile,
                                                                        118,
                                                                        123),
                                                 barStatementSequence);
        // elseBranch

        branches = new Vector<Branch>();
        branches.add(ifBranch);
        branches.add(elseBranch);

        LocationList somethingTermLocationList = this.createLocationList(builder,
                                                                         sourceFile,
                                                                         75,
                                                                         84);
        BasicBooleanTerm somethingTerm = builder.createBasicBooleanTerm(somethingTermLocationList);

        LocationList nothingTermLocationList = this.createLocationList(builder,
                                                                       sourceFile,
                                                                       88,
                                                                       95);
        BasicBooleanTerm nothingTerm = builder.createBasicBooleanTerm(nothingTermLocationList);

        operands = new Vector<BooleanTerm>();
        operands.addElement(somethingTerm);
        operands.addElement(nothingTerm);

        LocationList operatorTermLocationList = this.createLocationList(builder,
                                                                        sourceFile,
                                                                        85,
                                                                        87);

        BooleanOperator orOperator = createShortCircuitOr(builder);

        OperatorTerm operatorTerm = builder.createOperatorTerm(operatorTermLocationList,
                                                               orOperator,
                                                               operands);

        RootTerm ifTerm = builder.createRootTerm(operatorTerm,
                                                 builder.createCoverableItem(++counter
                                                         + ""));

        // ////////////////////////////////////////////////////// //
        // save the ifTerm in member variable for testing purpose
        this.ifRootTerm = ifTerm;
        // ////////////////////////////////////////////////////// //

        terms = new HashSet<RootTerm>();
        terms.add(ifTerm);

        LocationList ifStatementLocationList = this.createLocationList(builder,
                                                                       sourceFile,
                                                                       71,
                                                                       143);

        Location ifStatementKeywordLocation = builder.createLocation(sourceFile,
                                                                     71,
                                                                     73);

        ConditionalStatement ifStatement = builder.createConditionalStatement(ifStatementLocationList,
                                                                              builder.createCoverableItem(++counter
                                                                                      + ""),
                                                                              terms,
                                                                              branches,
                                                                              ifStatementKeywordLocation);
        // if

        statementList = new Vector<Statement>();
        statementList.addElement(ifStatement);
        statementList.addElement(whileStatement);

        LocationList mainSequenceLocationList = this.createLocationList(builder,
                                                                        sourceFile,
                                                                        71,
                                                                        197);
        StatementSequence mainSequence = builder.createStatementSequence(mainSequenceLocationList,
                                                                         statementList);

        HierarchyLevelType methodType = builder.createHierarchyLevelType("method",
                                                                         "method");

        sequenceList = new Vector<StatementSequence>();
        sequenceList.addElement(mainSequence);

        LocationList mainMethodLocationList = this.createLocationList(builder,
                                                                      sourceFile,
                                                                      26,
                                                                      202);
        LocationList mainMethodHeader = this.createLocationList(builder,
                                                                sourceFile,
                                                                26,
                                                                64);
        HierarchyLevel mainMethod = builder.createHierarchyLevel(mainMethodLocationList,
                                                                 "main method",
                                                                 mainMethodHeader,
                                                                 methodType,
                                                                 new Vector<HierarchyLevel>(),
                                                                 sequenceList);

        HierarchyLevelType classType = builder.createHierarchyLevelType("class",
                                                                        "class");

        LocationList testClassObjectLocationList = this.createLocationList(builder,
                                                                           sourceFile,
                                                                           0,
                                                                           203);
        LocationList testClassObjectHeader = this.createLocationList(builder,
                                                                     sourceFile,
                                                                     0,
                                                                     22);
        // testClassObjectHeader = database.createEmptyLocationList();

        children = new Vector<HierarchyLevel>();
        children.addElement(mainMethod);

        HierarchyLevel testClassObject = builder.createHierarchyLevel(testClassObjectLocationList,
                                                                      "Test Class",
                                                                      testClassObjectHeader,
                                                                      classType,
                                                                      children,
                                                                      new Vector<StatementSequence>());

        sourceFiles = new Vector<SourceFile>();
        sourceFiles.addElement(sourceFile);

        final Set<Criterion> criteria = new HashSet<Criterion>();
        criteria.add(LoopCoverage.getInstance());
        criteria.add(ConditionCoverage.getInstance());

        // Create the code base, with the class as top level mast element.
        testSessionContainer = new TestSessionContainer(testClassObject,
                                                        logger,
                                                        sourceFiles,
                                                        criteria,
                                                        UUID.randomUUID()
                                                            .toString(),
                                                        new Date());

        for (int k = 0; k < 3; k++) {

            TestSession testSession = testSessionContainer.createTestSession("TestSession"
                                                                                     + k,
                                                                             "42",
                                                                             new Date());

            for (int i = 0; i < 4; i++) {
                Map<String, Long> coverageData = new HashMap<String, Long>();
                Random random = new Random(); // TODO: should we do this in a test?

                long coverage;

                if (i % 2 == 0) {
                    coverage = 0;
                } else {
                    coverage = 1;
                }
                for (int a = 0; a < 14; a++) {
                    coverageData.put(a + "", coverage);
                }

                Map<String, BooleanAssignmentMap> assignments = new HashMap<String, BooleanAssignmentMap>();

                Map<BooleanAssignment, Long> subMap = new HashMap<BooleanAssignment, Long>();

                List<BooleanResult> resultList;
                BooleanAssignment booleanAssignment;

                // set assignments of if condition "if (something || nothing)"
                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.FALSE);
                resultList.add(BooleanResult.FALSE);

                booleanAssignment = new BooleanAssignment(resultList);

                subMap.put(booleanAssignment, (long) random.nextInt(100) + 1);

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.TRUE);
                resultList.add(BooleanResult.NOT_EVALUATED);

                booleanAssignment = new BooleanAssignment(resultList);

                subMap.put(booleanAssignment, (long) random.nextInt(100) + 1);

                assignments.put(ifTerm.getCoverableItem().getId(),
                                new BooleanAssignmentMap(2, subMap));
                // end assignment of if condition

                // set assignments of while condition "while (bar())"
                subMap = new HashMap<BooleanAssignment, Long>();

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.TRUE);
                booleanAssignment = new BooleanAssignment(resultList);

                subMap.put(booleanAssignment, (long) random.nextInt(100) + 1);

                resultList = new Vector<BooleanResult>();
                resultList.add(BooleanResult.FALSE);
                booleanAssignment = new BooleanAssignment(resultList);

                subMap.put(booleanAssignment, (long) random.nextInt(100) + 1);

                assignments.put(whileExpression.getCoverableItem().getId(),
                                new BooleanAssignmentMap(1, subMap));
                // end assignment of while condition

                org.codecover.model.TestCase testCase = testSession.createTestCase("TestCase"
                                                                                           + i,
                                                                                   "42",
                                                                                   new Date(),
                                                                                   coverageData,
                                                                                   assignments);
                testCase.setObjectMetaData("ObjectMetaData" + i,
                                           testClassObject,
                                           42l);
                testCase.setObjectMetaData("ObjectMetaData" + i % 2,
                                           ifBranch,
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
    */
    }

    
    /**
     * @author Markus Wittlinger
     * @param builder
     *            the {@link MASTBuilder}.
     * @return a ShortCircuitOr {@link BooleanOperator}.
     */
    private BooleanOperator createShortCircuitOr(MASTBuilder builder) {
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

        BooleanOperator orOperator = builder.createBooleanOperator(2,
                                                                   possibleAssignments,
                                                                   "shortCircuitOr");

        return orOperator;
    }

    /**
     * @author Markus Wittlinger
     * @param builder
     *            the {@link MASTBuilder}.
     * @return a NotOperator {@link BooleanOperator}.
     */
    private BooleanOperator createNotOperator(MASTBuilder builder) {
        Vector<BooleanResult> results;
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();

        results = new Vector<BooleanResult>();
        results.addElement(BooleanResult.TRUE);

        possibleAssignments.put(new BooleanAssignment(results), Boolean.FALSE);

        results = new Vector<BooleanResult>();
        results.addElement(BooleanResult.FALSE);

        possibleAssignments.put(new BooleanAssignment(results), Boolean.TRUE);

        BooleanOperator orOperator = builder.createBooleanOperator(1,
                                                                   possibleAssignments,
                                                                   "not");

        return orOperator;
    }

    /**
     * @author Markus Wittlinger
     * @param builder
     * @return a {@link LocationList}
     */
    private LocationList createLocationList(MASTBuilder builder,
                                            SourceFile sourceFile, int start,
                                            int end) {
        Location location = builder.createLocation(sourceFile, start, end);
        Vector<Location> locList = new Vector<Location>();
        locList.addElement(location);
        return builder.createLocationList(locList);
    }

}
