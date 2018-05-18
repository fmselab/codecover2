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

import java.util.*;

import org.codecover.model.mast.*;
import org.codecover.model.utils.Logger;

/**
 * This class is to be used in the creation of elements of the MAST.
 * <p>
 * One MAST must always be created with the same instance of a
 * {@link MASTBuilder}
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: MASTBuilder.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public class MASTBuilder {
    private static long actualId = 0;

    private final Logger logger;

    /**
     * Constructor
     * 
     * @param logger
     *            the logger to be used
     */
    public MASTBuilder(Logger logger) {
        this.logger = logger;
    }

    private static final List<Location> emptyListLocation = Collections
            .emptyList();

    private final LocationList emptyLocationList = createLocationList(emptyListLocation);

    /**
     * Creates and returns an instance of a {@link SourceFile} containing the
     * given data.
     * 
     * @param fileName
     *            the name of the file the {@link SourceFile} represents
     * @param content
     *            the content of the file the {@link SourceFile} represents
     * @return the created {@link SourceFile}
     */
    public SourceFile createSourceFile(String fileName, String content) {
        SourceFile file = Internal.createSourceFile(fileName, content,
                this.logger);

        return file;
    }

    /**
     * Creates and returns an instance of a {@link Location} containing the
     * given data.
     * 
     * @param file
     *            the {@link SourceFile}, which hold this {@link Location}
     * @param startOffset
     *            the start of the {@link Location} in the content of the
     *            {@link SourceFile}
     * @param endOffset
     *            the end of the {@link Location} in the content of the
     *            {@link SourceFile}
     * @return the created {@link Location}
     */
    public Location createLocation(SourceFile file, int startOffset,
            int endOffset) {
        Location location = Internal.createLocation(file, startOffset,
                endOffset, this.logger);

        return location;
    }

    /**
     * Creates and returns an instance of a {@link BasicStatement} containing
     * the given data.
     * 
     * @param location
     *            the {@link LocationList}, specifying the location of the
     *            {@link BasicStatement} in its {@link SourceFile}
     * @param coverableItem
     *            the {@link CoverableItem} of the {@link BasicStatement}
     * @param terms
     *            the {@link RootTerm}s associated with this
     *            {@link BasicStatement}
     * @return the created {@link BasicStatement}
     */
    public BasicStatement createBasicStatement(LocationList location,
            CoverableItem coverableItem, Set<RootTerm> terms, Set<QuestionMarkOperator> questionMarkOperators) {
        BasicStatement basicStatement = Internal.createBasicStatement(location,
                coverableItem, terms, questionMarkOperators, this.logger);

        return basicStatement;
    }

    
    /**
     * Creates and returns an instance of a {@link LocationList} containing the
     * given data.
     * 
     * @param locations
     *            the list of {@link Location}s
     * @return the created {@link LocationList}
     */
    public LocationList createLocationList(List<Location> locations) {
        LocationList locationList = Internal.createLocationList(locations,
                this.logger);

        return locationList;
    }

    /**
     * Returns a empty {@link LocationList}
     * 
     * @return the emtpy {@link LocationList}
     */
    public LocationList createEmptyLocationList() {
        return this.emptyLocationList;
    }

    /**
     * Creates and returns an instance of a {@link CoverableItem} containing the
     * given data.
     * 
     * @param prefix
     *            the prefix of the {@link CoverableItem}
     * @param id
     *            the id of the {@link CoverableItem}
     * @return the created {@link CoverableItem}
     */
    public CoverableItem createCoverableItem(String prefix, String id) {
        CoverableItem coverableItem = Internal.createCoverableItem(prefix, id,
                                                                   this.logger);

        return coverableItem;
    }

    /**
     * Creates and returns an instance of a {@link Branch} containing the given
     * data.
     * 
     * @param location
     *            the {@link LocationList}, specifying the location of the
     *            {@link Branch} in its {@link SourceFile}
     * @param coverableItem
     *            the {@link CoverableItem} of the {@link Branch}
     * @param implicit
     *            indicates whether or not the {@link Branch} actually exist or
     *            if it is an added {@link Branch} to e.g. an "if() then"
     *            without an else clause
     * @param decision
     *            the {@link LocationList}, specifying the location of the
     *            decision of the {@link Branch} in its {@link SourceFile}
     * @param statementSequence
     *            the {@link StatementSequence} contained in this {@link Branch}
     * @return the created {@link Branch}
     */
    public Branch createBranch(LocationList location,
            CoverableItem coverableItem, boolean implicit,
            LocationList decision, StatementSequence statementSequence) {
        Branch branch = Internal.createBranch(location, coverableItem,
                implicit, decision, statementSequence, this.logger);

        return branch;
    }

    /**
     * Creates and returns an instance of a {@link ConditionalStatement}
     * containing the given data.
     * 
     * @param location
     *            the {@link LocationList}, specifying the location of the
     *            {@link ConditionalStatement} in its {@link SourceFile}
     * @param coverableItem
     *            the {@link CoverableItem} of the {@link ConditionalStatement}
     * @param terms
     *            the {@link RootTerm}s associated with this
     *            {@link ConditionalStatement}
     * @param branches
     *            the list of {@link Branch}es this
     *            {@link ConditionalStatement} possesses.
     * @param keyword
     *            the {@link Location}, specifying the location of the keyword
     *            of the {@link ConditionalStatement} in its {@link SourceFile}
     * @return the created {@link ConditionalStatement}
     */
    public ConditionalStatement createConditionalStatement(
            LocationList location, CoverableItem coverableItem,
            Set<RootTerm> terms, List<Branch> branches, Location keyword, Set<QuestionMarkOperator> questionMarkOperators) {
        ConditionalStatement conditionalStatement = Internal
                .createConditionalStatement(location, coverableItem, terms,
                        branches, keyword, questionMarkOperators, this.logger);

        return conditionalStatement;
    }

    /**
     * Creates and returns an instance of a {@link LoopingStatement} containing
     * the given data.
     * 
     * @param location
     *            the {@link LocationList}, specifying the location of the
     *            {@link LoopingStatement} in its {@link SourceFile}
     * @param coverableItem
     *            the {@link CoverableItem} of the {@link LoopingStatement}
     * @param terms
     *            the {@link RootTerm}s associated with this
     *            {@link LoopingStatement}
     * @param statementSequence
     *            the {@link StatementSequence} contained in this
     *            {@link LoopingStatement}
     * @param keyword
     *            the {@link Location}, specifying the location of the keyword
     *            of the {@link LoopingStatement} in its {@link SourceFile}
     * @param neverExecutedItem
     *            the {@link CoverableItem} of the {@link LoopingStatement},
     *            that represents the number times the loop was not traversed
     * @param onceExecutedItem
     *            the {@link CoverableItem} of the {@link LoopingStatement},
     *            that represents the number times the loop was traversed only
     *            once
     * @param multipleExecutedItem
     *            the {@link CoverableItem} of the {@link LoopingStatement},
     *            that represents the number times the loop was traversed
     *            multiple times
     * @param optionalBodyExecution
     *            indicates, whether or not the body of the loop can be skipped
     *            entirely or is executed at least once, as it it the case with
     *            the "do ... while()" construct
     * @return the created {@link LoopingStatement}
     */
    public LoopingStatement createLoopingStatement(LocationList location,
            CoverableItem coverableItem, Set<RootTerm> terms,
            StatementSequence statementSequence, Location keyword,
            CoverableItem neverExecutedItem, CoverableItem onceExecutedItem,
            CoverableItem multipleExecutedItem, boolean optionalBodyExecution, Set<QuestionMarkOperator> questionMarkOperators) {
        LoopingStatement loopingStatement = Internal.createLoopingStatement(
                location, coverableItem, terms, statementSequence, keyword,
                neverExecutedItem, onceExecutedItem, multipleExecutedItem,
                optionalBodyExecution, questionMarkOperators, this.logger);

        return loopingStatement;
    }

    /**
     * Creates and returns an instance of a {@link StatementSequence} containing
     * the given data.
     * 
     * @param location
     *            the {@link LocationList}, specifying the location of the
     *            {@link StatementSequence} in its {@link SourceFile}
     * @param statements
     *            the list of {@link Statement}s the {@link StatementSequence}
     *            contains
     * @return the created {@link StatementSequence}
     */
    public StatementSequence createStatementSequence(LocationList location,
            List<Statement> statements) {
        StatementSequence statementSequence = Internal.createStatementSequence(
                location, statements, this.logger);

        return statementSequence;
    }

    /**
     * Creates and returns an instance of a {@link HierarchyLevelType}
     * containing the given data.
     * 
     * @param englishName
     *            the name of the {@link HierarchyLevelType} in english
     * @param internalName
     *            the name used internally
     * @return the created {@link HierarchyLevelType}
     */
    public HierarchyLevelType createHierarchyLevelType(String englishName,
            String internalName) {
        HierarchyLevelType hierarchyLevelType = Internal
                .createHierarchyLevelType(englishName, internalName,
                        this.logger);

        return hierarchyLevelType;
    }

    /**
     * Creates and returns an instance of a {@link HierarchyLevel} containing
     * the given data.
     * 
     * @param location
     *            the {@link LocationList}, specifying the location of the
     *            {@link HierarchyLevel} in its {@link SourceFile}
     * @param name
     *            the name of the {@link HierarchyLevel}
     * @param header
     *            the {@link LocationList}, specifying the location of the
     *            header of the {@link HierarchyLevel} in its {@link SourceFile}
     * @param type
     *            the {@link HierarchyLevelType} of the {@link HierarchyLevel}
     * @param children
     *            the list of {@link HierarchyLevel}s which are below the to be
     *            created {@link HierarchyLevel}
     * @param sequences
     *            the list of {@link StatementSequence} the
     *            {@link HierarchyLevel} contains
     * @return the created {@link HierarchyLevel}
     */
    public HierarchyLevel createHierarchyLevel(LocationList location,
            String name, LocationList header, HierarchyLevelType type,
            List<HierarchyLevel> children, List<StatementSequence> sequences) {
        return createHierarchyLevel(location, name, header, type, children,
                sequences, generateID());
    }

    /**
     * Creates and returns an instance of a {@link HierarchyLevel} containing
     * the given data.
     * 
     * @param location
     *            the {@link LocationList}, specifying the location of the
     *            {@link HierarchyLevel} in its {@link SourceFile}
     * @param name
     *            the name of the {@link HierarchyLevel}
     * @param header
     *            the {@link LocationList}, specifying the location of the
     *            header of the {@link HierarchyLevel} in its {@link SourceFile}
     * @param type
     *            the {@link HierarchyLevelType} of the {@link HierarchyLevel}
     * @param children
     *            the list of {@link HierarchyLevel}s which are below the to be
     *            created {@link HierarchyLevel}
     * @param sequences
     *            the list of {@link StatementSequence} the
     *            {@link HierarchyLevel} contains
     * @param id
     *            the id of the {@link HierarchyLevel}
     * @return the created {@link HierarchyLevel}
     */
    public HierarchyLevel createHierarchyLevel(LocationList location,
            String name, LocationList header, HierarchyLevelType type,
            List<HierarchyLevel> children, List<StatementSequence> sequences,
            String id) {
        HierarchyLevel hierarchyLevel = Internal.createHierarchyLevel(location,
                name, header, type, children, sequences, this.logger, id);

        return hierarchyLevel;
    }

    /**
     * Creates and returns an instance of a {@link BasicBooleanTerm} containing
     * the given data.
     * 
     * @param location
     *            the {@link LocationList}, specifying the location of the
     *            {@link BasicBooleanTerm} in its {@link SourceFile}
     * @return the created {@link BasicBooleanTerm}
     */
    public BasicBooleanTerm createBasicBooleanTerm(LocationList location) {
        BasicBooleanTerm basicBooleanTerm = Internal.createBasicBooleanTerm(
                location, this.logger);

        return basicBooleanTerm;
    }

    /**
     * Creates and returns an instance of a {@link BooleanOperator} containing
     * the given data.
     * 
     * @param arity
     *            the arity of the {@link BooleanOperator}
     * @param possibleAssignments
     *            the complete list of assignments, that can occur during
     *            execution
     * @param name
     *            the name of the {@link BooleanOperator}
     * @return the created {@link BooleanOperator}
     */
    public BooleanOperator createBooleanOperator(int arity,
            Map<BooleanAssignment, Boolean> possibleAssignments, String name) {
        BooleanOperator booleanOperator = Internal.createBooleanOperator(arity,
                possibleAssignments, name, this.logger);

        return booleanOperator;
    }

    /**
     * Creates and returns an instance of a {@link OperatorTerm} containing the
     * given data.
     * <p>
     * The arity of the {@link OperatorTerm} must be the same as the number of
     * operands assigned to this {@link OperatorTerm}
     * 
     * @param location
     *            the {@link LocationList}, specifying the location of the
     *            {@link BasicBooleanTerm} in its {@link SourceFile}
     * @param operator
     *            the {@link BooleanOperator} of the {@link OperatorTerm}
     * @param operands
     *            the list of {@link BooleanTerm}s that are the operands of
     *            {@link BooleanOperator} contained in this {@link OperatorTerm}
     * @return the created {@link OperatorTerm}
     */
    public OperatorTerm createOperatorTerm(LocationList location,
            BooleanOperator operator, List<BooleanTerm> operands) {
        OperatorTerm operatorTerm = Internal.createOperatorTerm(location,
                operator, operands, this.logger);

        return operatorTerm;
    }

    /**
     * Creates and returns an instance of a {@link RootTerm} containing the
     * given data.
     * 
     * @param term
     *            the {@link BooleanTerm} which represents the root of the
     *            expression tree
     * @param coverableItem
     *            the {@link CoverableItem} of the {@link RootTerm}
     * @return the created {@link RootTerm}
     */
    public RootTerm createRootTerm(BooleanTerm term, CoverableItem coverableItem) {
        RootTerm rootTerm = Internal.createRootTerm(term, coverableItem,
                this.logger);

        return rootTerm;
    }

    /**
     * Gets the logger
     * 
     * @return the logger
     */
    public Logger getLogger() {
        return this.logger;
    }

    /**
     * Generates a random ID
     * 
     * @return a new ID
     */
    private String generateID() {

        return Long.toString(++actualId);
        // return UUID.randomUUID().toString();
    }
}
