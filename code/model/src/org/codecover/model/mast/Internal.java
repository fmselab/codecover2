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

package org.codecover.model.mast;

import java.util.*;

import org.codecover.model.utils.Logger;

/**
 * All functions in this class are internal to the model and should not be used
 * outside.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: Internal.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public final class Internal {
    private Internal() {
        // Do nothing.
    }

    /**
     * Creates and returns an instance of a {@link SourceFile} containing the
     * given data.
     * 
     * @param fileName
     *            the name of the file the {@link SourceFile} represents
     * @param content
     *            the content of the file the {@link SourceFile} represents
     * @param logger
     *            the logger to be used
     * @return the created {@link SourceFile}
     */
    public static SourceFile createSourceFile(String fileName, String content,
            Logger logger) {
        SourceFile file = new SourceFile(fileName, content);

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
     * @param logger
     *            the logger to be used
     * @return the created {@link Location}
     */
    public static Location createLocation(SourceFile file, int startOffset,
            int endOffset, Logger logger) {
        Location location = new Location(file, startOffset, endOffset);

        return location;
    }

    /**
     * Creates and returns an instance of a {@link BasicStatement} containing
     * the given data.
     * 
     * @param location
     *            the {@link LocationList}, specifing the location of the
     *            {@link BasicStatement} in its {@link SourceFile}
     * @param coverableItem
     *            the {@link CoverableItem} of the {@link BasicStatement}
     * @param terms
     *            the {@link RootTerm}s associated with this
     *            {@link BasicStatement}
     * @param logger
     *            the logger to be used
     * @return the created {@link BasicStatement}
     */
    public static BasicStatement createBasicStatement(LocationList location,
            CoverableItem coverableItem, Set<RootTerm> terms, Set<QuestionMarkOperator> questionMarkOperators, Logger logger) {
    	
    	
        BasicStatement basicStatement = new BasicStatement(location,
                coverableItem, terms, questionMarkOperators);

        return basicStatement;
    }

        
    /**
     * Creates and returns an instance of a {@link LocationList} containing the
     * given data.
     * 
     * @param locations
     *            the list of {@link Location}s
     * @param logger
     *            the logger to be used
     * @return the created {@link LocationList}
     */
    public static LocationList createLocationList(List<Location> locations,
            Logger logger) {
        LocationList locationList = new LocationList(locations);

        return locationList;
    }

    /**
     * Creates and returns an instance of a {@link CoverableItem} containing the
     * given data.
     * 
     * @param id
     *            the id of the {@link CoverableItem}
     * @param prefix
     *            the prefix of the {@link CoverableItem}
     * @param logger
     *            the logger to be used
     * @return the created {@link CoverableItem}
     */
    public static CoverableItem createCoverableItem(String prefix, String id,
                                                    Logger logger) {
        CoverableItem coverableItem = new CoverableItem(prefix, id);

        return coverableItem;
    }

    /**
     * Creates and returns an instance of a {@link Branch} containing the given
     * data.
     * 
     * @param location
     *            the {@link LocationList}, specifing the location of the
     *            {@link Branch} in its {@link SourceFile}
     * @param coverableItem
     *            the {@link CoverableItem} of the {@link Branch}
     * @param implicit
     *            indicates whether or not the {@link Branch} actually exist or
     *            if it is an added {@link Branch} to e.g. an "if() then"
     *            without an else clause
     * @param decision
     *            the {@link LocationList}, specifing the location of the
     *            decision of the {@link Branch} in its {@link SourceFile}
     * @param statementSequence
     *            the {@link StatementSequence} contained in this {@link Branch}
     * @param logger
     *            the logger to be used
     * @return the created {@link Branch}
     */
    public static Branch createBranch(LocationList location,
            CoverableItem coverableItem, boolean implicit,
            LocationList decision, StatementSequence statementSequence,
            Logger logger) {
        Branch branch = new Branch(location, coverableItem, implicit, decision,
                statementSequence);

        return branch;
    }

    /**
     * Creates and returns an instance of a {@link ConditionalStatement}
     * containing the given data.
     * 
     * @param location
     *            the {@link LocationList}, specifing the location of the
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
     *            the {@link Location}, specifing the location of the keyword
     *            of the {@link ConditionalStatement} in its {@link SourceFile}
     * @param logger
     *            the logger to be used
     * @return the created {@link ConditionalStatement}
     */
    public static ConditionalStatement createConditionalStatement(
            LocationList location, CoverableItem coverableItem,
            Set<RootTerm> terms, List<Branch> branches, Location keyword, Set<QuestionMarkOperator> questionMarkOperators, 
            Logger logger) {
        ConditionalStatement conditionalStatement = new ConditionalStatement(
                location, coverableItem, terms, branches, keyword, questionMarkOperators);

        return conditionalStatement;
    }

    /**
     * Creates and returns an instance of a {@link LoopingStatement} containing
     * the given data.
     * 
     * @param location
     *            the {@link LocationList}, specifing the location of the
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
     *            the {@link Location}, specifing the location of the keyword
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
     * @param logger
     *            the logger to be used
     * @return the created {@link LoopingStatement}
     */
    public static LoopingStatement createLoopingStatement(
            LocationList location, CoverableItem coverableItem,
            Set<RootTerm> terms, StatementSequence statementSequence,
            Location keyword, CoverableItem neverExecutedItem,
            CoverableItem onceExecutedItem, CoverableItem multipleExecutedItem,
            boolean optionalBodyExecution, Set<QuestionMarkOperator> questionMarkOperators, Logger logger) {
        LoopingStatement loopingStatement = new LoopingStatement(location,
                coverableItem, terms, statementSequence, keyword,
                neverExecutedItem, onceExecutedItem, multipleExecutedItem,
                optionalBodyExecution, questionMarkOperators);

        return loopingStatement;
    }

    /**
     * Creates and returns an instance of a {@link StatementSequence} containing
     * the given data.
     * 
     * @param location
     *            the {@link LocationList}, specifing the location of the
     *            {@link StatementSequence} in its {@link SourceFile}
     * @param statements
     *            the list of {@link Statement}s the {@link StatementSequence}
     *            contains
     * @param logger
     *            the logger to be used
     * @return the created {@link StatementSequence}
     */
    public static StatementSequence createStatementSequence(
            LocationList location, List<Statement> statements, Logger logger) {
        StatementSequence statementSequence = new StatementSequence(location,
                statements);

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
     * @param logger
     *            the logger to be used
     * @return the created {@link HierarchyLevelType}
     */
    public static HierarchyLevelType createHierarchyLevelType(
            String englishName, String internalName, Logger logger) {
        HierarchyLevelType hierarchyLevelType = new HierarchyLevelType(
                englishName, internalName);

        return hierarchyLevelType;
    }

    /**
     * Creates and returns an instance of a {@link HierarchyLevel} containing
     * the given data.
     * 
     * @param location
     *            the {@link LocationList}, specifing the location of the
     *            {@link HierarchyLevel} in its {@link SourceFile}
     * @param name
     *            the name of the {@link HierarchyLevel}
     * @param header
     *            the {@link LocationList}, specifing the location of the
     *            header of the {@link HierarchyLevel} in its {@link SourceFile}
     * @param type
     *            the {@link HierarchyLevelType} of the {@link HierarchyLevel}
     * @param children
     *            the list of {@link HierarchyLevel}s which are below the to be
     *            created {@link HierarchyLevel}
     * @param sequences
     *            the list of {@link StatementSequence} the
     *            {@link HierarchyLevel} contains
     * @param logger
     *            the logger to be used
     * @param id
     *            the id of the {@link HierarchyLevel}
     * @return the created {@link HierarchyLevel}
     */
    public static HierarchyLevel createHierarchyLevel(LocationList location,
            String name, LocationList header, HierarchyLevelType type,
            List<HierarchyLevel> children, List<StatementSequence> sequences,
            Logger logger, String id) {
        HierarchyLevel hierarchyLevel = new HierarchyLevel(location, name,
                header, type, children, sequences, id);

        return hierarchyLevel;
    }

    /**
     * Creates and returns an instance of a {@link BasicBooleanTerm} containing
     * the given data.
     * 
     * @param location
     *            the {@link LocationList}, specifing the location of the
     *            {@link BasicBooleanTerm} in its {@link SourceFile}
     * @param logger
     *            the logger to be used
     * @return the created {@link BasicBooleanTerm}
     */
    public static BasicBooleanTerm createBasicBooleanTerm(
            LocationList location, Logger logger) {
        BasicBooleanTerm basicBooleanTerm = new BasicBooleanTerm(location);

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
     * @param logger
     *            the logger to be used
     * @return the created {@link BooleanOperator}
     */
    public static BooleanOperator createBooleanOperator(int arity,
            Map<BooleanAssignment, Boolean> possibleAssignments, String name,
            Logger logger) {
        BooleanOperator booleanOperator = new BooleanOperator(arity,
                possibleAssignments, name);

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
     *            the {@link LocationList}, specifing the location of the
     *            {@link BasicBooleanTerm} in its {@link SourceFile}
     * @param operator
     *            the {@link BooleanOperator} of the {@link OperatorTerm}
     * @param operands
     *            the list of {@link BooleanTerm}s that are the operands of
     *            {@link BooleanOperator} contained in this {@link OperatorTerm}
     * @param logger
     *            the logger to be used
     * @return the created {@link OperatorTerm}
     */
    public static OperatorTerm createOperatorTerm(LocationList location,
            BooleanOperator operator, List<BooleanTerm> operands, Logger logger) {
        OperatorTerm operatorTerm = new OperatorTerm(location, operator,
                operands);

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
     * @param logger
     *            the logger to be used
     * @return the created {@link RootTerm}
     */
    public static RootTerm createRootTerm(BooleanTerm term,
            CoverableItem coverableItem, Logger logger) {
        RootTerm rootTerm = new RootTerm(term, coverableItem);

        return rootTerm;
    }

    /**
     * Returns the id of a {@link MetaData} object
     * 
     * @param metaData
     *            the given {@link MetaData}
     * 
     * @return the id of the {@link MetaData}
     */
    public static long getMetaDataId(MetaData metaData) {
        return metaData.id;
    }
}
