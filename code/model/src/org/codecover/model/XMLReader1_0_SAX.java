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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.Vector;

import org.codecover.model.extensions.PluginManager;
import org.codecover.model.extensions.PluginUtils;
import org.codecover.model.mast.BasicBooleanTerm;
import org.codecover.model.mast.BasicStatement;
import org.codecover.model.mast.BooleanAssignment;
import org.codecover.model.mast.BooleanAssignmentMap;
import org.codecover.model.mast.BooleanOperator;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.Branch;
import org.codecover.model.mast.ConditionalStatement;
import org.codecover.model.mast.CoverableItem;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.HierarchyLevelType;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.LocationList;
import org.codecover.model.mast.LoopingStatement;
import org.codecover.model.mast.MetaDataObject;
import org.codecover.model.mast.OperatorTerm;
import org.codecover.model.mast.QuestionMarkOperator;
import org.codecover.model.mast.QuestionMarkOperatorExpression;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.mast.Statement;
import org.codecover.model.mast.StatementSequence;
import org.codecover.model.mast.SynchronizedStatement;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.criteria.Criterion;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 * This class represents a handler for a SAXParser, that is to be used in
 * reading {@link TestSessionContainer} xml files. Since the MAST elements need
 * all the necessary information, which is only complete with the closing xml
 * tag of an element, at the point of their creation, the information is stored
 * in temporary data structures, which in turn are stored in stacks. When an xml
 * element is closed, the appropriate data structure is popped and used in
 * creating a MAST element.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: XMLReader1_0_SAX.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
class XMLReader1_0_SAX extends XMLReaderBase implements XMLNames1_0 {
    private final Map<String, HierarchyLevelType> idHierarchyLevelTypeMap;

    private final Map<String, SourceFile> idSourceFileMap;

    private final Map<String, BooleanOperator> idBooleanOperatorMap;

    private final Map<String, MetaDataObject> idMetaDataObjectMap;

    private final Map<String, RootTerm> idRootTermMap;

    private final Map<String, QuestionMarkOperator> idQuestionMarkOperatorMap;
    
    private final MASTBuilder builder;

    private TestSessionContainerStore tscStore;

    private BooleanOperatorStore booleanOperatorStore;

    private TestSessionContainer testSessionContainer;

    private final Stack<HierarchyLevelStore> hierarchyLevelStoreStack;

    private final Stack<StatementSequenceStore> statementSequenceStoreStack;

    private final Stack<LoopStatementStore> loopStatementStoreStack;

    private final Stack<ConditionalStatementStore> conditionalStatementStoreStack;

    private String mapEntryParent;

    private BasicStatementStore basicStatementStore;

    private final Stack<String> currentLocationReciever;

    private final List<Location> locations;

    private final Stack<BranchStore> branchStoreStack;

    private final Stack<RootTermStore> rootTermStoreStack;

    private final Stack<QuestionMarkOperatorStore> questionMarkOperatorStoreStack;

    private QuestionMarkOperatorExpressionStore questionMarkOperatorExpression1 = null;
    private QuestionMarkOperatorExpressionStore questionMarkOperatorExpression2 = null;

    private Set<QuestionMarkOperator> questionMarkOperators = new HashSet<QuestionMarkOperator>();

    private final Stack<OperatorTermStore> operatorTermStoreStack;

    private final Stack<BasicBooleanTermStore> basicBooleanTermStoreStack;

    private final Stack<ObjectMetaDataListEntryStore> objectMetaDataStack;

    private final Stack<MetaDataListEntryStore> metaDataStack;

    private TestSession currentTestSession;

    private TestCaseStore currentTestCaseStore;

    private String currentCoverableItemPrefix;

    private AssignmentsListEntryStore currentAssignmentsListEntryStore;

    private final PluginManager pluginManager;
    
    private SynchronizedStatementStore synchronizedStatementStore;

    protected final class TestSessionContainerStore {
        Date date;

        String id;

        HierarchyLevel code;

        List<SourceFile> sourceFiles = new Vector<SourceFile>();

        List<Criterion> criteriaList = new Vector<Criterion>();
    }

    protected final class TestCaseStore {
        String comment;

        String name;

        Date date;

        Map<CoverableItem, Long> coverageData = new HashMap<CoverableItem, Long>();

        Map<CoverableItem, BooleanAssignmentMap> assignments = new HashMap<CoverableItem, BooleanAssignmentMap>();
    }

    protected final class BooleanOperatorStore {
        int arity;

        String internalId;

        String name;

        Map<BooleanAssignment, Boolean> assignmentsMap = new HashMap<BooleanAssignment, Boolean>();
    }

    protected final class HierarchyLevelStore {
        LocationList locationList;

        String internalId;

        String name;

        LocationList header;

        HierarchyLevelType type;

        List<HierarchyLevel> children = new Vector<HierarchyLevel>();

        List<StatementSequence> sequences = new Vector<StatementSequence>();

        String id;
    }

    protected final class StatementSequenceStore {
        LocationList locationList;

        String internalId;

        List<Statement> statements = new Vector<Statement>();
    }

    protected final class BasicStatementStore {
        LocationList locationList;

        CoverableItem coverableItem;

        String internalId;

        Set<RootTerm> terms = new HashSet<RootTerm>();
    }

    protected final class LoopStatementStore {
        LocationList locationList;

        CoverableItem coverableItem;

        String internalId;

        Set<RootTerm> terms = new HashSet<RootTerm>();

        StatementSequence statementSequence;

        Location keyword;

        CoverableItem neverExecutedItem;

        CoverableItem onceExecutedItem;

        CoverableItem multipleExecutedItem;

        boolean optionalBodyExecution;
    }

    protected final class ConditionalStatementStore {
        String internalId;

        LocationList locationList;

        CoverableItem coverableItem;

        Set<RootTerm> terms = new HashSet<RootTerm>();

        List<Branch> branches = new Vector<Branch>();

        Location keyword;
    }

    protected final class BranchStore {
        String internalId;

        LocationList locationList;

        CoverableItem coverableItem;

        boolean implicit;

        LocationList decision;

        StatementSequence statementSequence;
    }

    protected final class RootTermStore {
        String internalId;

        BooleanTerm term;

        CoverableItem coverableItem;
    }

    protected final class QuestionMarkOperatorStore {
        LocationList locationList;
        String internalId;

        QuestionMarkOperator questionMarkOperator;

        CoverableItem coverableItem;
    }
    
    protected final class QuestionMarkOperatorExpressionStore {
        LocationList locationList;
        String internalId;

        QuestionMarkOperatorExpression questionMarkOperatorExpression;

        CoverableItem coverableItem;
    }
    
    protected final class SynchronizedStatementStore {
        LocationList locationList;
        String internalId;

        SynchronizedStatement synchronizedStatement;

        CoverableItem coverableItem;

        CoverableItem coverableItem0;
        CoverableItem coverableItem1;
        CoverableItem coverableItem2;
    }

    protected final class OperatorTermStore {
        String internalId;

        LocationList locationList;

        BooleanOperator operator;

        List<BooleanTerm> operands = new Vector<BooleanTerm>();
    }

    protected final class BasicBooleanTermStore {
        String internalId;

        LocationList locationList;
    }

    protected final class AssignmentsListEntryStore {
        int length;

        Map<BooleanAssignment, Long> possibleAssignments = new HashMap<BooleanAssignment, Long>();

        CoverableItem coverableItem;
    }

    protected final class ObjectMetaDataListEntryStore {
        String name;

        MetaDataObject metaDataObject;

        Object metadata;
    }

    protected final class MetaDataListEntryStore {
        String name;

        Object metadata;
    }

    /**
     * Constructor
     * 
     * @param logger
     *            the {@link Logger} to use
     * @param builder
     *            the {@link MASTBuilder} to use in creating the mast elements
     * @param pluginManager
     *            the {@link PluginManager} to handle to plugin parts of
     *            CodeCover
     */
    public XMLReader1_0_SAX(Logger logger, MASTBuilder builder,
            PluginManager pluginManager) {
        super(logger);
        this.builder = builder;
        this.pluginManager = pluginManager;
        this.idSourceFileMap = new HashMap<String, SourceFile>();
        this.idBooleanOperatorMap = new HashMap<String, BooleanOperator>();
        this.idMetaDataObjectMap = new HashMap<String, MetaDataObject>();
        this.idRootTermMap = new HashMap<String, RootTerm>();
        this.idHierarchyLevelTypeMap = new HashMap<String, HierarchyLevelType>();
        this.hierarchyLevelStoreStack = new Stack<HierarchyLevelStore>();
        this.currentLocationReciever = new Stack<String>();
        this.locations = new Vector<Location>();
        this.statementSequenceStoreStack = new Stack<StatementSequenceStore>();
        this.loopStatementStoreStack = new Stack<LoopStatementStore>();
        this.conditionalStatementStoreStack = new Stack<ConditionalStatementStore>();
        this.branchStoreStack = new Stack<BranchStore>();
        this.rootTermStoreStack = new Stack<RootTermStore>();
        this.questionMarkOperatorStoreStack = new Stack<QuestionMarkOperatorStore>();
        this.operatorTermStoreStack = new Stack<OperatorTermStore>();
        this.basicBooleanTermStoreStack = new Stack<BasicBooleanTermStore>();
        this.objectMetaDataStack = new Stack<ObjectMetaDataListEntryStore>();
        this.metaDataStack = new Stack<MetaDataListEntryStore>();
        this.idQuestionMarkOperatorMap = new HashMap<String, QuestionMarkOperator>();
    }
    
    /**
     * (non-Javadoc)
     * 
     * @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String,
     *      java.lang.String, java.lang.String, org.xml.sax.Attributes)
     */
    @Override
    public void startElement(String uri, String localName, String qName,
            Attributes attributes) throws SAXException {
        if (qName.equals(ELEMENT_TEST_SESSION_CONTAINER)) {
            handleStartElementTestSessionContainer(attributes);
            return;
        }

        if (qName.equals(ELEMENT_SOURCE_FILE)) {
            handleStartElementSourceFile(attributes);
            return;
        }

        if (qName.equals(ELEMENT_CRITERIA_LIST_ENTRY)) {
            handleStartElementCriterion(attributes);
            return;
        }

        if (qName.equals(ELEMENT_BOOLEAN_OPERATOR)) {
            handleStartElementBooleanOperator(attributes);
            return;
        }

        if (qName.equals(ELEMENT_BOOLEAN_ASSIGNMENT_BOOLEAN_MAP_LIST)) {
            this.mapEntryParent = ELEMENT_BOOLEAN_ASSIGNMENT_BOOLEAN_MAP_LIST;
            return;
        }

        if (qName.equals(ELEMENT_MAP_ENTRY)) {
            // Two different elements use the ELEMENT_MAP_ENTRY as tag name,
            // so we check what the encompassing element's name was.
            if (this.mapEntryParent
                    .equals(ELEMENT_BOOLEAN_ASSIGNMENT_BOOLEAN_MAP_LIST)) {
                handleStartElementBABMapEntry(attributes);
            } else if (this.mapEntryParent
                    .equals(ELEMENT_ASSIGNMENT_LIST_ENTRY)) {
                handleStartElementBALMapEntry(attributes);
            }

            return;
        }

        if (qName.equals(ELEMENT_HIERARCHY_LEVEL_TYPE)) {
            handleStartElementHierarchyLevelType(attributes);
            return;
        }

        if (qName.equals(ELEMENT_HIERARCHY_LEVEL)) {
            handleStartElementHierarchyLevel(attributes);
            return;
        }

        if (qName.equals(ELEMENT_HEADER)) {
            handleStartElementLocationList(attributes);
            return;
        }

        if (qName.equals(ELEMENT_LOCATION)) {
            handleStartElementLocation(attributes);
            return;
        }

        if (qName.equals(ELEMENT_LOCATION_LIST)) {
            handleStartElementLocationList(attributes);
            return;
        }

        if (qName.equals(ELEMENT_STATEMENT_SEQUENCE)) {
            handleStartElementStatementSequence(attributes);
            return;
        }

        if (qName.equals(ELEMENT_BASIC_STATEMENT)) {
            handleStartElementBasicStatement(attributes);
            return;
        }

        if (qName.equals(ELEMENT_LOOPING_STATEMENT)) {
            handleStartElementLoopStatement(attributes);
            return;
        }

        if (qName.equals(ELEMENT_CONDITIONAL_STATEMENT)) {
            handleStartElementConditionalStatement(attributes);
            return;
        }

        if (qName.equals(ELEMENT_KEYWORD)) {
            handleStartElementKeyword(attributes);
            return;
        }

        if (qName.equals(ELEMENT_BRANCH)) {
            handleStartElementBranch(attributes);
            return;
        }

        if (qName.equals(ELEMENT_DECISION)) {
            handleStartElementLocationList(attributes);
            return;
        }

        if (qName.equals(ELEMENT_ROOT_TERM)) {
            handleStartElementRootTerm(attributes);
            return;
        }

        if (qName.equals(ELEMENT_OPERATOR_TERM)) {
            handleStartElementOperatorTerm(attributes);
            return;
        }

        if (qName.equals(ELEMENT_BASIC_BOOLEAN_TERM)) {
            handleStartElementBasicBooleanTerm(attributes);
            return;
        }

        if (qName.equals(ELEMENT_QUESTIONMARKOPERATOR)) {
            handleStartElementQuestionMarkOperator(attributes);
            return;
        }

       if (qName.equals(ELEMENT_QUESTIONMARKOPERATOR_EXPRESSION)) {
            handleStartElementQuestionMarkOperatorExpression(attributes);
            return;
        }

       if (qName.equals(ELEMENT_SYNCHRONIZED_STATEMENT)) {
           handleStartElementSynchronizedStatement(attributes);
           return;
       }
       
       if (qName.equals(ELEMENT_TEST_SESSION)) {
            handleStartElementTestSession(attributes);
            return;
        }

        if (qName.equals(ELEMENT_TEST_CASE)) {
            handleStartElementTestCase(attributes);
            return;
        }

        if (qName.equals(ELEMENT_COVERAGE_PREFIX)) {
            handleStartElementCoveragePrefix(attributes);
            return;
        }

        if (qName.equals(ELEMENT_COVERAGE)) {
            handleStartElementCoverage(attributes);
            return;
        }

        if (qName.equals(ELEMENT_ASSIGNMENT_PREFIX)) {
            handleStartElementAssignmentPrefix(attributes);
            return;
        }

        if (qName.equals(ELEMENT_ASSIGNMENT_LIST_ENTRY)) {
            this.mapEntryParent = ELEMENT_ASSIGNMENT_LIST_ENTRY;
            handleStartElementAssignmentListEntry(attributes);
            return;
        }

        if (qName.equals(ELEMENT_OBJECT_META_DATA_LIST_ENTRY)) {
            handleStartElementObjectMetaDataListEntry(attributes);
            return;
        }

        if (qName.equals(ELEMENT_META_DATA_LIST_ENTRY)) {
            handleStartElementMetaDataListEntry(attributes);
            return;
        }

        if (qName.equals(ELEMENT_META_DATA)) {
            handleStartElementMetaData(attributes);
            return;
        }
    }

    /**
     * @param attributes
     */
    protected void handleStartElementMetaData(Attributes attributes) {
        final Object serialisedObject;

        final String base64 = attributes.getValue(BASE64);

        serialisedObject = TSCXMLHelper.decodeMetaData(base64, getLogger());

        String currentReciever = this.currentLocationReciever.peek();

        if (currentReciever.equals(ELEMENT_OBJECT_META_DATA_LIST_ENTRY)) {
            ObjectMetaDataListEntryStore store = this.objectMetaDataStack
                    .peek();

            store.metadata = serialisedObject;
        } else if (currentReciever.equals(ELEMENT_META_DATA_LIST_ENTRY)) {
            MetaDataListEntryStore store = this.metaDataStack.peek();

            store.metadata = serialisedObject;
        }
    }

    /**
     * @param attributes
     */
    protected void handleStartElementMetaDataListEntry(Attributes attributes) {
        String name = attributes.getValue(NAME);

        MetaDataListEntryStore store = new MetaDataListEntryStore();

        store.name = name;

        this.metaDataStack.push(store);

        this.currentLocationReciever.push(ELEMENT_META_DATA_LIST_ENTRY);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementObjectMetaDataListEntry(
            Attributes attributes) {
        String name = attributes.getValue(NAME);
        MetaDataObject metaDataObject = this.idMetaDataObjectMap.get(attributes
                .getValue(META_DATA_OBJECT_ID));

        ObjectMetaDataListEntryStore store = new ObjectMetaDataListEntryStore();

        store.metaDataObject = metaDataObject;
        store.name = name;

        this.objectMetaDataStack.push(store);

        this.currentLocationReciever.push(ELEMENT_OBJECT_META_DATA_LIST_ENTRY);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementAssignmentListEntry(Attributes attributes) {
        final int length = Integer.parseInt(attributes.getValue(LENGTH));
        final String coverableItemId = attributes
                .getValue(ROOT_TERM_COVERABLE_ITEM_ID);

        this.currentAssignmentsListEntryStore = new AssignmentsListEntryStore();
        this.currentAssignmentsListEntryStore.coverableItem = this.builder
                .createCoverableItem(this.currentCoverableItemPrefix,
                        coverableItemId);
        this.currentAssignmentsListEntryStore.length = length;
    }

    /**
     * @param attributes
     */
    protected void handleStartElementAssignmentPrefix(Attributes attributes) {
        this.currentCoverableItemPrefix = attributes
                .getValue(ROOT_TERM_COVERABLE_ITEM_PREFIX);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementCoverage(Attributes attributes) {
        String coverableItemId = attributes.getValue(COVERABLE_ITEM_ID);
        Long value = Long.parseLong(attributes.getValue(VALUE));

        CoverableItem coverableItem = this.builder.createCoverableItem(
                this.currentCoverableItemPrefix, coverableItemId);

        this.currentTestCaseStore.coverageData.put(coverableItem, value);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementCoveragePrefix(Attributes attributes) {
        this.currentCoverableItemPrefix = attributes
                .getValue(COVERABLE_ITEM_PREFIX);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementBasicBooleanTerm(Attributes attributes) {
        String internalId = attributes.getValue(INTERNAL_ID);

        BasicBooleanTermStore store = new BasicBooleanTermStore();

        store.internalId = internalId;

        this.basicBooleanTermStoreStack.push(store);

        this.currentLocationReciever.push(ELEMENT_BASIC_BOOLEAN_TERM);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementQuestionMarkOperator(Attributes attributes) {
        String itemId = attributes.getValue(COVERABLE_ITEM_ID);
        String itemPrefix = attributes.getValue(COVERABLE_ITEM_PREFIX);
        String internalId = attributes.getValue(INTERNAL_ID);
        CoverableItem coverableItem = this.builder.createCoverableItem(
                itemPrefix, itemId);

        QuestionMarkOperatorStore store = new QuestionMarkOperatorStore();
        store.coverableItem = coverableItem;

        store.internalId = internalId;

        this.questionMarkOperatorStoreStack.push(store);

        this.currentLocationReciever.push(ELEMENT_QUESTIONMARKOPERATOR);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementSynchronizedStatement(Attributes attributes) {
        String itemId = attributes.getValue(COVERABLE_ITEM_ID);
        String itemPrefix = attributes.getValue(COVERABLE_ITEM_PREFIX);
        String internalId = attributes.getValue(INTERNAL_ID);
        CoverableItem coverableItem = this.builder.createCoverableItem(
                itemPrefix, itemId);

        CoverableItem coverableItem0 = this.builder.createCoverableItem(
                itemPrefix, attributes.getValue(ELEMENT_SYNCHRONIZED0));
        CoverableItem coverableItem1 = this.builder.createCoverableItem(
                itemPrefix, attributes.getValue(ELEMENT_SYNCHRONIZED1));
        CoverableItem coverableItem2 = this.builder.createCoverableItem(
                itemPrefix, attributes.getValue(ELEMENT_SYNCHRONIZED2));
                
        synchronizedStatementStore = new SynchronizedStatementStore();
        synchronizedStatementStore.coverableItem = coverableItem;

        synchronizedStatementStore.internalId = internalId;
        synchronizedStatementStore.coverableItem0 = coverableItem0;
        synchronizedStatementStore.coverableItem1 = coverableItem1;
        synchronizedStatementStore.coverableItem2 = coverableItem2;

        this.currentLocationReciever.push(ELEMENT_SYNCHRONIZED_STATEMENT);
    }
    
    
    /**
     * @param attributes
     */
    protected void handleStartElementQuestionMarkOperatorExpression(Attributes attributes) {
        String itemId = attributes.getValue(COVERABLE_ITEM_ID);
        String itemPrefix = attributes.getValue(COVERABLE_ITEM_PREFIX);
        String internalId = attributes.getValue(INTERNAL_ID);
        CoverableItem coverableItem = this.builder.createCoverableItem(
                itemPrefix, itemId);

        QuestionMarkOperatorExpressionStore store;

        if(this.questionMarkOperatorExpression1 == null) {
        	store = this.questionMarkOperatorExpression1 = new QuestionMarkOperatorExpressionStore();
        } else {
        	store = this.questionMarkOperatorExpression2 = new QuestionMarkOperatorExpressionStore();        	
        }
        store.internalId = internalId;
        store.coverableItem = coverableItem;

        this.currentLocationReciever.push(ELEMENT_QUESTIONMARKOPERATOR_EXPRESSION);
    }
  
    /**
     * @param attributes
     */
    protected void handleStartElementOperatorTerm(Attributes attributes) {
        String internalId = attributes.getValue(INTERNAL_ID);
        String booleanOperatorId = attributes.getValue(BOOLEAN_OPERATOR_ID);

        OperatorTermStore store = new OperatorTermStore();

        store.internalId = internalId;
        store.operator = this.idBooleanOperatorMap.get(booleanOperatorId);

        this.operatorTermStoreStack.push(store);

        this.currentLocationReciever.push(ELEMENT_OPERATOR_TERM);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementRootTerm(Attributes attributes) {
        String itemId = attributes.getValue(COVERABLE_ITEM_ID);
        String itemPrefix = attributes.getValue(COVERABLE_ITEM_PREFIX);
        String internalId = attributes.getValue(INTERNAL_ID);
        CoverableItem coverableItem = this.builder.createCoverableItem(
                itemPrefix, itemId);

        RootTermStore store = new RootTermStore();

        store.coverableItem = coverableItem;
        store.internalId = internalId;

        this.rootTermStoreStack.push(store);

        this.currentLocationReciever.push(ELEMENT_ROOT_TERM);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementKeyword(Attributes attributes) {
        handleStartElementLocation(attributes);

        if (this.locations.size() != 1) {
            // This should never occur, since the locations are always
            // cleared after they were collected into a LocationList
            throw new RuntimeException("Illegal number of locations in storage");
        }

        Location keyword = this.locations.get(0);

        String currentReciever = this.currentLocationReciever.peek();

        if (currentReciever.equals(ELEMENT_LOOPING_STATEMENT)) {
            LoopStatementStore store = this.loopStatementStoreStack.peek();

            store.keyword = keyword;
        } else if (currentReciever.equals(ELEMENT_CONDITIONAL_STATEMENT)) {
            ConditionalStatementStore store = this.conditionalStatementStoreStack
                    .peek();

            store.keyword = keyword;
        }

        this.locations.clear();
    }

    /**
     * @param attributes
     */
    protected void handleStartElementBranch(Attributes attributes) {
        String itemId = attributes.getValue(COVERABLE_ITEM_ID);
        String itemPrefix = attributes.getValue(COVERABLE_ITEM_PREFIX);
        String internalId = attributes.getValue(INTERNAL_ID);
        CoverableItem coverableItem = this.builder.createCoverableItem(
                itemPrefix, itemId);

        Boolean implicit = Boolean.parseBoolean(attributes.getValue(IMPLICIT));

        BranchStore store = new BranchStore();

        store.coverableItem = coverableItem;
        store.internalId = internalId;
        store.implicit = implicit.booleanValue();

        this.branchStoreStack.push(store);

        this.currentLocationReciever.push(ELEMENT_BRANCH);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementConditionalStatement(Attributes attributes) {
        String itemId = attributes.getValue(COVERABLE_ITEM_ID);
        String itemPrefix = attributes.getValue(COVERABLE_ITEM_PREFIX);
        String internalId = attributes.getValue(INTERNAL_ID);
        CoverableItem coverableItem = this.builder.createCoverableItem(
                itemPrefix, itemId);

        ConditionalStatementStore store = new ConditionalStatementStore();

        store.coverableItem = coverableItem;
        store.internalId = internalId;

        this.conditionalStatementStoreStack.push(store);

        this.currentLocationReciever.push(ELEMENT_CONDITIONAL_STATEMENT);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementLoopStatement(Attributes attributes) {
        String itemId = attributes.getValue(COVERABLE_ITEM_ID);
        String itemPrefix = attributes.getValue(COVERABLE_ITEM_PREFIX);
        String internalId = attributes.getValue(INTERNAL_ID);
        CoverableItem coverableItem = this.builder.createCoverableItem(
                itemPrefix, itemId);

        String neverExecutedId = attributes.getValue(NEVER_EXECUTED_ID);
        String neverExecutedPrefix = attributes.getValue(NEVER_EXECUTED_PREFIX);
        CoverableItem neverExecutedItem = this.builder.createCoverableItem(
                neverExecutedPrefix, neverExecutedId);

        String onceExecutedId = attributes.getValue(ONCE_EXECUTED_ID);
        String onceExecutedPrefix = attributes.getValue(ONCE_EXECUTED_PREFIX);
        CoverableItem onceExecutedItem = this.builder.createCoverableItem(
                onceExecutedPrefix, onceExecutedId);

        String multipleExecutedId = attributes.getValue(MULTIPLE_EXECUTED_ID);
        String multipleExecutedPrefix = attributes
                .getValue(MULTIPLE_EXECUTED_PREFIX);
        CoverableItem multipleExecutedItem = this.builder.createCoverableItem(
                multipleExecutedPrefix, multipleExecutedId);

        Boolean optionalBodyExecution = Boolean.parseBoolean(attributes
                .getValue(OPTIONAL_BODY_EXECUTION));

        LoopStatementStore store = new LoopStatementStore();

        store.internalId = internalId;
        store.coverableItem = coverableItem;
        store.multipleExecutedItem = multipleExecutedItem;
        store.onceExecutedItem = onceExecutedItem;
        store.neverExecutedItem = neverExecutedItem;
        store.optionalBodyExecution = optionalBodyExecution;

        this.loopStatementStoreStack.push(store);

        this.currentLocationReciever.push(ELEMENT_LOOPING_STATEMENT);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementBasicStatement(Attributes attributes) {
        String itemId = attributes.getValue(COVERABLE_ITEM_ID);
        String itemPrefix = attributes.getValue(COVERABLE_ITEM_PREFIX);
        String internalId = attributes.getValue(INTERNAL_ID);
        CoverableItem coverableItem = this.builder.createCoverableItem(
                itemPrefix, itemId);

        this.basicStatementStore = new BasicStatementStore();

        this.basicStatementStore.coverableItem = coverableItem;
        this.basicStatementStore.internalId = internalId;

        this.currentLocationReciever.push(ELEMENT_BASIC_STATEMENT);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementStatementSequence(Attributes attributes) {
        String internalId = attributes.getValue(INTERNAL_ID);

        StatementSequenceStore sequenceStore = new StatementSequenceStore();
        sequenceStore.internalId = internalId;

        this.statementSequenceStoreStack.push(sequenceStore);

        this.currentLocationReciever.push(ELEMENT_STATEMENT_SEQUENCE);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementLocation(Attributes attributes) {
        String sourceFileId = attributes.getValue(SOURCE_FILE_ID);
        int startOffset = Integer.parseInt(attributes.getValue(START_OFFSET));
        int endOffset = Integer.parseInt(attributes.getValue(END_OFFSET));

        SourceFile file = this.idSourceFileMap.get(sourceFileId);

        // This should never happen, unless the file was modified illegally
        if (file == null) {
            throw new IllegalArgumentException("Location has no sourcefile");
        }

        Location location = this.builder.createLocation(file, startOffset,
                endOffset);

        this.locations.add(location);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementLocationList(Attributes attributes) {
        this.locations.clear();
    }

    /**
     * @param attributes
     */
    protected void handleStartElementHierarchyLevel(Attributes attributes) {
        String name = attributes.getValue(NAME);
        String id = attributes.getValue(HIERARCHY_LEVEL_ID);
        String hierarchyLevelTypeId = attributes
                .getValue(HIERARCHY_LEVEL_TYPE_ID);
        String internalId = attributes.getValue(INTERNAL_ID);

        HierarchyLevelStore store = new HierarchyLevelStore();

        store.id = id;
        store.name = name;
        store.type = this.idHierarchyLevelTypeMap.get(hierarchyLevelTypeId);
        store.internalId = internalId;

        this.hierarchyLevelStoreStack.push(store);

        this.currentLocationReciever.push(ELEMENT_HIERARCHY_LEVEL);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementHierarchyLevelType(Attributes attributes) {
        String internalId = attributes.getValue(INTERNAL_ID);
        String englishName = attributes.getValue(ENGLISH_NAME);
        String internalName = attributes.getValue(INTERNAL_NAME);

        HierarchyLevelType hierarchyLevelType = this.builder
                .createHierarchyLevelType(englishName, internalName);

        this.idHierarchyLevelTypeMap.put(internalId, hierarchyLevelType);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementBooleanOperator(Attributes attributes) {
        int arity = Integer.parseInt(attributes.getValue(ARITY));
        String name = attributes.getValue(NAME);
        String internalId = attributes.getValue(INTERNAL_ID);

        this.booleanOperatorStore = new BooleanOperatorStore();
        this.booleanOperatorStore.arity = arity;
        this.booleanOperatorStore.internalId = internalId;
        this.booleanOperatorStore.name = name;
    }

    /**
     * @param attributes
     */
    protected void handleStartElementBALMapEntry(Attributes attributes) {
        String encodedBooleanAssignment = attributes
                .getValue(BOOLEAN_ASSIGNMENT);

        BooleanAssignment booleanAssignment = TSCXMLHelper
                .decodeBooleanAssignment(encodedBooleanAssignment, getLogger());

        Long value = Long.parseLong(attributes.getValue(VALUE));

        this.currentAssignmentsListEntryStore.possibleAssignments.put(
                booleanAssignment, value);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementBABMapEntry(Attributes attributes) {
        String encodedBooleanAssignment = attributes
                .getValue(BOOLEAN_ASSIGNMENT);

        BooleanAssignment booleanAssignment = TSCXMLHelper
                .decodeBooleanAssignment(encodedBooleanAssignment, getLogger());

        Boolean value = Boolean.parseBoolean(attributes.getValue(VALUE));

        this.booleanOperatorStore.assignmentsMap.put(booleanAssignment, value);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementCriterion(Attributes attributes) {
        String extensionName = attributes.getValue(NAME);
        String pluginName = attributes.getValue(PLUGIN_NAME);

        if (pluginName == null) {
            // TODO: remove this branch at some time
            getLogger()
                    .warning(
                            "File contains criterion reference without plugin reference");
            pluginName = "org.codecover";
        }

        final Criterion criterion = PluginUtils.getExtensionObjectByName(
                this.pluginManager, getLogger(), Criterion.class, pluginName,
                extensionName);

        this.tscStore.criteriaList.add(criterion);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementSourceFile(Attributes attributes) {
        String content = attributes.getValue(CONTENT);
        String filename = attributes.getValue(FILENAME);
        String internalId = attributes.getValue(INTERNAL_ID);

        SourceFile sourceFile = this.builder
                .createSourceFile(filename, content);

        // Add the sourceFile to the store of the testSessionContainer.
        this.tscStore.sourceFiles.add(sourceFile);

        // Cache the sourcefile under its internal id, for later usage.
        this.idSourceFileMap.put(internalId, sourceFile);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementTestCase(Attributes attributes) {
        final String comment = attributes.getValue(COMMENT);
        final String name = attributes.getValue(NAME);
        final Date date = new Date(Long.parseLong(attributes.getValue(DATE)));

        this.currentTestCaseStore = new TestCaseStore();

        this.currentTestCaseStore.comment = comment;
        this.currentTestCaseStore.date = date;
        this.currentTestCaseStore.name = name;
    }

    /**
     * @param attributes
     */
    protected void handleStartElementTestSession(Attributes attributes) {
        final String comment = attributes.getValue(COMMENT);
        final String name = attributes.getValue(NAME);
        final Date date = new Date(Long.parseLong(attributes.getValue(DATE)));

        this.currentTestSession = this.testSessionContainer.createTestSession(
                name, comment, date);
    }

    /**
     * @param attributes
     */
    protected void handleStartElementTestSessionContainer(Attributes attributes) {
        final String namespaceURI = attributes.getValue(NAMESPACE_IDENTIFIER);
        final String versionString = attributes.getValue(VERSION);
        final String containerId = attributes
                .getValue(TEST_SESSION_CONTAINER_ID);
        final Date date = new Date(Long.parseLong(attributes.getValue(DATE)));

        if (namespaceURI == null) {
            throw new IllegalArgumentException("Expected namespace "
                    + "Attribute, but found none.");
        }

        if (!namespaceURI.equals(NAMESPACE_TEST_SESSION_CONTAINER)) {
            throw new IllegalArgumentException("Expected namespace "
                    + NAMESPACE_TEST_SESSION_CONTAINER + ", got "
                    + namespaceURI);
        }

        if (versionString == null) {
            throw new IllegalArgumentException("No version information");
        }
        final String[] versionParts = versionString.split("\\.");
        if (versionParts.length != 2) {
            throw new IllegalArgumentException("Version ('" + versionString
                    + "') is not of form <major>.<minor>");
        }
        final int major = Integer.parseInt(versionParts[0]);
        final int minor = Integer.parseInt(versionParts[1]);

        if (major != VERSION_MAJOR) {
            // Some day we might support multiple major versions here
            // (evtl. with different loader classes)
            throw new IllegalArgumentException(
                    "Unsupported major version (supported is " + VERSION_MAJOR
                            + "): " + major);
        }

        if (minor != VERSION_MINOR) {
            // We don't care about minor version right now
        }

        // Cache the data of the TestSessionContainer
        this.tscStore = new TestSessionContainerStore();
        this.tscStore.date = date;
        this.tscStore.id = containerId;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.xml.sax.helpers.DefaultHandler#endElement(java.lang.String,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public void endElement(String uri, String localName, String qName)
            throws SAXException {
        if (qName.equals(ELEMENT_HIERARCHY_LEVEL)) {
            handleEndElementHierarchyLevel();
            return;
        }

        if (qName.equals(ELEMENT_LOCATION_LIST)) {
            handleEndElementLocationList();
            return;
        }

        if (qName.equals(ELEMENT_HEADER)) {
            handleEndElementHeader();
            return;
        }

        if (qName.equals(ELEMENT_STATEMENT_SEQUENCE)) {
            handleEndElementStatementSequence();
            return;
        }

        if (qName.equals(ELEMENT_BASIC_STATEMENT)) {
            handleEndElementBasicStatement();
            return;
        }

        if (qName.equals(ELEMENT_LOOPING_STATEMENT)) {
            handleEndElementLoopStatement();
            return;
        }

        if (qName.equals(ELEMENT_CONDITIONAL_STATEMENT)) {
            handleEndElementConditionalStatement();
            return;
        }

        if (qName.equals(ELEMENT_ROOT_TERM)) {
            handleEndElementRootTerm();
            return;
        }

        if (qName.equals(ELEMENT_OPERATOR_TERM)) {
            handleEndElementOperatorTerm();
            return;
        }

        if (qName.equals(ELEMENT_BASIC_BOOLEAN_TERM)) {
            handleEndElementBasicBooleanTerm();
            return;
        }

        if (qName.equals(ELEMENT_BRANCH)) {
            handleEndElementBranch();
            return;
        }

        if (qName.equals(ELEMENT_BOOLEAN_OPERATOR)) {
            handleEndElementBooleanOperator();
            return;
        }
        if (qName.equals(ELEMENT_DECISION)) {
            handleEndElementDecision();
            return;
        }

        if (qName.equals(ELEMENT_MAST_ROOT)) {
            handleEndElementMASTRoot();
            return;
        }

        if (qName.equals(ELEMENT_QUESTIONMARKOPERATOR)) {
            handleEndElementQuestionMarkOperator();
            return;
        }

        if (qName.equals(ELEMENT_QUESTIONMARKOPERATOR_EXPRESSION)) {
            handleEndElementQuestionMarkOperatorExpression();
            return;
        }

        if (qName.equals(ELEMENT_SYNCHRONIZED_STATEMENT)) {
        	handleEndElementSynchronizedStatement();
            return;
        }
        
        
        if (qName.equals(ELEMENT_TEST_CASE)) {
            handleEndElementTestCase();
            return;
        }

        if (qName.equals(ELEMENT_TEST_SESSION)) {
            handleEndElementTestSession();
            return;
        }

        if (qName.equals(ELEMENT_ASSIGNMENT_LIST_ENTRY)) {
            handleEndElementAssignmentsListEntry();
            return;
        }

        if (qName.equals(ELEMENT_OBJECT_META_DATA_LIST_ENTRY)) {
            handleEndElementObjectMetaDataListEntry();
            return;
        }

        if (qName.equals(ELEMENT_META_DATA_LIST_ENTRY)) {
            handleEndElementMetaDataListEntry();
            return;
        }

    }

    protected void handleEndElementMASTRoot() {
        this.testSessionContainer = new TestSessionContainer(
                this.tscStore.code, getLogger(), this.tscStore.sourceFiles,
                new HashSet<Criterion>(this.tscStore.criteriaList),
                this.tscStore.id, this.tscStore.date);
    }

    protected void handleEndElementBooleanOperator() {
        BooleanOperator booleanOperator = this.builder.createBooleanOperator(
                this.booleanOperatorStore.arity,
                this.booleanOperatorStore.assignmentsMap,
                this.booleanOperatorStore.name);

        this.idBooleanOperatorMap.put(this.booleanOperatorStore.internalId,
                booleanOperator);
    }

    protected void handleEndElementHierarchyLevel() {
        HierarchyLevelStore store = this.hierarchyLevelStoreStack.pop();

        HierarchyLevel hierarchyLevel = this.builder.createHierarchyLevel(
                store.locationList, store.name, store.header, store.type,
                store.children, store.sequences, store.id);

        this.currentLocationReciever.pop();

        if (!this.hierarchyLevelStoreStack.isEmpty()) {
            HierarchyLevelStore parentStore = this.hierarchyLevelStoreStack
                    .peek();

            parentStore.children.add(hierarchyLevel);
        } else {
            this.tscStore.code = hierarchyLevel;
        }

        // Extract the internal id of the mast element and put them in a
        // map, for later usage in extracting the metadata of the test
        // sessions and test cases.
        this.idMetaDataObjectMap.put(store.internalId, hierarchyLevel);

    }

    protected void handleEndElementHeader() {
        LocationList header;
        if (this.locations.isEmpty()) {
            header = this.builder.createEmptyLocationList();
        } else {
            header = this.builder.createLocationList(this.locations);
        }

        HierarchyLevelStore store = this.hierarchyLevelStoreStack.peek();

        store.header = header;

        this.locations.clear();
    }

    protected void handleEndElementLocationList() {
        LocationList locationList;
        if (this.locations.isEmpty()) {
            locationList = this.builder.createEmptyLocationList();
        } else {
            locationList = this.builder.createLocationList(this.locations);
        }

        String currentReciever = this.currentLocationReciever.peek();

        if (currentReciever.equals(ELEMENT_HIERARCHY_LEVEL)) {
            HierarchyLevelStore store = this.hierarchyLevelStoreStack.peek();

            store.locationList = locationList;
        } else if (currentReciever.equals(ELEMENT_STATEMENT_SEQUENCE)) {
            StatementSequenceStore store = this.statementSequenceStoreStack
                    .peek();

            store.locationList = locationList;
        } else if (currentReciever.equals(ELEMENT_BASIC_STATEMENT)) {
            this.basicStatementStore.locationList = locationList;
        } else if (currentReciever.equals(ELEMENT_LOOPING_STATEMENT)) {
            LoopStatementStore store = this.loopStatementStoreStack.peek();

            store.locationList = locationList;
        } else if (currentReciever.equals(ELEMENT_CONDITIONAL_STATEMENT)) {
            ConditionalStatementStore store = this.conditionalStatementStoreStack
                    .peek();

            store.locationList = locationList;
        } else if (currentReciever.equals(ELEMENT_BRANCH)) {
            BranchStore store = this.branchStoreStack.peek();

            store.locationList = locationList;
        } else if (currentReciever.equals(ELEMENT_BASIC_BOOLEAN_TERM)) {
            BasicBooleanTermStore store = this.basicBooleanTermStoreStack
                    .peek();

            store.locationList = locationList;
        } else if (currentReciever.equals(ELEMENT_OPERATOR_TERM)) {
            OperatorTermStore store = this.operatorTermStoreStack.peek();

            store.locationList = locationList;
        } else if (currentReciever.equals(ELEMENT_QUESTIONMARKOPERATOR)) {
            QuestionMarkOperatorStore store = this.questionMarkOperatorStoreStack.peek();

            store.locationList = locationList;
      
        } else if (currentReciever.equals(ELEMENT_QUESTIONMARKOPERATOR_EXPRESSION)) {
        
	    	if(this.questionMarkOperatorExpression2 == null) {
	    		this.questionMarkOperatorExpression1.locationList = locationList;
	    	} else {
	    		this.questionMarkOperatorExpression2.locationList = locationList;    		
	    	}
        } else if (currentReciever.equals(ELEMENT_SYNCHRONIZED_STATEMENT)) {
            this.synchronizedStatementStore.locationList = locationList;
        }
        this.locations.clear();
    }

    protected void handleEndElementStatementSequence() {
        StatementSequenceStore sequenceStore = this.statementSequenceStoreStack
                .pop();

        StatementSequence sequence = this.builder.createStatementSequence(
                sequenceStore.locationList, sequenceStore.statements);

        this.currentLocationReciever.pop();

        String currentReciever = this.currentLocationReciever.peek();

        if (currentReciever.equals(ELEMENT_BRANCH)) {
            BranchStore store = this.branchStoreStack.peek();

            store.statementSequence = sequence;
        } else if (currentReciever.equals(ELEMENT_LOOPING_STATEMENT)) {
            LoopStatementStore store = this.loopStatementStoreStack.peek();

            store.statementSequence = sequence;
        } else if (currentReciever.equals(ELEMENT_HIERARCHY_LEVEL)) {
            HierarchyLevelStore store = this.hierarchyLevelStoreStack.peek();

            store.sequences.add(sequence);
        }

        // Extract the internal id of the mast element and put them in a
        // map, for later usage in extracting the metadata of the test
        // sessions and test cases.
        this.idMetaDataObjectMap.put(sequenceStore.internalId, sequence);

    }

    protected void handleEndElementBasicStatement() {
        BasicStatement statement = this.builder.createBasicStatement(
                this.basicStatementStore.locationList,
                this.basicStatementStore.coverableItem,
                this.basicStatementStore.terms,
                questionMarkOperators);

        StatementSequenceStore sequenceStore = this.statementSequenceStoreStack
                .peek();

        this.currentLocationReciever.pop();

        sequenceStore.statements.add(statement);

        // Extract the internal id of the mast element and put them in a
        // map, for later usage in extracting the metadata of the test
        // sessions and test cases.
        this.idMetaDataObjectMap.put(this.basicStatementStore.internalId,
                statement);

    }
    
    protected void handleEndElementQuestionMarkOperator() {
    	    	
    	QuestionMarkOperatorStore store = this.questionMarkOperatorStoreStack.pop();
    	
    	QuestionMarkOperatorExpression expr1 = new QuestionMarkOperatorExpression(this.questionMarkOperatorExpression1.locationList, this.questionMarkOperatorExpression1.coverableItem);
    	QuestionMarkOperatorExpression expr2 = new QuestionMarkOperatorExpression(this.questionMarkOperatorExpression2.locationList, this.questionMarkOperatorExpression2.coverableItem);
    	QuestionMarkOperator questionMarkOperator = new QuestionMarkOperator(store.locationList, store.coverableItem, expr1, expr2);
    	
    	questionMarkOperators.add(questionMarkOperator); // reset in teh Statement Constructor        

        // Extract the internal id of the mast element and put them in a
        // map, for later usage in extracting the metadata of the test
        // sessions and test cases.
        this.idQuestionMarkOperatorMap.put(store.internalId, questionMarkOperator);

        this.idMetaDataObjectMap.put(store.internalId, questionMarkOperator);        
        this.currentLocationReciever.pop();
        
        // reset the both expressions
        this.questionMarkOperatorExpression1 = null;
        this.questionMarkOperatorExpression2 = null;
    }
    
    protected void handleEndElementQuestionMarkOperatorExpression() {
    	
    	
        this.currentLocationReciever.pop();
    }

    protected void handleEndElementSynchronizedStatement() {
    	
      	SynchronizedStatement synchronizedStatement = 
    		new SynchronizedStatement(synchronizedStatementStore.locationList, 
    				synchronizedStatementStore.coverableItem,
    				synchronizedStatementStore.locationList.getLocations().get(0),
    				synchronizedStatementStore.coverableItem0,
    				synchronizedStatementStore.coverableItem1,
    				synchronizedStatementStore.coverableItem2, questionMarkOperators);
 	
        this.currentLocationReciever.pop();
    			
        StatementSequenceStore sequenceStore = this.statementSequenceStoreStack
        .peek();

        sequenceStore.statements.add(synchronizedStatement);
   			
    }

    protected void handleEndElementLoopStatement() {
        LoopStatementStore store = this.loopStatementStoreStack.pop();

        LoopingStatement statement = this.builder.createLoopingStatement(
                store.locationList, store.coverableItem, store.terms,
                store.statementSequence, store.keyword,
                store.neverExecutedItem, store.onceExecutedItem,
                store.multipleExecutedItem, store.optionalBodyExecution, questionMarkOperators);

        this.currentLocationReciever.pop();

        StatementSequenceStore sequenceStore = this.statementSequenceStoreStack
                .peek();

        sequenceStore.statements.add(statement);

        // Extract the internal id of the mast element and put them in a
        // map, for later usage in extracting the metadata of the test
        // sessions and test cases.
        this.idMetaDataObjectMap.put(store.internalId, statement);

    }

    protected void handleEndElementConditionalStatement() {
        ConditionalStatementStore store = this.conditionalStatementStoreStack
                .pop();

        ConditionalStatement statement = this.builder
                .createConditionalStatement(store.locationList,
                        store.coverableItem, store.terms, store.branches,
                        store.keyword, questionMarkOperators);

        this.currentLocationReciever.pop();

        StatementSequenceStore sequenceStore = this.statementSequenceStoreStack
                .peek();

        sequenceStore.statements.add(statement);

        // Extract the internal id of the mast element and put them in a
        // map, for later usage in extracting the metadata of the test
        // sessions and test cases.
        this.idMetaDataObjectMap.put(store.internalId, statement);

    }

    protected void handleEndElementBranch() {
        BranchStore store = this.branchStoreStack.pop();

        Branch branch = this.builder.createBranch(store.locationList,
                store.coverableItem, store.implicit, store.decision,
                store.statementSequence);

        this.currentLocationReciever.pop();

        ConditionalStatementStore conditionalStatementStore = this.conditionalStatementStoreStack
                .peek();

        conditionalStatementStore.branches.add(branch);

        // Extract the internal id of the mast element and put them in a
        // map, for later usage in extracting the metadata of the test
        // sessions and test cases.
        this.idMetaDataObjectMap.put(store.internalId, branch);

    }

    protected void handleEndElementDecision() {
        LocationList decision;
        if (this.locations.isEmpty()) {
            decision = this.builder.createEmptyLocationList();
        } else {
            decision = this.builder.createLocationList(this.locations);
        }

        BranchStore store = this.branchStoreStack.peek();

        store.decision = decision;

        this.locations.clear();
    }

    protected void handleEndElementRootTerm() {
        RootTermStore termStore = this.rootTermStoreStack.pop();

        RootTerm rootTerm = this.builder.createRootTerm(termStore.term,
                termStore.coverableItem);

        this.currentLocationReciever.pop();

        String currentReciever = this.currentLocationReciever.peek();

        if (currentReciever.equals(ELEMENT_CONDITIONAL_STATEMENT)) {
            ConditionalStatementStore store = this.conditionalStatementStoreStack
                    .peek();

            store.terms.add(rootTerm);
        } else if (currentReciever.equals(ELEMENT_LOOPING_STATEMENT)) {
            LoopStatementStore store = this.loopStatementStoreStack.peek();

            store.terms.add(rootTerm);
        } else if (currentReciever.equals(ELEMENT_BASIC_STATEMENT)) {
            BasicStatementStore store = this.basicStatementStore;

            store.terms.add(rootTerm);
        }

        // Extract the internal id of the mast element and put them in a
        // map, for later usage in extracting the metadata of the test
        // sessions and test cases.
        this.idMetaDataObjectMap.put(termStore.internalId, rootTerm);
        this.idRootTermMap.put(termStore.internalId, rootTerm);

    }

    protected void handleEndElementOperatorTerm() {
        OperatorTermStore termStore = this.operatorTermStoreStack.pop();

        OperatorTerm operatorTerm = this.builder.createOperatorTerm(
                termStore.locationList, termStore.operator, termStore.operands);

        this.currentLocationReciever.pop();

        // If the stack is empty, it means there are no operatorTerms on a
        // higher level than the current operatorTerm. So this operatorTerm
        // can be added to the current rootTerm.
        if (this.operatorTermStoreStack.isEmpty()) {
            RootTermStore rootTermStore = this.rootTermStoreStack.peek();

            rootTermStore.term = operatorTerm;
        } else {
            OperatorTermStore parentStore = this.operatorTermStoreStack.peek();

            parentStore.operands.add(operatorTerm);
        }

        // Extract the internal id of the mast element and put them in a
        // map, for later usage in extracting the metadata of the test
        // sessions and test cases.
        this.idMetaDataObjectMap.put(termStore.internalId, operatorTerm);

    }

    protected void handleEndElementBasicBooleanTerm() {
        BasicBooleanTermStore termStore = this.basicBooleanTermStoreStack.pop();

        BasicBooleanTerm basicTerm = this.builder
                .createBasicBooleanTerm(termStore.locationList);

        this.currentLocationReciever.pop();

        // If the stack is empty, it means there are no operatorTerms on a
        // higher level than the current BasicBooleanTerm. So this
        // BasicBooleanTerm can be added to the current rootTerm.
        if (this.operatorTermStoreStack.isEmpty()) {
            RootTermStore rootTermStore = this.rootTermStoreStack.peek();

            rootTermStore.term = basicTerm;
        } else {
            OperatorTermStore parentStore = this.operatorTermStoreStack.peek();

            parentStore.operands.add(basicTerm);
        }

        // Extract the internal id of the mast element and put them in a
        // map, for later usage in extracting the metadata of the test
        // sessions and test cases.
        this.idMetaDataObjectMap.put(termStore.internalId, basicTerm);

    }

    protected void handleEndElementTestSession() {
        for (MetaDataListEntryStore store : this.metaDataStack) {
            this.currentTestSession.setMetaData(store.name, store.metadata);
        }

        this.metaDataStack.clear();
    }

    protected void handleEndElementTestCase() {
        TestCase testCase = this.currentTestSession.createTestCase(
                this.currentTestCaseStore.name,
                this.currentTestCaseStore.comment,
                this.currentTestCaseStore.date,
                this.currentTestCaseStore.coverageData,
                this.currentTestCaseStore.assignments);

        for (ObjectMetaDataListEntryStore store : this.objectMetaDataStack) {
            testCase.setObjectMetaData(store.name, store.metaDataObject,
                    store.metadata);
        }

        this.objectMetaDataStack.clear();

        for (MetaDataListEntryStore store : this.metaDataStack) {
            testCase.setMetaData(store.name, store.metadata);
        }

        this.metaDataStack.clear();

        // We set it to null, so that the destination for the metadata later
        // can be determined.
        this.currentTestCaseStore = null;
    }

    protected void handleEndElementAssignmentsListEntry() {
        AssignmentsListEntryStore store = this.currentAssignmentsListEntryStore;

        BooleanAssignmentMap map = new BooleanAssignmentMap(store.length,
                store.possibleAssignments);

        this.currentTestCaseStore.assignments.put(store.coverableItem, map);
    }

    protected void handleEndElementObjectMetaDataListEntry() {
        this.currentLocationReciever.pop();
    }

    protected void handleEndElementMetaDataListEntry() {
        this.currentLocationReciever.pop();
    }

    @Override
    public TestSessionContainer getTestSessionContainer() {
        return this.testSessionContainer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.codecover.model.XMLNames2_0#getMajorVersion()
     */
    public int getMajorVersion() {
        return VERSION_MAJOR;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.codecover.model.XMLNames2_0#getMinorVersion()
     */
    public int getMinorVersion() {
        return VERSION_MINOR;
    }
}
