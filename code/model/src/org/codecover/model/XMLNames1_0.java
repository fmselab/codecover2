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

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: XMLNames1_0.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
interface XMLNames1_0 extends XMLNames {

    /**
     * 1
     */
    static final int VERSION_MAJOR = 1;

    /**
     * 0
     */
    static final int VERSION_MINOR = 0;

    /**
     * SrcFile
     */
    static final String ELEMENT_SOURCE_FILE = "SrcFile";

    /**
     * Filename
     */
    static final String FILENAME = "Filename";

    /**
     * Content
     */
    static final String CONTENT = "Content";

    /**
     * Intrnl_Id
     */
    static final String INTERNAL_ID = "Intrnl_Id";

    /**
     * MASTRoot
     */
    static final String ELEMENT_MAST_ROOT = "MASTRoot";

    /**
     * HierarchyLvl
     */
    static final String ELEMENT_HIERARCHY_LEVEL = "HierarchyLvl";

    /**
     * HierarchyLvlType
     */
    static final String ELEMENT_HIERARCHY_LEVEL_TYPE = "HierarchyLvlType";

    /**
     * IntrnlName
     */
    static final String INTERNAL_NAME = "IntrnlName";

    /**
     * EnglishName
     */
    static final String ENGLISH_NAME = "EnglishName";

    /**
     * Header
     */
    static final String ELEMENT_HEADER = "Header";

    /**
     * Loc
     */
    static final String ELEMENT_LOCATION = "Loc";

    /**
     * StartOffset
     */
    static final String START_OFFSET = "StartOffset";

    /**
     * EndOffset
     */
    static final String END_OFFSET = "EndOffset";

    /**
     * SrcFileId
     */
    static final String SOURCE_FILE_ID = "SrcFileId";

    /**
     * LocList
     */
    static final String ELEMENT_LOCATION_LIST = "LocList";

    /**
     * Name
     */
    static final String NAME = "Name";

    /**
     * StmntSeq
     */
    static final String ELEMENT_STATEMENT_SEQUENCE = "StmntSeq";

    /**
     * BasicStmnt
     */
    static final String ELEMENT_BASIC_STATEMENT = "BasicStmnt";

    /**
     * CovItemId
     */
    static final String COVERABLE_ITEM_ID = "CovItemId";

    /**
     * CovItemPrefix
     */
    static final String COVERABLE_ITEM_PREFIX = "CovItemPrefix";

    /**
     * RootTerm
     */
    static final String ELEMENT_ROOT_TERM = "RootTerm";

    /**
     * BasicBoolTerm
     */
    static final String ELEMENT_BASIC_BOOLEAN_TERM = "BasicBoolTerm";

    /**
     * OpTerm
     */
    static final String ELEMENT_OPERATOR_TERM = "OpTerm";

    /**
     * BoolOp
     */
    static final String ELEMENT_BOOLEAN_OPERATOR = "BoolOp";

    /**
     * ?
     */
    static final String ELEMENT_QUESTIONMARKOPERATOR = "QMO";

    /**
     * ?
     */
    static final String ELEMENT_QUESTIONMARKOPERATOR_EXPRESSION = "QMO_E";
    
    /**
     * Arity
     */
    static final String ARITY = "Arity";

    /**
     * MapEntry
     */
    static final String ELEMENT_MAP_ENTRY = "MapEntry";

    /**
     * Length
     */
    static final String LENGTH = "Length";

    /**
     * Value
     */
    static final String VALUE = "Value";

    /**
     * LoopStmnt
     */
    static final String ELEMENT_LOOPING_STATEMENT = "LoopStmnt";

    /**
     * MltplExecId
     */
    static final String MULTIPLE_EXECUTED_ID = "MltplExecId";

    /**
     * NvrExecId
     */
    static final String NEVER_EXECUTED_ID = "NvrExecId";

    /**
     * OnceExecutedId
     */
    static final String ONCE_EXECUTED_ID = "OnceExecutedId";

    /**
     * MltplExecPrefix
     */
    static final String MULTIPLE_EXECUTED_PREFIX = "MltplExecPrefix";

    /**
     * NvrExecPrefix
     */
    static final String NEVER_EXECUTED_PREFIX = "NvrExecPrefix";

    /**
     * OnceExecutedPrefix
     */
    static final String ONCE_EXECUTED_PREFIX = "OnceExecutedPrefix";

    /**
     * CondStmnt
     */
    static final String ELEMENT_CONDITIONAL_STATEMENT = "CondStmnt";

    
    /**
     * 
     */
    static final String ELEMENT_SYNCHRONIZED_STATEMENT = "SyncStmnt";
    
    
    /**
     * 
     */
    static final String ELEMENT_SYNCHRONIZED0 = "SyncStmnt0";
    static final String ELEMENT_SYNCHRONIZED1 = "SyncStmnt1";
    static final String ELEMENT_SYNCHRONIZED2 = "SyncStmnt2";

    /**
     * Branch
     */
    static final String ELEMENT_BRANCH = "Branch";

    /**
     * Cond
     */
    static final String ELEMENT_DECISION = "Cond";

    /**
     * TestSession
     */
    static final String ELEMENT_TEST_SESSION = "TestSession";

    /**
     * Comment
     */
    static final String COMMENT = "Comment";

    /**
     * TestCase
     */
    static final String ELEMENT_TEST_CASE = "TestCase";

    /**
     * Cov
     */
    static final String ELEMENT_COVERAGE = "Cov";

    /**
     * Keyword
     */
    static final String ELEMENT_KEYWORD = "Keyword";

    /**
     * OptBodyExec
     */
    static final String OPTIONAL_BODY_EXECUTION = "OptBodyExec";

    /**
     * Implct
     */
    static final String IMPLICIT = "Implct";

    /**
     * BooleanOpId
     */
    static final String BOOLEAN_OPERATOR_ID = "BooleanOpId";

    /**
     * ObjMetaDataList
     */
    static final String ELEMENT_OBJECT_META_DATA_LIST = "ObjMetaDataList";

    /**
     * ObjMetaDataListEntry
     */
    static final String ELEMENT_OBJECT_META_DATA_LIST_ENTRY = "ObjMetaDataListEntry";

    /**
     * MetaDataObjId
     */
    static final String META_DATA_OBJECT_ID = "MetaDataObjId";

    /**
     * MetaData
     */
    static final String ELEMENT_META_DATA = "MetaData";

    /**
     * MetaDataList
     */
    static final String ELEMENT_META_DATA_LIST = "MetaDataList";

    /**
     * MetaDataListEntry
     */
    static final String ELEMENT_META_DATA_LIST_ENTRY = "MetaDataListEntry";

    /**
     * Base64
     */
    static final String BASE64 = "Base64";

    /**
     * CovList
     */
    static final String ELEMENT_COVERAGE_LIST = "CovList";

    /**
     * AssgnmntList
     */
    static final String ELEMENT_ASSIGNMENT_LIST = "AssgnmntList";

    /**
     * BoolAssgnmntBoolMapList
     */
    static final String ELEMENT_BOOLEAN_ASSIGNMENT_BOOLEAN_MAP_LIST = "BoolAssgnmntBoolMapList";

    /**
     * AssgnmntListEntry
     */
    static final String ELEMENT_ASSIGNMENT_LIST_ENTRY = "AssgnmntListEntry";

    /**
     * RootTermCovItemId
     */
    static final String ROOT_TERM_COVERABLE_ITEM_ID = "RootTermCovItemId";

    /**
     * CritList
     */
    static final String ELEMENT_CRITERIA_LIST = "CritList";

    /**
     * CritListEntry
     */
    static final String ELEMENT_CRITERIA_LIST_ENTRY = "CritListEntry";

    /**
     * PluginName
     */
    static final String PLUGIN_NAME = "PluginName";

    /**
     * SrcFileList
     */
    static final String ELEMENT_SOURCE_FILE_LIST = "SrcFileList";

    /**
     * BoolOpList
     */
    static final String ELEMENT_BOOLEAN_OPERATOR_LIST = "BoolOpList";



    /**
     * HierarchyLvlTypeId
     */
    static final String HIERARCHY_LEVEL_TYPE_ID = "HierarchyLvlTypeId";

    /**
     * HierarchyLvlTypeList
     */
    static final String ELEMENT_HIERARCHY_LEVEL_TYPE_LIST = "HierarchyLvlTypeList";

    /**
     * BoolAssgnmnt
     */
    static final String BOOLEAN_ASSIGNMENT = "BoolAssgnmnt";

    /**
     * HierarchyLvlId
     */
    static final String HIERARCHY_LEVEL_ID = "HierarchyLvlId";

    /**
     * CovPrefix
     */
    static final String ELEMENT_COVERAGE_PREFIX = "CovPrefix";

    /**
     * AssgnmntPrefix
     */
    static final String ELEMENT_ASSIGNMENT_PREFIX = "AssgnmntPrefix";

    /**
     * RootTermCovItemPrefix
     */
    static final String ROOT_TERM_COVERABLE_ITEM_PREFIX = "RootTermCovItemPrefix";
}
