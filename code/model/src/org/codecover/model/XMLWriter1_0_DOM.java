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
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Map;
import java.util.Stack;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.codecover.model.utils.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

/**
 * This class deals with the writing of the data of the given
 * {@link TestSessionContainer} into a xml-file.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: XMLWriter1_0_DOM.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
class XMLWriter1_0_DOM extends XMLWriter1_0_Base {

    private Stack<Element> elementStack = new Stack<Element>();

    private Document xmlDoc;

    XMLWriter1_0_DOM(Logger logger) {
        super(logger);
    }

    /**
     * Writes the data contained in the given {@link TestSessionContainer} to
     * the file represented by the given filename
     * 
     * @param filename
     *            the filename representing the file in which the data is to be
     *            stored
     * @param testSessionContainer
     *            the {@link TestSessionContainer} containing the to be saved
     *            data
     * @throws ParserConfigurationException
     * @throws TransformerException
     * @throws IOException
     * @throws SAXException
     */
    void writeFile(String filename, TestSessionContainer testSessionContainer)
            throws ParserConfigurationException, IOException,
            TransformerException, SAXException {
        writeFile(new File(filename), testSessionContainer);
    }

    /**
     * Writes the data contained in the given {@link TestSessionContainer} to
     * the given file
     * 
     * @param file
     *            the file in which the data is to be stored
     * @param testSessionContainer
     *            the testSessionContainer containing the data to be saved
     * @throws ParserConfigurationException
     * @throws TransformerException
     * @throws IOException
     * @throws SAXException
     */
    void writeFile(File file, TestSessionContainer testSessionContainer)
            throws ParserConfigurationException, IOException,
            TransformerException, SAXException {
        Document xmlDoc = null;
        DocumentBuilderFactory docBFac;
        DocumentBuilder docBuild;

        docBFac = DocumentBuilderFactory.newInstance();
        docBFac.setNamespaceAware(true);
        docBuild = docBFac.newDocumentBuilder();
        xmlDoc = docBuild.newDocument();

        if (xmlDoc != null) {
            this.xmlDoc = xmlDoc;

            createTestSessionContainerElement(testSessionContainer);
        }

        FileOutputStream out = new FileOutputStream(file);
        try {
            DOMSource domSource = new DOMSource(xmlDoc);
            StreamResult streamResult = new StreamResult(out);
            TransformerFactory tf = TransformerFactory.newInstance();
            Transformer serializer = tf.newTransformer();
            serializer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
            serializer.setOutputProperty(OutputKeys.INDENT, "yes");
            serializer.transform(domSource, streamResult);
            out.flush();
        } finally {
            out.close();
        }
    }

    // /**
    // * Creates dom element for {@link TestCase}s
    // *
    // * @param testCase
    // * the given test case
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the model
    // */
    // private Element createTestCaseElement(TestCase testCase, Document xmlDoc)
    // {
    //
    // if (testCase == null) {
    // throw new NullPointerException("testCase == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element testCaseElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_TEST_CASE);
    //
    // testCaseElement.setAttribute(NAME, testCase.getName());
    // testCaseElement.setAttribute(COMMENT, testCase.getComment());
    // testCaseElement.setAttribute(DATE, Long.toString(testCase.getDate()
    // .getTime()));
    //
    // // Create and append the list of coverage data of the test case
    // Element coverageListElement = createCoverageListElement(testCase
    // .getCoverageData(), xmlDoc);
    //
    // testCaseElement.appendChild(coverageListElement);
    //
    // // Create and apped the list of assignments of the rootterms of the test
    // // case
    // Element assigmentListElement = createAssignmentListElement(testCase
    // .getAssignmentsMap(), xmlDoc);
    //
    // testCaseElement.appendChild(assigmentListElement);
    //
    // // Create and append the list of objectMetaData associated with this
    // // test case
    // Element objectMetaDataListElement = createObjectMetaDataListElement(
    // testCase.getObjectMetaDataMapEntries(), xmlDoc);
    //
    // testCaseElement.appendChild(objectMetaDataListElement);
    //
    // // Create and append the list of metaData associated with this test case
    // Element metaDataListElement = createMetaDataListElement(testCase
    // .getMetaDataMapEntries(), xmlDoc);
    //
    // testCaseElement.appendChild(metaDataListElement);
    //
    // return testCaseElement;
    // }
    //
    // /**
    // * Creates dom element for a map of {@link RootTerm}s to another map of
    // * {@link BooleanAssignment}s and {@link Boolean}s
    // *
    // * @param assignmentsMap
    // * the map to be saved
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given map
    // */
    // private Element createAssignmentListElement(
    // Map<CoverableItem, BooleanAssignmentMap> assignmentsMap,
    // Document xmlDoc) {
    // if (assignmentsMap == null) {
    // throw new NullPointerException("assignmentsMapEntries == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    // Element assignmentListElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_ASSIGNMENT_LIST);
    //
    // // Create and append an element for each root term, which itself
    // // contains the map entries of its corresponding "Map<BooleanAssignment,
    // // Boolean>"
    // Map<String, Map<String, BooleanAssignmentMap>> convertedEntry =
    // convertMap(assignmentsMap);
    //
    // for (Entry<String, Map<String, BooleanAssignmentMap>> entry :
    // convertedEntry
    // .entrySet()) {
    //
    // Element prefixElement = createAssignmentPrefixElement(entry
    // .getKey(), entry.getValue(), xmlDoc);
    //
    // assignmentListElement.appendChild(prefixElement);
    //
    // }
    //
    // return assignmentListElement;
    // }
    //
    // private Element createAssignmentPrefixElement(String prefix,
    // Map<String, BooleanAssignmentMap> map, Document xmlDoc) {
    // Element prefixElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_ASSIGNMENT_PREFIX);
    //
    // prefixElement.setAttribute(ROOT_TERM_COVERABLE_ITEM_PREFIX, prefix);
    //
    // for (Map.Entry<String, BooleanAssignmentMap> entry : map.entrySet()) {
    // String rootTermId = entry.getKey();
    //
    // Element assignmentListEntryElement =
    // createBooleanAssignmentLongMapListElement(
    // ELEMENT_ASSIGNMENT_LIST_ENTRY, entry.getValue().getData()
    // .entrySet(), xmlDoc);
    //
    // assignmentListEntryElement.setAttribute(LENGTH, Integer
    // .toString(entry.getValue().getLength()));
    //
    // // Set the id of the root term to identify the correct term
    // // during the load of the saved file
    // assignmentListEntryElement.setAttribute(
    // ROOT_TERM_COVERABLE_ITEM_ID, rootTermId);
    //
    // prefixElement.appendChild(assignmentListEntryElement);
    //
    // }
    //
    // return prefixElement;
    // }
    //
    // /**
    // * Creates dom element for the list of coverage counters.
    // *
    // * @param coverageData
    // * the given set containg the id and the value of the counter
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the model
    // */
    // private Element createCoverageListElement(
    // Map<CoverableItem, Long> coverageData, Document xmlDoc) {
    //
    // if (coverageData == null) {
    // throw new NullPointerException("coverageData == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element coverageListElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_COVERAGE_LIST);
    //
    // Map<String, Map<String, Long>> convertedMap = convertMap(coverageData);
    //
    // for (Map.Entry<String, Map<String, Long>> entry : convertedMap
    // .entrySet()) {
    // Element prefixElement = createCoveragePrefixElement(entry.getKey(),
    // entry.getValue(), xmlDoc);
    //
    // coverageListElement.appendChild(prefixElement);
    // }
    //
    // return coverageListElement;
    // }
    //
    // /**
    // * Creates dom element for the prefix of a {@link CoverableItem} and
    // appends
    // * all the counters with the given prefix to the element.
    // *
    // * @param prefix
    // * the prefix
    // * @param map
    // * the map holding the counter ids and the counter values.
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element
    // */
    // private Element createCoveragePrefixElement(String prefix,
    // Map<String, Long> map, Document xmlDoc) {
    // Element prefixElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_COVERAGE_PREFIX);
    //
    // prefixElement.setAttribute(COVERABLE_ITEM_PREFIX, prefix);
    //
    // for (Entry<String, Long> entry : map.entrySet()) {
    // // Only create an element, if the counter value isn't 0.
    // if (entry.getValue().longValue() != 0L) {
    // Element coverageElement = createCoverageElement(entry.getKey(),
    // entry.getValue(), xmlDoc);
    //
    // prefixElement.appendChild(coverageElement);
    // }
    // }
    //
    // return prefixElement;
    // }
    //
    // /**
    // * Creates dom element for lists of {@link MetaData}
    // *
    // * @param metaDataMapEntries
    // * the given entryset containing the object used as
    // * {@link MetaData}, as well as the keys used to map them
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createMetaDataListElement(
    // Set<Entry<String, Object>> metaDataMapEntries, Document xmlDoc) {
    //
    // if (metaDataMapEntries == null) {
    // throw new NullPointerException("metaDataMapEntries == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element metaDataListElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_META_DATA_LIST);
    //
    // for (Entry<String, Object> entry : metaDataMapEntries) {
    // if (entry.getValue() != null) {
    // Element metaDataListEntryElement = createMetaDataListEntryElement(
    // entry.getKey(), entry.getValue(), xmlDoc);
    //
    // metaDataListElement.appendChild(metaDataListEntryElement);
    // }
    // }
    //
    // return metaDataListElement;
    // }
    //
    // /**
    // * Creates dom element for an entry of a list of {@link MetaData}
    // *
    // * @param name
    // * the name used to map the {@link MetaData}
    // * @param value
    // * the given object used as {@link MetaData}
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createMetaDataListEntryElement(String name, Object value,
    // Document xmlDoc) {
    //
    // if (name == null) {
    // throw new NullPointerException("name == null");
    // }
    //
    // if (value == null) {
    // throw new NullPointerException("value == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element metaDataListEntryElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_META_DATA_LIST_ENTRY);
    //
    // metaDataListEntryElement.setAttribute(NAME, name);
    //
    // // Create and append element for the meta data to be saved.
    // Element metaDataElement = createMetaDataElement(value, xmlDoc);
    //
    // if (metaDataElement != null) {
    // metaDataListEntryElement.appendChild(metaDataElement);
    // }
    //
    // return metaDataListEntryElement;
    // }
    //
    // /**
    // * Creates dom element for lists of {@link MetaData} associated with
    // * {@link MetaDataObject}s
    // *
    // * @param objectMetaDataMap
    // * the map containing the name, with which the {@link MetaData}
    // * is mapped, as well as a reference to the
    // * {@link MetaDataObject} the {@link MetaData} is associated
    // * with.
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createObjectMetaDataListElement(
    // Map<String, Map<Long, Object>> objectMetaDataMap, Document xmlDoc) {
    //
    // if (objectMetaDataMap == null) {
    // throw new NullPointerException("objectMetaDataMapEntries == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element objectMetaDataListElement = xmlDoc
    // .createElementNS(NAMESPACE_TEST_SESSION_CONTAINER,
    // ELEMENT_OBJECT_META_DATA_LIST);
    //
    // for (Entry<String, Map<Long, Object>> entry : objectMetaDataMap
    // .entrySet()) {
    //
    // for (Map.Entry<Long, Object> subEntry : entry.getValue().entrySet()) {
    //
    // if (subEntry != null) {
    // Element objectMetaDataListEntryElement =
    // createObjectMetaDataListEntryElement(
    // entry.getKey(), this.metaDataObjectIdMap
    // .get(subEntry.getKey()), subEntry
    // .getValue(), xmlDoc);
    //
    // objectMetaDataListElement
    // .appendChild(objectMetaDataListEntryElement);
    // }
    // }
    // }
    //
    // return objectMetaDataListElement;
    // }
    //
    // /**
    // * Creates dom element for an entry of a list of {@link MetaData}
    // associated
    // * with {@link MetaDataObject}s
    // *
    // * @param name
    // * the name used to map the {@link MetaData}
    // * @param id
    // * the id used to reference the {@link MetaDataObject} the
    // * {@link MetaData} is associated with
    // * @param value
    // * the given object used as {@link MetaData}
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createObjectMetaDataListEntryElement(String name,
    // String id, Object value, Document xmlDoc) {
    //
    // if (name == null) {
    // throw new NullPointerException("name == null");
    // }
    //
    // if (id == null) {
    // throw new NullPointerException("id == null");
    // }
    //
    // if (value == null) {
    // throw new NullPointerException("value == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element objectMetaDataListEntryElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER,
    // ELEMENT_OBJECT_META_DATA_LIST_ENTRY);
    //
    // objectMetaDataListEntryElement.setAttribute(NAME, name);
    // objectMetaDataListEntryElement.setAttribute(META_DATA_OBJECT_ID, id);
    //
    // // Create and append element for the meta data to be saved.
    // Element metaDataElement = createMetaDataElement(value, xmlDoc);
    //
    // if (metaDataElement != null) {
    // objectMetaDataListEntryElement.appendChild(metaDataElement);
    // }
    //
    // return objectMetaDataListEntryElement;
    // }
    //
    // /**
    // * Creates dom element for {@link Object}s used as {@link MetaData} in an
    // * {@link MetaDataProvider}
    // *
    // * @param value
    // * the given object used as {@link MetaData}
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createMetaDataElement(Object value, Document xmlDoc) {
    //
    // if (value == null) {
    // throw new NullPointerException("value == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // final Element metaDataElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_META_DATA);
    //
    // final String base64 = TSCXMLHelper.encodeMetaData(value, getLogger());
    //
    // metaDataElement.setAttribute(BASE64, base64);
    //
    // return metaDataElement;
    // }
    //
    // /**
    // * Creates dom element for a counter, with its id and value.
    // *
    // * @param coverableItemId
    // * the id of the counter
    // * @param value
    // * the value of the counter
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // counter
    // */
    // private Element createCoverageElement(String coverableItemId, Long value,
    // Document xmlDoc) {
    //
    // if (coverableItemId == null) {
    // throw new NullPointerException("coverableItemId == null");
    // }
    //
    // if (value == null) {
    // throw new NullPointerException("value == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element coverageElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_COVERAGE);
    //
    // coverageElement.setAttribute(COVERABLE_ITEM_ID, coverableItemId);
    // coverageElement.setAttribute(VALUE, Long.toString(value));
    //
    // return coverageElement;
    // }
    //
    // /**
    // * Creates dom element for {@link TestSession}s
    // *
    // * @param testSession
    // * the given test session
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the model
    // */
    // private Element createTestSessionElement(TestSession testSession,
    // Document xmlDoc) {
    //
    // if (testSession == null) {
    // throw new NullPointerException("testSession == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element testSessionElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_TEST_SESSION);
    //
    // testSessionElement.setAttribute(NAME, testSession.getName());
    // testSessionElement.setAttribute(COMMENT, testSession.getComment());
    // testSessionElement.setAttribute(DATE, Long.toString(testSession
    // .getDate().getTime()));
    //
    // for (TestCase testCase : testSession.getTestCases()) {
    // Element testCaseElement = createTestCaseElement(testCase, xmlDoc);
    //
    // testSessionElement.appendChild(testCaseElement);
    // }
    //
    // // Create and append the list of metaData associated with this test
    // // session
    // Element metaDataListElement = createMetaDataListElement(testSession
    // .getMetaDataMapEntries(), xmlDoc);
    //
    // testSessionElement.appendChild(metaDataListElement);
    //
    // return testSessionElement;
    // }
    //
    // /**
    // * Creates dom element for the mast
    // *
    // * @param code
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createMastElement(HierarchyLevel code, Document xmlDoc) {
    //
    // if (code == null) {
    // throw new NullPointerException("code == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element mastRootElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_MAST_ROOT);
    //
    // if (code != null) {
    // Element topHierarchyLevelElement = createHierarchyLevelElement(
    // code, xmlDoc);
    //
    // mastRootElement.appendChild(topHierarchyLevelElement);
    // }
    //
    // return mastRootElement;
    // }
    //
    // /**
    // * Creates dom element for {@link HierarchyLevel}s
    // *
    // * @param level
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createHierarchyLevelElement(HierarchyLevel level,
    // Document xmlDoc) {
    //
    // if (level == null) {
    // throw new NullPointerException("level == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element hierarchyLevelElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_HIERARCHY_LEVEL);
    //
    // // Generate and set an id for the current mast element, for later usage
    // // in saving the meta data of the test sessions and test cases.
    // String internalId = generateID();
    // this.metaDataObjectIdMap.put(Internal
    // .getMetaDataId(level.getMetaData()), internalId);
    // hierarchyLevelElement.setAttribute(INTERNAL_ID, internalId);
    //
    // hierarchyLevelElement.setAttribute(NAME, level.getName());
    //
    // hierarchyLevelElement.setAttribute(HIERARCHY_LEVEL_ID, level.getId());
    //
    // String hierarchyLevelTypeId;
    // if (this.hierarchyLevelTypeIdMap.containsKey(level.getType())) {
    // hierarchyLevelTypeId = this.hierarchyLevelTypeIdMap.get(level
    // .getType());
    //
    // } else {
    // hierarchyLevelTypeId = generateID();
    // this.hierarchyLevelTypeIdMap.put(level.getType(),
    // hierarchyLevelTypeId);
    // }
    //
    // hierarchyLevelElement.setAttribute(HIERARCHY_LEVEL_TYPE_ID,
    // hierarchyLevelTypeId);
    //
    // // Create and append header element
    // Element locationListHeaderElement = createLocationListElement(
    // ELEMENT_HEADER, level.getHeader(), xmlDoc);
    //
    // hierarchyLevelElement.appendChild(locationListHeaderElement);
    //
    // // Create and append location list element
    // Element locationListElement = createLocationListElement(
    // ELEMENT_LOCATION_LIST, level.getLocation(), xmlDoc);
    //
    // hierarchyLevelElement.appendChild(locationListElement);
    //
    // // Create and append all the statement sequences of the given
    // // hierarchyLevel
    // for (StatementSequence sequence : level.getSequences()) {
    // Element statementSequenceElement = createStatementSequenceElement(
    // sequence, xmlDoc);
    //
    // hierarchyLevelElement.appendChild(statementSequenceElement);
    // }
    //
    // // Call this method recursively to create and append all child hierarchy
    // // levels.
    // for (HierarchyLevel childLevel : level.getChildren()) {
    // Element childElement = createHierarchyLevelElement(childLevel,
    // xmlDoc);
    //
    // hierarchyLevelElement.appendChild(childElement);
    // }
    //
    // return hierarchyLevelElement;
    //
    // }
    //
    // /**
    // * Creates dom element for {@link LocationList}s
    // *
    // * @param name
    // * the name used in naming the element
    // * @param locationList
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createLocationListElement(String name,
    // LocationList locationList, Document xmlDoc) {
    //
    // if (name == null) {
    // throw new NullPointerException("name == null");
    // }
    //
    // if (locationList == null) {
    // throw new NullPointerException("locationList == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element locationListElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, name);
    //
    // for (Location location : locationList.getLocations()) {
    // Element locationElement = createLocationElement(ELEMENT_LOCATION,
    // location, xmlDoc);
    //
    // locationListElement.appendChild(locationElement);
    // }
    //
    // return locationListElement;
    // }
    //
    // /**
    // * Creates dom element for {@link Location}s
    // *
    // * @param name
    // * the name used in naming the element
    // * @param location
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createLocationElement(String name, Location location,
    // Document xmlDoc) {
    //
    // if (name == null) {
    // throw new NullPointerException("name == null");
    // }
    //
    // if (location == null) {
    // throw new NullPointerException("location == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element locationElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, name);
    //
    // locationElement.setAttribute(START_OFFSET, Integer.toString(location
    // .getStartOffset()));
    // locationElement.setAttribute(END_OFFSET, Integer.toString(location
    // .getEndOffset()));
    // locationElement.setAttribute(SOURCE_FILE_ID, this.sourceFileIdMap
    // .get(location.getFile()));
    // return locationElement;
    // }
    //
    // /**
    // * Creates a dom element for a list of {@link HierarchyLevelType}s
    // *
    // * @param map
    // * the Map containing the {@link HierarchyLevelType} and their
    // * ids
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createHierarchyLevelTypeListElement(
    // Map<HierarchyLevelType, String> map, Document xmlDoc) {
    // if (map == null) {
    // throw new NullPointerException("map == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element hierarchyLevelTypeListElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER,
    // ELEMENT_HIERARCHY_LEVEL_TYPE_LIST);
    //
    // for (Entry<HierarchyLevelType, String> entry : map.entrySet()) {
    // Element hierarchyLevelTypeElement = createHierarchyLevelTypeElement(
    // entry.getKey(), entry.getValue(), xmlDoc);
    // hierarchyLevelTypeListElement
    // .appendChild(hierarchyLevelTypeElement);
    // }
    //
    // return hierarchyLevelTypeListElement;
    // }
    //
    // /**
    // * Creates dom element for {@link HierarchyLevelType}s
    // *
    // * @param type
    // * the given element of the mast
    // * @param id
    // * the id of the {@link HierarchyLevelType}
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createHierarchyLevelTypeElement(HierarchyLevelType type,
    // String id, Document xmlDoc) {
    //
    // if (type == null) {
    // throw new NullPointerException("type == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element hierarchyLevelTypeElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_HIERARCHY_LEVEL_TYPE);
    //
    // hierarchyLevelTypeElement.setAttribute(INTERNAL_ID, id);
    // hierarchyLevelTypeElement.setAttribute(INTERNAL_NAME, type
    // .getInternalName());
    // hierarchyLevelTypeElement.setAttribute(ENGLISH_NAME, type
    // .getEnglishName());
    //
    // return hierarchyLevelTypeElement;
    // }
    //
    // /**
    // * Creates dom element for {@link StatementSequence}s
    // *
    // * @param statementSequence
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createStatementSequenceElement(
    // StatementSequence statementSequence, Document xmlDoc) {
    //
    // if (statementSequence == null) {
    // throw new NullPointerException("statementSequence == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element statementSequenceElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_STATEMENT_SEQUENCE);
    //
    // // Generate and set an id for the current mast element, for later usage
    // // in saving the meta data of the test sessions and test cases.
    // String internalId = generateID();
    // this.metaDataObjectIdMap.put(Internal.getMetaDataId(statementSequence
    // .getMetaData()), internalId);
    // statementSequenceElement.setAttribute(INTERNAL_ID, internalId);
    //
    // // Create and append location list element
    // Element locationListElement = this.createLocationListElement(
    // ELEMENT_LOCATION_LIST, statementSequence.getLocation(), xmlDoc);
    //
    // statementSequenceElement.appendChild(locationListElement);
    //
    // // Create and append all the statements in sequence.
    // for (Statement statement : statementSequence.getStatements()) {
    // Element statementElement = createStatementElement(statement, xmlDoc);
    //
    // // the statement element should never be null, since a statement
    // // must be one of the above choices, but better safe than sorry.
    // if (statementElement != null) {
    // statementSequenceElement.appendChild(statementElement);
    // }
    // }
    //
    // return statementSequenceElement;
    // }
    //
    // /**
    // * Creates dom element for {@link Statement}s. The element is filled with
    // * the data common to all {@link Statement}s and then passed to a method
    // * for each one of the subclasses of {@link Statement}, to assign the
    // * specific data
    // *
    // * @param statement
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createStatementElement(Statement statement, Document
    // xmlDoc) {
    //
    // if (statement == null) {
    // throw new NullPointerException("statement == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element statementElement = null;
    //
    // // Call the respective method for each type of statement.
    // if (statement instanceof BasicStatement) {
    // statementElement = createBasicStatementElement(
    // (BasicStatement) statement, xmlDoc);
    // } else if (statement instanceof ComplexStatement) {
    // statementElement = createComplexStatementElement(
    // (ComplexStatement) statement, xmlDoc);
    // }
    //
    // // Generate and set an id for the current mast element, for later usage
    // // in saving the meta data of the test sessions and test cases.
    // String internalId = generateID();
    // this.metaDataObjectIdMap.put(Internal.getMetaDataId(statement
    // .getMetaData()), internalId);
    // statementElement.setAttribute(INTERNAL_ID, internalId);
    //
    // // set the id of the coverable item as an attribute.
    // statementElement.setAttribute(COVERABLE_ITEM_ID, statement
    // .getCoverableItem().getId());
    //
    // // set the prefix of the coverable item as an attribute.
    // statementElement.setAttribute(COVERABLE_ITEM_PREFIX, statement
    // .getCoverableItem().getPrefix());
    //
    // // Create and append location list element
    // Element locationListElement = createLocationListElement(
    // ELEMENT_LOCATION_LIST, statement.getLocation(), xmlDoc);
    //
    // statementElement.appendChild(locationListElement);
    //
    // // Create and append all the root terms of the statement
    // for (RootTerm rootTerm : statement.getTerms()) {
    // Element rootTermElement = createRootTermElement(rootTerm, xmlDoc);
    //
    // statementElement.appendChild(rootTermElement);
    // }
    //
    // return statementElement;
    // }
    //
    // /**
    // * Creates dom element for {@link ComplexStatement}s.The element is filled
    // * with the data common to all {@link ComplexStatement}s and then passed
    // to
    // * a method for each one of the subclasses of {@link ComplexStatement}, to
    // * assign the specific data
    // *
    // * @param complexStatement
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createComplexStatementElement(
    // ComplexStatement complexStatement, Document xmlDoc) {
    //
    // if (complexStatement == null) {
    // throw new NullPointerException("complexStatement == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element complexStatementElement = null;
    //
    // if (complexStatement instanceof LoopingStatement) {
    // complexStatementElement = createLoopingStatementElement(
    // (LoopingStatement) complexStatement, xmlDoc);
    // } else if (complexStatement instanceof ConditionalStatement) {
    // complexStatementElement = createConditionalStatementElement(
    // (ConditionalStatement) complexStatement, xmlDoc);
    // }
    //
    // // Create and append keyword element
    // Element keywordElement = createLocationElement(ELEMENT_KEYWORD,
    // complexStatement.getKeyword(), xmlDoc);
    //
    // complexStatementElement.appendChild(keywordElement);
    //
    // return complexStatementElement;
    // }
    //
    // /**
    // * Creates dom element for {@link ConditionalStatement}s
    // *
    // * @param conditionalStatement
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createConditionalStatementElement(
    // ConditionalStatement conditionalStatement, Document xmlDoc) {
    //
    // if (conditionalStatement == null) {
    // throw new NullPointerException("conditionalStatement == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element conditionalStatementElement = xmlDoc
    // .createElementNS(NAMESPACE_TEST_SESSION_CONTAINER,
    // ELEMENT_CONDITIONAL_STATEMENT);
    //
    // // Create and append all the branches of the statement.
    // for (Branch branch : conditionalStatement.getBranches()) {
    // Element branchElement = createBranchElement(branch, xmlDoc);
    //
    // conditionalStatementElement.appendChild(branchElement);
    // }
    //
    // return conditionalStatementElement;
    // }
    //
    // /**
    // * Creates dom element for {@link Branch}es
    // *
    // * @param branch
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createBranchElement(Branch branch, Document xmlDoc) {
    //
    // if (branch == null) {
    // throw new NullPointerException("branch == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element branchElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_BRANCH);
    //
    // // Generate and set an id for the current mast element, for later usage
    // // in saving the meta data of the test sessions and test cases.
    // String internalId = generateID();
    // this.metaDataObjectIdMap.put(Internal.getMetaDataId(branch
    // .getMetaData()), internalId);
    // branchElement.setAttribute(INTERNAL_ID, internalId);
    //
    // // set the id of the coverable item as an attribute.
    // branchElement.setAttribute(COVERABLE_ITEM_ID, branch.getCoverableItem()
    // .getId());
    //
    // // set the prefix of the coverable item as an attribute.
    // branchElement.setAttribute(COVERABLE_ITEM_PREFIX, branch
    // .getCoverableItem().getPrefix());
    //
    // branchElement.setAttribute(IMPLICIT, Boolean.toString(branch
    // .isImplicit()));
    //
    // // Create and append decision element
    // Element decisionElement = createLocationListElement(ELEMENT_DECISION,
    // branch.getDecision(), xmlDoc);
    //
    // branchElement.appendChild(decisionElement);
    //
    // // Create and append location list element
    // Element locationListElement = createLocationListElement(
    // ELEMENT_LOCATION_LIST, branch.getLocation(), xmlDoc);
    //
    // branchElement.appendChild(locationListElement);
    //
    // // Create and append the statement sequence element
    // Element statementSequenceElement = createStatementSequenceElement(
    // branch.getSequence(), xmlDoc);
    //
    // branchElement.appendChild(statementSequenceElement);
    //
    // return branchElement;
    // }
    //
    // /**
    // * Creates dom element for {@link LoopingStatement}s
    // *
    // * @param loopingStatement
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createLoopingStatementElement(
    // LoopingStatement loopingStatement, Document xmlDoc) {
    //
    // if (loopingStatement == null) {
    // throw new NullPointerException("loopingStatement == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element loopingStatementElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_LOOPING_STATEMENT);
    //
    // loopingStatementElement.setAttribute(MULTIPLE_EXECUTED_ID,
    // loopingStatement.getMultipleExecutedItem().getId());
    // loopingStatementElement.setAttribute(MULTIPLE_EXECUTED_PREFIX,
    // loopingStatement.getMultipleExecutedItem().getPrefix());
    // loopingStatementElement.setAttribute(NEVER_EXECUTED_ID,
    // loopingStatement.getNeverExecutedItem().getId());
    // loopingStatementElement.setAttribute(NEVER_EXECUTED_PREFIX,
    // loopingStatement.getNeverExecutedItem().getPrefix());
    // loopingStatementElement.setAttribute(ONCE_EXECUTED_ID, loopingStatement
    // .getOnceExecutedItem().getId());
    // loopingStatementElement.setAttribute(ONCE_EXECUTED_PREFIX,
    // loopingStatement.getOnceExecutedItem().getPrefix());
    // loopingStatementElement.setAttribute(OPTIONAL_BODY_EXECUTION, Boolean
    // .toString(loopingStatement.isOptionalBodyExecution()));
    //
    // // Create and append the statement sequence element
    // Element statementSequenceElement = createStatementSequenceElement(
    // loopingStatement.getBody(), xmlDoc);
    //
    // loopingStatementElement.appendChild(statementSequenceElement);
    //
    // return loopingStatementElement;
    // }
    //
    // /**
    // * Creates dom element for {@link BasicStatement}s
    // *
    // * @param basicStatement
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createBasicStatementElement(BasicStatement
    // basicStatement,
    // Document xmlDoc) {
    //
    // if (basicStatement == null) {
    // throw new NullPointerException("basicStatement == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element basicStatementElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_BASIC_STATEMENT);
    //
    // return basicStatementElement;
    // }
    //
    // /**
    // * Creates dom element for {@link RootTerm}s
    // *
    // * @param rootTerm
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createRootTermElement(RootTerm rootTerm, Document xmlDoc)
    // {
    //
    // if (rootTerm == null) {
    // throw new NullPointerException("rootTerm == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element rootTermElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_ROOT_TERM);
    //
    // // Generate and set an id for the current mast element, for later usage
    // // in saving the meta data of the test sessions and test cases.
    // String internalId = generateID();
    // this.metaDataObjectIdMap.put(Internal.getMetaDataId(rootTerm
    // .getMetaData()), internalId);
    // this.rootTermIdMap.put(rootTerm, internalId);
    // rootTermElement.setAttribute(INTERNAL_ID, internalId);
    //
    // // set the id of the coverable item as an attribute.
    // rootTermElement.setAttribute(COVERABLE_ITEM_ID, rootTerm
    // .getCoverableItem().getId());
    //
    // // set the prefix of the coverable item as an attribute.
    // rootTermElement.setAttribute(COVERABLE_ITEM_PREFIX, rootTerm
    // .getCoverableItem().getPrefix());
    //
    // Element booleanTermElement = createBooleanTermElement(rootTerm
    // .getTerm(), xmlDoc);
    //
    // // the boolean term element should never be null, since a boolean term
    // // must be one of the above choices, but better safe than sorry.
    // if (booleanTermElement != null) {
    // rootTermElement.appendChild(booleanTermElement);
    // }
    //
    // return rootTermElement;
    // }
    //
    // /**
    // * Creates dom element for {@link BooleanTerm}s
    // *
    // * @param booleanTerm
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createBooleanTermElement(BooleanTerm booleanTerm,
    // Document xmlDoc) {
    //
    // if (booleanTerm == null) {
    // throw new NullPointerException("booleanTerm == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element booleanTermElement = null;
    //
    // // Call the respective method for each type of boolean term.
    // if (booleanTerm instanceof BasicBooleanTerm) {
    // booleanTermElement = createBasicBooleanTermElement(
    // (BasicBooleanTerm) booleanTerm, xmlDoc);
    // } else if (booleanTerm instanceof OperatorTerm) {
    // booleanTermElement = createOperatorTermElement(
    // (OperatorTerm) booleanTerm, xmlDoc);
    // }
    //
    // return booleanTermElement;
    // }
    //
    // /**
    // * Creates dom element for {@link OperatorTerm}s
    // *
    // * @param operatorTerm
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createOperatorTermElement(OperatorTerm operatorTerm,
    // Document xmlDoc) {
    //
    // if (operatorTerm == null) {
    // throw new NullPointerException("operatorTerm == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element operatorTermElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_OPERATOR_TERM);
    //
    // // Generate and set an id for the current mast element, for later usage
    // // in saving the meta data of the test sessions and test cases.
    // String internalId = generateID();
    // this.metaDataObjectIdMap.put(Internal.getMetaDataId(operatorTerm
    // .getMetaData()), internalId);
    // operatorTermElement.setAttribute(INTERNAL_ID, internalId);
    //
    // // Create and append location list element
    // Element locationListElement = createLocationListElement(
    // ELEMENT_LOCATION_LIST, operatorTerm.getLocation(), xmlDoc);
    //
    // operatorTermElement.appendChild(locationListElement);
    //
    // String operatorId;
    // if (this.operatorIdMap.containsKey(operatorTerm.getOperator())) {
    // operatorId = this.operatorIdMap.get(operatorTerm.getOperator());
    //
    // } else {
    // operatorId = generateID();
    // this.operatorIdMap.put(operatorTerm.getOperator(), operatorId);
    // }
    //
    // operatorTermElement.setAttribute(BOOLEAN_OPERATOR_ID, operatorId);
    //
    // // Create and append all the operand terms of the operator term
    // for (BooleanTerm operands : operatorTerm.getOperands()) {
    // Element booleanTermElement = createBooleanTermElement(operands,
    // xmlDoc);
    //
    // operatorTermElement.appendChild(booleanTermElement);
    // }
    //
    // return operatorTermElement;
    // }
    //
    // /**
    // * Creates dom element for {@link BooleanOperator}s
    // *
    // * @param booleanOperator
    // * the given element of the mast
    // * @param id
    // * the id used for referenceing the operator in the remaining
    // * document, in order to optimise space usage
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createBooleanOperatorElement(
    // BooleanOperator booleanOperator, String id, Document xmlDoc) {
    //
    // if (booleanOperator == null) {
    // throw new NullPointerException("booleanOperator == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // if (id == null) {
    // throw new NullPointerException("id == null");
    // }
    //
    // Element booleanOperatorElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_BOOLEAN_OPERATOR);
    //
    // booleanOperatorElement.setAttribute(INTERNAL_ID, id);
    //
    // booleanOperatorElement.setAttribute(ARITY, Integer
    // .toString(booleanOperator.getArity()));
    // booleanOperatorElement.setAttribute(NAME, booleanOperator.getName());
    //
    // Element booleanAssignmentBooleanMapListElement =
    // createBooleanAssignmentBooleanMapListElement(
    // ELEMENT_BOOLEAN_ASSIGNMENT_BOOLEAN_MAP_LIST, booleanOperator
    // .getPossibleAssignments().entrySet(), xmlDoc);
    //
    // booleanOperatorElement
    // .appendChild(booleanAssignmentBooleanMapListElement);
    //
    // return booleanOperatorElement;
    // }
    //
    // /**
    // * Creates dom element for a map of {@link BooleanAssignment}s and
    // * {@link Boolean}s
    // *
    // * @param name
    // * the name of the tag
    // * @param mapEntries
    // * the entries of the map
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createBooleanAssignmentBooleanMapListElement(String name,
    // Set<Map.Entry<BooleanAssignment, Boolean>> mapEntries,
    // Document xmlDoc) {
    //
    // if (name == null) {
    // throw new NullPointerException("name == null");
    // }
    //
    // if (mapEntries == null) {
    // throw new NullPointerException("mapEntries == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element booleanAssignmentBooleanMapListElement = xmlDoc
    // .createElementNS(NAMESPACE_TEST_SESSION_CONTAINER, name);
    //
    // for (Map.Entry<BooleanAssignment, Boolean> entry : mapEntries) {
    // Element mapEntry = createBooleanAssignmentStringMapListEntryElement(
    // entry.getKey(), entry.getValue().toString(), xmlDoc);
    //
    // booleanAssignmentBooleanMapListElement.appendChild(mapEntry);
    // }
    //
    // return booleanAssignmentBooleanMapListElement;
    // }
    //
    // /**
    // * Creates dom element for an entry of a map containing
    // * {@link BooleanAssignment}s and {@link Boolean}s
    // *
    // * @param booleanAssignment
    // * the booleanAssignment to be encoded
    // * @param value
    // * the value as a String
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given map
    // */
    // private Element createBooleanAssignmentStringMapListEntryElement(
    // BooleanAssignment booleanAssignment, String value, Document xmlDoc) {
    //
    // if (booleanAssignment == null) {
    // throw new NullPointerException("booleanAssignment");
    // }
    // if (value == null) {
    // throw new NullPointerException("value");
    // }
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element mapEntryElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_MAP_ENTRY);
    //
    // mapEntryElement.setAttribute(BOOLEAN_ASSIGNMENT, TSCXMLHelper
    // .encodeBooleanAssignment(booleanAssignment, getLogger()));
    // mapEntryElement.setAttribute(VALUE, value);
    //
    // return mapEntryElement;
    // }
    //
    // /**
    // * Creates dom element for a map of {@link BooleanAssignment}s and
    // * {@link Long}s
    // *
    // * @param name
    // * the name of the tag
    // * @param mapEntries
    // * the entries of the map
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createBooleanAssignmentLongMapListElement(String name,
    // Set<Map.Entry<BooleanAssignment, Long>> mapEntries, Document xmlDoc) {
    //
    // if (name == null) {
    // throw new NullPointerException("name == null");
    // }
    //
    // if (mapEntries == null) {
    // throw new NullPointerException("mapEntries == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element booleanAssignmentLongMapListElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, name);
    //
    // for (Map.Entry<BooleanAssignment, Long> entry : mapEntries) {
    // Element mapEntry = createBooleanAssignmentStringMapListEntryElement(
    // entry.getKey(), entry.getValue().toString(), xmlDoc);
    //
    // booleanAssignmentLongMapListElement.appendChild(mapEntry);
    // }
    //
    // return booleanAssignmentLongMapListElement;
    // }
    //
    // /**
    // * Creates dom element for {@link BasicBooleanTerm}s
    // *
    // * @param basicBooleanTerm
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createBasicBooleanTermElement(
    // BasicBooleanTerm basicBooleanTerm, Document xmlDoc) {
    //
    // if (basicBooleanTerm == null) {
    // throw new NullPointerException("basicBooleanTerm == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element basicBooleanTermElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_BASIC_BOOLEAN_TERM);
    //
    // // Generate and set an id for the current mast element, for later usage
    // // in saving the meta data of the test sessions and test cases.
    // String internalId = generateID();
    // this.metaDataObjectIdMap.put(Internal.getMetaDataId(basicBooleanTerm
    // .getMetaData()), internalId);
    // basicBooleanTermElement.setAttribute(INTERNAL_ID, internalId);
    //
    // // Create and append location list element
    // Element locationListElement = createLocationListElement(
    // ELEMENT_LOCATION_LIST, basicBooleanTerm.getLocation(), xmlDoc);
    //
    // basicBooleanTermElement.appendChild(locationListElement);
    //
    // return basicBooleanTermElement;
    // }
    //
    // /**
    // * Creates dom element for {@link TestSessionContainer}s
    // *
    // * @param testSessionContainer
    // * the given element of the mast
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createTestSessionContainerElement(
    // TestSessionContainer testSessionContainer, Document xmlDoc) {
    //
    // if (testSessionContainer == null) {
    // throw new NullPointerException("testSessionContainer == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element testSessionContainerElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER,
    // ELEMENT_TEST_SESSION_CONTAINER);
    //
    // testSessionContainerElement.setAttribute(VERSION, VERSION_MAJOR + "."
    // + VERSION_MINOR);
    // testSessionContainerElement.setAttribute(DATE, Long
    // .toString(testSessionContainer.getDate().getTime()));
    // testSessionContainerElement.setAttribute(TEST_SESSION_CONTAINER_ID,
    // testSessionContainer.getId());
    //
    // // Create and append the list of source files associated with the
    // // testSessionContainer
    // Element sourceFileListElement = createSourceFileListElement(
    // testSessionContainer.getFiles(), xmlDoc);
    //
    // testSessionContainerElement.appendChild(sourceFileListElement);
    //
    // // Create and append the list of criteria associated with the
    // // testSessionContainer
    // Element criteriaListElement = createCriteriaListElement(
    // testSessionContainer.getCriteria(), xmlDoc);
    //
    // testSessionContainerElement.appendChild(criteriaListElement);
    //
    // // Create and append the element representing the mast
    // Element mastElement = createMastElement(testSessionContainer.getCode(),
    // xmlDoc);
    //
    // // The mast element tree must have been created before the boolean
    // // operators are appended, since the map of operators is filled during
    // // the creation of the mast element tree.
    // // Append all the BooleanOperators before the mast element
    //
    // Element booleanOperatorListElement = createBooleanOperatorListElement(
    // this.operatorIdMap, xmlDoc);
    //
    // testSessionContainerElement.appendChild(booleanOperatorListElement);
    //
    // // The mast element tree must have been created before the
    // // hierarchyLevelTypes are appended, since the map of
    // // hierarchyLevelTypes is filled during the creation of the mast element
    // // tree.
    // // Append all the HierarchyLevelTypes before the mast element
    // Element hierarchyLevelTypeListElement =
    // createHierarchyLevelTypeListElement(
    // this.hierarchyLevelTypeIdMap, xmlDoc);
    //
    // testSessionContainerElement.appendChild(hierarchyLevelTypeListElement);
    //
    // // Append mast element
    // testSessionContainerElement.appendChild(mastElement);
    //
    // // Create and append every testsession, with its testcases
    // // to the testSessionContainer
    // for (TestSession testSession : testSessionContainer.getTestSessions()) {
    // Element testSessionElement = createTestSessionElement(testSession,
    // xmlDoc);
    //
    // testSessionContainerElement.appendChild(testSessionElement);
    // }
    //
    // return testSessionContainerElement;
    // }
    //
    // /**
    // * Creates dom element for a list of {@link BooleanOperator}s
    // *
    // * @param map
    // * the map of {@link BooleanOperator}s and their ids
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given list
    // of
    // * {@link BooleanOperator}s
    // */
    // private Element createBooleanOperatorListElement(
    // Map<BooleanOperator, String> map, Document xmlDoc) {
    // Element booleanOperatorListElement = xmlDoc
    // .createElementNS(NAMESPACE_TEST_SESSION_CONTAINER,
    // ELEMENT_BOOLEAN_OPERATOR_LIST);
    // for (Map.Entry<BooleanOperator, String> entry : map.entrySet()) {
    //
    // Element booleanOperatorElement = createBooleanOperatorElement(entry
    // .getKey(), entry.getValue(), xmlDoc);
    //
    // booleanOperatorListElement.appendChild(booleanOperatorElement);
    // }
    // return booleanOperatorListElement;
    // }
    //
    // /**
    // * Creates dom element for a list of {@link SourceFile}s
    // *
    // * @param files
    // * the list of {@link SourceFile}s
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given list
    // of
    // * {@link SourceFile}s
    // */
    // private Element createSourceFileListElement(List<SourceFile> files,
    // Document xmlDoc) {
    // Element sourceFileListElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_SOURCE_FILE_LIST);
    // // Attach all source files of the testSessionContainer to the element
    // for (SourceFile sourceFile : files) {
    // // Generate an id to be used later in the document, to
    // // refer to this sourcefile.
    // String sourceFileId = generateID();
    // Element sourceFileElement = createSourceFileElement(sourceFile,
    // sourceFileId, xmlDoc);
    //
    // // For easy retrieval of the id of the sourcefile, it is
    // // put into a map.
    // this.sourceFileIdMap.put(sourceFile, sourceFileId);
    //
    // sourceFileListElement.appendChild(sourceFileElement);
    // }
    // return sourceFileListElement;
    // }
    //
    // /**
    // * Creates dom element for a list of {@link Criterion}s
    // *
    // * @param criteria
    // * the list of {@link Criterion}s
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given list
    // of
    // * {@link Criterion}s
    // */
    // private Element createCriteriaListElement(Set<Criterion> criteria,
    // Document xmlDoc) {
    // if (criteria == null) {
    // throw new NullPointerException("criteria == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element criteriaListElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_CRITERIA_LIST);
    //
    // for (Criterion criterion : criteria) {
    //
    // Element criteriaListEntryElement = createCriteriaListEntryElement(
    // criterion, xmlDoc);
    //
    // criteriaListElement.appendChild(criteriaListEntryElement);
    // }
    //
    // return criteriaListElement;
    // }
    //
    // /**
    // * Creates dom element for {@link Criterion}s
    // *
    // * @param criterion
    // * the given criterion.
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // * criterion
    // */
    // private Element createCriteriaListEntryElement(Criterion criterion,
    // Document xmlDoc) {
    // if (criterion == null) {
    // throw new NullPointerException("criterion == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element criteriaListEntryElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_CRITERIA_LIST_ENTRY);
    //
    // criteriaListEntryElement.setAttribute(NAME, criterion
    // .getExtensionName());
    //
    // criteriaListEntryElement.setAttribute(PLUGIN_NAME, criterion
    // .getPluginName());
    //
    // return criteriaListEntryElement;
    // }
    //
    // /**
    // * Creates dom element for {@link SourceFile}s
    // *
    // * @param sourceFile
    // * the given element of the mast
    // * @param id
    // * the id used for referenceing the {@link SourceFile} in the
    // * remaining document, in order to optimise space usage
    // * @param xmlDoc
    // * the document, which will contain the element
    // * @return the created element, which contains the data of the given
    // element
    // * of the mast
    // */
    // private Element createSourceFileElement(SourceFile sourceFile, String id,
    // Document xmlDoc) {
    //
    // if (sourceFile == null) {
    // throw new NullPointerException("sourceFile == null");
    // }
    //
    // if (xmlDoc == null) {
    // throw new NullPointerException("xmlDoc == null");
    // }
    //
    // Element sourceFileElement = xmlDoc.createElementNS(
    // NAMESPACE_TEST_SESSION_CONTAINER, ELEMENT_SOURCE_FILE);
    //
    // sourceFileElement.setAttribute(INTERNAL_ID, id);
    // sourceFileElement.setAttribute(FILENAME, sourceFile.getFileName());
    // sourceFileElement.setAttribute(CONTENT, sourceFile.getContent());
    //
    // return sourceFileElement;
    // }

    private Document getXMLDocument() {
        return this.xmlDoc;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.codecover.model.XMLWriter1_0_Base#endElement(java.lang.String)
     */
    @Override
    protected void endElement(String elementName) throws SAXException {
        Element currentElement = this.elementStack.pop();

        if (!this.elementStack.isEmpty()) {
            Element parentElement = this.elementStack.peek();
            parentElement.appendChild(currentElement);
        } else {
            getXMLDocument().appendChild(currentElement);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.codecover.model.XMLWriter1_0_Base#startElement(java.lang.String,
     *      java.util.Map)
     */
    @Override
    protected void startElement(String elementName,
            Map<String, String> attributes) throws SAXException {
        Element element = getXMLDocument().createElementNS(
                NAMESPACE_TEST_SESSION_CONTAINER, elementName);

        for (Map.Entry<String, String> entry : attributes.entrySet()) {
            String attributeName = entry.getKey();
            String attributeValue = entry.getValue();

            if (attributeName == null) {
                throw new NullPointerException("attributeName == null");
            }
            if (attributeValue == null) {
                throw new NullPointerException("attributeValue == null");
            }

            element.setAttribute(attributeName, attributeValue);
        }

        this.elementStack.push(element);
    }
}
