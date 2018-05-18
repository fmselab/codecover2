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

package org.codecover.report;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import junit.framework.TestCase;

import org.codecover.model.utils.Logger;
import org.codecover.report.exceptions.TemplateException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Tests the class <code>org.codecover.report.Template</code>.
 *
 * @author Robert Hanussek
 * @version 1.0 ($Id: TemplateTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class TemplateTest extends TestCase {

    private Template template;

    private Document templateDocument;

    private Element templateElement;

    private static final String TEMPL_DOCUMENT_VERSION = Template.READER_VERSION;
    private static final String TEMPL_TEMPLATE_VERSION = "1";
    private static final String TEMPL_NAME_EN = "Dummy report generator";
    private static final String TEMPL_NAME_DE = "Report generator Attrappe";
    private static final String TEMPL_DESC_EN
            = "Dummy report generator for testing purposes.";
    private static final String TEMPL_DESC_DE
            = "Dummy report generator zum Testen.";

    private static final String DUMMYREPORTGEN_PATH
            = "org.codecover.report.dummy.DummyReportGenerator";

    private static final String DUMMYPLUGIN
            = "org.codecover.report.dummy";

    protected void setUp() throws ParserConfigurationException,
    TemplateException {
        // set this.templateDocument and this.templateElement
        createDummyTemplate();
        this.template = new Template(Logger.NULL);
        this.template.setDocument(this.templateDocument);
        this.template.read();
    }

    protected void tearDown() throws Exception {

    }

    public void testGetDocument() {
        assertEquals(this.templateDocument, this.template.getDocument());
    }

    public void testGetTemplate() {
        assertEquals(this.templateElement, this.template.getTemplate());
    }

    public void testGetReportGeneratorName() {
        assertEquals(TemplateTest.DUMMYREPORTGEN_PATH,
                this.template.getReportGeneratorName());
    }

    public void testGetDocumentVersion() {
        assertEquals(TemplateTest.TEMPL_DOCUMENT_VERSION,
                this.template.getDocumentVersion());
    }

    public void testGetTemplateVersion() {
        assertEquals(TemplateTest.TEMPL_TEMPLATE_VERSION,
                this.template.getTemplateVersion());
    }

    public void testGetName() {
        assertEquals(TemplateTest.TEMPL_NAME_DE,
                this.template.getName("de",false));
   }

    public void testGetEnglishName() {
        assertEquals(TemplateTest.TEMPL_NAME_EN,
                this.template.getEnglishName());
    }

    public void testGetDescription() {
        assertEquals(TemplateTest.TEMPL_DESC_DE,
                this.template.getDescription("de",false));
    }

    public void testGetEnglishDescription() {
        assertEquals(TemplateTest.TEMPL_DESC_EN,
                this.template.getEnglishDescription());
    }

    /**
     * Creates a dummy template for testing purpose of course.
     *
     * @see #testGenerateReport()
     */
    private void createDummyTemplate() throws ParserConfigurationException {
        DocumentBuilderFactory docBFac = DocumentBuilderFactory.newInstance();
        DocumentBuilder docBuild = docBFac.newDocumentBuilder();
        Document template = docBuild.newDocument();
        Element repElemChild;
        // create root element (<report ...>-tag)
        Element reportElement = template.createElementNS(
                Template.REPORT_NAMESPACE, Template.REPORT_ELEMENT);
        reportElement.setAttribute(Template.REPORT_VERSION_ATTR,
                TemplateTest.TEMPL_DOCUMENT_VERSION);
        template.appendChild(reportElement);
        // create generator element
        repElemChild = template.createElementNS(
                Template.REPORT_NAMESPACE, Template.GENERATOR_ELEMENT);
        repElemChild.setTextContent(TemplateTest.DUMMYREPORTGEN_PATH);
        reportElement.appendChild(repElemChild);
        // create plugin element
        repElemChild = template.createElementNS(
                Template.REPORT_NAMESPACE, Template.PLUGIN_ELEMENT);
        repElemChild.setTextContent(TemplateTest.DUMMYPLUGIN);
        reportElement.appendChild(repElemChild);
        // create name element
        repElemChild = template.createElementNS(Template.REPORT_NAMESPACE,
                Template.NAME_ELEMENT);
        repElemChild.setAttributeNS(Template.XML_NAMESPACE,
                Template.NAME_LANG_ATTR, "en");
        repElemChild.setTextContent(TemplateTest.TEMPL_NAME_EN);
        reportElement.appendChild(repElemChild);
        // create name element
        repElemChild = template.createElementNS(Template.REPORT_NAMESPACE,
                Template.NAME_ELEMENT);
        repElemChild.setAttributeNS(Template.XML_NAMESPACE,
                Template.NAME_LANG_ATTR, "de");
        repElemChild.setTextContent(TemplateTest.TEMPL_NAME_DE);
        reportElement.appendChild(repElemChild);
        // create description element
        repElemChild = template.createElementNS(Template.REPORT_NAMESPACE,
                Template.DESC_ELEMENT);
        repElemChild.setAttributeNS(Template.XML_NAMESPACE,
                Template.DESC_LANG_ATTR, "en");
        repElemChild.setTextContent(TemplateTest.TEMPL_DESC_EN);
        reportElement.appendChild(repElemChild);
        // create description element
        repElemChild = template.createElementNS(Template.REPORT_NAMESPACE,
                Template.DESC_ELEMENT);
        repElemChild.setAttributeNS(Template.XML_NAMESPACE,
                Template.DESC_LANG_ATTR, "de");
        repElemChild.setTextContent(TemplateTest.TEMPL_DESC_DE);
        reportElement.appendChild(repElemChild);
        // create template element
        repElemChild = null;
        this.templateElement = template.createElement(Template.TEMPLATE_ELEMENT);
        this.templateElement.setAttribute(Template.TEMPLATE_VERSION_ATTR,
                TemplateTest.TEMPL_TEMPLATE_VERSION);
        reportElement.appendChild(this.templateElement);

        this.templateDocument = template;
    }
}
