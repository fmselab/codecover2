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

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.codecover.model.utils.Logger;
import org.xml.sax.ContentHandler;
import org.xml.sax.DTDHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.AttributesImpl;

/**
 * This class deals with the writing of the data of the given
 * {@link TestSessionContainer} into a xml-file.
 *
 * @author Markus Wittlinger
 * @version 1.0 ($Id: XMLWriter1_0_SAX.java 64 2009-09-28 15:11:11Z ahija $)
 */
class XMLWriter1_0_SAX extends XMLWriter1_0_Base implements XMLReader {

    private ContentHandler contentHandler;

    private ErrorHandler errorHandler;

    private EntityResolver entityResolver;

    private DTDHandler dtdHandler;

    XMLWriter1_0_SAX(Logger logger) {
        super(logger);
    }

    @Override
    protected void startElement(String elementName,
            Map<String, String> attributes) throws SAXException {
        AttributesImpl atts = new AttributesImpl();

        for (Map.Entry<String, String> entry : attributes.entrySet()) {
            String attributeName = entry.getKey();
            String attributeValue = entry.getValue();

            if (attributeName == null) {
                throw new NullPointerException("attributeName == null");
            }
            if (attributeValue == null) {
                throw new NullPointerException("attributeValue == null");
            }

            atts.addAttribute(NAMESPACE_TEST_SESSION_CONTAINER, attributeName,
                    attributeName, "CDATA", attributeValue);
        }

        getContentHandler().startElement(NAMESPACE_TEST_SESSION_CONTAINER,
                elementName, elementName, atts);
    }

    @Override
    protected void endElement(String elementName) throws SAXException {
        getContentHandler().endElement(NAMESPACE_TEST_SESSION_CONTAINER,
                elementName, elementName);
    }

    protected static Map<String, String> getEmptyAttributes() {
        return Collections.emptyMap();
    }

    protected static Map<String, String> getNewAttributes() {
        return new HashMap<String, String>();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.xml.sax.XMLReader#getContentHandler()
     */
    public ContentHandler getContentHandler() {
        return this.contentHandler;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.xml.sax.XMLReader#getDTDHandler()
     */
    public DTDHandler getDTDHandler() {
        return this.dtdHandler;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.xml.sax.XMLReader#getEntityResolver()
     */
    public EntityResolver getEntityResolver() {
        return this.entityResolver;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.xml.sax.XMLReader#getErrorHandler()
     */
    public ErrorHandler getErrorHandler() {
        return this.errorHandler;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.xml.sax.XMLReader#getFeature(java.lang.String)
     */
    public boolean getFeature(String name) throws SAXNotRecognizedException,
            SAXNotSupportedException {
        return false;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.xml.sax.XMLReader#getProperty(java.lang.String)
     */
    public Object getProperty(String name) throws SAXNotRecognizedException,
            SAXNotSupportedException {
        return null;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.xml.sax.XMLReader#parse(org.xml.sax.InputSource)
     */
    public void parse(InputSource input) throws IOException, SAXException {
        if (!(input instanceof TestSessionContainerInputSource)) {
            throw new IllegalArgumentException(
                    "Input NOT instance of TestSessionContainerInputSource");
        }

        TestSessionContainer testSessionContainer = ((TestSessionContainerInputSource) input)
                .getTestSessionContainer();

        if (testSessionContainer == null) {
            throw new NullPointerException("testSessionContainer == null");
        }

        getContentHandler().startDocument();


        createTestSessionContainerElement(testSessionContainer);


        getContentHandler().endDocument();

    }

    /*
     * (non-Javadoc)
     *
     * @see org.xml.sax.XMLReader#parse(java.lang.String)
     */
    public void parse(String systemId) throws IOException, SAXException {
        /* Do nothing here. */
    }

    /*
     * (non-Javadoc)
     *
     * @see org.xml.sax.XMLReader#setContentHandler(org.xml.sax.ContentHandler)
     */
    public void setContentHandler(ContentHandler handler) {
        this.contentHandler = handler;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.xml.sax.XMLReader#setDTDHandler(org.xml.sax.DTDHandler)
     */
    public void setDTDHandler(DTDHandler handler) {
        this.dtdHandler = handler;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.xml.sax.XMLReader#setEntityResolver(org.xml.sax.EntityResolver)
     */
    public void setEntityResolver(EntityResolver resolver) {
        this.entityResolver = resolver;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.xml.sax.XMLReader#setErrorHandler(org.xml.sax.ErrorHandler)
     */
    public void setErrorHandler(ErrorHandler handler) {
        this.errorHandler = handler;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.xml.sax.XMLReader#setFeature(java.lang.String, boolean)
     */
    public void setFeature(String name, boolean value)
            throws SAXNotRecognizedException, SAXNotSupportedException {
        /* Do nothing here. */
    }

    /*
     * (non-Javadoc)
     *
     * @see org.xml.sax.XMLReader#setProperty(java.lang.String,
     *      java.lang.Object)
     */
    public void setProperty(String name, Object value)
            throws SAXNotRecognizedException, SAXNotSupportedException {
        /* Do nothing here. */
    }
}
