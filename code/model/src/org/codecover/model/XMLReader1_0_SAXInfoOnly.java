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

import org.codecover.model.utils.Logger;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 * This class represents a handler for a SAX Parser and subclasses the
 * {@link XMLReader1_0_SAX}. The handler skips the MAST of a
 * {@link TestSessionContainer} xml file and reads only the information about
 * {@link TestSession}s and {@link TestCase}s. This is done for performance
 * reason, in cases where the MAST is not required.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: XMLReader1_0_SAXInfoOnly.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
class XMLReader1_0_SAXInfoOnly extends XMLReader1_0_SAX {

    /**
     * Constructor
     * 
     * @param logger
     * @param builder
     */
    public XMLReader1_0_SAXInfoOnly(Logger logger, MASTBuilder builder) {
        super(logger, builder, null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.codecover.model.XMLReader1_0_SAX#startElement(java.lang.String,
     *      java.lang.String, java.lang.String, org.xml.sax.Attributes)
     */
    @Override
    public void startElement(String uri, String localName, String qName,
            Attributes attributes) throws SAXException {
        if (qName.equals(ELEMENT_TEST_SESSION_CONTAINER)) {
            handleStartElementTestSessionContainer(attributes);
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
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.codecover.model.XMLReader1_0_SAX#endElement(java.lang.String,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public void endElement(String uri, String localName, String qName)
            throws SAXException {
        if (qName.equals(ELEMENT_MAST_ROOT)) {
            handleEndElementMASTRoot();
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
    }
}
