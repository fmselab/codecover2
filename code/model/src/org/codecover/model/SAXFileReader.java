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
import java.io.InputStream;

import javax.xml.XMLConstants;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.sax.SAXSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * This class is to be used in the parsing of {@link TestSessionContainer} xml
 * files.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: SAXFileReader.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class SAXFileReader {

    /**
     * This method uses the given {@link XMLReaderBase} to parse the
     * {@link TestSessionContainer} xml file contained in the given
     * {@link InputStream} and return the created {@link TestSessionContainer}.
     * 
     * @param inputStream
     *            the given {@link InputStream} containing the xml file.
     * @param readerBase
     *            the SAX handler to be used in parsing the xml file.
     * @return the parsed {@link TestSessionContainer}
     * @throws ParserConfigurationException
     *             a {@link ParserConfigurationException}
     * @throws SAXException
     *             a {@link SAXException}
     * @throws IOException
     *             a {@link IOException}
     */
    public static TestSessionContainer parse(InputStream inputStream,
            XMLReaderBase readerBase) throws ParserConfigurationException,
            SAXException, IOException {
        final SAXParserFactory factory = SAXParserFactory.newInstance();

        final SchemaFactory schemaFactory = SchemaFactory
                .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);

        final InputStream schemaInputStream = SAXFileReader.class
                .getResourceAsStream("resources/testSessionContainerSchema.xsd");
        
        if (schemaInputStream == null) {
            throw new RuntimeException("Cannot find schema resource");
        }
        
        final Schema schema = schemaFactory.newSchema(new SAXSource(
                new InputSource(schemaInputStream)));

        factory.setSchema(schema);
        final SAXParser parser = factory.newSAXParser();

        parser.parse(inputStream, readerBase);

        return readerBase.getTestSessionContainer();
    }
}
