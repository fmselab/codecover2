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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.parsers.ParserConfigurationException;

import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.exceptions.FileLoadIOException;
import org.codecover.model.exceptions.FileVersionException;
import org.codecover.model.extensions.PluginManager;
import org.codecover.model.utils.Logger;
import org.xml.sax.SAXException;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: XML_Tests.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class XML_Tests extends DatabaseTest {

    /**
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     * @throws FileVersionException
     */
    public void testXMLResaveDOM1_0() throws SAXException, IOException,
            ParserConfigurationException, FileVersionException {
        new File("test").mkdir();

        final String filename = "test/testparserDOM1_0.xml";
        final String resaveFilename = "test/testparserDOM1_0Resaved.xml";

        Logger logger = this.logger;
        MASTBuilder mastBuilder = new MASTBuilder(logger);
        try {
            XMLWriter1_0_DOM testXMLWriter = new XMLWriter1_0_DOM(logger);
            testXMLWriter.writeFile(new File(filename),
                    this.testSessionContainer);
        } catch (Exception e) {
            logger.fatal("An error occurred during saving", e);
        }

        TestSessionContainer newTestSessionContainer;
        try {
            XMLReader1_0_SAX reader = new XMLReader1_0_SAX(logger, mastBuilder,
                    PluginManager.create());

            SAXFileReader.parse(getInputStream(new File(filename)), reader);

            newTestSessionContainer = reader.getTestSessionContainer();

            try {
                XMLWriter1_0_DOM testXMLWriter = new XMLWriter1_0_DOM(logger);
                testXMLWriter.writeFile(new File(resaveFilename),
                        newTestSessionContainer);
            } catch (Exception e) {
                logger.fatal("An error occurred during saving", e);
            }
        } catch (FileLoadException e) {
            logger.fatal("An error occurred during loading", e);
        }
    }

    /**
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     * @throws FileVersionException
     */
    public void testXMLResaveSAX1_0() throws SAXException, IOException,
            ParserConfigurationException, FileVersionException {
        new File("test").mkdir();

        final String filename = "test/testparserSAX1_0.xml";
        final String resaveFilename = "test/testparserSAX1_0Resaved.xml";

        Logger logger = this.logger;
        MASTBuilder mastBuilder = new MASTBuilder(logger);
        try {
            SAXFileWriter.write(this.testSessionContainer, new File(filename),
                    new XMLWriter1_0_SAX(logger));
        } catch (Exception e) {
            logger.fatal("An error occurred during saving", e);
        }

        TestSessionContainer newTestSessionContainer;
        try {
            XMLReader1_0_SAX reader = new XMLReader1_0_SAX(logger, mastBuilder,
                    PluginManager.create());

            SAXFileReader.parse(getInputStream(new File(filename)), reader);

            newTestSessionContainer = reader.getTestSessionContainer();

            try {
                SAXFileWriter.write(newTestSessionContainer, new File(
                        resaveFilename), new XMLWriter1_0_SAX(logger));
            } catch (Exception e) {
                logger.fatal("An error occurred during saving", e);
            }
        } catch (FileLoadException e) {
            logger.fatal("An error occurred during loading", e);
        }
    }

    /**
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     * @throws FileVersionException
     */
    public void testXMLResaveBiggerTSCDOM1_0() throws SAXException,
            IOException, ParserConfigurationException, FileVersionException {
        new File("test").mkdir();

        final String srcFile = "testsource/tscBigger.xml";
        final String filename = "test/testparserBiggerTSCDOM1_0.xml";
        final String resaveFilename = "test/testparserBiggerTSCDOM1_0Resaved.xml";

        Logger logger = this.logger;
        MASTBuilder mastBuilder = new MASTBuilder(logger);
        try {
            TestSessionContainer container = TestSessionContainer.load(
                    PluginManager.create(), logger, mastBuilder, srcFile);

            XMLWriter1_0_DOM testXMLWriter = new XMLWriter1_0_DOM(logger);
            testXMLWriter.writeFile(new File(filename), container);
        } catch (Exception e) {
            logger.fatal("An error occurred during saving", e);
        }

        TestSessionContainer newTestSessionContainer;
        try {
            XMLReader1_0_SAX reader = new XMLReader1_0_SAX(logger, mastBuilder,
                    PluginManager.create());

            SAXFileReader.parse(getInputStream(new File(filename)), reader);

            newTestSessionContainer = reader.getTestSessionContainer();

            try {
                XMLWriter1_0_DOM testXMLWriter = new XMLWriter1_0_DOM(logger);
                testXMLWriter.writeFile(new File(resaveFilename),
                        newTestSessionContainer);
            } catch (Exception e) {
                logger.fatal("An error occurred during saving", e);
            }
        } catch (FileLoadException e) {
            logger.fatal("An error occurred during loading", e);
        }
    }

    /**
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     * @throws FileVersionException
     */
    public void testXMLResaveBiggerTSCSAX1_0() throws SAXException,
            IOException, ParserConfigurationException, FileVersionException {
        new File("test").mkdir();

        final String srcFile = "testsource/tscBigger.xml";
        final String filename = "test/testparserBiggerTSCSAX1_0.xml";
        final String resaveFilename = "test/testparserBiggerTSCSAX1_0Resaved.xml";

        Logger logger = this.logger;
        MASTBuilder mastBuilder = new MASTBuilder(logger);
        try {
            TestSessionContainer container = TestSessionContainer.load(
                    PluginManager.create(), logger, mastBuilder, srcFile);

            SAXFileWriter.write(container, new File(filename),
                    new XMLWriter1_0_SAX(logger));
        } catch (Exception e) {
            logger.fatal("An error occurred during saving", e);
        }

        TestSessionContainer newTestSessionContainer;
        try {
            XMLReader1_0_SAX reader = new XMLReader1_0_SAX(logger, mastBuilder,
                    PluginManager.create());

            SAXFileReader.parse(getInputStream(new File(filename)), reader);

            newTestSessionContainer = reader.getTestSessionContainer();

            try {
                SAXFileWriter.write(newTestSessionContainer, new File(
                        resaveFilename), new XMLWriter1_0_SAX(logger));
            } catch (Exception e) {
                logger.fatal("An error occurred during saving", e);
            }
        } catch (FileLoadException e) {
            logger.fatal("An error occurred during loading", e);
        }
    }

    /**
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     * @throws FileVersionException
     */
    public void testXMLInvalidTSC() throws SAXException, IOException,
            ParserConfigurationException, FileVersionException {
        final String srcFile = "testsource/invalidTSC.xml";

        Logger logger = this.logger;
        MASTBuilder mastBuilder = new MASTBuilder(logger);
        try {
            TestSessionContainer.load(PluginManager.create(), logger,
                    mastBuilder, srcFile);
            fail();
        } catch (FileLoadException e) {
            assertTrue(e.getCause() instanceof SAXException);
        }
    }

    // /**
    // * @throws SAXException
    // * @throws IOException
    // * @throws ParserConfigurationException
    // * @throws FileVersionException
    // */
    // public void testXMLEvenBiggerTSCSAX1_0() throws SAXException,
    // IOException,
    // ParserConfigurationException, FileVersionException {
    // new File("test").mkdir();
    //
    // final String srcFile = "testsource/testcoverage-r2270.xml";
    // final String filename = "test/testparserSAX1_0EvenBigger.xml";
    // final String resaveName = "test/testparserSAX1_0EvenBiggerResaved.xml";
    //
    // Logger logger = this.logger;
    // MASTBuilder mastBuilder = new MASTBuilder(logger);
    // try {
    // TestSessionContainer container = TestSessionContainer.load(
    // PluginManager.create(), logger, mastBuilder, srcFile);
    //
    // try {
    // SAXFileWriter.write(container, new File(filename),
    // new XMLWriter1_0_SAX(logger));
    // } catch (Exception e) {
    // logger.fatal("An error occurred during saving", e);
    // }
    //
    // TestSessionContainer newTestSessionContainer;
    // try {
    // XMLReader1_0_SAX reader = new XMLReader1_0_SAX(logger,
    // mastBuilder, PluginManager.create());
    //
    // SAXFileReader.parse(getInputStream(new File(filename)), reader);
    //
    // newTestSessionContainer = reader.getTestSessionContainer();
    //
    // try {
    // SAXFileWriter.write(newTestSessionContainer, new File(
    // resaveName), new XMLWriter1_0_SAX(logger));
    // } catch (Exception e) {
    // logger.fatal("An error occurred during saving", e);
    // }
    // } catch (FileLoadException e) {
    // logger.fatal("An error occurred during loading", e);
    // }
    // } catch (FileLoadException e) {
    // logger.fatal("An error occurred during loading", e);
    // }
    // }
    //
    // /**
    // * @throws SAXException
    // * @throws IOException
    // * @throws ParserConfigurationException
    // * @throws FileVersionException
    // */
    // public void testXMLEvenBiggerTSCDOM1_0() throws SAXException,
    // IOException,
    // ParserConfigurationException, FileVersionException {
    // new File("test").mkdir();
    //
    // final String srcFile = "testsource/testcoverage-r2270.xml";
    // final String filename = "test/testparserSAX1_0EvenBigger.xml";
    // final String resaveName = "test/testparserSAX1_0EvenBiggerResaved.xml";
    //
    // Logger logger = this.logger;
    // MASTBuilder mastBuilder = new MASTBuilder(logger);
    // try {
    // TestSessionContainer container = TestSessionContainer.load(
    // PluginManager.create(), logger, mastBuilder, srcFile);
    //
    // try {
    // new XMLWriter1_0_DOM(logger).writeFile(new File(filename),
    // container);
    // } catch (Exception e) {
    // logger.fatal("An error occurred during saving", e);
    // }
    //
    // TestSessionContainer newTestSessionContainer;
    // try {
    // XMLReader1_0_SAX reader = new XMLReader1_0_SAX(logger,
    // mastBuilder, PluginManager.create());
    //
    // SAXFileReader.parse(getInputStream(new File(filename)), reader);
    //
    // newTestSessionContainer = reader.getTestSessionContainer();
    //
    // try {
    // new XMLWriter1_0_DOM(logger).writeFile(
    // new File(resaveName), newTestSessionContainer);
    // } catch (Exception e) {
    // logger.fatal("An error occurred during saving", e);
    // }
    // } catch (FileLoadException e) {
    // logger.fatal("An error occurred during loading", e);
    // }
    // } catch (FileLoadException e) {
    // logger.fatal("An error occurred during loading", e);
    // }
    // }

    private InputStream getInputStream(File file) throws FileLoadIOException {
        File absoluteFile = file.getAbsoluteFile();

        try {
            return new FileInputStream(absoluteFile);
        } catch (FileNotFoundException e) {
            throw new FileLoadIOException("A FileLoadIOException"
                    + " has occured", e);
        }
    }
}
