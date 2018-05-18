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
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: XMLReaderBase.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
abstract class XMLReaderBase extends DefaultHandler implements XMLNames {
    private final Logger logger;

    /**
     * Constructor, which sets the {@link Logger} to be used.
     * 
     * @param logger
     *            the {@link Logger}
     */
    XMLReaderBase(final Logger logger) {
        if (logger == null) {
            throw new NullPointerException("logger == null");
        }

        this.logger = logger;
    }

    /**
     * Gets the logger.
     * 
     * @return the logger
     */
    public final Logger getLogger() {
        return this.logger;
    }

    /**
     * Gets the read {@link TestSessionContainer}
     * 
     * @return the {@link TestSessionContainer}
     */
    public abstract TestSessionContainer getTestSessionContainer();

    /*
     * (non-Javadoc)
     * 
     * @see org.xml.sax.helpers.DefaultHandler#error(org.xml.sax.SAXParseException)
     */
    @Override
    public void error(SAXParseException e) throws SAXException {
        getLogger().error("A SAX error occurred: " + e.getMessage());
        throw new SAXException("A SAX error occurred.", e);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.xml.sax.helpers.DefaultHandler#fatalError(org.xml.sax.SAXParseException)
     */
    @Override
    public void fatalError(SAXParseException e) throws SAXException {
        getLogger().fatal("A SAX fatal error occurred.", e);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.xml.sax.helpers.DefaultHandler#warning(org.xml.sax.SAXParseException)
     */
    @Override
    public void warning(SAXParseException e) throws SAXException {
        getLogger().warning("A SAX warning occurred: " + e.getMessage());
    }

}
