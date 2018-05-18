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

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: XMLNames.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface XMLNames {
    /**
     * http://www.codecover.org/xml/testsession-container
     */
    static final String NAMESPACE_TEST_SESSION_CONTAINER = "http://www.codecover.org/xml/testsession-container";

    /**
     * xmlns
     */
    static final String NAMESPACE_IDENTIFIER = "xmlns";

    /**
     * TestSessionContainer
     */
    static final String ELEMENT_TEST_SESSION_CONTAINER = "TestSessionContainer";

    /**
     * version
     */
    static final String VERSION = "version";

    /**
     * Date
     */
    static final String DATE = "Date";

    /**
     * TestSessionContainerId
     */
    static final String TEST_SESSION_CONTAINER_ID = "TestSessionContainerId";

    /**
     * Gets the logger
     * 
     * @return the logger
     */
    public Logger getLogger();

    /**
     * Gets the major version of the file format
     * 
     * @return the major version.
     */
    public int getMajorVersion();

    /**
     * Gets the minor version of the file format
     * 
     * @return the minor version.
     */
    public int getMinorVersion();
}
