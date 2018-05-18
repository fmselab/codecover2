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

import org.codecover.model.extensions.*;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.SimpleLogger;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: SAXParserTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class SAXParserTest extends junit.framework.TestCase {
    /**
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#setUp()
     */
    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    /**
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#tearDown()
     */
    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    /**
     * @throws Exception
     */
    public void testParse() throws Exception {
        Logger logger = new SimpleLogger();

        File file = new File("test/databasetest.xml");
        TestSessionContainer tsc = TestSessionContainer.load(PluginManager
                .create(), logger, new MASTBuilder(logger), file);

        tsc.save("test/databasetestSAX.xml");

    }

    /**
     * @throws Exception
     */
    public void testInfoOnlyParse() throws Exception {
        Logger logger = new SimpleLogger();

        File file = new File("test/databasetest.xml");
        TestSessionContainer tsc = TestSessionContainer.loadInfoOnly(
                PluginManager.create(), logger, new MASTBuilder(logger), file);

        tsc.save("test/databasetestSAXInfoOnly.xml");
    }

    /**
     * @throws Exception
     */
    public void testInfoOnlyWithBigFile() throws Exception {
        Logger logger = new SimpleLogger();

        File file = new File("/Users/Markus/tsc.xml");

        TestSessionContainer tsc = TestSessionContainer.loadInfoOnly(
                PluginManager.create(), logger, new MASTBuilder(logger), file);

        tsc.save("test/databasetestSAXInfoOnlyBig.xml");
    }

}
