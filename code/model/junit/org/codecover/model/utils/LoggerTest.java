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

package org.codecover.model.utils;

/**
 * JUnit Testcase of Logger class
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: LoggerTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class LoggerTest extends junit.framework.TestCase {
    Logger logger;

    @Override
    protected void setUp() throws Exception {
        //logger = new SimpleLogger();
        // a bit more quiet:
        logger = Logger.NULL;
    }

    /**
     * 
     * 
     */
    public void test() {
        logger.debug("Debug");
        logger.info("Info");
        logger.warning("Warning");
        logger.error("Error");
        try {
            logger.fatal("Fatal");
            throw new RuntimeException();
        } catch (FatalException e) {
        }

        Exception e = null;
        try {
            throw new Exception();
        } catch (Exception exc) {
            e = exc;
        }

        logger.debug("Debug", e);
        logger.info("Info", e);
        logger.warning("Warning", e);
        logger.error("Error", e);
        try {
            logger.fatal("Fatal", e);
            throw new RuntimeException();
        } catch (FatalException ex) {
        }
    }
}
