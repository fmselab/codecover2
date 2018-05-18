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

package org.codecover.componenttest.metrics;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * 
 * @author Tilmann Scheller
 * @version 1.0 ($Id: AllTests.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public class AllTests {

    /**
     * @return the suite containing the test cases for the metrics component
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(
                "Test for org.codecover.componenttest.metrics");
        // $JUnit-BEGIN$
        suite.addTestSuite(CM0001.class);
        suite.addTestSuite(CM0002.class);
        suite.addTestSuite(CM0003.class);
        suite.addTestSuite(CM0004.class);
        suite.addTestSuite(CM0005.class);
        suite.addTestSuite(CM0006.class);
        suite.addTestSuite(CM0007.class);
        suite.addTestSuite(CM0008.class);
        suite.addTestSuite(CM0009.class);
        suite.addTestSuite(CM0010.class);
        suite.addTestSuite(CM0011.class);
        suite.addTestSuite(CM0012.class);
        suite.addTestSuite(CM0013.class);
        // $JUnit-END$
        return suite;
    }

}
