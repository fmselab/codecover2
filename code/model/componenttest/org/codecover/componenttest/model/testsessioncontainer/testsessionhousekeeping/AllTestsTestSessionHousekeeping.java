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

package org.codecover.componenttest.model.testsessioncontainer.testsessionhousekeeping;

import junit.framework.*;

/**
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: AllTestsTestSessionHousekeeping.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class AllTestsTestSessionHousekeeping {

    /**
     * @return the suite containing the test cases of the subsection
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(
                "Test for org.codecover.componenttest.model.testsessioncontainer.testsessionhousekeeping");
        // $JUnit-BEGIN$
        suite.addTestSuite(CDTS0001.class);
        suite.addTestSuite(CDTS0002.class);
        suite.addTestSuite(CDTS0003.class);
        suite.addTestSuite(CDTS0004.class);
        suite.addTestSuite(CDTS0005.class);
        suite.addTestSuite(CDTS0006.class);
        // $JUnit-END$
        return suite;
    }

}
