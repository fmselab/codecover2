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

package org.codecover.componenttest.model.testsession;

import junit.framework.*;

/**
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: AllTestsTestSessions.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class AllTestsTestSessions {
    /**
     * @return the suite containing the test cases of the subsection
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(
                "Test for org.codecover.componenttest.model.testsession");
        // TODO: fix CDTT0001 because behavior of deleted test sessions changed (they are still readable)
        // $JUnit-BEGIN$
        suite.addTestSuite(CDTT0007.class);
        suite.addTestSuite(CDTT0002.class);
        suite.addTestSuite(CDTT0004.class);
        suite.addTestSuite(CDTT0006.class);
        suite.addTestSuite(CDTT0005.class);
        //suite.addTestSuite(CDTT0001.class);
        suite.addTestSuite(CDTT0003.class);
        // $JUnit-END$
        return suite;
    }

}
