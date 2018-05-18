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

package org.codecover.componenttest.model.testsessioncontainer.loadsave;

import junit.framework.*;

/**
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: AllTestsLoadSave.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class AllTestsLoadSave {

    /**
     * @return the suite containing the test cases of the subsection
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(
                "Test for org.codecover.componenttest.model.testsessioncontainer.loadsave");
        // $JUnit-BEGIN$
        suite.addTestSuite(CDLS0001.class);
        suite.addTestSuite(CDLS0002.class);
        suite.addTestSuite(CDLS0003.class);
        suite.addTestSuite(CDLS0004.class); // TODO: do this test in a way that it works on Linux and Windows
        suite.addTestSuite(CDLS0005.class); // TODO: do this test in a way that it works on Linux and Windows
        suite.addTestSuite(CDLS0006.class);
        suite.addTestSuite(CDLS0007.class);
        suite.addTestSuite(CDLS0011.class);
        suite.addTestSuite(CDLS0012.class);
        suite.addTestSuite(CDLS0013.class);
        suite.addTestSuite(CDLS0014.class);
        suite.addTestSuite(CDLS0015.class);
        suite.addTestSuite(CDLS0016.class);
        // $JUnit-END$
        return suite;
    }

}
