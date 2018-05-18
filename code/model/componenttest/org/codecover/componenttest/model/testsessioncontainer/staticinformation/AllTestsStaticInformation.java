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

package org.codecover.componenttest.model.testsessioncontainer.staticinformation;

import junit.framework.*;

/**
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: AllTestsStaticInformation.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class AllTestsStaticInformation {

    /**
     * @return the suite containing the test cases of the subsection
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(
                "Test for org.codecover.componenttest.model.testsessioncontainer.staticinformation");
        // $JUnit-BEGIN$
        suite.addTestSuite(CDP0001.class);
        suite.addTestSuite(CDP0002.class);
        suite.addTestSuite(CDP0003.class);
        suite.addTestSuite(CDP0004.class);
        suite.addTestSuite(CDP0005.class);
        suite.addTestSuite(CDP0006.class);
        // $JUnit-END$
        return suite;
    }

}
