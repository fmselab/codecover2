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

package org.codecover.componenttest.model.testcases;

import junit.framework.*;

/**
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: AllTestsTestCases.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class AllTestsTestCases {

    /**
     * @return the suite containing the test cases of the subsection
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(
                "Test for org.codecover.componenttest.model.testsessioncontainer.testcases");
        // TODO: fix CDTC0001 because behavior of deleted test cases changed (they are still readable)
        // $JUnit-BEGIN$
        //suite.addTestSuite(CDTC0001.class);
        // $JUnit-END$
        return suite;
    }

}
