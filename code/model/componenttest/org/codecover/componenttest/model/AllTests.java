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

package org.codecover.componenttest.model;

import org.codecover.componenttest.model.testsession.AllTestsTestSessions;
import org.codecover.componenttest.model.testsessioncontainer.AllTestsTestSessionContainer;
import org.codecover.componenttest.model.testcases.AllTestsTestCases;

import junit.framework.*;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: AllTests.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class AllTests {

    /**
     * @return the suite containing all the {@link junit.framework.TestCase}s
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(
                "Test for org.codecover.componenttest.model");
        // $JUnit-BEGIN$
        suite.addTest(AllTestsTestSessionContainer.suite());
        suite.addTest(AllTestsTestSessions.suite());
        suite.addTest(AllTestsTestCases.suite());
        // $JUnit-END$
        return suite;
    }

}
