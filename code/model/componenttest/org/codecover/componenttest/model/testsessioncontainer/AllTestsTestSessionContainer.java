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

package org.codecover.componenttest.model.testsessioncontainer;

import junit.framework.*;

import org.codecover.componenttest.model.testsessioncontainer.loadsave.AllTestsLoadSave;
import org.codecover.componenttest.model.testsessioncontainer.staticinformation.AllTestsStaticInformation;
import org.codecover.componenttest.model.testsessioncontainer.testsessionhousekeeping.*;

/**
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: AllTestsTestSessionContainer.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class AllTestsTestSessionContainer {
    /**
     * @return the suite containing the suites of the subsections
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(
                "Test for org.codecover.componenttest.model.testsessioncontainer");
        // $JUnit-BEGIN$
        suite.addTest(AllTestsTestSessionHousekeeping.suite());
        suite.addTest(AllTestsLoadSave.suite());
        suite.addTest(AllTestsStaticInformation.suite());
        // $JUnit-END$
        return suite;
    }
}
