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

package org.codecover.instrumentation.java15.manipulators;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.swingui.TestRunner;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: AllTests.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class AllTests extends TestSuite {

    /**
     * Starts all the test cases hierarchical using the JUnit swingui TestRunner.
     * 
     * @param args
     *            "swing" &rarr; use {@link TestRunner}<br>
     *            "text" &rarr; use {@link junit.textui.TestRunner}<br>
     *            nothing &rarr; use {@link TestRunner}<br>
     */
    public static void main(String[] args) {
        if (args == null || args.length != 1) {
            args = new String[]{"swing"};
        }

        if (args[0].toLowerCase().equals("swing")) {
            TestRunner.run(AllTests.class);
        } else if (args[0].toLowerCase().equals("text")) {
            junit.textui.TestRunner.run(AllTests.suite());
        } else {
            System.out.println("usage: \"AllTests (swing | text)\"");
        }
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(AllTests.class.getCanonicalName());

        // add testcases in this package
        suite.addTestSuite(DefaultCommentManipulatorTest.class);

        // add testsuites in subpackages
        //suite.addTest(org.codecover.instrumentation.java15.measurement.AllTests.suite());

        return suite;
    }
}
