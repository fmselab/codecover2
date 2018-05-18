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

package org.codecover.instrumentation.java15;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.swingui.TestRunner;

import org.codecover.instrumentation.java15.parser.JavaCharStreamTest;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: AllTests.java 15 2008-05-24 20:59:06Z ahija $)
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
        suite.addTestSuite(JavaExpressionParserTest.class);
        suite.addTestSuite(TokenPositionTest.class);
        suite.addTestSuite(JavaBooleanOperatorsTest.class);

        suite.addTestSuite(JavaCharStreamTest.class);

        suite.addTestSuite(UnicodeEscapeTest.class);

        // add testsuites in subpackages
        suite.addTest(org.codecover.instrumentation.java15.counter.AllTests.suite());
        suite.addTest(org.codecover.instrumentation.java15.measurement.AllTests.suite());
        suite.addTest(org.codecover.instrumentation.java15.location.AllTests.suite());
        suite.addTest(org.codecover.instrumentation.java15.manipulators.AllTests.suite());
        suite.addTest(org.codecover.instrumentation.java15.parser.AllTests.suite());
        suite.addTest(org.codecover.instrumentation.java15.syntaxtree.AllTests.suite());

        // make these test at last
        suite.addTestSuite(JavaNonInstrumentationTest.class);
        suite.addTestSuite(JavaStatementTest.class);
        suite.addTestSuite(JavaBranchTest.class);
        suite.addTestSuite(JavaConditionTest.class);
        suite.addTestSuite(JavaLoopTest.class);
        suite.addTestSuite(JavaAllInstrumentationTest.class);

        return suite;
    }
}
