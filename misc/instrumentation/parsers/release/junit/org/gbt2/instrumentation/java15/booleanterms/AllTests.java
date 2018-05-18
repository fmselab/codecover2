///////////////////////////////////////////////////////////////////////////////
//
// $Id: AllTests.java 1 2007-12-12 17:37:26Z t-scheller $
// 
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.booleanterms;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.swingui.TestRunner;

/**
 * @author Christoph Müller
 */
public class AllTests extends TestSuite {

    /**
     * Starts all the test cases hierachical using the JUnit swingui TestRunner.
     * 
     * @param args
     *            not needed
     */
    public static void main(String[] args) {
        TestRunner.run(AllTests.class);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(AllTests.class.getCanonicalName());

        // add testcases in this package
        suite.addTestSuite(JavaExpressionParserTest.class);

        // add testsuites in subpackages
        // suite.addTest(org.gbt2.[..].AllTests.suite());

        return suite;
    }
}
