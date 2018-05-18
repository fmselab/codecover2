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

package org.codecover.junit4;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import junit.framework.TestSuite;

import org.codecover.UtilsForTestingJunit;
import org.codecover.instrumentation.java.measurement.CoverageLogPath;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: AllTests.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
@RunWith(Suite.class)
@SuiteClasses({
    ExamplePersonTest.class,
    ExampleProgramerTest.class
    })
public class AllTests extends TestSuite {

    public static final Map<String, Class<?>> testRunners = new HashMap<String, Class<?>>();
    static {
        testRunners.put("text4.3", org.codecover.junit4.core.TestRunner.class);
    }

    public static void main(String[] args) {
        try {
            if (args == null || args.length != 2 || !(args[1].equals("methods") ||
                args[1].equals("nomethods"))) {
                throw new IllegalArgumentException("Usage: " + 
                        AllTests.class.getName() + 
                        "TestRunnerShortName (methods|nomethods)");
            }

            String testRunnerName = args[0].intern();
            boolean useMethods = args[1].equals("methods"); 
            String suffix = useMethods ? "m" : "";
           
            Class<?> testRunnerClass = testRunners.get(testRunnerName);
            
            if (testRunnerClass == null) {
                throw new IllegalArgumentException("testrunner " + testRunnerName + " unknown"); 
            }

            System.setProperty(CoverageLogPath.PROPERTY_PATH_VARIABLE,
                    "testtarget/coverage_log_" + testRunnerName + suffix + ".clf");
            System.setProperty(CoverageLogPath.PROPERTY_OVERWRITE_VARIABLE, "true");
            Object[] testRunnerArguments;
            if (useMethods) {
                testRunnerArguments = new Object[]{new String[]{"-methodsAsTestCases", AllTests.class.getName()}};
            } else {
                testRunnerArguments = new Object[]{new String[]{AllTests.class.getName()}};
            }

            Method mainMethod = testRunnerClass.getMethod("main", new Class<?>[]{String[].class});
            mainMethod.invoke(null, testRunnerArguments);
        } catch (Throwable t) {
            UtilsForTestingJunit.handleException(t);
        }
    }
}
