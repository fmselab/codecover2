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

package org.codecover.junit4.core;

import junit.framework.TestCase;

import org.codecover.junit.HelperMethods;
import org.codecover.junit4.JUnitRunListenerMethod;
import org.codecover.junit4.JUnitRunListenerTestCase;
import org.junit.internal.JUnitSystem;
import org.junit.internal.RealSystem;
import org.junit.runner.JUnitCore;
import org.junit.runner.Result;

/**
 * This is a CodeCover TestRunner which encapsulates a
 * {@link org.junit.runner.JUnitCore} of JUnit 4.x<br>
 * <br>
 * Usage from commandline:
 *
 * <pre>
 * org.codecover.junit4.core.TestRunner [-methodsAsTestCases] &lt;TestClass&gt;+
 * </pre>
 *
 * The <code>&lt;TestClass&gt;</code> is a class, that uses JUnit 4.x Annotations
 * to describe test cases.
 *
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: TestRunner.java 48 2009-05-25 10:19:53Z ahija $)
 *
 * @see org.junit.runner.JUnitCore
 */
public class TestRunner extends JUnitCore {

    /**
     * Starts a new {@link TestRunner} with the given arguments.
     *
     * @param args
     *            see {@link TestRunner} for description.
     *
     * @see junit.textui.TestRunner#main(String[]) Copied from there.
     */
    /**
     * Run the tests contained in the classes named in the <code>args</code>.
     * If all tests run successfully, exit with a status of 0. Otherwise exit with a status of 1.
     * Write feedback while tests are running and write
     * stack traces for all failed tests after the tests all complete.
     * @param args names of classes in which to find tests to run
     */
    public static void main(String... args) {
        runMainAndExit(new RealSystem(), args);
    }

    public static void runMainAndExit(JUnitSystem system, String... args) {
       boolean useMethodsAsTestcases = HelperMethods.useMethodsAsArguments(args);
       args = HelperMethods.removeMethodsOption(args);
       Result result= new TestRunner(useMethodsAsTestcases).runMain(system, args);
       system.exit(result.wasSuccessful() ? 0 : 1);
    }

    public static Result runClasses(Class<?>... classes) {
         return runClasses(false, classes);
    }

    /**
     * Runs the classes as test cases and uses test methods as test cases, if
     * true;
     *
     * @param useMethodsAsTestcases
     *            true &rarr; use methods as test cases; false &rarr; not.
     * @param classes
     *            All the classes to be run.
     * @return The {@link Result} of the test run.
     */
    public static Result runClasses(boolean useMethodsAsTestcases, Class<?>... classes) {
        return new TestRunner(useMethodsAsTestcases).run(classes);
    }

    /**
     *
     * @param useMethodsAsTestcases
     *            true &rarr; methods of a {@link TestCase} are used as test
     *            cases<br>
     *            false &rarr; the whole {@link TestCase} is used as a test
     *            case.
     */
    public TestRunner(boolean useMethodsAsTestcases) {
        super();
        if (useMethodsAsTestcases) {
            super.addListener(new JUnitRunListenerMethod());
        } else {
            super.addListener(new JUnitRunListenerTestCase());
        }
    }
}
