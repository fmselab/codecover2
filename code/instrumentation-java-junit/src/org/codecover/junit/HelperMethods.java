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

package org.codecover.junit;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import junit.framework.Assert;

import org.codecover.instrumentation.java.measurement.Protocol;
import org.codecover.instrumentation.java.measurement.TestMethod;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: HelperMethods.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class HelperMethods {

    /** The name of the option to use methods as test cases */
    public static final String METHODS_OPTION = "-methodsAsTestCases";

    private static final String METHOD_EXTRACT_REGEXP = "([^\\(]*)\\(([^\\)]*)\\)";

    /**
     * A regexp to extract the method and test class name from a description
     * of Junit 4.
     */
    public static final Pattern METHOD_EXTRACT_PATTERN = Pattern.compile(METHOD_EXTRACT_REGEXP);

    private static final String[] STACK_TRACE_EXCLUSIONS = {Assert.class.getName()};

    /**
     * Tests whether one of the arguments has the option {@link #METHODS_OPTION}.
     * 
     * @param args The arguments of a main() method.
     * 
     * @return true &rarr; yes; false &rarr; no
     */
    public static boolean useMethodsAsArguments(String[] args) {
        for (int i = 0; i < args.length; i++) {
            if (args[i].equals(METHODS_OPTION)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Searches in args for the String {@value #METHODS_OPTION} and removes it.
     * 
     * @param args The args from a main method.
     * 
     * @return The args Array without the String {@value #METHODS_OPTION}.
     */
    public static String[] removeMethodsOption(String[] args) {
        ArrayList<String> list = new ArrayList<String>(args.length);
        for (int i = 0; i < args.length; i++) {
            list.add(args[i]);
        }
        return list.toArray(new String[list.size()]);
    }

    /**
     * Gets a description of the form:<br>
     * <code>methodName(de.package.TestClassName)</code> and splits it in a 
     * String Array:<br>
     * <code>{"de.package.TestClassName", "methodName"}</code>. If the description
     * is not of the form, <code>{description, null}</code>
     * is returned.
     * 
     * @param description
     *          The description.
     * 
     * @return The described String result array.
     */
    public static String[] extractTestCaseAndMethod(String description) {
        String[] result = new String[2];
        Matcher matcher = METHOD_EXTRACT_PATTERN.matcher(description);
        if (matcher.matches()) {
            result[0] = matcher.group(2);
            result[1] = matcher.group(1);
        } else {
            result[0] = description;
            result[1] = null;
        }

        return result;
    }

    /**
     * Tries to load {@link Protocol} with the same {@link ClassLoader} of an
     * object.
     * 
     * @param objectOfSameClassLoader
     *            The object, whose {@link ClassLoader} should be used.
     * 
     * @return {@link Protocol}.class.
     * @see JUnitResultListenerTestCase for full description.
     */
    public static Class getProtocolClass(Object objectOfSameClassLoader)
    throws ClassNotFoundException {
        ClassLoader classLoader = objectOfSameClassLoader.getClass().getClassLoader();
        return classLoader.loadClass(Protocol.class.getName());
    }

    /**
     * Tries to ignore the top most {@link StackTraceElement}s, that target to
     * {@link Assert} and returns just the first Element of the stack trace,
     * that does not contain a {@link #STACK_TRACE_EXCLUSIONS} element.
     * 
     * @param stackTrace
     *            The stack trace to filter.
     * 
     * @return The filtered stack trace
     * 
     * @see #STACK_TRACE_EXCLUSIONS
     */
    public static StackTraceElement[] getFilteredStackTrace(
            StackTraceElement[] stackTrace) {
        for (int i = 0; i < stackTrace.length; i++) {
            String toString = stackTrace[i].toString();
            for (int j = 0; j < STACK_TRACE_EXCLUSIONS.length; j++) {
                if (toString.indexOf(STACK_TRACE_EXCLUSIONS[j]) >= 0) {
                    return new StackTraceElement[] { stackTrace[i] };
                }
            }
        }
    
        // could not be filtered
        return stackTrace;
    }

    /*
     * testMethodList: A List of TestMethod
     */
    public static String writeTestMethods(List testMethodList) {
        StringBuffer targetContainer = new StringBuffer();
        if (testMethodList.isEmpty()) {
            return "";
        }

        Iterator iterator = testMethodList.iterator();
        while (iterator.hasNext()) {
            TestMethod thisTestMethod = (TestMethod) iterator.next();
            targetContainer.append(thisTestMethod.getName());
            targetContainer.append('\n');
            JUnitFailureContainer.writeFailures(thisTestMethod.getFailures(),
                    targetContainer);
            if (iterator.hasNext()) {
                targetContainer.append('\n');
            }
        }
        return targetContainer.toString();
    }
}
