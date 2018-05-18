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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * @author Christoph Müller 
 *
 * @version 1.0 ($Id: HelperMethodsTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class HelperMethodsTest extends TestCase {

    public void testRegExp() throws Exception {
        Pattern pattern = HelperMethods.METHOD_EXTRACT_PATTERN;
        Matcher matcher = pattern.matcher("method(org.codecover.TestClass)");
        Assert.assertTrue(matcher.matches());
        Assert.assertEquals("method", matcher.group(1));
        Assert.assertEquals("org.codecover.TestClass", matcher.group(2));
    }
}
