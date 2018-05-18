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

package org.codecover.instrumentation.cobol85;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * @author Christop Müller
 *
 * @version 1.0 ($Id: RegExpTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class RegExpTest extends TestCase {
    private static final String nameRegExp = "(([^\"\\\\\n\r])|(\\\\[ntbrf\\\\'\"]))*";
    private static final String regExp = "\\*\\>STARTTESTCASE \"(" + nameRegExp + ")\"( \"(" + nameRegExp + ")\")?";
    private static final Pattern pattern = Pattern.compile(regExp);

    public void testRegExp1() {
        String testCaseName = "t!est case name";
        String testCaseComment = "comment";

        String string = "*>STARTTESTCASE \"" + testCaseName + "\" \"" + testCaseComment + "\"";
        Matcher matcher = pattern.matcher(string);

        Assert.assertTrue(matcher.matches());
        Assert.assertEquals(testCaseName, matcher.group(1));
        Assert.assertEquals(testCaseComment, matcher.group(6));
    }

    public void testRegExp2() {
        String testCaseName = "t!est \\\"case name\\\"";
        String testCaseComment = "comment";

        String string = "*>STARTTESTCASE \"" + testCaseName + "\" \"" + testCaseComment + "\"";
        Matcher matcher = pattern.matcher(string);

        Assert.assertTrue(matcher.matches());
        Assert.assertEquals(testCaseName, matcher.group(1));
        Assert.assertEquals(testCaseComment, matcher.group(6));
    }

    public void testRegExp3() {
        String simpleRegExp = "(([^\"\\\\\n\r])|(\\\\[ntbrf\\\\'\"]))*";
        Pattern pattern = Pattern.compile(simpleRegExp);
        Assert.assertTrue(pattern.matcher("\\\"").matches());
    }
}
