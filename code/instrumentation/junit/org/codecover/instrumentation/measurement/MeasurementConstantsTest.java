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

package org.codecover.instrumentation.measurement;

import org.codecover.model.utils.StringUtil;

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * @author Christoph Müller 
 *
 * @version 1.0 ($Id: MeasurementConstantsTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MeasurementConstantsTest extends TestCase {

    public void testEscapeNames() {
        String before;
        String after;

        before = "\n";
        after = MeasurementConstants.escapeName(before);
        Assert.assertEquals(" ", after);

        before = "hallo\nDu\nund\nich";
        after = MeasurementConstants.escapeName(before);
        Assert.assertEquals("hallo Du und ich", after);

        before = "\r";
        after = MeasurementConstants.escapeName(before);
        Assert.assertEquals(" ", after);

        before = "hallo\rDu\rund\rich";
        after = MeasurementConstants.escapeName(before);
        Assert.assertEquals("hallo Du und ich", after);

        before = "\r\n";
        after = MeasurementConstants.escapeName(before);
        Assert.assertEquals(" ", after);

        before = "hallo\r\nDu\r\nund\r\nich";
        after = MeasurementConstants.escapeName(before);
        Assert.assertEquals("hallo Du und ich", after);

        before = "\"";
        after = MeasurementConstants.escapeName(before);
        Assert.assertEquals("\\\"", after);

        before = "ich heiße \"Christoph Müller\"";
        after = MeasurementConstants.escapeName(before);
        Assert.assertEquals("ich heiße \\\"Christoph Müller\\\"", after);
        
        before = "\\\"";
        after = MeasurementConstants.escapeName(before);
        Assert.assertEquals("\\\\\\\"", after);
    }

    public void testEscapeComments() {
        String before;
        String after;

        before = "\n";
        after = MeasurementConstants.escapeComment(before);
        Assert.assertEquals("\\n", after);

        before = "hallo\nDu\nund\nich";
        after = MeasurementConstants.escapeComment(before);
        Assert.assertEquals("hallo\\nDu\\nund\\nich", after);

        before = "\r";
        after = MeasurementConstants.escapeComment(before);
        Assert.assertEquals("\\r", after);

        before = "hallo\rDu\rund\rich";
        after = MeasurementConstants.escapeComment(before);
        Assert.assertEquals("hallo\\rDu\\rund\\rich", after);

        before = "\r\n";
        after = MeasurementConstants.escapeComment(before);
        Assert.assertEquals("\\r\\n", after);

        before = "hallo\r\nDu\r\nund\r\nich";
        after = MeasurementConstants.escapeComment(before);
        Assert.assertEquals("hallo\\r\\nDu\\r\\nund\\r\\nich", after);

        before = "\"";
        after = MeasurementConstants.escapeComment(before);
        Assert.assertEquals("\\\"", after);

        before = "ich heiße \"Christoph Müller\"";
        after = MeasurementConstants.escapeComment(before);
        Assert.assertEquals("ich heiße \\\"Christoph Müller\\\"", after);

        before = "\\\"";
        after = MeasurementConstants.escapeComment(before);
        Assert.assertEquals("\\\\\\\"", after);
    }

    public void testParseStringLiteral() {
        String before;
        String after;

        before = "\"Mein Name ist \\\"Juhuu\\\"\"";
        after = StringUtil.parseStringLiteral(before);
        Assert.assertEquals("Mein Name ist \"Juhuu\"", after);

        before = "\"oben\\nunten\"";
        after = StringUtil.parseStringLiteral(before);
        Assert.assertEquals("oben\nunten", after);

        before = "\"oben\\runten\"";
        after = StringUtil.parseStringLiteral(before);
        Assert.assertEquals("oben\runten", after);

        before = "\"oben\\r\\nunten\"";
        after = StringUtil.parseStringLiteral(before);
        Assert.assertEquals("oben\r\nunten", after);

        before = "\"oben\\tunten\"";
        after = StringUtil.parseStringLiteral(before);
        Assert.assertEquals("oben\tunten", after);
    }

    public void testEscapeAndParse() throws Exception {
        escapeAndParse("oben\tunten");
        escapeAndParse("oben\nunten");
        escapeAndParse("oben\r\nunten");
        escapeAndParse("oben\runten");
        escapeAndParse("oben\"unten");
        escapeAndParse("oben\funten");
        escapeAndParse("oben\funten");
        escapeAndParse("oben\\unten");
        escapeAndParse("heute ist ein schöner \\tag");
        escapeAndParse("heute ist ein schöner \\nag");
        escapeAndParse("heute ist ein schöner \\rag");
        escapeAndParse("heute ist ein schöner \\rnag");
        escapeAndParse("heute ist ein schöner \\fag");
        escapeAndParse("heute ist ein schöner \\\"ag");
    }

    private void escapeAndParse (String stringToCheck) {
        String escaped = MeasurementConstants.escapeComment(stringToCheck);
        escaped = "\"" + escaped + "\"";
        Assert.assertEquals(stringToCheck,
                StringUtil.parseStringLiteral(escaped));
    }
}
