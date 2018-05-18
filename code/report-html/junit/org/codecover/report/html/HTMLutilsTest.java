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

package org.codecover.report.html;

import junit.framework.TestCase;

/**
 * Tests the class <code>org.codecover.report.Template</code>.
 *
 * @author Robert Hanussek
 * @version ($Id: HTMLutilsTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class HTMLutilsTest extends TestCase {

    public void testReplaceWhiteSpacesByNonbreakingSpaces() {
        HTMLutils htmlUtils = new HTMLutils();
        String nbsp = htmlUtils.getNBSP();
        String line = "\t" +
                      "\t" +
                      "1 3\t" +
                      "1\t" +
                      "\t" +
                      "0";
        int tabWidth = 4;
        String expectedResult = nbsp+nbsp+nbsp+nbsp +
                                nbsp+nbsp+nbsp+nbsp +
                                "1" +nbsp+"3" +nbsp +
                                "1" +nbsp+nbsp+nbsp +
                                nbsp+nbsp+nbsp+nbsp +
                                "0";
        assertEquals(expectedResult,
                htmlUtils.replaceWhiteSpacesByNonbreakingSpaces(line, tabWidth));
    }
}
