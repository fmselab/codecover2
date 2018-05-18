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

import org.codecover.instrumentation.InstrumenterDescriptor;

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: InstrumenterDescriptorTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InstrumenterDescriptorTest extends TestCase {

    /**
     * Test method for {@link org.codecover.instrumentation.cobol85.InstrumenterDescriptor#isLanguageSupported(java.lang.String)}.
     */
    public final void testIsLanguageSupported() {
        InstrumenterDescriptor descriptor = new org.codecover.instrumentation.cobol85.InstrumenterDescriptor();
        Assert.assertEquals("VS COBOL 1985", descriptor.getLanguageName());
        Assert.assertEquals("Stefan Franke", descriptor.getAuthor());
        Assert.assertTrue(descriptor.isLanguageSupported("cobol"));
        Assert.assertTrue(descriptor.isLanguageSupported("cobol85"));
        Assert.assertTrue(descriptor.isLanguageSupported("cobol1985"));
        Assert.assertTrue(descriptor.isLanguageSupported("cobol 85"));
        Assert.assertTrue(descriptor.isLanguageSupported("cobol 1985"));
        Assert.assertTrue(descriptor.isLanguageSupported("vs cobol85"));
        Assert.assertTrue(descriptor.isLanguageSupported("vs cobol1985"));
        Assert.assertTrue(descriptor.isLanguageSupported("vs cobol 85"));
        Assert.assertTrue(descriptor.isLanguageSupported("vs cobol 1985"));
        Assert.assertTrue(descriptor.isLanguageSupported("VS cobol85"));
        Assert.assertTrue(descriptor.isLanguageSupported("VS cobol1985"));
        Assert.assertTrue(descriptor.isLanguageSupported("VS cobol 85"));
        Assert.assertTrue(descriptor.isLanguageSupported("VS cobol 1985"));

        Assert.assertTrue(descriptor.isLanguageSupported("Cobol"));
        Assert.assertTrue(descriptor.isLanguageSupported("Cobol85"));
        Assert.assertTrue(descriptor.isLanguageSupported("Cobol1985"));
        Assert.assertTrue(descriptor.isLanguageSupported("Cobol 85"));
        Assert.assertTrue(descriptor.isLanguageSupported("Cobol 1985"));
        Assert.assertTrue(descriptor.isLanguageSupported("vs Cobol85"));
        Assert.assertTrue(descriptor.isLanguageSupported("vs Cobol1985"));
        Assert.assertTrue(descriptor.isLanguageSupported("vs Cobol 85"));
        Assert.assertTrue(descriptor.isLanguageSupported("vs Cobol 1985"));
        Assert.assertTrue(descriptor.isLanguageSupported("VS Cobol85"));
        Assert.assertTrue(descriptor.isLanguageSupported("VS Cobol1985"));
        Assert.assertTrue(descriptor.isLanguageSupported("VS Cobol 85"));
        Assert.assertTrue(descriptor.isLanguageSupported("VS Cobol 1985"));

        Assert.assertTrue(descriptor.isLanguageSupported("COBOL"));
        Assert.assertTrue(descriptor.isLanguageSupported("COBOL85"));
        Assert.assertTrue(descriptor.isLanguageSupported("COBOL1985"));
        Assert.assertTrue(descriptor.isLanguageSupported("COBOL 85"));
        Assert.assertTrue(descriptor.isLanguageSupported("COBOL 1985"));
        Assert.assertTrue(descriptor.isLanguageSupported("vs COBOL85"));
        Assert.assertTrue(descriptor.isLanguageSupported("vs COBOL1985"));
        Assert.assertTrue(descriptor.isLanguageSupported("vs COBOL 85"));
        Assert.assertTrue(descriptor.isLanguageSupported("vs COBOL 1985"));
        Assert.assertTrue(descriptor.isLanguageSupported("VS COBOL85"));
        Assert.assertTrue(descriptor.isLanguageSupported("VS COBOL1985"));
        Assert.assertTrue(descriptor.isLanguageSupported("VS COBOL 85"));
        Assert.assertTrue(descriptor.isLanguageSupported("VS COBOL 1985"));
    }
}
