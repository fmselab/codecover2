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

package org.codecover.instrumentation.booleanterms;

import static org.codecover.UtilsForTestingInstr.handleException;

import java.io.IOException;
import java.io.StringWriter;
import java.util.LinkedList;
import java.util.List;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingInstr;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.SourceFile;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: InstrBasicBooleanTermTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InstrBasicBooleanTermTest extends TestCase {

    private MASTBuilder builder;

    private SourceFile sourceFile;

    @Override
    protected void setUp() {
        this.builder = UtilsForTestingInstr.newMASTBuilder();
        this.sourceFile = this.builder.createSourceFile("test source file", "            hello()");
    }

    public void testInstrBasicBooleanTerm() {
        String image = "hello()";
        InstrBasicBooleanTerm term = new InstrBasicBooleanTerm(image, 12, 19);

        Assert.assertSame(image, term.termToString());
        Assert.assertSame(image, term.toString());

        StringWriter target = new StringWriter();
        try {
            term.writeToTarget(target);
            Assert.assertEquals(image, target.toString());
        } catch (IOException e) {
            handleException(e);
        }

        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        term.getAllBasicBooleanTerms(basicTerms);
        Assert.assertEquals(1, basicTerms.size());
        Assert.assertSame(term, basicTerms.get(0));

        String imageNew = "false";
        term.modifyImage(imageNew);
        Assert.assertNotSame(image, term.termToString());
        Assert.assertNotSame(image, term.toString());

        Assert.assertSame(imageNew, term.termToString());
        Assert.assertSame(imageNew, term.toString());
        
        BooleanTerm booleanTerm = term.toBooleanTerm(this.builder,
                this.sourceFile);
        Assert.assertEquals(1, booleanTerm.getBasicBooleanTerms());
        Assert.assertEquals("Location in 'test source file 19B' from char 12 to char 19: 'hello()'", booleanTerm.getLocation().toString());
    }

    public void testNullConstructor() {
        String image = null;
        try {
            new InstrBasicBooleanTerm(image, -1, -1);
            Assert.fail("NullPointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
            Assert.assertEquals("image == null", e.getMessage());
        }
    }
}
