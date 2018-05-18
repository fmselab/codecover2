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

package org.codecover.instrumentation;

import static org.codecover.UtilsForTestingInstr.handleException;

import java.io.File;
import java.nio.charset.Charset;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingInstr;
import org.codecover.instrumentation.exceptions.FactoryMisconfigurationException;
import org.codecover.model.utils.ProgressHandler;
import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.ConditionCoverage;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: InstrumenterFactoryTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InstrumenterFactoryTest extends TestCase {

    InstrumenterFactory factory;

    @Override
    protected void setUp() {
        this.factory = new DefaultInstrumenterFactory();
    }

    @Override
    protected void tearDown() {
        this.factory = null;
    }

    /**
     * Test method for {@link org.codecover.instrumentation.InstrumenterFactory#getInstrumenter()}.
     * 
     * set and test
     */
    public void testGetInstrumenter1() {
        InstrumenterDescriptor descr = new UtilsForTestingInstr.TestInstrumenterDescriptor1();
        this.factory.setDescriptor(descr);
        Instrumenter instrumenter = null; 
        try {
            instrumenter = this.factory.getInstrumenter();
        } catch (FactoryMisconfigurationException e) {
            handleException(e);
        }

        Assert.assertNotNull(instrumenter);
        Assert.assertEquals(descr.getDefaultCharset(), instrumenter.getCharset());
        Assert.assertFalse(instrumenter.pretend());
        Assert.assertSame(ProgressHandler.NULL, instrumenter.getProgressHandler());
        Assert.assertTrue(instrumenter.allowsFileListInstrumentation());
        Assert.assertTrue(instrumenter.getCriteria().isEmpty());
        Assert.assertFalse(instrumenter.isCriterionSet(StatementCoverage.getInstance()));
        Assert.assertFalse(instrumenter.isCriterionSet(BranchCoverage.getInstance()));
        Assert.assertFalse(instrumenter.isCriterionSet(ConditionCoverage.getInstance()));
        Assert.assertFalse(instrumenter.isCriterionSet(LoopCoverage.getInstance()));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.InstrumenterFactory#getInstrumenter()}.
     * 
     * set and test
     */
    public void testGetInstrumenter2() {
        InstrumenterDescriptor descr = new UtilsForTestingInstr.TestInstrumenterDescriptor1();
        this.factory.setDescriptor(descr);
        this.factory.setCharset(Charset.forName("ISO-8859-15"));
        this.factory.setPretendMode(false);
        this.factory.setVerboseMode(false);
        Instrumenter instrumenter = null; 
        try {
            instrumenter = this.factory.getInstrumenter();
        } catch (FactoryMisconfigurationException e) {
            handleException(e);
        }

        Assert.assertNotNull(instrumenter);
        Assert.assertEquals(Charset.forName("ISO-8859-15"), instrumenter.getCharset());
        Assert.assertFalse(instrumenter.pretend());
        Assert.assertFalse(instrumenter.verbose());
        Assert.assertSame(ProgressHandler.NULL, instrumenter.getProgressHandler());
        Assert.assertTrue(instrumenter.allowsFileListInstrumentation());
        Assert.assertTrue(instrumenter.getCriteria().isEmpty());
        Assert.assertFalse(instrumenter.isCriterionSet(StatementCoverage.getInstance()));
        Assert.assertFalse(instrumenter.isCriterionSet(BranchCoverage.getInstance()));
        Assert.assertFalse(instrumenter.isCriterionSet(ConditionCoverage.getInstance()));
        Assert.assertFalse(instrumenter.isCriterionSet(LoopCoverage.getInstance()));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.InstrumenterFactory#getInstrumenter()}.
     * 
     * set and test
     */
    public void testGetInstrumenter3() {
        ProgressHandler progressHandler = new ProgressHandler() {
            public void setProgress(float progress) {
                // do nothing;
            }
        };
        
        InstrumenterDescriptor descr = new UtilsForTestingInstr.TestInstrumenterDescriptor2();
        this.factory.setDescriptor(descr);
        this.factory.setCharset("UTF-16BE");
        this.factory.setPretendMode(true);
        this.factory.setVerboseMode(false);
        this.factory.setProgressHandler(progressHandler);
        this.factory.addCriterion(ConditionCoverage.getInstance());
        this.factory.addCriterion(LoopCoverage.getInstance());
        Instrumenter instrumenter = null;
        try {
            instrumenter = this.factory.getInstrumenter();
        } catch (FactoryMisconfigurationException e) {
            handleException(e);
        }

        Assert.assertNotNull(instrumenter);
        Assert.assertEquals(Charset.forName("UTF-16BE"), instrumenter.getCharset());
        Assert.assertTrue(instrumenter.pretend());
        Assert.assertFalse(instrumenter.verbose());
        Assert.assertSame(progressHandler, instrumenter.getProgressHandler());
        Assert.assertFalse(instrumenter.allowsFileListInstrumentation());
        Assert.assertEquals(2, instrumenter.getCriteria().size());
        Assert.assertTrue(instrumenter.getCriteria().contains(ConditionCoverage.getInstance()));
        Assert.assertTrue(instrumenter.getCriteria().contains(LoopCoverage.getInstance()));
        Assert.assertFalse(instrumenter.isCriterionSet(StatementCoverage.getInstance()));
        Assert.assertFalse(instrumenter.isCriterionSet(BranchCoverage.getInstance()));
        Assert.assertTrue(instrumenter.isCriterionSet(ConditionCoverage.getInstance()));
        Assert.assertTrue(instrumenter.isCriterionSet(LoopCoverage.getInstance()));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.InstrumenterFactory#getInstrumenter()}.
     * 
     * set and test
     */
    public void testGetInstrumenter4() {
        InstrumenterDescriptor descr = new UtilsForTestingInstr.TestInstrumenterDescriptor2();
        this.factory.setDescriptor(descr);
        this.factory.setCharset("UTF-16BE");
        this.factory.setVerboseMode(true);
        this.factory.addCriterion(ConditionCoverage.getInstance());
        this.factory.addCriterion(LoopCoverage.getInstance());
        Instrumenter instrumenter = null;
        try {
            instrumenter = this.factory.getInstrumenter();
        } catch (FactoryMisconfigurationException e) {
            handleException(e);
        }
        
        Assert.assertNotNull(instrumenter);
        Assert.assertEquals(Charset.forName("UTF-16BE"), instrumenter.getCharset());
        Assert.assertFalse(instrumenter.pretend());
        Assert.assertTrue(instrumenter.verbose());
        Assert.assertFalse(instrumenter.allowsFileListInstrumentation());
        Assert.assertEquals(2, instrumenter.getCriteria().size());
        Assert.assertTrue(instrumenter.getCriteria().contains(ConditionCoverage.getInstance()));
        Assert.assertTrue(instrumenter.getCriteria().contains(LoopCoverage.getInstance()));
        Assert.assertFalse(instrumenter.isCriterionSet(StatementCoverage.getInstance()));
        Assert.assertFalse(instrumenter.isCriterionSet(BranchCoverage.getInstance()));
        Assert.assertTrue(instrumenter.isCriterionSet(ConditionCoverage.getInstance()));
        Assert.assertTrue(instrumenter.isCriterionSet(LoopCoverage.getInstance()));
    }

    /**
     * Test method for {@link org.codecover.instrumentation.InstrumenterFactory#getInstrumenter()}.
     * 
     * provoke exceptions
     */
    public void testGetInstrumenterException1() {
        this.factory.setCharset("UTF-16BE");
        this.factory.setPretendMode(true);
        try {
            this.factory.getInstrumenter();
            Assert.fail("FactoryMisconfigurationException expected");
        } catch (FactoryMisconfigurationException e) {
            // expected
        }
    }

    /**
     * Test method for {@link org.codecover.instrumentation.InstrumenterFactory#getInstrumenter()}.
     * 
     * provoke exceptions
     */
    public void testGetInstrumenterException2() {
        InstrumenterDescriptor descr =
            new InstrumenterDescriptor(InstrumenterFactoryTest.class.getName() + "#testGetInstrumenterException2") {
            @Override
            protected Instrumenter getInstrumenter() {
                return null;
            }

            @Override
            public boolean accept(File file) {
                return false;
            }
        };
        this.factory.setDescriptor(descr);
        this.factory.setPretendMode(true);
        try {
            this.factory.getInstrumenter();
            Assert.fail("FactoryMisconfigurationException expected");
        } catch (FactoryMisconfigurationException e) {
            // expected
        }
    }
}
