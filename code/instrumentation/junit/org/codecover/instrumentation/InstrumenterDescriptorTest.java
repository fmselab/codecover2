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

import java.io.File;
import java.nio.charset.Charset;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.ConditionCoverage;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: InstrumenterDescriptorTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InstrumenterDescriptorTest extends TestCase {

    private InstrumenterDescriptor instance;

    @Override
    protected void setUp() {
        this.instance = new TestDescriptor1();
    }

    @Override
    protected void tearDown() {
        this.instance = null;
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.InstrumenterDescriptor#InstrumenterDescriptor(String)}.
     */
    public void testInstrumenterDescriptor() {
        Assert.assertEquals(TestDescriptor1.class.getName(), this.instance.getUniqueKey());
        Assert.assertNull(this.instance.getLanguageName());
        Assert.assertNull(this.instance.getDescription());
        Assert.assertNull(this.instance.getAuthor());
        Assert.assertTrue(this.instance.getSupportedCriteria().isEmpty());
        Assert.assertNull(this.instance.getDefaultCharset());
        Assert.assertNull(this.instance.getInstrumenter());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.InstrumenterDescriptor#getLanguageName()}.
     */
    public void testGetLanguageName() {
        String name = "Java";
        this.instance.setLanguageName(name);

        Assert.assertEquals(TestDescriptor1.class.getName(), this.instance.getUniqueKey());
        Assert.assertSame(name, this.instance.getLanguageName());
        Assert.assertNull(this.instance.getDescription());
        Assert.assertNull(this.instance.getAuthor());
        Assert.assertTrue(this.instance.getSupportedCriteria().isEmpty());
        Assert.assertNull(this.instance.getDefaultCharset());
        Assert.assertNull(this.instance.getInstrumenter());

        try {
            this.instance.setLanguageName(null);
            Assert.fail("NullpointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
        }
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.InstrumenterDescriptor#getDescription()}.
     */
    public void testGetDescription() {
        String description = "Java 1.5 Instrumenter";
        this.instance.setDescription(description);

        Assert.assertEquals(TestDescriptor1.class.getName(), this.instance.getUniqueKey());
        Assert.assertNull(this.instance.getLanguageName());
        Assert.assertSame(description, this.instance.getDescription());
        Assert.assertNull(this.instance.getAuthor());
        Assert.assertTrue(this.instance.getSupportedCriteria().isEmpty());
        Assert.assertNull(this.instance.getDefaultCharset());
        Assert.assertNull(this.instance.getInstrumenter());

        try {
            this.instance.setDescription(null);
            Assert.fail("NullpointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
        }
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.InstrumenterDescriptor#getAuthor()}.
     */
    public void testGetAuthor() {
        String author = "Sir Bartholomy José Francis Drake van the victorious war II.";
        this.instance.setAuthor(author);

        Assert.assertEquals(TestDescriptor1.class.getName(), this.instance.getUniqueKey());
        Assert.assertNull(this.instance.getLanguageName());
        Assert.assertNull(this.instance.getDescription());
        Assert.assertSame(author, this.instance.getAuthor());
        Assert.assertTrue(this.instance.getSupportedCriteria().isEmpty());
        Assert.assertNull(this.instance.getDefaultCharset());
        Assert.assertNull(this.instance.getInstrumenter());

        try {
            this.instance.setAuthor(null);
            Assert.fail("NullpointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
        }
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.InstrumenterDescriptor#getDefaultCharset()}.
     */
    public void testGetDefaultCharset() {
        Charset charset = Charset.forName("UTF-16");
        this.instance.setDefaultCharset(charset);

        Assert.assertEquals(TestDescriptor1.class.getName(), this.instance.getUniqueKey());
        Assert.assertNull(this.instance.getLanguageName());
        Assert.assertNull(this.instance.getDescription());
        Assert.assertNull(this.instance.getAuthor());
        Assert.assertTrue(this.instance.getSupportedCriteria().isEmpty());
        Assert.assertSame(charset, this.instance.getDefaultCharset());
        Assert.assertNull(this.instance.getInstrumenter());

        try {
            charset = null;
            this.instance.setDefaultCharset(charset);
            Assert.fail("NullpointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
        }
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.InstrumenterDescriptor#setDefaultCharset(java.lang.String)}.
     */
    public void testSetDefaultCharsetString() {
        String charset = "UTF-16";
        this.instance.setDefaultCharset(charset);

        Assert.assertEquals(TestDescriptor1.class.getName(), this.instance.getUniqueKey());
        Assert.assertNull(this.instance.getLanguageName());
        Assert.assertNull(this.instance.getDescription());
        Assert.assertNull(this.instance.getAuthor());
        Assert.assertTrue(this.instance.getSupportedCriteria().isEmpty());
        Assert.assertEquals(charset, this.instance.getDefaultCharset().toString());
        Assert.assertNull(this.instance.getInstrumenter());

        try {
            charset = null;
            this.instance.setDefaultCharset(charset);
            Assert.fail("NullpointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
        }
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.InstrumenterDescriptor#getSupportedCriteria()}.
     */
    public void testGetSupportedCriteria() {
        Assert.assertEquals(TestDescriptor1.class.getName(), this.instance.getUniqueKey());
        Assert.assertFalse(this.instance.isCriterionSupported(StatementCoverage.getInstance()));
        Assert.assertFalse(this.instance.isCriterionSupported(BranchCoverage.getInstance()));
        Assert.assertFalse(this.instance.isCriterionSupported(ConditionCoverage.getInstance()));
        Assert.assertFalse(this.instance.isCriterionSupported(LoopCoverage.getInstance()));

        Assert.assertTrue(this.instance.getSupportedCriteria().isEmpty());
        this.instance.addSupportedCriteria(StatementCoverage.getInstance());
        Assert.assertFalse(this.instance.getSupportedCriteria().isEmpty());
        Assert.assertEquals(1, this.instance.getSupportedCriteria().size());
        Assert.assertSame(StatementCoverage.getInstance(), this.instance.getSupportedCriteria().iterator().next());
        Assert.assertTrue(this.instance.getSupportedCriteria().contains(StatementCoverage.getInstance()));
        Assert.assertTrue(this.instance.isCriterionSupported(StatementCoverage.getInstance()));
        Assert.assertFalse(this.instance.isCriterionSupported(BranchCoverage.getInstance()));
        Assert.assertFalse(this.instance.isCriterionSupported(ConditionCoverage.getInstance()));
        Assert.assertFalse(this.instance.isCriterionSupported(LoopCoverage.getInstance()));
        tearDown();
        setUp();

        Assert.assertTrue(this.instance.getSupportedCriteria().isEmpty());
        this.instance.addSupportedCriteria(BranchCoverage.getInstance());
        Assert.assertFalse(this.instance.getSupportedCriteria().isEmpty());
        Assert.assertEquals(1, this.instance.getSupportedCriteria().size());
        Assert.assertSame(BranchCoverage.getInstance(), this.instance.getSupportedCriteria().iterator().next());
        Assert.assertTrue(this.instance.getSupportedCriteria().contains(BranchCoverage.getInstance()));
        Assert.assertFalse(this.instance.isCriterionSupported(StatementCoverage.getInstance()));
        Assert.assertTrue(this.instance.isCriterionSupported(BranchCoverage.getInstance()));
        Assert.assertFalse(this.instance.isCriterionSupported(ConditionCoverage.getInstance()));
        Assert.assertFalse(this.instance.isCriterionSupported(LoopCoverage.getInstance()));
        tearDown();
        setUp();

        Assert.assertTrue(this.instance.getSupportedCriteria().isEmpty());
        this.instance.addSupportedCriteria(ConditionCoverage.getInstance());
        Assert.assertFalse(this.instance.getSupportedCriteria().isEmpty());
        Assert.assertEquals(1, this.instance.getSupportedCriteria().size());
        Assert.assertSame(ConditionCoverage.getInstance(), this.instance.getSupportedCriteria().iterator().next());
        Assert.assertTrue(this.instance.getSupportedCriteria().contains(ConditionCoverage.getInstance()));
        Assert.assertFalse(this.instance.isCriterionSupported(StatementCoverage.getInstance()));
        Assert.assertFalse(this.instance.isCriterionSupported(BranchCoverage.getInstance()));
        Assert.assertTrue(this.instance.isCriterionSupported(ConditionCoverage.getInstance()));
        Assert.assertFalse(this.instance.isCriterionSupported(LoopCoverage.getInstance()));
        tearDown();
        setUp();

        Assert.assertTrue(this.instance.getSupportedCriteria().isEmpty());
        this.instance.addSupportedCriteria(LoopCoverage.getInstance());
        Assert.assertFalse(this.instance.getSupportedCriteria().isEmpty());
        Assert.assertEquals(1, this.instance.getSupportedCriteria().size());
        Assert.assertSame(LoopCoverage.getInstance(), this.instance.getSupportedCriteria().iterator().next());
        Assert.assertTrue(this.instance.getSupportedCriteria().contains(LoopCoverage.getInstance()));
        Assert.assertFalse(this.instance.isCriterionSupported(StatementCoverage.getInstance()));
        Assert.assertFalse(this.instance.isCriterionSupported(BranchCoverage.getInstance()));
        Assert.assertFalse(this.instance.isCriterionSupported(ConditionCoverage.getInstance()));
        Assert.assertTrue(this.instance.isCriterionSupported(LoopCoverage.getInstance()));
        tearDown();
        setUp();

        Assert.assertTrue(this.instance.getSupportedCriteria().isEmpty());
        this.instance.addSupportedCriteria(StatementCoverage.getInstance());
        Assert.assertFalse(this.instance.getSupportedCriteria().isEmpty());
        Assert.assertEquals(1, this.instance.getSupportedCriteria().size());
        this.instance.addSupportedCriteria(BranchCoverage.getInstance());
        Assert.assertFalse(this.instance.getSupportedCriteria().isEmpty());
        Assert.assertEquals(2, this.instance.getSupportedCriteria().size());
        this.instance.addSupportedCriteria(ConditionCoverage.getInstance());
        Assert.assertFalse(this.instance.getSupportedCriteria().isEmpty());
        Assert.assertEquals(3, this.instance.getSupportedCriteria().size());
        this.instance.addSupportedCriteria(LoopCoverage.getInstance());
        Assert.assertFalse(this.instance.getSupportedCriteria().isEmpty());
        Assert.assertEquals(4, this.instance.getSupportedCriteria().size());
        this.instance.addSupportedCriteria(LoopCoverage.getInstance());
        Assert.assertFalse(this.instance.getSupportedCriteria().isEmpty());
        Assert.assertEquals(4, this.instance.getSupportedCriteria().size());
        Assert.assertTrue(this.instance.getSupportedCriteria().contains(StatementCoverage.getInstance()));
        Assert.assertTrue(this.instance.getSupportedCriteria().contains(BranchCoverage.getInstance()));
        Assert.assertTrue(this.instance.getSupportedCriteria().contains(ConditionCoverage.getInstance()));
        Assert.assertTrue(this.instance.getSupportedCriteria().contains(LoopCoverage.getInstance()));
        Assert.assertTrue(this.instance.isCriterionSupported(StatementCoverage.getInstance()));
        Assert.assertTrue(this.instance.isCriterionSupported(BranchCoverage.getInstance()));
        Assert.assertTrue(this.instance.isCriterionSupported(ConditionCoverage.getInstance()));
        Assert.assertTrue(this.instance.isCriterionSupported(LoopCoverage.getInstance()));

        try {
            this.instance.addSupportedCriteria(null);
            Assert.fail("NullpointerException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof NullPointerException);
        }

        Assert.assertNull(this.instance.getLanguageName());
        Assert.assertNull(this.instance.getDescription());
        Assert.assertNull(this.instance.getAuthor());
        Assert.assertNull(this.instance.getDefaultCharset());
        Assert.assertNull(this.instance.getInstrumenter());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.InstrumenterDescriptor#compareTo(org.codecover.instrumentation.InstrumenterDescriptor)}.
     */
    public void testCompareTo() {
        InstrumenterDescriptor descr1 = new TestDescriptor1();
        InstrumenterDescriptor descr2 = new TestDescriptor2();
        InstrumenterDescriptor descr3 = new TestDescriptor3();

        Assert.assertEquals(0, descr1.compareTo(descr1));
        Assert.assertEquals(0, descr2.compareTo(descr2));
        Assert.assertEquals(0, descr2.compareTo(descr2));

        Assert.assertTrue(descr1.compareTo(descr2) < 0);
        Assert.assertTrue(descr2.compareTo(descr3) < 0);
        Assert.assertTrue(descr1.compareTo(descr3) < 0);

        Assert.assertTrue(descr2.compareTo(descr1) > 0);
        Assert.assertTrue(descr3.compareTo(descr1) > 0);
        Assert.assertTrue(descr3.compareTo(descr2) > 0);
    }

    static class TestDescriptor1 extends InstrumenterDescriptor {
        
        public TestDescriptor1() {
            super(TestDescriptor1.class.getName());
        }
        
        @Override
        protected Instrumenter getInstrumenter() {
            return null;
        }

        @Override
        public boolean accept(File file) {
            return false;
        }
    }

    static class TestDescriptor2 extends InstrumenterDescriptor {

        public TestDescriptor2() {
            super(TestDescriptor2.class.getName());
        }

        @Override
        protected Instrumenter getInstrumenter() {
            return null;
        }

        @Override
        public boolean accept(File file) {
            return false;
        }
    }

    static class TestDescriptor3 extends InstrumenterDescriptor {

        public TestDescriptor3() {
            super(TestDescriptor3.class.getName());
        }

        @Override
        protected Instrumenter getInstrumenter() {
            return null;
        }

        @Override
        public boolean accept(File file) {
            return false;
        }
    }
}
