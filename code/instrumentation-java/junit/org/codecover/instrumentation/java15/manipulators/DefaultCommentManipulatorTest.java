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

package org.codecover.instrumentation.java15.manipulators;

import java.io.IOException;
import java.io.StringWriter;
import java.util.regex.Pattern;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.instrumentation.java15.parser.JavaParserConstants;
import org.codecover.instrumentation.java15.parser.Token;
import org.codecover.instrumentation.java15.syntaxtree.NodeToken;
import org.codecover.instrumentation.java15.visitor.TreeDumperWithException;

/**
 * @see DefaultCommentManipulator
 * 
 * @author Christoph Müller 
 *
 * @version 1.0 ($Id: DefaultCommentManipulatorTest.java 22 2008-05-25 20:08:53Z ahija $)
 */
public class DefaultCommentManipulatorTest extends TestCase {

    private DefaultCommentManipulator commentManipulator;
    private StringWriter stringWriter;

    @Override
    protected void setUp() throws Exception {
        this.commentManipulator = new DefaultCommentManipulator();
        this.stringWriter = new StringWriter();
        this.commentManipulator.setTreeDumper(new TreeDumperWithException(this.stringWriter));
    }

    @Override
    protected void tearDown() throws Exception {
        this.commentManipulator = null;
        this.stringWriter = null;
    }

    public void testStartPattern() throws Exception {
        Pattern pattern = DefaultCommentManipulator.START_PATTERN;

        Assert.assertFalse(pattern.matcher("").matches());
        Assert.assertFalse(pattern.matcher("//").matches());
        Assert.assertFalse(pattern.matcher("// ").matches());
        Assert.assertFalse(pattern.matcher("startTestCase()").matches());
        Assert.assertFalse(pattern.matcher("startTestCase(\"\")").matches());
        Assert.assertFalse(pattern.matcher("//startTestCase(\"\")").matches());
        Assert.assertFalse(pattern.matcher("startTestCase();").matches());
        Assert.assertFalse(pattern.matcher("startTestCase(\"\");").matches());
        Assert.assertFalse(pattern.matcher("//startTestCase(\"\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"\")").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"\");").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"hello\");").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"\", \"\");").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"hello\", \"comment\");").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"hello\\\"\", \"comment\");").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"hello\\\'\", \"comment\");").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"hello\\\\\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\n\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\r\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\t\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\b\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\f\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\", \"comment\n\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\", \"comment\r\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\", \"comment\f\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\", \"comment\b\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\", \"comment\t\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\\n\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\\r\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\\t\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\\b\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\\f\", \"comment\");").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"hello\", \"comment\\n\");").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"hello\", \"comment\\r\");").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"hello\", \"comment\\f\");").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"hello\", \"comment\\b\");").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"hello\", \"comment\\t\");").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"hello\", \"comment\\\\\");").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"hello\", \"comment\\\"\");").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"hello\", \"comment\\\'\");").matches());
        Assert.assertTrue(pattern.matcher("// startTestCase(\"First\");").matches());

        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\");").matches());
    }

    public void testEndPattern() throws Exception {
        Pattern pattern = DefaultCommentManipulator.END_PATTERN;

        Assert.assertFalse(pattern.matcher("").matches());
        Assert.assertFalse(pattern.matcher("//").matches());
        Assert.assertFalse(pattern.matcher("// ").matches());
        Assert.assertFalse(pattern.matcher("endTestCase()").matches());
        Assert.assertFalse(pattern.matcher("endTestCase(\"\")").matches());
        Assert.assertFalse(pattern.matcher("//endTestCase(\"\")").matches());
        Assert.assertFalse(pattern.matcher("endTestCase();").matches());
        Assert.assertFalse(pattern.matcher("endTestCase(\"\");").matches());
        Assert.assertFalse(pattern.matcher("//endTestCase(\"\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"\")").matches());

        Assert.assertTrue(pattern.matcher("// endTestCase();").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\\\"\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\\\'\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\\\\\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\", \"comment\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\\\"\", \"comment\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\\\'\", \"comment\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\\\\\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\n\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\r\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\t\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\b\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\f\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\", \"comment\n\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\", \"comment\r\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\", \"comment\f\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\", \"comment\b\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\", \"comment\t\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\\n\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\\r\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\\t\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\\b\", \"comment\");").matches());
        Assert.assertFalse(pattern.matcher("// endTestCase(\"hello\\f\", \"comment\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\", \"comment\\n\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\", \"comment\\r\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\", \"comment\\f\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\", \"comment\\b\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\", \"comment\\t\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\", \"comment\\\\\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\", \"comment\\\"\");").matches());
        Assert.assertTrue(pattern.matcher("// endTestCase(\"hello\", \"comment\\\'\");").matches());

        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\\n\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\\r\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\\t\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\\b\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"hello\\f\");").matches());
        Assert.assertFalse(pattern.matcher("// startTestCase(\"First\");").matches());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialStart1() throws Exception {
        String call = "startTestCase(\"Name\");";
        Assert.assertTrue(manipulate("// " + call));
        Assert.assertEquals("org.codecover.instrumentation.java.measurement.Protocol." + call + "\n", this.stringWriter.toString());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialStart2() throws Exception {
        String call = "startTestCase(\"\");";
        Assert.assertTrue(manipulate("// " + call));
        Assert.assertEquals("org.codecover.instrumentation.java.measurement.Protocol." + call + "\n", this.stringWriter.toString());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialStart3() throws Exception {
        String call = "startTestCase(\"Name\", \"Comment\");";
        Assert.assertTrue(manipulate("// " + call));
        Assert.assertEquals("org.codecover.instrumentation.java.measurement.Protocol." + call + "\n", this.stringWriter.toString());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialStart4() throws Exception {
        String call = "startTestCase(\"Name\", \"Comment\");";
        Assert.assertFalse(manipulate("//" + call));
        Assert.assertEquals("", this.stringWriter.toString());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialStart5() throws Exception {
        String call = " this is a comment";
        Assert.assertFalse(manipulate("// " + call));
        Assert.assertEquals("", this.stringWriter.toString());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialEnd() throws Exception {
        String call = "endTestCase();";
        Assert.assertTrue(manipulate("// " + call));
        Assert.assertEquals("org.codecover.instrumentation.java.measurement.Protocol." + call + "\n", this.stringWriter.toString());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialEnd2() throws Exception {
        String call = "endTestCase(\"\");";
        Assert.assertTrue(manipulate("// " + call));
        Assert.assertEquals("org.codecover.instrumentation.java.measurement.Protocol." + call + "\n", this.stringWriter.toString());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialEnd3() throws Exception {
        String call = "endTestCase(\"Name\");";
        Assert.assertTrue(manipulate("// " + call));
        Assert.assertEquals("org.codecover.instrumentation.java.measurement.Protocol." + call + "\n", this.stringWriter.toString());
    }
    
    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialEnd4() throws Exception {
        String call = "endTestCase(\"Name\", \"comment\");";
        Assert.assertTrue(manipulate("// " + call));
        Assert.assertEquals("org.codecover.instrumentation.java.measurement.Protocol." + call + "\n", this.stringWriter.toString());
    }
    
    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialEnd5() throws Exception {
        String call = "endTestCase(\"Name\", \"comment\");";
        Assert.assertFalse(manipulate("//" + call));
        Assert.assertEquals("", this.stringWriter.toString());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialEnd6() throws Exception {
        String call = "endTestCase(\"Name\");";
        Assert.assertFalse(manipulate("//" + call));
        Assert.assertEquals("", this.stringWriter.toString());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialEnd7() throws Exception {
        String call = " this is a comment";
        Assert.assertFalse(manipulate("// " + call));
        Assert.assertEquals("", this.stringWriter.toString());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialFinish1() throws Exception {
        String call = "finishTestSession();";
        Assert.assertTrue(manipulate("// " + call));
        Assert.assertEquals("org.codecover.instrumentation.java.measurement.Protocol." + call + "\n", this.stringWriter.toString());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialFinish2() throws Exception {
        String call = "finishTestSession();";
        Assert.assertFalse(manipulate("//" + call));
        Assert.assertEquals("", this.stringWriter.toString());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialFinish3() throws Exception {
        String call = "finishTestSession()";
        Assert.assertFalse(manipulate("// " + call));
        Assert.assertEquals("", this.stringWriter.toString());
    }

    /**
     * Test method for
     * {@link org.codecover.instrumentation.java15.visitor.InstrumentationVisitor#visitSpecial(org.codecover.instrumentation.java15.syntaxtree.NodeToken)}.
     */
    public final void testVisitSpecialFinish4() throws Exception {
        String call = "finishTestSession(\"\");";
        Assert.assertFalse(manipulate("//" + call));
        Assert.assertEquals("", this.stringWriter.toString());
    }

    private boolean manipulate(String stringToBeTheComment) throws IOException {
        Token token = Token.newToken(JavaParserConstants.SINGLE_LINE_COMMENT);
        token.setImages(stringToBeTheComment, stringToBeTheComment);
        NodeToken nodeToken = NodeToken.createToken(token);
        return this.commentManipulator.manipulate(nodeToken);
    }
}
