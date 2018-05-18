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

package org.codecover.model.utils.file;

import static org.codecover.model.utils.file.AntPathMatcher.SEPARATOR_CHAR;

import java.io.File;

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: AntPathMatcherTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class AntPathMatcherTest extends TestCase {

    private static AntPathMatcher APM = new AntPathMatcher(); 

    public void testNormalize1() throws Exception {
        String before;
    
        before = "**";
        Assert.assertEquals("**", APM.normalizePattern(before));
    
        before = "**\\**";
        Assert.assertEquals("**", APM.normalizePattern(before));
    
        before = "**\\**\\**\\**\\**";
        Assert.assertEquals("**", APM.normalizePattern(before));
    
        before = "**\\hello\\**";
        Assert.assertEquals("**" + SEPARATOR_CHAR + "hello" + SEPARATOR_CHAR + "**", APM.normalizePattern(before));
    
        before = "**\\**\\**\\**\\**\\hello\\**\\**\\**\\**\\**";
        Assert.assertEquals("**" + SEPARATOR_CHAR + "hello" + SEPARATOR_CHAR + "**", APM.normalizePattern(before));
    
        before = "a";
        Assert.assertEquals("a", APM.normalizePattern(before));
    
        before = "";
        Assert.assertEquals("", APM.normalizePattern(before));
    
        before = "**\\**\\*\\**\\**\\hello\\**\\**\\**\\**\\**";
        Assert.assertEquals("**" + SEPARATOR_CHAR + "*" + SEPARATOR_CHAR + "**" + SEPARATOR_CHAR + "hello" + SEPARATOR_CHAR + "**", APM.normalizePattern(before));
    
        before = "foo\\";
        Assert.assertEquals("foo" + SEPARATOR_CHAR + "**", APM.normalizePattern(before));
    }

    public void testNormalize2() throws Exception {
        String before;
    
        before = "**";
        Assert.assertEquals("**", APM.normalizePattern(before));
    
        before = "**/**";
        Assert.assertEquals("**", APM.normalizePattern(before));
    
        before = "**/**/**/**/**";
        Assert.assertEquals("**", APM.normalizePattern(before));
    
        before = "**/hello/**";
        Assert.assertEquals("**" + SEPARATOR_CHAR + "hello" + SEPARATOR_CHAR + "**", APM.normalizePattern(before));
    
        before = "**/**/**/**/**/hello/**/**/**/**/**";
        Assert.assertEquals("**" + SEPARATOR_CHAR + "hello" + SEPARATOR_CHAR + "**", APM.normalizePattern(before));
    
        before = "a";
        Assert.assertEquals("a", APM.normalizePattern(before));
    
        before = "";
        Assert.assertEquals("", APM.normalizePattern(before));
    
        before = "**/**/*/**/**/hello/**/**/**/**/**";
        Assert.assertEquals("**" + SEPARATOR_CHAR + "*" + SEPARATOR_CHAR + "**" + SEPARATOR_CHAR + "hello" + SEPARATOR_CHAR + "**", APM.normalizePattern(before));
    
        before = "foo/";
        Assert.assertEquals("foo" + SEPARATOR_CHAR + "**", APM.normalizePattern(before));
    }

    public void testNormalize3() throws Exception {
        String before;

        before = "hello";
        Assert.assertSame(before, APM.normalizePath(before));
        
        before = "/";
        Assert.assertSame(before, APM.normalizePath(before));
        
        before = "hello/world";
        Assert.assertSame(before, APM.normalizePath(before));
        
        before = "hello\\world";
        Assert.assertEquals("hello/world", APM.normalizePath(before));
        
        before = "hello\\nice\\world";
        Assert.assertEquals("hello/nice/world", APM.normalizePath(before));
    }

    /**
     * Test method for {@link AntPathMatcher#doMatch(java.lang.String, java.lang.String, boolean)}.
     */
    public void testDoMatchTrue1() {
        String pattern = "hello";
        String pathToMatch = "hello";
        assertMatches(pattern, pathToMatch, true);
    }

    /**
     * Test method for {@link AntPathMatcher#doMatch(java.lang.String, java.lang.String, boolean)}.
     */
    public void testDoMatchTrue2() {
        String pattern = "hel?o";
        String pathToMatch = "hello";
        assertMatches(pattern, pathToMatch, true);
    }

    /**
     * Test method for {@link AntPathMatcher#doMatch(java.lang.String, java.lang.String, boolean)}.
     */
    public void testDoMatchTrue3() {
        String pattern = "he?o";
        String pathToMatch = "hello";
        assertMatchesNot(pattern, pathToMatch, true);
    }

    /**
     * Test method for {@link AntPathMatcher#doMatch(java.lang.String, java.lang.String, boolean)}.
     */
    public void testDoMatchTrue4() {
        String pattern = "he*o";
        String pathToMatch = "hello";
        assertMatches(pattern, pathToMatch, true);
    }

    /**
     * Test method for {@link AntPathMatcher#doMatch(java.lang.String, java.lang.String, boolean)}.
     */
    public void testDoMatchTrue5() {
        String pattern;
        String pathToMatch;

        pattern = "src\\he*o";
        pathToMatch = "src\\hello";
        assertMatches(pattern, pathToMatch, true);
        pathToMatch = "src\\helo";
        assertMatches(pattern, pathToMatch, true);
        pathToMatch = "src\\heo";
        assertMatches(pattern, pathToMatch, true);
    }

    /**
     * Test method for {@link AntPathMatcher#doMatch(java.lang.String, java.lang.String, boolean)}.
     */
    public void testDoMatchTrue6() {
        String pattern;
        String pathToMatch;

        pattern = "src/he*o";
        pathToMatch = "src\\hello";
        assertMatches(pattern, pathToMatch, true);

        pathToMatch = "src\\helo";
        assertMatches(pattern, pathToMatch, true);

        pathToMatch = "src\\heo";
        assertMatches(pattern, pathToMatch, true);
    }

    /**
     * Test method for {@link AntPathMatcher#doMatch(java.lang.String, java.lang.String, boolean)}.
     */
    public void testDoMatchTrue7() {
        String pattern;
        String pathToMatch;

        pattern = "src\\*";
        pathToMatch = "src\\hello";
        assertMatches(pattern, pathToMatch, true);

        pattern = "**\\*";
        pathToMatch = "src\\hello";
        assertMatches(pattern, pathToMatch, true);

        pattern = "**\\hello";
        pathToMatch = "src\\hello";
        assertMatches(pattern, pathToMatch, true);

        pattern = "**\\hello\\";
        pathToMatch = "src\\hello\\";
        assertMatches(pattern, pathToMatch, true);

        pattern = "**\\hello\\";
        pathToMatch = "src\\hello";
        assertMatches(pattern, pathToMatch, true);

        pattern = "**\\hello";
        pathToMatch = "src\\hello\\";
        assertMatches(pattern, pathToMatch, true);
    }

    /**
     * Test method for {@link AntPathMatcher#doMatch(java.lang.String, java.lang.String, boolean)}.
     */
    public void testDoMatchTrue8() {
        String pattern;
        String pathToMatch;

        pattern = "**\\src\\**\\hello";
        pathToMatch = "src\\hello";
        assertMatches(pattern, pathToMatch, true);

        pathToMatch = "src\\ahija\\hello";
        assertMatches(pattern, pathToMatch, true);

        pathToMatch = "ahija\\src\\hello";
        assertMatches(pattern, pathToMatch, true);

        pathToMatch = "ahija\\src\\ahija\\hello";
        assertMatches(pattern, pathToMatch, true);

        pathToMatch = "org\\de\\com\\ahija\\src\\klein\\groß\\ahija\\hello";
        assertMatches(pattern, pathToMatch, true);
    }

    /**
     * Test method for {@link AntPathMatcher#doMatch(java.lang.String, java.lang.String, boolean)}.
     */
    public void testDoMatchTrue9() {
        String pattern;
        String pathToMatch;

        pattern = "src\\ahija";
        pathToMatch = "src\\";
        assertMatchesNot(pattern, pathToMatch, true);

        pathToMatch = "src\\ahija";
        assertMatches(pattern, pathToMatch, true);
    }
    
    /**
     * Test method for {@link AntPathMatcher#doMatch(java.lang.String, java.lang.String, boolean)}.
     */
    public void testDoMatchTrue10() {
        String pattern;
        String pathToMatch;
        
        pattern = "src\\**\\ahija\\home";
        pathToMatch = "src\\ahija\\home";
        assertMatches(pattern, pathToMatch, true);
    }

    /**
     * Test method for {@link AntPathMatcher#doMatch(java.lang.String, java.lang.String, boolean)}.
     */
    public void testDoMatchFalse1() {
        String pattern;
        String pathToMatch;

        pattern = "**\\src\\**\\hello";
        pathToMatch = "src\\hello";
        assertMatches(pattern, pathToMatch, false);

        pathToMatch = "src\\ahija\\hello";
        assertMatches(pattern, pathToMatch, false);

        pathToMatch = "ahija\\src\\hello";
        assertMatches(pattern, pathToMatch, false);

        pathToMatch = "ahija\\src\\ahija\\hello";
        assertMatches(pattern, pathToMatch, false);

        pathToMatch = "org\\de\\com\\ahija\\src\\klein\\groß\\ahija\\hello";
        assertMatches(pattern, pathToMatch, false);
    }

    /**
     * Test method for {@link AntPathMatcher#doMatch(java.lang.String, java.lang.String, boolean)}.
     */
    public void testDoMatchFalse2() {
        String pattern;
        String pathToMatch;

        pattern = "**\\src\\ahija";
        pathToMatch = "src\\";
        assertMatches(pattern, pathToMatch, false);

        pathToMatch = "src\\ahija";
        assertMatches(pattern, pathToMatch, false);

        pathToMatch = "ahija\\src";
        assertMatches(pattern, pathToMatch, false);

        pathToMatch = "ahija\\src\\ahija";
        assertMatches(pattern, pathToMatch, false);

        pathToMatch = "org\\de\\com\\ahija\\src\\ahija\\klein\\groß";
        assertMatches(pattern, pathToMatch, false);
    }
    
    /**
     * Test method for {@link AntPathMatcher#doMatch(java.lang.String, java.lang.String, boolean)}.
     */
    public void testDoMatchFalse3() {
        String pattern;
        String pathToMatch;

        pattern = "src\\ahija";
        pathToMatch = "src\\";
        assertMatches(pattern, pathToMatch, false);

        pathToMatch = "src\\ahija";
        assertMatches(pattern, pathToMatch, false);

        pathToMatch = "src\\ahija\\";
        assertMatchesNot(pattern, pathToMatch, false);

        pattern = "src\\*\\";
        pathToMatch = "src\\ahija";
        assertMatches(pattern, pathToMatch, false);

        pathToMatch = "src\\ahija\\";
        assertMatches(pattern, pathToMatch, false);
    }

    public void testTime() {
        long time1 = System.currentTimeMillis();
        String pattern;
        String pathToMatch;

        pattern = "**\\src\\**\\hello";
        pathToMatch = "org\\de\\com\\ahija\\src\\klein\\groß\\ahija\\hello";
        for (int i = 0; i < 1E3; i++) {
            assertMatches(pattern, pathToMatch, false);
        }
        time1 = (System.currentTimeMillis() - time1);
    }

    /**
     * This is a test to verify that the caching of the tokenized patterns
     * is faster than without caching. This time test requires, that the
     * computer has a constant processor speed using for this test.
     * Unfortunately this test fails from time to time. Not used.
     */
    public void no_testTokenizeToStringArray() throws Exception {
        String pattern1 = "org\\de\\com\\ahija\\src\\ahija\\klein\\groß";
        String pattern2 = "org\\de\\com\\ahija\\src\\ahija\\klein\\";
        String pattern3 = "org\\de\\com\\ahija\\src\\ahija\\groß";
        String pattern4 = "org\\klein\\groß";
        String pattern5 = "**";

        // with caching
        long time1 = System.currentTimeMillis();
        for (int i = 0; i < 1E6; i++) {
            APM.tokenizeToStringArray(pattern1, File.separator, true);
            APM.tokenizeToStringArray(pattern2, File.separator, true);
            APM.tokenizeToStringArray(pattern3, File.separator, true);
            APM.tokenizeToStringArray(pattern4, File.separator, true);
            APM.tokenizeToStringArray(pattern5, File.separator, true);
        }
        time1 = (System.currentTimeMillis() - time1);

        // without caching
        APM = new AntPathMatcher();
        long time2 = System.currentTimeMillis();
        for (int i = 0; i < 1E5; i++) {
            APM.tokenizeToStringArray(pattern1, File.separator, false);
            APM.tokenizeToStringArray(pattern2, File.separator, false);
            APM.tokenizeToStringArray(pattern3, File.separator, false);
            APM.tokenizeToStringArray(pattern4, File.separator, false);
            APM.tokenizeToStringArray(pattern5, File.separator, false);
        }
        time2 = (System.currentTimeMillis() - time2);

        Assert.assertTrue("Caching is slower than not chaching. How it comes? (" + time1 + "<" + time2 +")",
            time1 < time2);
    }

    private void assertMatches(String pattern, String pathToMatch, boolean fullMatch) {
      Assert.assertTrue(checkMatching(pattern, pathToMatch, fullMatch));
    }

    private void assertMatchesNot(String pattern, String pathToMatch, boolean fullMatch) {
      Assert.assertFalse(checkMatching(pattern, pathToMatch, fullMatch));
    }

    private boolean checkMatching(String pattern, String pathToMatch, boolean fullMatch) {
      return APM.doMatch(APM.normalizePattern(pattern),
                         APM.normalizePath(pathToMatch),
                         fullMatch);
    }
}
