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

package org.codecover.instrumentation.java15.measurement;

import static org.codecover.UtilsForTestingJava.BASEDIR;
import static org.codecover.UtilsForTestingJava.TARGET;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJava;
import org.codecover.instrumentation.java.measurement.CoverageLogPath;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: CoverageLogPathTest.java 15 2008-05-24 20:59:06Z ahija $)
 */
public class CoverageLogPathTest extends TestCase {

    private Class<?> instrumentationUtilClass = CoverageLogPath.class;

    @Override
    protected void setUp() throws Exception {
        UtilsForTestingJava.clearTarget();
        System.clearProperty(CoverageLogPath.PROPERTY_PATH_VARIABLE);
        reloadClass();
    }
    
    private void reloadClass() throws Exception {
        URL url = new File(UtilsForTestingJava.BASEDIR + ".." + File.separator + "instrumentation-java-measurement" + File.separator + "bin").getCanonicalFile().toURL();
        URLClassLoader loader = new URLClassLoader(new URL[]{url}, null);
        Class<?> instrumentationUtilClassOld = this.instrumentationUtilClass;
        this.instrumentationUtilClass = loader.loadClass("org.codecover.instrumentation.java.measurement.CoverageLogPath");
        Assert.assertNotSame(instrumentationUtilClassOld, this.instrumentationUtilClass);
        Assert.assertNotSame(CoverageLogPath.class, this.instrumentationUtilClass);
    }

    public void testGetCoverageLogFileProperty() throws Exception {
        String clfPath = TARGET + "clf.clf";
        System.setProperty(CoverageLogPath.PROPERTY_PATH_VARIABLE, clfPath);
        File getCovLF = getCoverageLog();
        Assert.assertEquals(new File(clfPath), getCovLF);
        Assert.assertSame(getCovLF, getCoverageLog());
    }

    public void testGetCoverageLogFileUsingCoverageLogPath() throws Exception {
        File clfFile = getCoverageLog();
        String name = clfFile.getName();
        Assert.assertEquals(new File(BASEDIR), clfFile.getParentFile());
        Assert.assertTrue("does not match: " + name, name.matches("coverage-log-\\d\\d\\d\\d-\\d\\d-\\d\\d-\\d\\d-\\d\\d-\\d\\d-\\d\\d\\d\\.clf"));
        Assert.assertSame(clfFile, getCoverageLog());
    }

    public void testGetCoverageLogFileExists() throws Exception {
        File clfFile = new File(TARGET + "clf.clf").getAbsoluteFile();
        File getCovLF;
        System.setProperty(CoverageLogPath.PROPERTY_PATH_VARIABLE, clfFile.getAbsolutePath());
        getCovLF = getCoverageLog();
        Assert.assertEquals(clfFile, getCovLF);
        Assert.assertSame(getCovLF, getCoverageLog());

        reloadClass();
        System.setProperty(CoverageLogPath.PROPERTY_PATH_VARIABLE, clfFile.getAbsolutePath());
        Assert.assertTrue(clfFile.createNewFile());
        File clfFileAlt = new File(TARGET + "clf(1).clf").getAbsoluteFile();
        getCovLF = getCoverageLog();
        Assert.assertEquals(clfFileAlt, getCovLF);
        Assert.assertSame(getCovLF, getCoverageLog());

        reloadClass();
        System.setProperty(CoverageLogPath.PROPERTY_PATH_VARIABLE, clfFile.getAbsolutePath());
        // now we set overwrite true
        System.setProperty(CoverageLogPath.PROPERTY_OVERWRITE_VARIABLE, "");
        getCovLF = getCoverageLog();
        Assert.assertEquals(clfFile, getCovLF);
        Assert.assertSame(getCovLF, getCoverageLog());

        reloadClass();
        System.setProperty(CoverageLogPath.PROPERTY_PATH_VARIABLE, clfFile.getAbsolutePath());
        // now we set overwrite true
        System.setProperty(CoverageLogPath.PROPERTY_OVERWRITE_VARIABLE, "true");
        getCovLF = getCoverageLog();
        Assert.assertEquals(clfFile, getCovLF);
        Assert.assertSame(getCovLF, getCoverageLog());

        reloadClass();
        System.setProperty(CoverageLogPath.PROPERTY_PATH_VARIABLE, clfFile.getAbsolutePath());
        // now we set overwrite false
        System.setProperty(CoverageLogPath.PROPERTY_OVERWRITE_VARIABLE, "false");
        getCovLF = getCoverageLog();
        Assert.assertEquals(clfFileAlt, getCovLF);
        Assert.assertSame(getCovLF, getCoverageLog());

        reloadClass();
        System.setProperty(CoverageLogPath.PROPERTY_PATH_VARIABLE, clfFile.getAbsolutePath());
        // now we set overwrite false
        System.setProperty(CoverageLogPath.PROPERTY_OVERWRITE_VARIABLE, "no");
        getCovLF = getCoverageLog();
        Assert.assertEquals(clfFileAlt, getCovLF);
        Assert.assertSame(getCovLF, getCoverageLog());
    }

    public void testPattern() throws Exception {
        final Pattern UNIQUE_COVERAGE_LOG_PATTERN = Pattern
        .compile(CoverageLogPath.ALTERNATIVE_FILE_PATTERN_STRING);

        Matcher matcher;
        String search;

        matcher = UNIQUE_COVERAGE_LOG_PATTERN.matcher("coverage-log");
        Assert.assertFalse(matcher.matches());

        search = "coverage-log(1)";
        matcher = UNIQUE_COVERAGE_LOG_PATTERN.matcher(search);
        Assert.assertTrue(matcher.matches());
        Assert.assertEquals(13, matcher.start(1));
        Assert.assertEquals(14, matcher.end(1));
        Assert.assertEquals("1", search.substring(matcher.start(1), matcher.end(1)));

        search = "coverage-log(12)";
        matcher = UNIQUE_COVERAGE_LOG_PATTERN.matcher(search);
        Assert.assertTrue(matcher.matches());
        Assert.assertEquals(13, matcher.start(1));
        Assert.assertEquals(15, matcher.end(1));
        Assert.assertEquals("12", search.substring(matcher.start(1), matcher.end(1)));
    }

    public void testGetAlternativeFileSimple() {
        File clfFileBefore;
        File clfFileAfter;

        clfFileBefore = new File(TARGET + "COVERAGE.clf");
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        Assert.assertEquals(clfFileBefore, clfFileAfter);
    }

    public void testGetAlternativeFileSimpleNoExtension() {
        File clfFileBefore;
        File clfFileAfter;

        clfFileBefore = new File(TARGET + "COVERAGE");
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        Assert.assertEquals(clfFileBefore, clfFileAfter);
    }

    public void testGetAlternativeFileExists() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;

        clfFileBefore = new File(TARGET + "COVERAGE.clf");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "COVERAGE(1).clf");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }

    public void testGetAlternativeFileExistsNoExtension() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;

        clfFileBefore = new File(TARGET + "COVERAGE");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "COVERAGE(1)");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }

    public void testGetAlternativeFileExistsTwice() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;

        clfFileBefore = new File(TARGET + "COVERAGE(1).clf");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileBefore = new File(TARGET + "COVERAGE.clf");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "COVERAGE(2).clf");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }

    public void testGetAlternativeFileExistsTwiceNoExtension() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;

        clfFileBefore = new File(TARGET + "COVERAGE(1)");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileBefore = new File(TARGET + "COVERAGE");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "COVERAGE(2)");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }

    public void testGetAlternativeFileExists189() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;

        clfFileBefore = new File(TARGET + "COVERAGE(189).clf");
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "COVERAGE(189).clf");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }

    public void testGetAlternativeFileExists189NoExtension() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;

        clfFileBefore = new File(TARGET + "COVERAGE(189)");
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "COVERAGE(189)");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }

    public void testGetAlternativeFileExists189Exists() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;
        
        clfFileBefore = new File(TARGET + "COVERAGE(194).clf");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileBefore = new File(TARGET + "COVERAGE(193).clf");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileBefore = new File(TARGET + "COVERAGE(192).clf");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileBefore = new File(TARGET + "COVERAGE(191).clf");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileBefore = new File(TARGET + "COVERAGE(190).clf");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileBefore = new File(TARGET + "COVERAGE(189).clf");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "COVERAGE(195).clf");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }
    
    public void testGetAlternativeFileExists189ExistsNoExtension() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;
        
        clfFileBefore = new File(TARGET + "COVERAGE(194)");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileBefore = new File(TARGET + "COVERAGE(193)");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileBefore = new File(TARGET + "COVERAGE(192)");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileBefore = new File(TARGET + "COVERAGE(191)");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileBefore = new File(TARGET + "COVERAGE(190)");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileBefore = new File(TARGET + "COVERAGE(189)");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "COVERAGE(195)");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }

    public void testGetAlternativeSmall() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;

        clfFileBefore = new File(TARGET + "r.clf");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(1).clf");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }

    public void testGetAlternativeSmallNoExtension() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;

        clfFileBefore = new File(TARGET + "r");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(1)");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }

    public void testGetAlternativeLargeNumber() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;

        clfFileBefore = new File(TARGET + "r(123456789012345678).clf");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(123456789012345679).clf");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }

    public void testGetAlternativeLargeNumberNoExtension() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;

        clfFileBefore = new File(TARGET + "r(123456789012345678)");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(123456789012345679)");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }

    public void testGetAlternativeLargeNumberLimit() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;

        clfFileBefore = new File(TARGET + "r(999999999999999999).clf");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(1000000000000000000).clf");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
        
        clfFileBefore = clfFileExpected;
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(1000000000000000000)(1).clf");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }

    public void testGetAlternativeLargeNumberLimitNoExtension() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;

        clfFileBefore = new File(TARGET + "r(999999999999999999)");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(1000000000000000000)");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
        
        clfFileBefore = clfFileExpected;
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(1000000000000000000)(1)");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }
    
    public void testGetAlternativeVeryLargeNumber() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;

        clfFileBefore = new File(TARGET + "r(1234567890123456789).clf");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(1234567890123456789)(1).clf");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }

    public void testGetAlternativeVeryLargeNumberNoExtension() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;

        clfFileBefore = new File(TARGET + "r(1234567890123456789)");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(1234567890123456789)(1)");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }
    
    public void testGetAlternativeCharacters() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;

        clfFileBefore = new File(TARGET + "r(ABC).clf");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(ABC)(1).clf");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }
    
    public void testGetAlternativeCharactersNoExtension() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;
        
        clfFileBefore = new File(TARGET + "r(ABC)");
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(ABC)(1)");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }
    
    public void testGetAlternativeDoubleBrackets() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;
        
        clfFileBefore = new File(TARGET + "r(128)(458).clf");
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(128)(458).clf");
        Assert.assertEquals(clfFileExpected, clfFileAfter);

        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(128)(459).clf");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }
    
    public void testGetAlternativeDoubleBracketsNoExtension() throws IOException {
        File clfFileBefore;
        File clfFileAfter;
        File clfFileExpected;
        
        clfFileBefore = new File(TARGET + "r(128)(458)");
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(128)(458)");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
        
        Assert.assertTrue(clfFileBefore.createNewFile());
        clfFileAfter = CoverageLogPath.getAlternativeFile(clfFileBefore);
        clfFileExpected = new File(TARGET + "r(128)(459)");
        Assert.assertEquals(clfFileExpected, clfFileAfter);
    }

    private File getCoverageLog() throws Exception {
        Method getCLF = this.instrumentationUtilClass.getMethod("getCoverageLogFile", new Class[]{String.class});
        return (File) getCLF.invoke(null, new Object[]{null});
    }
}
