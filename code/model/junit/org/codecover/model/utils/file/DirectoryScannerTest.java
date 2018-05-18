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

import static org.codecover.UtilsForTestingModel.BASEDIR;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.LinkedList;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.model.utils.file.listener.FileFoundListener;

/**
 * Tests for {@link DirectoryScanner}.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: DirectoryScannerTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DirectoryScannerTest extends TestCase {

    private static final File ROOT_DIR = new File(BASEDIR + "src" + File.separatorChar);

    private static final File LARGE_DIR = ROOT_DIR.getParentFile();

    private static final String REL_PATH_TO_EXC = "org" +
    File.separatorChar + "codecover" + File.separatorChar +
    "model" + File.separatorChar + "exceptions" + File.separatorChar;

    private static final File ROOT_DIR_EXC = new File(BASEDIR + "src" + 
            File.separatorChar + REL_PATH_TO_EXC);

    private DirectoryScanner directoryScanner;

    private AssertFileFoundListener aFFL;

    @Override
    protected void setUp() throws Exception {
        this.directoryScanner = new DirectoryScanner();
        this.aFFL = new AssertFileFoundListener();
    }

    @Override
    protected void tearDown() throws Exception {
        this.directoryScanner = null;
        this.aFFL = null;
    }

    public void testIncludeNothing() throws Exception {
        this.directoryScanner.scan(ROOT_DIR, this.aFFL);
        this.aFFL.finish();
    }

    public void testIgnoreEverything() throws Exception {
        this.directoryScanner.addIgnorePattern("**");
        this.directoryScanner.addIncludePattern("**");
        this.directoryScanner.scan(ROOT_DIR, this.aFFL);
        this.aFFL.finish();
    }

    public void testIncludeEverything() throws Exception {
        this.aFFL.expectedIncludes.add("FileLoadException.java");
        this.aFFL.expectedIncludes.add("FileLoadIOException.java");
        this.aFFL.expectedIncludes.add("FileLoadParseException.java");
        this.aFFL.expectedIncludes.add("FileSaveException.java");
        this.aFFL.expectedIncludes.add("FileSaveIOException.java");
        this.aFFL.expectedIncludes.add("MergeException.java");
        this.aFFL.expectedIncludes.add("ModelException.java");
        this.aFFL.expectedIncludes.add("NameAlreadyUsedException.java");
        this.aFFL.expectedIncludes.add("package-info.java");

        this.directoryScanner.addIgnorePattern("**/.svn/**");
        this.directoryScanner.addIncludePattern("**");
        this.directoryScanner.scan(ROOT_DIR_EXC, this.aFFL);
        this.aFFL.finish();
    }

    public void testIncludesEverything() throws Exception {
        this.aFFL.expectedIncludes.add("FileLoadException.java");
        this.aFFL.expectedIncludes.add("FileLoadIOException.java");
        this.aFFL.expectedIncludes.add("FileLoadParseException.java");
        this.aFFL.expectedIncludes.add("FileSaveException.java");
        this.aFFL.expectedIncludes.add("FileSaveIOException.java");
        this.aFFL.expectedIncludes.add("MergeException.java");
        this.aFFL.expectedIncludes.add("ModelException.java");
        this.aFFL.expectedIncludes.add("NameAlreadyUsedException.java");
        this.aFFL.expectedIncludes.add("package-info.java");

        this.directoryScanner.addIgnorePattern("**/.svn/**");
        this.directoryScanner.addIncludePatterns(new String[]{"**", "**/.java"});
        this.directoryScanner.scan(ROOT_DIR_EXC, this.aFFL);
        this.aFFL.finish();
    }

    public void testIncludeJava() throws Exception {
        this.aFFL.expectedIncludes.add("FileLoadException.java");
        this.aFFL.expectedIncludes.add("FileLoadIOException.java");
        this.aFFL.expectedIncludes.add("FileLoadParseException.java");
        this.aFFL.expectedIncludes.add("FileSaveException.java");
        this.aFFL.expectedIncludes.add("FileSaveIOException.java");
        this.aFFL.expectedIncludes.add("MergeException.java");
        this.aFFL.expectedIncludes.add("ModelException.java");
        this.aFFL.expectedIncludes.add("NameAlreadyUsedException.java");
        this.aFFL.expectedIncludes.add("package-info.java");

        this.directoryScanner.addIgnorePattern("**/.svn/**");
        this.directoryScanner.addIncludePattern("*.java");
        this.directoryScanner.scan(ROOT_DIR_EXC, this.aFFL);
        this.aFFL.finish();
    }

    public void testSomeExcludes() throws Exception {
        this.aFFL.expectedIncludes.add("FileLoadException.java");
        this.aFFL.expectedIncludes.add("FileLoadIOException.java");
        this.aFFL.expectedIncludes.add("FileLoadParseException.java");
        this.aFFL.expectedIncludes.add("MergeException.java");
        this.aFFL.expectedIncludes.add("ModelException.java");
        this.aFFL.expectedIncludes.add("NameAlreadyUsedException.java");

        this.aFFL.expectedExcludes.add("FileSaveException.java");
        this.aFFL.expectedExcludes.add("FileSaveIOException.java");
        this.aFFL.expectedExcludes.add("package-info.java");

        this.directoryScanner.addIgnorePattern("**/.svn/**");
        this.directoryScanner.addIncludePattern("**/*.java");
        this.directoryScanner.addExcludePattern("**/FileS*.java");
        this.directoryScanner.addExcludePattern("**/*-i?f?.java");
        this.directoryScanner.scan(ROOT_DIR_EXC, this.aFFL);
        this.aFFL.finish();
    }

    public void testExcludeAll() throws Exception {
        this.aFFL.expectedExcludes.add("FileLoadException.java");
        this.aFFL.expectedExcludes.add("FileLoadIOException.java");
        this.aFFL.expectedExcludes.add("FileLoadParseException.java");
        this.aFFL.expectedExcludes.add("FileSaveException.java");
        this.aFFL.expectedExcludes.add("FileSaveIOException.java");
        this.aFFL.expectedExcludes.add("MergeException.java");
        this.aFFL.expectedExcludes.add("ModelException.java");
        this.aFFL.expectedExcludes.add("NameAlreadyUsedException.java");
        this.aFFL.expectedExcludes.add("package-info.java");

        this.directoryScanner.addIgnorePattern("**/.svn/**");
        this.directoryScanner.addIncludePattern("**");
        this.directoryScanner.addExcludePattern("**/*?*?*?*?*.j?va");
        this.directoryScanner.scan(ROOT_DIR_EXC, this.aFFL);
        this.aFFL.finish();
    }

    public void testFileFilterUsage1() throws Exception {
        this.aFFL.expectedIncludes.add("FileLoadException.java");
        this.aFFL.expectedIncludes.add("FileLoadIOException.java");
        this.aFFL.expectedIncludes.add("FileLoadParseException.java");
        this.aFFL.expectedIncludes.add("FileSaveException.java");
        this.aFFL.expectedIncludes.add("FileSaveIOException.java");
        this.aFFL.expectedIncludes.add("MergeException.java");
        this.aFFL.expectedIncludes.add("ModelException.java");
        this.aFFL.expectedIncludes.add("NameAlreadyUsedException.java");
        this.aFFL.expectedIncludes.add("package-info.java");

        this.directoryScanner.addIgnorePattern("**/.svn/**");
        this.directoryScanner.addIncludePattern("**");
        this.directoryScanner.setFileFilter(new FileFilter() {
            public boolean accept(File pathname) {
                return FileTool.getExtension(pathname).equals("java");
            }
        });
        this.directoryScanner.scan(ROOT_DIR_EXC, this.aFFL);
        this.aFFL.finish();
    }

    public void testFileFilterUsage2() throws Exception {
        this.aFFL.expectedIncludes.add("FileLoadException.java");
        this.aFFL.expectedIncludes.add("FileLoadIOException.java");
        this.aFFL.expectedIncludes.add("FileLoadParseException.java");
        this.aFFL.expectedIncludes.add("FileSaveException.java");
        this.aFFL.expectedIncludes.add("FileSaveIOException.java");

        this.aFFL.expectedExcludes.add("MergeException.java");
        this.aFFL.expectedExcludes.add("ModelException.java");
        this.aFFL.expectedExcludes.add("NameAlreadyUsedException.java");
        this.aFFL.expectedExcludes.add("package-info.java");

        this.directoryScanner.addIgnorePattern("**/.svn/**");
        this.directoryScanner.addIncludePattern("**");
        this.directoryScanner.setFileFilter(new FileFilter() {
            public boolean accept(File pathname) {
                return pathname.getName().startsWith("File");
            }
        });
        this.directoryScanner.scan(ROOT_DIR_EXC, this.aFFL);
        this.aFFL.finish();
    }

    public void testFileFilterUsage3() throws Exception {
        this.aFFL.expectedIncludes.add("FileLoadException.java");
        this.aFFL.expectedIncludes.add("FileLoadIOException.java");
        this.aFFL.expectedIncludes.add("FileLoadParseException.java");

        this.aFFL.expectedExcludes.add("FileSaveException.java");
        this.aFFL.expectedExcludes.add("FileSaveIOException.java");
        this.aFFL.expectedExcludes.add("MergeException.java");
        this.aFFL.expectedExcludes.add("ModelException.java");
        this.aFFL.expectedExcludes.add("NameAlreadyUsedException.java");
        this.aFFL.expectedExcludes.add("package-info.java");

        this.directoryScanner.addIgnorePattern("**/.svn/**");
        this.directoryScanner.addIncludePattern("*Load*");
        this.directoryScanner.setFileFilter(new FileFilter() {
            public boolean accept(File pathname) {
                return pathname.getName().startsWith("File");
            }
        });
        this.directoryScanner.scan(ROOT_DIR_EXC, this.aFFL);
        this.aFFL.finish();
    }

    public void testWrongRootDir() throws Exception {
        String rootDir = "C*?ßüäö&$³²°^`´:\\Program Files\\D:\\Program Files\\";

        // no exception, cause skip
        try {
            this.directoryScanner.scan(new File(rootDir), this.aFFL);
            Assert.fail();
        } catch (IOException e) {
            // expected
        }

        this.directoryScanner.addIncludePattern("**");
        try {
            this.directoryScanner.scan(new File(rootDir), this.aFFL);
            Assert.fail();
        } catch (IOException e) {
            // expected
        }
    }

    public void testAllTime() throws Exception {
        CountFileFoundListener cFFL;

        cFFL = new CountFileFoundListener();
        long time1 = System.currentTimeMillis();
        this.directoryScanner.scan(LARGE_DIR, cFFL);
        time1 = (System.currentTimeMillis() - time1);
        Assert.assertEquals(cFFL.includes, 0);
        Assert.assertEquals(cFFL.excludes, 0);

        cFFL = new CountFileFoundListener();
        this.directoryScanner = new DirectoryScanner();
        this.directoryScanner.addIncludePattern("**");
        this.directoryScanner.addExcludePatterns(DefaultIgnores.getIgnorePatterns());
        long time4 = System.currentTimeMillis();
        this.directoryScanner.scan(LARGE_DIR, cFFL);
        time4 = (System.currentTimeMillis() - time4);
        int allCountWithoutSVN = cFFL.includes;
        int allCountSVN = cFFL.excludes;

        cFFL = new CountFileFoundListener();
        this.directoryScanner = new DirectoryScanner();
        this.directoryScanner.addIncludePattern("**");
        this.directoryScanner.addExcludePattern("**/.svn/**");
        long time3 = System.currentTimeMillis();
        this.directoryScanner.scan(LARGE_DIR, cFFL);
        time3 = (System.currentTimeMillis() - time3);
        Assert.assertTrue(cFFL.includes > 200);
        Assert.assertTrue(cFFL.excludes > 200);
        Assert.assertEquals(allCountWithoutSVN, cFFL.includes);
        Assert.assertEquals(allCountSVN, cFFL.excludes);
        int allCount = cFFL.includes + cFFL.excludes;

        cFFL = new CountFileFoundListener();
        this.directoryScanner = new DirectoryScanner();
        for (int i = 1; i <= 1000; i++) {
            this.directoryScanner.addIncludePatterns(new String[] {"**\\**\\**\\**\\**\\**\\**\\**\\**",
                    "**\\a\\**\\c\\**\\d**\\**", "**\\1\\**\\c\\**\\d**\\**"});
            this.directoryScanner.addIncludePattern("**\\**\\*" + Integer.toHexString(i) + "**\\**");
            this.directoryScanner.addIncludePattern("**\\**\\*" + Integer.toBinaryString(i) + "**\\**");
            this.directoryScanner.addIncludePattern("**\\**\\*" + Integer.toOctalString(i) + "**\\**");
        }
        long time2 = System.currentTimeMillis();
        this.directoryScanner.scan(LARGE_DIR, cFFL);
        time2 = (System.currentTimeMillis() - time2);
        Assert.assertTrue(cFFL.includes > 400);
        Assert.assertEquals(cFFL.excludes, 0);
        Assert.assertEquals(allCount, cFFL.includes);

        // times are no good expected results
        // Assert.assertTrue("Timetest 1 too long", time1 <= 20);
        // Assert.assertTrue("Timetest 4 >= 3 " + "(" + time4 + " >= " + time3 + ")", time4 >= time3);
        // Assert.assertTrue("Timetest 3 >= 2 " + "(" + time3 + " >= " + time2 + ")", time3 >= time2);
    }

    static class AssertFileFoundListener implements FileFoundListener {
        LinkedList<String> expectedIncludes = new LinkedList<String>();

        LinkedList<String> expectedExcludes = new LinkedList<String>();

        public boolean considerNotIncluded() {
            return true;
        }

        public void includedFileFound(File includedFile, String relativePath) {
            Assert.assertEquals(this.expectedIncludes.poll(), relativePath);
        }

        public void notIncludedFileFound(File notIncludedFile,
                String relativePath) {
            Assert.assertEquals(this.expectedExcludes.poll(), relativePath);
        }

        public void finish() {
            Assert.assertTrue(this.expectedIncludes.isEmpty());
            Assert.assertTrue(this.expectedExcludes.isEmpty());
        }
    }

    static class CountFileFoundListener implements FileFoundListener {
        int includes = 0;

        int excludes = 0;

        public boolean considerNotIncluded() {
            return true;
        }

        public void includedFileFound(File includedFile, String relativePath) {
            this.includes++;
        }

        public void notIncludedFileFound(File notIncludedFile,
                String relativePath) {
            this.excludes++;
        }
    }
}
