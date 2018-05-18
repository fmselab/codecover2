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

import static org.codecover.UtilsForTestingJava.SOURCE;
import static org.codecover.UtilsForTestingJava.TARGET;
import static org.codecover.UtilsForTestingJava.TEST_SOURCE;
import static org.codecover.UtilsForTestingJava.TEST_TARGET;
import static org.codecover.UtilsForTestingJava.handleException;
import static org.codecover.UtilsForTestingJava.isCompileableJava;
import static org.codecover.UtilsForTestingJava.simpleTestSessionContainerTests;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJava;
import org.codecover.instrumentation.exceptions.InstrumentationException;
import org.codecover.instrumentation.java15.InstrumenterDescriptor;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.utils.file.DefaultIgnores;
import org.codecover.model.utils.file.DirectoryScanner;
import org.codecover.model.utils.file.SourceTargetContainer;
import org.codecover.model.utils.file.listener.SourceTargetIncludedListener;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: InstrumenterTest.java 15 2008-05-24 20:59:06Z ahija $)
 * 
 * just testing the instrumentation
 */
public class InstrumenterTest extends TestCase {

    Instrumenter instrumenter;

    MASTBuilder builder;
    TestSessionContainer testSessionContainer;

    /**
     * @throws java.lang.Exception
     */
    protected void setUp() throws Exception {
        UtilsForTestingJava.clearTarget();
        UtilsForTestingJava.copyMeasurementHelpersToBin();
        this.builder = UtilsForTestingJava.newMASTBuilder(); 

        this.instrumenter = UtilsForTestingJava.getDefaultJavaInstrumenter();
    }

    @Override
    protected void tearDown() {
        this.instrumenter = null;
    }

    public void testInstrumentSingleFile() {
        String srcPath = TEST_SOURCE + "test2/AVLTree.java";
        File source = new File(srcPath);
        String targetPath = TEST_TARGET + "test2/AVLTree.java";
        File target = new File(targetPath);
        Date dateBefore = new Date();
        Collection<SourceTargetContainer> container = Collections.<SourceTargetContainer>singleton(
                new SourceTargetContainer(source, target));

        try {
            this.testSessionContainer = this.instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    container,
                    this.builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }

        Assert.assertTrue(isCompileableJava(new File(targetPath)));
        Assert.assertFalse(new File(TEST_TARGET + "test2/AVLNode.java").exists());
        Assert.assertFalse(new File(TEST_TARGET + "test2/AVLTreeException.java").exists());
        Assert.assertFalse(new File(TEST_TARGET + "test2/CNullIterator.java").exists());
        Assert.assertFalse(new File(TEST_TARGET + "test2/NodeIteratorDown.java").exists());
        Assert.assertFalse(new File(TEST_TARGET + "test2/NodeIteratorLevel.java").exists());
        Assert.assertFalse(new File(TEST_TARGET + "test2/NodeIteratorUp.java").exists());
        simpleTestSessionContainerTests(dateBefore, this.testSessionContainer, this.instrumenter);
    }

    public void testInstrumentPatternFolder() throws IOException {
        String targetPath = TEST_TARGET + "test2/";
        Date dateBefore = new Date();
        DirectoryScanner scanner = new DirectoryScanner();
        scanner.addIncludePattern("org/codecover/instrumentation/java15/test/test2/*.java");
        scanner.addIgnorePatterns(DefaultIgnores.getIgnorePatterns());
        SourceTargetIncludedListener fileFoundListener = new SourceTargetIncludedListener(
                new File(SOURCE), new File(TARGET));
        scanner.scan(new File(SOURCE), fileFoundListener);

        Assert.assertEquals(7, fileFoundListener.getIncludedSourceTargetContainers().size());

        try {
            this.testSessionContainer = this.instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    fileFoundListener.getIncludedSourceTargetContainers(),
                    this.builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }

        Assert.assertTrue(isCompileableJava(new File(targetPath + "AVLNode.java")));
        Assert.assertTrue(isCompileableJava(new File(targetPath + "AVLTree.java")));
        Assert.assertTrue(isCompileableJava(new File(targetPath + "AVLTreeException.java")));
        Assert.assertTrue(isCompileableJava(new File(targetPath + "CNullIterator.java")));
        Assert.assertTrue(isCompileableJava(new File(targetPath + "NodeIteratorDown.java")));
        Assert.assertTrue(isCompileableJava(new File(targetPath + "NodeIteratorLevel.java")));
        Assert.assertTrue(isCompileableJava(new File(targetPath + "NodeIteratorUp.java")));
        simpleTestSessionContainerTests(dateBefore, this.testSessionContainer, this.instrumenter);
    }

    public void testInstrumentPatternFile() throws IOException {
        Date dateBefore = new Date();

        DirectoryScanner scanner = new DirectoryScanner();
        scanner.addIncludePattern("org/codecover/instrumentation/java15/test/test2/AVLNode.java");
        scanner.addIncludePattern("org/codecover/instrumentation/java15/test/test2/AVLTree.java");
        scanner.addIncludePattern("org/codecover/instrumentation/java15/test/test2/NodeIteratorUp.java");
        scanner.addIgnorePatterns(DefaultIgnores.getIgnorePatterns());
        SourceTargetIncludedListener fileFoundListener = new SourceTargetIncludedListener(
                new File(SOURCE), new File(TARGET));
        scanner.scan(new File(SOURCE), fileFoundListener);

        Assert.assertEquals(3, fileFoundListener.getIncludedSourceTargetContainers().size());

        try {
            this.testSessionContainer = this.instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    fileFoundListener.getIncludedSourceTargetContainers(),
                    this.builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }

        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/AVLNode.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/AVLTree.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/NodeIteratorUp.java")));
        Assert.assertFalse(new File(TEST_TARGET + "test2/AVLTreeException.java").exists());
        Assert.assertFalse(new File(TEST_TARGET + "test2/CNullIterator.java").exists());
        Assert.assertFalse(new File(TEST_TARGET + "test2/NodeIteratorDown.java").exists());
        Assert.assertFalse(new File(TEST_TARGET + "test2/NodeIteratorLevel.java").exists());
        simpleTestSessionContainerTests(dateBefore, this.testSessionContainer, this.instrumenter);
    }

    public void testInstrumentSingleFilePretend() {
        this.instrumenter.setPretendMode(true);
        String srcPath = TEST_SOURCE + "test2/AVLTree.java";
        File source = new File(srcPath);
        String targetPath = TEST_TARGET + "test2/AVLTree.java";
        File target = new File(targetPath);
        Date dateBefore = new Date();
        Collection<SourceTargetContainer> container = Collections.<SourceTargetContainer>singleton(
                new SourceTargetContainer(source, target));

        try {
            this.testSessionContainer = this.instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    container,
                    this.builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }

        Assert.assertFalse(new File(TARGET + "org").exists());
        Assert.assertFalse(new File(TARGET + "de").exists());
        Assert.assertFalse(new File(TARGET + "TestClass.java").exists());
        simpleTestSessionContainerTests(dateBefore, this.testSessionContainer, this.instrumenter);
    }

    public void testInstrumentPatternFolderPretend() throws IOException {
        this.instrumenter.setPretendMode(true);
        Date dateBefore = new Date();
        DirectoryScanner scanner = new DirectoryScanner();
        scanner.addIncludePattern("org/codecover/instrumentation/java15/test/test2/*.java");
        scanner.addIgnorePatterns(DefaultIgnores.getIgnorePatterns());
        SourceTargetIncludedListener fileFoundListener = new SourceTargetIncludedListener(
                new File(SOURCE), new File(TARGET));
        scanner.scan(new File(SOURCE), fileFoundListener);

        Assert.assertEquals(7, fileFoundListener.getIncludedSourceTargetContainers().size());

        try {
            this.testSessionContainer = this.instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    fileFoundListener.getIncludedSourceTargetContainers(),
                    this.builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }

        Assert.assertFalse(new File(TARGET + "org").exists());
        Assert.assertFalse(new File(TARGET + "de").exists());
        Assert.assertFalse(new File(TARGET + "TestClass.java").exists());
        simpleTestSessionContainerTests(dateBefore, this.testSessionContainer, this.instrumenter);
    }

    public void testInstrumentPatternFilePretend() throws IOException {
        this.instrumenter.setPretendMode(true);
        Date dateBefore = new Date();

        DirectoryScanner scanner = new DirectoryScanner();
        scanner.addIncludePattern("org/codecover/instrumentation/java15/test/test2/AVLNode.java");
        scanner.addIncludePattern("org/codecover/instrumentation/java15/test/test2/AVLTree.java");
        scanner.addIncludePattern("org/codecover/instrumentation/java15/test/test2/NodeIteratorUp.java");
        scanner.addIgnorePatterns(DefaultIgnores.getIgnorePatterns());
        SourceTargetIncludedListener fileFoundListener = new SourceTargetIncludedListener(
                new File(SOURCE), new File(TARGET));
        scanner.scan(new File(SOURCE), fileFoundListener);

        Assert.assertEquals(3, fileFoundListener.getIncludedSourceTargetContainers().size());

        try {
            this.testSessionContainer = this.instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    fileFoundListener.getIncludedSourceTargetContainers(),
                    this.builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }

        Assert.assertFalse(new File(TARGET + "org").exists());
        Assert.assertFalse(new File(TARGET + "de").exists());
        Assert.assertFalse(new File(TARGET + "TestClass.java").exists());
        simpleTestSessionContainerTests(dateBefore, this.testSessionContainer, this.instrumenter);
    }
}
