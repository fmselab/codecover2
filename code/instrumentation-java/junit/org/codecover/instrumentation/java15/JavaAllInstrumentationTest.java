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

package org.codecover.instrumentation.java15;

import static org.codecover.UtilsForTestingJava.SESSION_CONTAINER;
import static org.codecover.UtilsForTestingJava.SOURCE;
import static org.codecover.UtilsForTestingJava.TARGET;
import static org.codecover.UtilsForTestingJava.TEST_SOURCE;
import static org.codecover.UtilsForTestingJava.TEST_TARGET;
import static org.codecover.UtilsForTestingJava.checkAreMeasurementHelpersCopied;
import static org.codecover.UtilsForTestingJava.clearTarget;
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
import org.codecover.instrumentation.DefaultInstrumenterFactory;
import org.codecover.instrumentation.Instrumenter;
import org.codecover.instrumentation.InstrumenterFactory;
import org.codecover.instrumentation.exceptions.InstrumentationException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.ConditionCoverage;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;
import org.codecover.model.utils.file.DefaultIgnores;
import org.codecover.model.utils.file.DirectoryScanner;
import org.codecover.model.utils.file.SourceTargetContainer;
import org.codecover.model.utils.file.listener.SourceTargetIncludedListener;

/**
 * This class contains {@link #instrumentationTests(Instrumenter)}, that is 
 * called by other test cases to test all files of the test folder.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: JavaAllInstrumentationTest.java 22 2008-05-25 20:08:53Z ahija $)
 */
public class JavaAllInstrumentationTest extends TestCase {

    public void test() throws Exception {
        instrumentationTests(UtilsForTestingJava.getDefaultJavaInstrumenter());
    }

    public static void instrumentationTests(Instrumenter instrumenter)
      throws Exception {
        MASTBuilder builder = UtilsForTestingJava.newMASTBuilder();
        UtilsForTestingJava.copyMeasurementHelpersToBin();

        clearTarget();
        UtilsForTestingJava.collectGarbage();
        UtilsForTestingJava.resetTimestamp();
        startTest1(instrumenter, builder);
        UtilsForTestingJava.printTimestamp();
        UtilsForTestingJava.printMemory();

        clearTarget();
        UtilsForTestingJava.collectGarbage();
        UtilsForTestingJava.resetTimestamp();
        startTest2(instrumenter, builder);
        UtilsForTestingJava.printTimestamp();
        UtilsForTestingJava.printMemory();

        clearTarget();
        UtilsForTestingJava.collectGarbage();
        UtilsForTestingJava.resetTimestamp();
        startTest3(instrumenter, builder);
        UtilsForTestingJava.printTimestamp();
        UtilsForTestingJava.printMemory();

        clearTarget();
        UtilsForTestingJava.collectGarbage();
        UtilsForTestingJava.resetTimestamp();
        startTest4(instrumenter, builder);
        UtilsForTestingJava.printTimestamp();
        UtilsForTestingJava.printMemory();

        clearTarget();
        UtilsForTestingJava.collectGarbage();
        UtilsForTestingJava.resetTimestamp();
        startTest5(instrumenter, builder);
        UtilsForTestingJava.printTimestamp();
        UtilsForTestingJava.printMemory();

        clearTarget();
        UtilsForTestingJava.collectGarbage();
        UtilsForTestingJava.resetTimestamp();
        startTest6(instrumenter, builder);
        UtilsForTestingJava.printTimestamp();
        UtilsForTestingJava.printMemory();

        clearTarget();
        UtilsForTestingJava.collectGarbage();
        UtilsForTestingJava.resetTimestamp();
        startTest7(instrumenter, builder);
        UtilsForTestingJava.printTimestamp();
        UtilsForTestingJava.printMemory();
        
        clearTarget();
        UtilsForTestingJava.collectGarbage();
        UtilsForTestingJava.resetTimestamp();
        startTest8(instrumenter, builder);
        UtilsForTestingJava.printTimestamp();
        UtilsForTestingJava.printMemory();

        clearTarget();
        UtilsForTestingJava.collectGarbage();
        UtilsForTestingJava.resetTimestamp();
        startTest9(instrumenter, builder);
        UtilsForTestingJava.printTimestamp();
        UtilsForTestingJava.printMemory();

        clearTarget();
        UtilsForTestingJava.collectGarbage();
        UtilsForTestingJava.resetTimestamp();
        startTestAllJava(instrumenter, builder);
        UtilsForTestingJava.printTimestamp();
        UtilsForTestingJava.printMemory();
    }

    private static void startTest1(Instrumenter instrumenter, MASTBuilder builder) {
        TestSessionContainer testSessionContainer = null;
        String srcPath = TEST_SOURCE + "test1/TestClass.java";
        File source = new File(srcPath);
        String targetPath = TEST_TARGET + "test1/TestClass.java";
        File target = new File(targetPath);
        Date dateBefore = new Date();
        Collection<SourceTargetContainer> container = Collections.<SourceTargetContainer>singleton(
                new SourceTargetContainer(source, target));

        try {
          testSessionContainer = instrumenter.instrument(new File(SOURCE),
                  new File(TARGET),
                  container,
                  builder,
                  new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }
        Assert.assertTrue(target.exists());
        simpleTestSessionContainerTests(dateBefore, testSessionContainer, instrumenter);
        Assert.assertTrue(isCompileableJava(target));
        checkAreMeasurementHelpersCopied();

        try {
            testSessionContainer.save(SESSION_CONTAINER);
        } catch (Exception e) {
            handleException(e);
        }
        Assert.assertTrue(new File(SESSION_CONTAINER).exists());
    }

    private static void startTest2(Instrumenter instrumenter, MASTBuilder builder)
        throws Exception {
        TestSessionContainer testSessionContainer = null;
        Date dateBefore = new Date();
        DirectoryScanner scanner = new DirectoryScanner();
        scanner.addIncludePattern("org/codecover/instrumentation/java15/test/test2/*.java");
        scanner.addIgnorePatterns(DefaultIgnores.getIgnorePatterns());
        SourceTargetIncludedListener fileFoundListener = new SourceTargetIncludedListener(
                new File(SOURCE), new File(TARGET));
        scanner.scan(new File(SOURCE), fileFoundListener);

        Assert.assertEquals(7, fileFoundListener.getIncludedSourceTargetContainers().size());

        try {
            testSessionContainer = instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    fileFoundListener.getIncludedSourceTargetContainers(),
                    builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }

        Assert.assertTrue(new File(TEST_TARGET + "test2/").exists());
        simpleTestSessionContainerTests(dateBefore, testSessionContainer, instrumenter);
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/AVLNode.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/AVLTree.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/AVLTreeException.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/CNullIterator.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/NodeIteratorDown.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/NodeIteratorLevel.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/NodeIteratorUp.java")));
        checkAreMeasurementHelpersCopied();

        try {
            testSessionContainer.save(SESSION_CONTAINER);
        } catch (Exception e) {
            handleException(e);
        }
        Assert.assertTrue(new File(SESSION_CONTAINER).exists());
    }

    private static void startTest3(Instrumenter instrumenter, MASTBuilder builder) {
        TestSessionContainer testSessionContainer = null;
        String srcPath = TEST_SOURCE + "test3/CodeExample.java";
        File source = new File(srcPath);
        String targetPath = TEST_TARGET + "test3/CodeExample.java";
        File target = new File(targetPath);
        Date dateBefore = new Date();
        Collection<SourceTargetContainer> container = Collections.<SourceTargetContainer>singleton(
                new SourceTargetContainer(source, target));
    
        try {
            testSessionContainer = instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    container,
                    builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }
    
        Assert.assertTrue(target.exists());
        simpleTestSessionContainerTests(dateBefore, testSessionContainer, instrumenter);
        Assert.assertTrue(isCompileableJava(target));
        checkAreMeasurementHelpersCopied();
    
        try {
            testSessionContainer.save(SESSION_CONTAINER);
        } catch (Exception e) {
            handleException(e);
        }
        Assert.assertTrue(new File(SESSION_CONTAINER).exists());
    }

    private static void startTest4(Instrumenter instrumenter, MASTBuilder builder) {
        TestSessionContainer testSessionContainer = null;
        String srcPath = SOURCE + "TestClass.java";
        File source = new File(srcPath);
        String targetPath = TARGET + "TestClass.java";
        File target = new File(targetPath);
        Date dateBefore = new Date();
        Collection<SourceTargetContainer> container = Collections.<SourceTargetContainer>singleton(
                new SourceTargetContainer(source, target));

        try {
            testSessionContainer = instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    container,
                    builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }
        Assert.assertTrue(target.exists());
        simpleTestSessionContainerTests(dateBefore, testSessionContainer, instrumenter);
        Assert.assertTrue(isCompileableJava(target));
        checkAreMeasurementHelpersCopied();

        try {
            testSessionContainer.save(SESSION_CONTAINER);
        } catch (Exception e) {
            handleException(e);
        }
        Assert.assertTrue(new File(SESSION_CONTAINER).exists());
    }
    
    private static void startTest5(Instrumenter instrumenter, MASTBuilder builder) {
        TestSessionContainer testSessionContainer = null;
        String srcPath = TEST_SOURCE + "test5/ComplexInterface.java";
        File source = new File(srcPath);
        String targetPath = TEST_TARGET + "test5/ComplexInterface.java";
        File target = new File(targetPath);
        Date dateBefore = new Date();
        Collection<SourceTargetContainer> container = Collections.<SourceTargetContainer>singleton(
                new SourceTargetContainer(source, target));

        try {
            testSessionContainer = instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    container,
                    builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }

        Assert.assertTrue(target.exists());
        simpleTestSessionContainerTests(dateBefore, testSessionContainer, instrumenter);
        Assert.assertTrue(isCompileableJava(target));
        checkAreMeasurementHelpersCopied();

        try {
            testSessionContainer.save(SESSION_CONTAINER);
        } catch (Exception e) {
            handleException(e);
        }
        Assert.assertTrue(new File(SESSION_CONTAINER).exists());
    }
    
    private static void startTest6(Instrumenter instrumenter, MASTBuilder builder) {
        TestSessionContainer testSessionContainer = null;
        String srcPath = TEST_SOURCE + "test6/ComplexEnum.java";
        File source = new File(srcPath);
        String targetPath = TEST_TARGET + "test6/ComplexEnum.java";
        File target = new File(targetPath);
        Date dateBefore = new Date();
        Collection<SourceTargetContainer> container = Collections.<SourceTargetContainer>singleton(
                new SourceTargetContainer(source, target));

        try {
            testSessionContainer = instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    container,
                    builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }
        
        Assert.assertTrue(target.exists());
        simpleTestSessionContainerTests(dateBefore, testSessionContainer, instrumenter);
        Assert.assertTrue(isCompileableJava(target));
        checkAreMeasurementHelpersCopied();

        try {
            testSessionContainer.save(SESSION_CONTAINER);
        } catch (Exception e) {
            handleException(e);
        }
        Assert.assertTrue(new File(SESSION_CONTAINER).exists());
    }
    
    private static void startTest7(Instrumenter instrumenter, MASTBuilder builder) {
        TestSessionContainer testSessionContainer = null;
        String srcPath = TEST_SOURCE + "test7/ComplexAnnotation.java";
        File source = new File(srcPath);
        String targetPath = TEST_TARGET + "test7/ComplexAnnotation.java";
        File target = new File(targetPath);
        Date dateBefore = new Date();
        Collection<SourceTargetContainer> container = Collections.<SourceTargetContainer>singleton(
                new SourceTargetContainer(source, target));

        try {
            testSessionContainer = instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    container,
                    builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }
        
        Assert.assertTrue(target.exists());
        simpleTestSessionContainerTests(dateBefore, testSessionContainer, instrumenter);
        Assert.assertTrue(isCompileableJava(target));
        checkAreMeasurementHelpersCopied();

        try {
            testSessionContainer.save(SESSION_CONTAINER);
        } catch (Exception e) {
            handleException(e);
        }
        Assert.assertTrue(new File(SESSION_CONTAINER).exists());
    }

    private static void startTest8(Instrumenter instrumenter, MASTBuilder builder) throws Exception {
        // we need another instrumenter:
        org.codecover.instrumentation.InstrumenterDescriptor descriptor = new InstrumenterDescriptor();
        InstrumenterFactory factory = new DefaultInstrumenterFactory();
        factory.setDescriptor(descriptor);
        factory.setCharset(UnicodeEscapeTest.CODE_EXAMPLE_ESCAPED_CHARSET);
        factory.setPretendMode(false);

        factory.addCriterion(StatementCoverage.getInstance());
        factory.addCriterion(BranchCoverage.getInstance());
        factory.addCriterion(ConditionCoverage.getInstance());
        factory.addCriterion(LoopCoverage.getInstance());

        instrumenter = factory.getInstrumenter();
        
        TestSessionContainer testSessionContainer = null;
        String srcPath = TEST_SOURCE + "test8/CodeExample.java";
        File source = new File(srcPath);
        String targetPath = TEST_TARGET + "test8/CodeExample.java";
        File target = new File(targetPath);
        Date dateBefore = new Date();
        Collection<SourceTargetContainer> container = Collections.<SourceTargetContainer>singleton(
                new SourceTargetContainer(source, target));

        try {
            testSessionContainer = instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    container,
                    builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }

        Assert.assertTrue(target.exists());
        simpleTestSessionContainerTests(dateBefore, testSessionContainer, instrumenter);
        Assert.assertTrue(isCompileableJava(target, UnicodeEscapeTest.CODE_EXAMPLE_ESCAPED_CHARSET));
        checkAreMeasurementHelpersCopied();
    
        try {
            testSessionContainer.save(SESSION_CONTAINER);
        } catch (Exception e) {
            handleException(e);
        }
        Assert.assertTrue(new File(SESSION_CONTAINER).exists());
    }

    private static void startTest9(Instrumenter instrumenter, MASTBuilder builder) {
        TestSessionContainer testSessionContainer = null;
        String srcPath = TEST_SOURCE + "test9/ClassDeclarationInField.java";
        File source = new File(srcPath);
        String targetPath = TEST_TARGET + "test9/ClassDeclarationInField.java";
        File target = new File(targetPath);
        Date dateBefore = new Date();
        Collection<SourceTargetContainer> container = Collections.<SourceTargetContainer>singleton(
                new SourceTargetContainer(source, target));

        try {
            testSessionContainer = instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    container,
                    builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }

        Assert.assertTrue(target.exists());
        simpleTestSessionContainerTests(dateBefore, testSessionContainer, instrumenter);
        Assert.assertTrue(isCompileableJava(target));
        checkAreMeasurementHelpersCopied();

        try {
            testSessionContainer.save(SESSION_CONTAINER);
        } catch (Exception e) {
            handleException(e);
        }
        Assert.assertTrue(new File(SESSION_CONTAINER).exists());
    }

    public static TestSessionContainer startTestAllJava(Instrumenter instrumenter,
                                                        MASTBuilder builder)
        throws IOException {
        TestSessionContainer testSessionContainer = null;
        Date dateBefore = new Date();
        DirectoryScanner scanner = new DirectoryScanner();
        scanner.addIncludePattern("**/*.java");
        scanner.addExcludePattern("de/**");
        scanner.addExcludePattern("**/test8/**");
        scanner.addExcludePattern("**/bugs/**");
        scanner.addIgnorePatterns(DefaultIgnores.getIgnorePatterns());
        SourceTargetIncludedListener fileFoundListener = new SourceTargetIncludedListener(
                new File(SOURCE), new File(TARGET));
        scanner.scan(new File(SOURCE), fileFoundListener);

        Assert.assertEquals(16, fileFoundListener.getIncludedSourceTargetContainers().size());

        try {
            testSessionContainer = instrumenter.instrument(new File(SOURCE),
                    new File(TARGET),
                    fileFoundListener.getIncludedSourceTargetContainers(),
                    builder,
                    new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }
        Assert.assertTrue(new File(TARGET).exists());
        simpleTestSessionContainerTests(dateBefore, testSessionContainer, instrumenter);
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test1/TestClass.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test1/LoopTest.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/AVLNode.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/AVLTree.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/AVLTreeException.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/CNullIterator.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/NodeIteratorDown.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/NodeIteratorLevel.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test2/NodeIteratorUp.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test3/CodeExample.java")));
        Assert.assertTrue(isCompileableJava(new File(TARGET + "TestClass.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test5/ComplexInterface.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test6/ComplexEnum.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test7/ComplexAnnotation.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test9/ClassDeclarationInField.java")));
        Assert.assertTrue(isCompileableJava(new File(TEST_TARGET + "test10/CoverableItemTest.java")));
        checkAreMeasurementHelpersCopied();

        try {
            testSessionContainer.save(SESSION_CONTAINER);
        } catch (Exception e) {
            handleException(e);
        }
        Assert.assertTrue(new File(SESSION_CONTAINER).exists());
        
        return testSessionContainer;
    }
}
