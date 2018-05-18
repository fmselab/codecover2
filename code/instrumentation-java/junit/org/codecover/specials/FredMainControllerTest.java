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

package org.codecover.specials;

import static org.codecover.UtilsForTestingJava.SOURCE;
import static org.codecover.UtilsForTestingJava.TARGET;
import static org.codecover.UtilsForTestingJava.handleException;
import static org.codecover.UtilsForTestingJava.locationAssertion;
import static org.codecover.UtilsForTestingJava.simpleTestSessionContainerTests;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJava;
import org.codecover.instrumentation.Instrumenter;
import org.codecover.instrumentation.exceptions.InstrumentationException;
import org.codecover.instrumentation.java15.InstrumenterDescriptor;
import org.codecover.instrumentation.java15.parser.JavaParser;
import org.codecover.instrumentation.java15.parser.JavaParserConstants;
import org.codecover.instrumentation.java15.parser.ParseException;
import org.codecover.instrumentation.java15.syntaxtree.CompilationUnit;
import org.codecover.instrumentation.java15.syntaxtree.NodeToken;
import org.codecover.instrumentation.java15.visitor.DepthFirstVisitor;
import org.codecover.instrumentation.java15.visitor.Visitor;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.utils.file.SourceTargetContainer;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: FredMainControllerTest.java 15 2008-05-24 20:59:06Z ahija $)
 */
public class FredMainControllerTest extends TestCase {

    public void testLoadWorkFile() throws Exception {
        // we had Problems with some escaped Unicodes, which ended in wrong
        // Locations -> here is the test case

        UtilsForTestingJava.clearTarget();
        UtilsForTestingJava.copyMeasurementHelpersToBin();

        Instrumenter instrumenter = UtilsForTestingJava.getDefaultJavaInstrumenter();
        MASTBuilder builder = UtilsForTestingJava.newMASTBuilder();

        // instrument
        final String srcPath = SOURCE + "de/sopra06/controller/MainController.java";
        final File source = new File(srcPath);
        final String targetPath = TARGET + "de/sopra06/controller/MainController.java";
        final File target = new File(targetPath);
        Date dateBefore = new Date();
        TestSessionContainer testSessionContainer = null;
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
        HierarchyLevel h1 = testSessionContainer.getCode();
        Assert.assertEquals("default package", h1.getName());
        Assert.assertEquals("default package", h1.getType().getInternalName());
        Assert.assertEquals("default package", h1.getType().getEnglishName());
        Assert.assertTrue(h1.getSequences().isEmpty());
        Assert.assertEquals(1, h1.getChildren().size());
        
        HierarchyLevel h2 = h1.getChildren().get(0);
        Assert.assertEquals("de", h2.getName());
        Assert.assertEquals("package", h2.getType().getInternalName());
        Assert.assertEquals("package", h2.getType().getEnglishName());
        Assert.assertTrue(h2.getSequences().isEmpty());
        Assert.assertEquals(1, h2.getChildren().size());

        HierarchyLevel h3 = h2.getChildren().get(0);
        Assert.assertEquals("sopra06", h3.getName());
        Assert.assertEquals("package", h3.getType().getInternalName());
        Assert.assertEquals("package", h3.getType().getEnglishName());
        Assert.assertTrue(h3.getSequences().isEmpty());
        Assert.assertEquals(1, h3.getChildren().size());
        
        HierarchyLevel h4 = h3.getChildren().get(0);
        Assert.assertEquals("controller", h4.getName());
        Assert.assertEquals("package", h4.getType().getInternalName());
        Assert.assertEquals("package", h4.getType().getEnglishName());
        Assert.assertTrue(h4.getSequences().isEmpty());
        Assert.assertEquals(1, h4.getChildren().size());

        HierarchyLevel classMainController = h4.getChildren().get(0);
        Assert.assertEquals("MainController", classMainController.getName());
        Assert.assertEquals("class", classMainController.getType().getInternalName());
        Assert.assertEquals("class", classMainController.getType().getEnglishName());
        Assert.assertEquals(1, classMainController.getSequences().size());
        Assert.assertEquals(5, classMainController.getSequences().get(0).getStatements().size());
        Assert.assertEquals(93, classMainController.getChildren().size());
        locationAssertion("public class MainController extends Observable", classMainController.getHeader());

        HierarchyLevel methodMainController = classMainController.getChildren().get(0);
        Assert.assertEquals("MainController", methodMainController.getName());
        Assert.assertEquals("method", methodMainController.getType().getInternalName());
        Assert.assertEquals("method", methodMainController.getType().getEnglishName());
        Assert.assertTrue(methodMainController.getSequences().isEmpty());
        Assert.assertTrue(methodMainController.getChildren().isEmpty());
        locationAssertion("private MainController()\n" +
                "  {\n" +
                "    // no op\n" +
                "  }", methodMainController.getLocation());
        locationAssertion("private MainController()", methodMainController.getHeader());
    }

    public void testTokenPosition() {
        try {
            final String srcPath = SOURCE + "de/sopra06/controller/MainController.java";
            File source = new File(srcPath);

            // use the fileInputStream to get the content of the source file
            FileInputStream fileInputStream = new FileInputStream(source);
            InputStreamReader inputStreamReader = new InputStreamReader(
                    fileInputStream, Charset.forName("UTF-8"));

            // the number of characters is appropriated with the length of the
            // source file -> it will be smaller
            int sourceFileLength = (int)source.length();
            // prepate an array, that is long enough
            char[] content = new char[sourceFileLength];
            // read from the inputStreamReader and get the exact character count
            sourceFileLength = Math.max(inputStreamReader.read(content, 0,
                    sourceFileLength), 0);

            String sourceFileContent = new String(content, 0, sourceFileLength);
            
            JavaParser parser = new JavaParser(sourceFileContent);
            CompilationUnit compilationUnit = parser.CompilationUnit();
            Visitor visitor = new TokenTester(sourceFileContent);
            visitor.visit(compilationUnit);
        } catch (FileNotFoundException e) {
            handleException(e);
        } catch (ParseException e) {
            handleException(e);
        } catch (IOException e) {
            handleException(e);
        }
    }

    static final class TokenTester extends DepthFirstVisitor {
        private String sourceFileContent;

        public TokenTester(String sourceFileContent) {
            this.sourceFileContent = sourceFileContent;
        }

        @Override
        public void visit(NodeToken n) {
            String substring = this.sourceFileContent.substring(n.startOffset, n.endOffset);
            
            if (n.kind == JavaParserConstants.EOF) {
                return;
            }

            String tokenImage = n.getSourceFileImage();
            if (!substring.equals(tokenImage)){
                String message = String.format("!substring.equals(n.tokenImage)%n" +
                                               "substring:     %s%n" +
                                               "t.kind:        %s%n" +
                                               "t.image:       %s%n" +
                                               "t.beginOffset: %d%n" +
                                               "t.endOffset:   %d%n",
                         substring,
                         new Integer(n.kind),
                         tokenImage,
                         new Integer(n.startOffset),
                         new Integer(n.endOffset));

                Assert.fail(message);
            }

            if (n.numSpecials() > 0) {
                for (NodeToken thisNT : n.specialTokens) {
                    thisNT.accept(this);
                }
            }
        }
    }
}
