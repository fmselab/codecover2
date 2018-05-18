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

import static org.codecover.UtilsForTestingJava.TEST_SOURCE;
import static org.codecover.UtilsForTestingJava.handleException;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.instrumentation.java15.parser.JavaParser;
import org.codecover.instrumentation.java15.parser.JavaParserConstants;
import org.codecover.instrumentation.java15.parser.ParseException;
import org.codecover.instrumentation.java15.syntaxtree.CompilationUnit;
import org.codecover.instrumentation.java15.syntaxtree.NodeToken;
import org.codecover.instrumentation.java15.visitor.DepthFirstVisitor;
import org.codecover.instrumentation.java15.visitor.Visitor;
import org.codecover.instrumentation.measurement.parser.SimpleCharStream;

/**
 * Test of the modification of {@link SimpleCharStream} for position getting.
 * 
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: TokenPositionTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class TokenPositionTest extends TestCase {

    public void testTokenPosition() {
        try {
            String srcPath = TEST_SOURCE + "test3/CodeExample.java";
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
            String tokenImage = n.getSourceFileImage();
            if (tokenImage.equals(substring)) {
                // juhuu
            } else if (n.kind == JavaParserConstants.EOF) {
                // is EOF expected?
            } else {
                Assert.assertEquals(tokenImage + " vs. " + substring,
                        tokenImage, substring);
            }
            substring = null;

            if (n.numSpecials() > 0) {
                for (NodeToken thisNT : n.specialTokens) {
                    thisNT.accept(this);
                }
            }
        }
    }
}
