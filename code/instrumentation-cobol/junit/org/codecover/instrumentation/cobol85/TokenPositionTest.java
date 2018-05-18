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

package org.codecover.instrumentation.cobol85;

import static org.codecover.UtilsForTestingCobol.SOURCE;
import static org.codecover.UtilsForTestingCobol.handleException;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.nio.charset.Charset;

import junit.framework.Assert;
import junit.framework.AssertionFailedError;
import junit.framework.TestCase;

import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectives;
import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectivesManipulator;
import org.codecover.instrumentation.cobol85.compilerDirectives.DefaultCompilerDirectivesManipulator;
import org.codecover.instrumentation.cobol85.parser.CobolParser;
import org.codecover.instrumentation.cobol85.parser.CobolParserConstants;
import org.codecover.instrumentation.cobol85.parser.SimpleCharStream;
import org.codecover.instrumentation.cobol85.syntaxtree.CompilationUnit;
import org.codecover.instrumentation.cobol85.syntaxtree.NodeToken;
import org.codecover.instrumentation.cobol85.visitor.DepthFirstVisitor;
import org.codecover.instrumentation.cobol85.visitor.Visitor;
import org.codecover.instrumentation.exceptions.ParseException;

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
            String srcPath = SOURCE + "AllCoverage/allCoverage.cob";
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
            StringReader stringReader = new StringReader(sourceFileContent);
            CompilerDirectivesManipulator compilerDirectivesManipulator = new DefaultCompilerDirectivesManipulator();
            SimpleCharStream simpleCharStream = new SimpleCharStream(stringReader, compilerDirectivesManipulator);
            CobolParser cobolParser = new CobolParser(simpleCharStream);
            CompilationUnit compilationUnit = cobolParser.CompilationUnit();
            Visitor visitor = new TokenTester(sourceFileContent);
            visitor.visit(compilationUnit);
        } catch (FileNotFoundException e) {
            handleException(e);
        } catch (ParseException e) {
            handleException(e);;
        } catch (IOException e) {
            handleException(e);
        }
    }
    
    public void testTokenPositionExample() {
        try {
            String srcPath = SOURCE + "Example/A18O012_out.TXT";
            File source = new File(srcPath);
            
            if (!source.exists()) {
                System.out.println(source.getPath() + " was not found. SKIPPED TEST!");
                return;
            }
            
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
            StringReader stringReader = new StringReader(sourceFileContent);
            CompilerDirectivesManipulator compilerDirectivesManipulator = new DefaultCompilerDirectivesManipulator();
            SimpleCharStream simpleCharStream = new SimpleCharStream(stringReader, compilerDirectivesManipulator);
            CobolParser cobolParser = new CobolParser(simpleCharStream);
            CompilationUnit compilationUnit = cobolParser.CompilationUnit();
            Visitor visitor = new TokenTester(sourceFileContent);
            visitor.visit(compilationUnit);
        } catch (FileNotFoundException e) {
            handleException(e);
        } catch (ParseException e) {
            handleException(e);;
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
            if (n.kind == CobolParserConstants.EOF || 
                (n.beginLine == 0 && n.tokenImage.startsWith("\n"))) {
                return;
            }
            
            if (n.endOffset == this.sourceFileContent.length() + 1) {
                n.endOffset--;
                n.tokenImage = n.tokenImage.substring(0, n.tokenImage.length() - 1);
            }
            
            String substring;
            
            try {
                substring = this.sourceFileContent.substring(n.startOffset, n.endOffset);
            } catch (RuntimeException e) {
                throw new AssertionFailedError(e.getMessage());
            }

            if (!n.tokenImage.equals(substring)) {
                Assert.assertEquals(n.tokenImage, substring);
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
