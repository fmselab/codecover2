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

package org.codecover.instrumentation.xampil;

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

import org.codecover.instrumentation.exceptions.ParseException;
import org.codecover.instrumentation.xampil.parser.InstrumentableItemCounter;
import org.codecover.instrumentation.xampil.parser.SimpleCharStream;
import org.codecover.instrumentation.xampil.parser.XampilParser;
import org.codecover.instrumentation.xampil.parser.XampilParserConstants;
import org.codecover.instrumentation.xampil.syntaxtree.CompilationUnit;
import org.codecover.instrumentation.xampil.syntaxtree.NodeToken;
import org.codecover.instrumentation.xampil.visitor.DepthFirstVisitor;
import org.codecover.instrumentation.xampil.visitor.Visitor;
import org.codecover.model.utils.file.FileTool;

/**
 * Test of the modification of {@link SimpleCharStream} for position getting.
 * 
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: TokenPositionTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class TokenPositionTest extends TestCase {

    public void testTokenPosition() throws Exception {
        String srcPath = "../../website/development/programming-language-files/example.xpl";
        File source = new File(srcPath).getCanonicalFile();

        // use the fileInputStream to get the content of the source file
        String sourceFileContent = FileTool.getContentFromFile(source, Charset.forName("UTF-8"));
        StringReader stringReader = new StringReader(sourceFileContent);
        SimpleCharStream simpleCharStream = new SimpleCharStream(stringReader);
        XampilParser cobolParser = new XampilParser(simpleCharStream);
        InstrumentableItemCounter counter = new InstrumentableItemCounter();
        CompilationUnit compilationUnit = cobolParser.CompilationUnit(counter);
        Visitor visitor = new TokenTester(sourceFileContent);
        visitor.visit(compilationUnit);
    }

    static final class TokenTester extends DepthFirstVisitor {
        private String sourceFileContent;

        public TokenTester(String sourceFileContent) {
            this.sourceFileContent = sourceFileContent;
        }

        @Override
        public void visit(NodeToken n) {
            if (n.kind == XampilParserConstants.EOF || 
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
