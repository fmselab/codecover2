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

import static org.codecover.UtilsForTestingCobol.handleException;

import java.io.Reader;
import java.io.StringReader;

import junit.framework.TestCase;

import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectives;
import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectivesManipulator;
import org.codecover.instrumentation.cobol85.compilerDirectives.DefaultCompilerDirectivesManipulator;
import org.codecover.instrumentation.cobol85.parser.CobolParser;
import org.codecover.instrumentation.cobol85.parser.SimpleCharStream;
import org.codecover.instrumentation.cobol85.syntaxtree.Condition;
import org.codecover.instrumentation.exceptions.ParseException;

public class CobolConditionParserTest extends TestCase {

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public void testExample1() {
        String expressionString = "NOT A > B OR A + B = C AND D IS POSITIVE";
        Reader reader = new StringReader(expressionString);
        CompilerDirectivesManipulator compilerDirectivesManipulator = new DefaultCompilerDirectivesManipulator();
        SimpleCharStream simpleCharStream = new SimpleCharStream(reader, compilerDirectivesManipulator);
        CobolParser cobolParser = new CobolParser(simpleCharStream);
        try {
            Condition condition = cobolParser.Condition();

        } catch (ParseException e) {
            handleException(e);
        }
    }

}
