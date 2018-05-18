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

package org.codecover.instrumentation.cobol85.compilerDirectives;

/**
 * This implementation of the CompilerDirectivesManipulator does not
 * change any characters.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: DefaultCompilerDirectivesManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DefaultCompilerDirectivesManipulator implements CompilerDirectivesManipulator {

    private static final String EMPTY_STRING = "";

    public char manipulate(int column, int position, char c,
            CompilerDirectives compilerDirectives) {
        return c;
    }
    
    /**
     * Needed for verbose info.
     */
    @Override
    public String toString() {
        return "default";
    }

    public String insertWhiteSpace() {
        return EMPTY_STRING;
    }

    public String insertWhiteSpaceDeclaration() {
        return EMPTY_STRING;
    }
}
