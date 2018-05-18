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
 * This implementation of the CompilerDirectivesManipulator saves and replaces 
 * compiler directives in the first seven columns.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: BWCompilerDirectivesManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class BWCompilerDirectivesManipulator implements CompilerDirectivesManipulator {

    private static final String SEVEN_WHITE_SPACES = "       ";

    private static final String ELEVEN_WHITE_SPACES = "           ";

    public char manipulate(int column, int position, char c,
            CompilerDirectives compilerDirectives) {
        if ((column <= 7) && c != '\n' && c != '\r' && c != ' ' && c != '*') {
            CompilerDirective compilerDirective = new CompilerDirective(c, position);
            compilerDirectives.addCompilerDirective(compilerDirective);
            return ' ';
        }
        return c;
    }

    /**
     * Needed for verbose info.
     */
    @Override
    public String toString() {
        return "Saves and replaces compiler directives in the first seven columns.";
    }

    public String insertWhiteSpace() {
        return ELEVEN_WHITE_SPACES;
    }

    public String insertWhiteSpaceDeclaration() {
        return SEVEN_WHITE_SPACES;
    }
}
