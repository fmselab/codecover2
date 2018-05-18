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
 * @author Stefan Franke
 * @version 1.0 ($Id: CompilerDirective.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * A {@link CompilerDirective} is used to store compiler directives appearing in 
 * the source code. That have to be done, because of the fact, that the parser
 * does not understand compiler directives.
 */
public class CompilerDirective {

    private char directive;
    
    private int position;

    /**
     * Constructs a new {@link CompilerDirective}.
     *
     * @param directive a character that is part of a compiler directive
     * @param position the position in the source file
     */
    public CompilerDirective(char directive, int position) {
        this.directive = directive;
        this.position = position;
    }
    
    /**
     * Returns the directive character.
     * 
     * @return the directive character
     */
    public char getDirective() {
        return this.directive;
    }
    
    /**
     * Returns the directive position.
     * 
     * @return the directive position
     */
    public int getPosition() {
        return this.position;
    }
}
