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
 * This is an interface for compiler directive manipulators. A compiler 
 * directive manipulator should be used to save and
 * replace compiler directive before the instrumentation starts.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: CompilerDirectivesManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface CompilerDirectivesManipulator {
    
    /**
     * Manipulates the SimpleCharStream to save compiler directive and
     * replace them before the instrumentation starts.
     * 
     * @param column the column of the char
     * @param position the position of the char
     * @param c the char
     * @param compilerDirectives the compiler directive queue
     * @return the char that replaces the saved char
     */
    public char manipulate(int column, int position, char c,
            CompilerDirectives compilerDirectives);
    
    /**
     * Returns a string containing the number of white spaces that should be
     * inserted at the beginning of a line for non declarations.
     * 
     * @return the manipulated string
     */
    public String insertWhiteSpace();

    /**
     * Returns a string containing the number of white spaces that should be
     * inserted at the beginning of a line for declarations.
     * 
     * @return the manipulated string
     */
    public String insertWhiteSpaceDeclaration(); 

}