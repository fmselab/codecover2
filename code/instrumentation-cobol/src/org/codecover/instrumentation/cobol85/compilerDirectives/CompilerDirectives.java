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

import java.util.LinkedList;
import java.util.Queue;

/**
 * @author Stefan Franke
 * @version 1.0 ($Id: CompilerDirectives.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * This is a container for compiler directives. It saves the compiler directives in a queue
 * and has methods to add, get and remove.
 */
public class CompilerDirectives {
    
    private Queue<CompilerDirective> directives = new LinkedList<CompilerDirective>();
    
    /**
     * Add a compiler directive to the queue.
     * 
     * @param compilerDirective the compiler directive
     */
    public void addCompilerDirective(CompilerDirective compilerDirective) {
        this.directives.add(compilerDirective);
    }
    
    /**
     * Returns the compiler directive from the head of the queue.
     * 
     * @return the compiler directive
     */
    public char getCompilerDirective() {
        return this.directives.element().getDirective();
    }
    
    /**
     * 
     * @return 
     */
    public int getCompilerDirectivePosition() {
        return this.directives.element().getPosition();
    }
    
    /**
     * Removes the head compiler directive from the queue.
     */
    public void goToNextCompilerDirective() {
        this.directives.poll();
    }
    
    /**
     * Returns true if none compiler directive is stored.
     * 
     * @return true if none compiler directive is stored
     */
    public boolean isEmpty() {
        return this.directives.isEmpty();
    }
    
}
