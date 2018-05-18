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

package org.codecover.report.highlighting.annotation;

/**
 * Information about the number of executions of a code line. Used for
 * annotations.
 *
 * @author Johannes Langauf
 * @version 1.0 ($Id: ExecutionInformation.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface ExecutionInformation {

    /**
     * Determine how often the most often executed Annotation intersecting this
     * Annotation was executed. If this is a CoverageAnnotation this is how
     * often the annotated element was executed.  
     * 
     * @return the executions, -1 iff none possible
     */
    public abstract long getExecutions();
    
    /**
     * Determine if the number of executions of this element is available.
     * 
     * @return false, iff not executable
     */
    public abstract boolean hasExecutions();
}
