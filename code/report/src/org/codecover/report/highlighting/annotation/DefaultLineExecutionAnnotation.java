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

import org.codecover.model.mast.SourceFile;

/**
 * Mark a consecutive region of code as a line with it's line number and
 * execution count.
 *
 * @author Johannes Langauf
 * @version 1.0 ($Id: DefaultLineExecutionAnnotation.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DefaultLineExecutionAnnotation extends DefaultLineAnnotation
        implements LineExecutionAnnotation {

    final long executions;
    
    /**
     * 
     * @param src
     * the source file to extract content from
     * @param offset
     * the offset as from {@link #getStartOffset()}
     * @param endOffset
     * the endOffset as from {@link #getEndOffset()}
     * @param line
     * the line number relative to src.getContent()
     * @param executions
     * the executions as in {@link #getExecutions()}
     */
    public DefaultLineExecutionAnnotation(SourceFile src, int offset,
            int endOffset, int line, long executions) {
        super(src, offset, endOffset, line);

        this.executions = executions;
        checkExecutions();
    }

    /**
     * 
     * @param src
     * the source file to extract content from
     * @param offset
     * the offset as from {@link #getStartOffset()}
     * @param endOffset
     * the endOffset as from {@link #getEndOffset()}
     * @param line
     * the line number relative to src.getContent()
     * @param executions
     * the executions as in {@link #getExecutions()}
     */
    public DefaultLineExecutionAnnotation(String src, int offset,
            int endOffset, int line, long executions) {
        super(src, offset, endOffset, line);
        this.executions = executions;
        checkExecutions();
    }
    
    private void checkExecutions() {
        if (executions < -1) {
            throw new IllegalArgumentException("executions " + executions
                                                   + " is < -1");
        }
    }
    
    /* (non-Javadoc)
     * @see org.codecover.report.highlighting.annotation.ILineExecutionAnnotation#getExecutions()
     */
    public long getExecutions() {
        return executions;
    }

    /* (non-Javadoc)
     * @see org.codecover.report.highlighting.annotation.ILineExecutionAnnotation#hasExecutions()
     */
    public boolean hasExecutions() {
        return executions != -1;
    }
    
    /**
     * @return the text in this line excluding the line break character(s)
     */
    public String getText() {
        return getContent();
    }
    
    public String toString() {
        return super.toString() + "{ executions=" + getExecutions() + ", lineNo=" + getLineNo() + "}";
    }
}
