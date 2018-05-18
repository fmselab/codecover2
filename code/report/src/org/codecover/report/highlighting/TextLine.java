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

package org.codecover.report.highlighting;

/**
 * A line from a SourceFile with additional Info for reporting.
 * 
 * @author Michael Starzmann
 * @version 1.0 ($Id: TextLine.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class TextLine {
    
    /**
     * The line number of this line. The first line has number 1. Always >= 1.
     */
    private final int lineNo;
    
    /**
     * How often the most often executed part of this line was executed i.e.
     * covered. This may include incomplete executions. -1 for undefined.
     */
    private final long executions;
    
    
    private final String text;
    
    /**
     * Creates a new TextLine object representing one line of
     * text with its line number
     * 
     * @param lineNo  the line number belonging to the text
     * @param text    the text belonging to the line number
     */
    public TextLine (int lineNo, long executions, String text) {
        /* preconditions: */
        if (lineNo < 1) {
            throw new IllegalArgumentException("lineNo " + lineNo + " is < 1");
        }
        if (executions < -1) {
            throw new IllegalArgumentException("executions " + executions
                                                   + " is < -1");
        }
        
        this.lineNo = lineNo;
        this.text = text;
        this.executions = executions;
    }
    
    /**
     * Get {@link TextLine#lineNo}
     * @return lineNo 
     */
    public int getLineNo() {
        return lineNo;
    }
    
    /**
     * The text of the line, usually withsome (HTML)-Markup in it.
     * 
     * @return the text as set in the constructor
     */
    public String getText() {
        return text;
    }
    
    /**
     * Get {@link TextLine#executions}
     * @return executions 
     */
    public long getExecutions() {
        return executions;
    }

}