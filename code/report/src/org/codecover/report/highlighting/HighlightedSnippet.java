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

import java.util.List;

import org.codecover.report.highlighting.annotation.ExecutionInformation;
import org.codecover.report.highlighting.annotation.LineExecutionAnnotation;


/**
 * A HighlightedSnippet is an excerpt of a source file with line wise coverage
 * information. 
 * 
 * @author Johannes Langauf
 * @version 1.0 ($Id: HighlightedSnippet.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class HighlightedSnippet implements ExecutionInformation {
    
    private final String fileName;
    private final List<LineExecutionAnnotation> textLines;
    private final long maxExecutions;
    
    /**
     * @param fileName
     * the name of the file the lines are from, "" for none
     * @param textLines
     * the lines of the file in order, may not be changed afterwards
     * @param maxExecutions
     * the maximum getExecutions() of all code lines, -1 if no line is executable
     */
    public HighlightedSnippet(String fileName,
            List<LineExecutionAnnotation> textLines,
            long maxExecutions) {

        /* check some preconditions */
        if (fileName == null) {
            throw new IllegalArgumentException("fileName is null");
        }
        if (fileName == null) {
            throw new IllegalArgumentException("textLines is null");
        }
        if (maxExecutions < -1) {
            throw new IllegalArgumentException("maxExecutions invalid: < -1");
        }
        
        this.fileName = fileName;
        this.textLines = textLines;
        this.maxExecutions = maxExecutions;
    }

    /**
     * @return the name of the file this snipped is taken from
     */
    public String getFileName() {
        return fileName;
    }
    
    public long getExecutions() {
        return maxExecutions;
    }

    public boolean hasExecutions() {
        return maxExecutions != -1;
    }
    
    /**
     * @return the lines of text in this snippet, without line breaks
     */
    public List<LineExecutionAnnotation> getTextLines() {
        return textLines;
    }

}