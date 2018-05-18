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
 * Mark a consecutive region of code as a line with it's line number.
 *
 * @author Johannes Langauf
 * @version 1.0 ($Id: DefaultLineAnnotation.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DefaultLineAnnotation extends DefaultLightAnnotation implements
        LineAnnotation {

    /**
     * The line number of this line. The first line has number 1. Always >= 1.
     */
    final int lineNo;
    
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
     */
    public DefaultLineAnnotation(SourceFile src, int offset, int endOffset, int line) {
        super(offset, endOffset, src);
        
        /* precondition */
        if (line < 1) {
            throw new IllegalArgumentException("lineNo " + line + " is < 1");
        }
        
        lineNo = line;
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
     */
    public DefaultLineAnnotation(String src, int offset, int endOffset, int line) {
        super(offset, endOffset, src);
        
        /* precondition */
        if (line < 1) {
            throw new IllegalArgumentException("line < 1");
        }
        
        lineNo = line;
    }
    
    public int getLineNo() {
        return lineNo;
    }

}
