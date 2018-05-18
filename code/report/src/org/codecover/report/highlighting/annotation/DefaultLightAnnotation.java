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


import org.codecover.model.mast.Location;
import org.codecover.model.mast.SourceFile;

/**
 * Mark a consecutive region of code. To be subclassed to add useful info.
 *
 * @author Johannes Langauf
 * @version 1.0 ($Id: DefaultLightAnnotation.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DefaultLightAnnotation extends AbstractAnnotation {
    
    final int offset;
    final int length;
    final String wholeFile;
    
    /**
     * 
     * @param src
     * the source file to extract content from
     * @param offset
     * the offset as from {@link #getStartOffset()}
     * @param length
     * the length as from {@link #getLength()}
     */
    public DefaultLightAnnotation(SourceFile src, int offset, int length) {
        wholeFile = src.getContent();
        this.length = length;
        this.offset = offset;
        check();
    }
    
    /**
     * 
     * @param src
     * the source file to extract content from
     * @param offset
     * the offset as from {@link #getStartOffset()}
     * @param endOffset
     * the endOffset as from {@link #getEndOffset()}
     */
    public DefaultLightAnnotation(int offset, int endOffset, SourceFile src) {
        wholeFile = src.getContent();
        this.length = endOffset - offset;
        this.offset = offset;
        check();
    }

    /**
     * 
     * @param src
     * the source file to extract content from
     * @param offset
     * the offset as from {@link #getStartOffset()}
     * @param length
     * the length as from {@link #getLength()}
     */
    public DefaultLightAnnotation(String src, int offset, int length) {
        wholeFile = src;
        this.length = length;
        this.offset = offset;
        check();
    }
    
    /**
     * 
     * @param src
     * the source file to extract content from
     * @param offset
     * the offset as from {@link #getStartOffset()}
     * @param endOffset
     * the endOffset as from {@link #getEndOffset()}
     */
    public DefaultLightAnnotation(int offset, int endOffset, String src) {
        wholeFile = src;
        this.length = endOffset - offset;
        this.offset = offset;
        check();
    }

    @Override
    protected void check() {
        if (getEndOffset() > wholeFile.length()) {
            throw new IllegalArgumentException("End offset outside src");
        }
        
        super.check();
    }

    @Override
    public int getStartOffset() {
        return offset;
    }

    @Override
    public int getLength() {
        return length;
    }

    public String getContent() {
        return getContent(wholeFile);
    }
}
