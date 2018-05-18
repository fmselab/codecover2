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
 * Class to ease coding of annotation classes.
 * <p> Clients must implement {@link #getContent()} and two of the Methods:
 * <ul>
 *     <li> {@link #getStartOffset()}
 *     <li> {@link #getEndOffset()}
 *     <li> {@link #getLength()}
 * </ul>
 *
 * @author Johannes Langauf
 * @version 1.0 ($Id: AbstractAnnotation.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public abstract class AbstractAnnotation implements Annotation {

    public int getEndOffset() {
        return getStartOffset() + getLength();
    }

    public int getStartOffset() {
        return getEndOffset() - getLength();
    }

    public int getLength() {
        return getEndOffset() - getStartOffset();
    }
    
    public boolean intersects(Annotation a) {
        return a.getStartOffset() < getEndOffset() && a.getStartOffset() >= getStartOffset()
        || a.getEndOffset() < getEndOffset() && a.getEndOffset() > getStartOffset()
        || a.getStartOffset() <= getStartOffset() && a.getEndOffset() >= a.getEndOffset();
    }
    
    /** 
     * @param wholeFile
     * the whole text of the source file
     * @return result of {@link #getContent()}
     */
    protected String getContent(String wholeFile) {
        return wholeFile.substring(getStartOffset(), getEndOffset());
    }

    /**
     * Test if the values are valid against the interface definition. 
     */
    protected void check() {
        int l = getLength();
        int o = getStartOffset();
        if (l < 0) {
            throw new IllegalArgumentException("length" + l + " < 0");
        }
        if (o < 0) {
            throw new IllegalArgumentException("offset" + o + " < 0");
        }
        if (getContent().length() != l) {
            throw new IllegalArgumentException("length doesn't match length of content.");
        }
    }
    
    @Override
    public String toString() {
        return getQuote();
    }
    
    public String getQuote() {
        return getContent() + "\n   --- " + getReference();
    }
    
    /**
     * may overwrite with more exact reference for {@link #toString()} 
     */
    protected String getReference() {
        return "at [" + getStartOffset() + ";" + getEndOffset() + "[";
    }

}
