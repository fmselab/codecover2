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

package org.codecover.model.mast;

/**
 * A Location is a segment in a code file. It is given by its startOffset
 * (which is the offset of the first char belonging to the location), its
 * endOffset (which is the offset of the first char no longer belonging to the
 * location), and the length which contains this location.
 * 
 * @author Steffen Kieß, Johannes Langauf
 * @version 1.1 ($Id: Location.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public final class Location {
    private final int startOffset;

    private final int endOffset;

    private final SourceFile file;

    Location(SourceFile file, int startOffset, int endOffset) {
        if (file == null) {
            throw new NullPointerException("file == null");
        }

        if (startOffset < 0) {
            throw new IllegalArgumentException("startOffset < 0");
        }

        if (endOffset < 0) {
            throw new IllegalArgumentException("endOffset < 0");
        }

        if (startOffset > endOffset) {
            throw new IllegalArgumentException("startOffset > endOffset");
        }

        if (endOffset > file.getContent().length()) {
            throw new IllegalArgumentException(
                    "endOffset > file.getContent().length()");
        }

        this.file = file;
        this.startOffset = startOffset;
        this.endOffset = endOffset;
    }

    /**
     * Get the offset of the first char no longer belonging to the location.
     * Nothing belongs to this location if it equals startOffset.
     * 
     * @return the EndOffset
     */
    public int getEndOffset() {
        return this.endOffset;
    }

    /**
     * @return the file
     */
    public SourceFile getFile() {
        return this.file;
    }

    /**
     * Get the offset of the first char belonging to the location. 0 to include
     * the first character of the SourceFile.
     * @return the startOffset 
     */
    public int getStartOffset() {
        return this.startOffset;
    }

    /**
     * Get the number of characters contained in the location.
     * @return the length
     */
    public int getLength() {
        return this.endOffset - this.startOffset;
    }
    
    /**
     * Check if this Location contains <code>inner</code> entirely.
     * <p>Note: {@link Location#contains(Location)} is transitive and (1,2)
     * contains both (1,1) and (2,2), with the first number being the
     * startOffset and the second the endOffset.
     * 
     * @param inner
     * not null
     * @return true, iff inner is in the range of this Location or they equal
     */
    public boolean contains(Location inner) {
        if (inner == null) {
            throw new NullPointerException("inner is null");
        }
        
        SourceFile innerFile = inner.getFile();
        SourceFile thisFile = getFile();
        if (! thisFile.getFileName().equals(innerFile.getFileName())
                || ! thisFile.getContent().equals(innerFile.getContent())) {
            return false;
        }
        
        /* source files match */
        
        if (inner.getStartOffset() >= getStartOffset()
            && inner.getEndOffset() <= getEndOffset()) {
            /* inner is within bounds of this */
            return true;
        } else {
            return false;
        }
    }
    
    /**
     * @return the content of the location
     */
    public String getContent() {
        return getFile().getContent().substring(getStartOffset(),
                getEndOffset());
    }

    /**
     * Get a debugging representation of the {@link Location}
     * 
     * @return a debugging representation of the {@link Location}
     */
    @Override
    public String toString() {
        return "Location in '" + getFile() + "' from char " + getStartOffset() + " to char " + getEndOffset() + ": '" + getContent() + "'" ;
    }
}
