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

import java.util.Comparator;

/**
 * Mark a consecutive region of code, referenced relative to the file start.
 * <p>This class only requires a reference to the annotated region of the code
 * file, while {@link FatAnnotation} requires a reference to the complete code
 * of the source file.
 * <p>
 * To be subclassed to add useful info.
 *
 * @author Johannes Langauf
 * @version 1.0 ($Id: Annotation.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface Annotation {

    /**
     * Comparator to compare a {@link Annotation} by start offset,
     * smaller first.
     */
    public final static Comparator<Annotation> compareByStart = new Comparator<Annotation>() {
        /**
         * Compares two CoverageAnnotations and returns
         * <ul><li>zero, if they are equal,</li>
         * <li>a negative integer, if c1 comes before c2</li>
         * <li>a positive integer, if c1 comes after c2</li></ul>
         * The order follows the following criteria:<br>
         * <ol>
         * <li>smaller start position first</li>
         * <li>bigger end position first</li>
         * </ol>
         *  
         * 
         * @param c1 the first IAnnotation
         * @param c2 the second IAnnotation  
         * @return an int representing the order (see description) 
         */
        public int compare(Annotation c1, Annotation c2) {
            int result = c1.getStartOffset() - c2.getStartOffset();
            if (result == 0) {
                result = c2.getEndOffset() - c1.getEndOffset();
            }
            return result;
        }
    };
    
    /**
     * Comparator to compare a {@link FatAnnotation} by end offset,
     * smaller first.
     */
    public final static Comparator<Annotation> compareByEnd = new Comparator<Annotation>() {
        /**
         * Compares two CoverageAnnotations and returns
         * <ul><li>zero, if they are equal,</li>
         * <li>a negative integer, if c1 comes before c2</li>
         * <li>a positive integer, if c1 comes after c2</li></ul>
         * The order follows the following criteria:<br>
         * <ol>
         * <li>smaller end position first</li>
         * <li>bigger start position first</li>
         * </ol>
         *  
         * 
         * @param c1 the first IAnnotation
         * @param c2 the second IAnnotation  
         * @return an int representing the order (see description) 
         */
        public int compare(Annotation c1, Annotation c2) {
            int result = c1.getEndOffset() - c2.getEndOffset();
            if (result == 0) {
                result = c2.getStartOffset() - c1.getStartOffset();
            }
            return result;
        }
    };

    /**
     * Get the offset of the first char no longer belonging to the annotation.
     * Nothing belongs to this anotation if it equals startOffset.
     * 
     * @return the EndOffset
     */
    public abstract int getEndOffset();

    /**
     * Get the offset of the first char belonging to the location. 0 to include
     * the first character of the SourceFile.
     * @return the startOffset 
     */
    public abstract int getStartOffset();

    /**
     * @return the number of characters annotated
     */
    public abstract int getLength();
    
    /**
     * @param a
     * @return true, iff <code>a</code> overlaps in at lest one character with
     *  <code>this</code>
     */
    public boolean intersects(Annotation a);
    
    /**
     * @return the annotated content
     * //FIXME: remove deprecation or method from Interface
     * //currently required in Report for LineAnnoation
     * @deprecated used almost never and requires a string for each annotation
     */
    public abstract String getContent();

    /**
     * <p>Gets the annotated content as a quote for debugging. The quote
     * includes at least the first and last line of getContent() as well as
     * the offsets.
     * 
     * @return an exact reference of this annotation in the text for debugging
     */
    public abstract String getQuote();

}
