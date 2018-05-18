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

import org.codecover.model.mast.Location;
import org.codecover.model.mast.SourceFile;

//TODO: Decide if really needed, then drop it. ;-)

/**
 * Mark a consecutive region of code with full reference to the complete
 * SourceFile.<br>
 * To be subclassed to add useful info.
 *
 * @author Johannes Langauf
 * @version 1.0 ($Id: FatAnnotation.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface FatAnnotation extends Annotation {

    /*
     * Comparators to compare by Start- and End-Offset
     */
    
    
    /**
     * Comparator to compare a {@link FatAnnotation} by start offset,
     * smaller first.
     */
    public final static Comparator<FatAnnotation> compareByStart = new Comparator<FatAnnotation>() {
        /**
         * Compares two CoverageAnnotations and returns
         * <ul><li>zero, if they are equal,</li>
         * <li>a negative integer, if c1 comes before c2</li>
         * <li>a positive integer, if c1 comes after c2</li></ul>
         * The order follows the following criteria:<br>
         * <ol>
         * <li>lexicographical by filename</li>
         * <li>smaller start position first</li>
         * <li>bigger end position first</li>
         * </ol>
         *  
         * 
         * @param c1 the first IAnnotation
         * @param c2 the second IAnnotation  
         * @return an int representing the order (see description) 
         */
        public int compare(FatAnnotation c1, FatAnnotation c2) {
            int result = 0;
            Location l1 = c1.getLocation();
            Location l2 = c2.getLocation();
            if (!l1.getFile().equals(l2.getFile())) {
                result = l1.getFile().getFileName().compareTo(
                        l2.getFile().getFileName());
                if (result == 0) {
                    result = l1.getFile().hashCode() - l2.getFile().hashCode();
                }
            } else {
                result = l1.getStartOffset() - l2.getStartOffset();
                if (result == 0) {
                    result = l2.getEndOffset() - l1.getEndOffset();
                }
            }
            return result;
        }
    };

    /**
     * Comparator to compare a {@link FatAnnotation} by end offset,
     * smaller first.
     */
    public final static Comparator<FatAnnotation> compareByEnd = new Comparator<FatAnnotation>() {
        /**
         * Compares two CoverageAnnotations and returns
         * <ul><li>zero, if they are equal,</li>
         * <li>a negative integer, if c1 comes before c2</li>
         * <li>a positive integer, if c1 comes after c2</li></ul>
         * The order follows the following criteria:<br>
         * <ol>
         * <li>lexicographical by filename</li>
         * <li>smaller end position first</li>
         * <li>bigger start position first</li>
         * </ol>
         *  
         * 
         * @param c1 the first IAnnotation
         * @param c2 the second IAnnotation  
         * @return an int representing the order (see description) 
         */
        public int compare(FatAnnotation c1, FatAnnotation c2) {
            int result = 0;
            Location l1 = c1.getLocation();
            Location l2 = c2.getLocation();
            if (!l1.getFile().equals(l2.getFile())) {
                result = l1.getFile().getFileName().compareTo(
                        l2.getFile().getFileName());
                if (result == 0) {
                    result = l1.getFile().hashCode() - l2.getFile().hashCode();
                }
            } else {
                result = l1.getEndOffset() - l2.getEndOffset();
                if (result == 0) {
                    result = l2.getStartOffset() - l1.getStartOffset();
                }
            }
            return result;
        }
    };

    /**
     * @return the location
     */
    public abstract Location getLocation();


    /**
     * @return the file
     */
    public abstract SourceFile getFile();

}
