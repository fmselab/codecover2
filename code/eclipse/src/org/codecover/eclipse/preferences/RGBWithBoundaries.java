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

package org.codecover.eclipse.preferences;

import org.eclipse.jface.resource.DataFormatException;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.graphics.RGB;

/**
 * This class saves two integer-boundaries together with a color, so it can be
 * used as a definition of a range of values with a corresponding color, e.g.
 * for the Correlation Matrix.<br>
 * At every time, (lowerBoundary <= upperBoundary) is true.
 * 
 * @author Michael Starzmann
 * @version 1.0 ($Id: RGBWithBoundaries.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class RGBWithBoundaries implements Comparable<RGBWithBoundaries> {

    private static final String SEPERATOR = ";"; //$NON-NLS-1$

    private RGB rgb;

    private int lowerBoundary;

    private int upperBoundary;

    /**
     * Creates a new RGBWithBoundaries setting all values according to the
     * parameters. The two given boundaries are used as upper- and lowerBoundary
     * according to their value.
     * 
     * @param rgb
     *            the RGB this RGBWithBoundaries should have
     * @param firstBoundary
     *            the lowerBoundary if it's smaller than secondBoundary, the
     *            upperBoundary otherwise
     * @param secondBoundary
     *            the lowerBoundary if it's smaller than firstBoundary, the
     *            upperBoundary otherwise
     */
    public RGBWithBoundaries(RGB rgb, int firstBoundary, int secondBoundary) {
        super();
        setValues(rgb, firstBoundary, secondBoundary);
    }

    /**
     * Creates a new RGBWithBoundaries with the values encoded in the string
     * 
     * @param string
     *            an encoding of a RGBWithBoundaries which can be get using
     *            RGBWithBoundaries.toString()
     * @throws DataFormatException
     *             thrown, if stringformat doesn't fit
     */
    public RGBWithBoundaries(String string) throws DataFormatException {
        super();
        setValues(string);
    }

    /**
     * sets all values of this RGBWithBoundaries in a convenient way
     * 
     * @param rgb
     *            the RGB this RGBWithBoundaries should have
     * @param firstBoundary
     *            the lowerBoundary if it's smaller than secondBoundary, the
     *            upperBoundary otherwise
     * @param secondBoundary
     *            the lowerBoundary if it's smaller than firstBoundary, the
     *            upperBoundary otherwise
     */
    public void setValues(RGB rgb, int firstBoundary, int secondBoundary) {
        this.rgb = rgb;
        if (firstBoundary <= secondBoundary) {
            this.lowerBoundary = firstBoundary;
            this.upperBoundary = secondBoundary;
        } else {
            this.lowerBoundary = secondBoundary;
            this.upperBoundary = firstBoundary;
        }
    }

    /**
     * Sets the values of this RGBWithBoundaries according to the given string,
     * which can be created by calling toString(). UpperBoundary & lowerBoundary
     * are set so lowerBoundary <= upperBoundary
     * 
     * @param string
     *            an encoding of a RGBWithBoundaries
     * @throws DataFormatException
     *             thrown, if stringformat doesn't fit
     */
    public void setValues(String string) throws DataFormatException {
        String[] elements = string.split(SEPERATOR);
        if (elements.length == 3) {
            this.setValues(StringConverter.asRGB(elements[0]), StringConverter
                    .asInt(elements[1]), StringConverter.asInt(elements[2]));
        } else {
            throw new DataFormatException("Invalid data format. " //$NON-NLS-1$
                    + string + " doesn't consist of three parts devided by \"" //$NON-NLS-1$
                    + SEPERATOR + "\""); //$NON-NLS-1$
        }
    }

    /**
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return StringConverter.asString(this.rgb) + SEPERATOR
                + StringConverter.asString(this.lowerBoundary) + SEPERATOR
                + StringConverter.asString(this.upperBoundary);
    }

    /**
     * @return the upperBoundary
     */
    public int getUpperBoundary() {
        return this.upperBoundary;
    }

    /**
     * @return the lowerBoundary
     */
    public int getLowerBoundary() {
        return this.lowerBoundary;
    }

    /**
     * Compares a give {@link RGBWithBoundaries} with this one.
     * 
     * @param o
     *            the {@link RGBWithBoundaries} for the comparison
     * @return the comparison result.
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
	public int compareTo(RGBWithBoundaries o) {
        if (this.lowerBoundary == o.lowerBoundary) {
            return this.upperBoundary - o.upperBoundary;
        }
        return Float.compare(this.lowerBoundary, o.lowerBoundary);
    }

    /**
     * @return the rgb
     */
    public RGB getRGB() {
        return this.rgb;
    }

    /**
     * @param rgb
     *            the rgb to set
     */
    public void setRGB(RGB rgb) {
        this.rgb = rgb;
    }

    /**
     * This method ensures, that lowerBoundary is not greater than upperBoundary
     * by setting upperBoundary to the same value, if it would be.
     * 
     * @param lowerBoundary
     *            the lowerBoundary to set
     */
    public void setLowerBoundary(int lowerBoundary) {
        this.lowerBoundary = lowerBoundary;
        if (this.upperBoundary < lowerBoundary) {
            this.upperBoundary = lowerBoundary;
        }
    }

    /**
     * This method ensures, that upperBoundary is not smaller than lowerBoundary
     * by setting lowerBoundary to the same value, if it would be.
     * 
     * @param upperBoundary
     *            the upperBoundary to set
     */
    public void setUpperBoundary(int upperBoundary) {
        this.upperBoundary = upperBoundary;
        if (this.lowerBoundary > upperBoundary) {
            this.lowerBoundary = upperBoundary;
        }
    }
}
