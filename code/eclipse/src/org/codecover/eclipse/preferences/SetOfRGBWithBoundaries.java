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

import java.util.Iterator;
import java.util.TreeSet;

import org.eclipse.jface.resource.DataFormatException;

/**
 * A SetOfRGBWithBoundaries represents a TreeSet of RGBWithBoundaries and
 * therefore ensures, that all contained RGBWithBoundaries are sorted and no two
 * RGBWithBoundaries with equal boundaries exist. Furthermore, this class
 * provides methods to encode itself to and decode itself from a string.
 * 
 * @author Michael Starzmann
 * @version 1.0 ($Id: SetOfRGBWithBoundaries.java 1863 2007-08-15 15:32:57Z
 *          starzmml $)
 */
public class SetOfRGBWithBoundaries extends TreeSet<RGBWithBoundaries> {

    private static final long serialVersionUID = -7918972288052795558L;

    private static final String SEPERATOR = ":"; //$NON-NLS-1$

    /**
     * Standard-constructor
     */
    public SetOfRGBWithBoundaries() {
        super();
    }

    /**
     * Creates a set with elements according to values encoded in a string.
     * SetOfRGBWithBoundaries.toString() returns such a string.
     * 
     * @param string
     *            an encoded representation of all the elements this set shall
     *            have
     * @throws DataFormatException
     *             thrown, if stringformat doesn't fit
     */
    public SetOfRGBWithBoundaries(String string) throws DataFormatException {
        super();
        this.setTo(string);
    }

    /**
     * Sets this set according to elements encoded in a string
     * 
     * @param string
     *            an encoded representation of all the elements this set shall
     *            have
     * @throws DataFormatException
     *             thrown, if stringformat doesn't fit
     */
    public void setTo(String string) throws DataFormatException {
        this.clear();
        String[] elements = string.split(SEPERATOR);
        for (String s : elements) {
            this.add(new RGBWithBoundaries(s));
        }
    }

    /**
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        Iterator<RGBWithBoundaries> iterator = this.iterator();
        while (iterator.hasNext()) {
            sb.append(iterator.next().toString());
            if (iterator.hasNext()) {
                sb.append(SEPERATOR);
            }
        }
        return sb.toString();
    }
}
