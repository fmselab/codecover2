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

import java.io.Serializable;
import java.util.*;

import org.codecover.model.utils.*;

/**
 * A BooleanAssignment assigns every basic boolean term of a boolean term an
 * BooleanResult.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: BooleanAssignment.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public final class BooleanAssignment implements Serializable {
    private static final long serialVersionUID = -5013863950617680255L;

    private final List<BooleanResult> results;

    private final int length;

    /**
     * Constructor
     * 
     * @param results
     *            the list of {@link BooleanResult}s stored in this instance
     */
    public BooleanAssignment(List<BooleanResult> results) {
        if (results == null) {
            throw new NullPointerException("results == null");
        }

        this.results = CollectionUtil.copy(results);
        this.length = this.results.size();
    }

    /**
     * Constructor
     * 
     * @param resultsInArray
     *            the {@link BooleanResult}s stored in an array
     */
    public BooleanAssignment(BooleanResult... resultsInArray) {
        Vector<BooleanResult> resultVector = new Vector<BooleanResult>(
                resultsInArray.length);
        for (BooleanResult thisResult : resultsInArray) {
            resultVector.add(thisResult);
        }
        this.results = Collections.unmodifiableList(resultVector);
        this.length = this.results.size();
    }

    /**
     * @return the results
     */
    public List<BooleanResult> getResults() {
        return this.results;
    }

    /**
     * @return the length of the assignment
     */
    public int getLength() {
        return this.length;
    }

    /**
     * Checks, whether or not the contents of the lists of {@link BooleanResult}s
     * match
     * 
     * @param obj
     *            the object to be compared.
     * @return true if the contents of the lists of {@link BooleanResult}s
     *         match, false if they do not match.
     */
    @Override
    public boolean equals(Object obj) {

        if (!(obj instanceof BooleanAssignment)) {
            return false;
        }

        BooleanAssignment that = (BooleanAssignment) obj;

        // If the sizes do not match, the content can never match
        if (this.getResults().size() != that.getResults().size()) {
            return false;
        }

        for (int i = 0; i < this.getResults().size(); i++) {
            boolean match = this.getResults().get(i).equals(
                    that.getResults().get(i));

            // If there was on instance of a mismatch, the two objects
            // can not be equal.
            if (!match) {
                return false;
            }
        }

        return true;
    }

    /**
     * Calculates the hash code to be used in e.g. {@link HashMap}s
     * 
     * @return the hash code
     */
    @Override
    public int hashCode() {
        int result = getResults().size();

        for (BooleanResult booleanResult : getResults()) {
            result *= 3;
            result += booleanResult.ordinal();
        }

        return result;
    }

    /**
     * Gets the toString() result of the list of {@link BooleanResult}s
     * 
     * @return the toString() result
     */
    @Override
    public String toString() {
        return this.results.toString();
    }
}
