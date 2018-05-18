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

package org.codecover.metrics.coverage;

import java.io.Serializable;

/**
 * This class is used to store the results calculated during the analysis of
 * coverable items.
 * 
 * @author Markus Wittlinger, Tilmann Scheller
 * @version 1.0 ($Id: CoverageResult.java 67 2009-09-29 07:47:16Z ahija $)
 */
public final class CoverageResult implements Serializable {

    /**
     * Represents a {@link CoverageResult} with <code>0</code> in both fields
     */
    public static final CoverageResult NULL = new CoverageResult(0, 0);

    private static final long serialVersionUID = 1086417719942544939L;

    private final int totalItems;

    private final int coveredItems;

    /**
     * Constructor, with the two values, so no uninitialized CoverageResults
     * exist.
     * 
     * @param coveredItems
     *            The number of covered coverable items examined during the
     *            computation of the coverage. Is always smaller or equal to the
     *            total number of coverable items.
     * @param totalItems
     *            The total number of all coverable items examined during the
     *            computation of the coverage.
     */
    public CoverageResult(int coveredItems, int totalItems) {
        if (coveredItems > totalItems || coveredItems < 0 || totalItems < 0) {
            throw new IllegalArgumentException(coveredItems + " | " + totalItems);
        }

        this.coveredItems = coveredItems;
        this.totalItems = totalItems;
    }

    /**
     * @return the coveredItems
     */
    public int getCoveredItems() {
        return this.coveredItems;
    }

    /**
     * @return the totalItems
     */
    public int getTotalItems() {
        return this.totalItems;
    }

    /**
     * Check for trivial case that this result has nothing to cover.
     * @return true, iff this result has no coverable items
     */
    public boolean isNull() {
        return this.totalItems == 0;
    }

    @Override
    public boolean equals(Object o) {
        final CoverageResult co;
        if (!(o instanceof CoverageResult)) {
            return false;
        }
        co = (CoverageResult)o;
        return this.coveredItems == co.coveredItems
            && this.totalItems   == co.totalItems;
    }

    @Override
    public int hashCode() {
        return this.coveredItems ^ this.totalItems;
    }
}
