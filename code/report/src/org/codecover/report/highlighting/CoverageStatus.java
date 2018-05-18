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

package org.codecover.report.highlighting;

import org.codecover.metrics.coverage.CoverageResult;

/**
 * Indicates the coverage status of a <code>Location</code>.
 * Each value has a text, that can be used e.g. for HTML-Tags
 * showing which CoverageStatus was reached
 */
public enum CoverageStatus {
    /**
     * no information measured; lowest status (ordinal)
     */
    NONE   ("none"),
    
    /**
     * not covered
     */
    NOT    ("not"),
    
    /**
     * covered partly
     */
    PARTLY ("partly"),
    
    /**
     * covered fully; highest status (ordinal)
     */
    FULLY  ("fully");
    
    private final String text;
    
    private CoverageStatus(String text) {
        this.text = text;
    }
    
    /**
     * Get a string representing this Constant. Each String
     * is the same as the lowercased Constant-Name e.g.
     * FULLY.getText() returns "fully"
     * 
     * @return the text belonging to this enum-Constant 
     */
    public String getText() {
        return text;
    }

    /**
     * Returns the CoverageStatus representing the coverage that is reached
     * if you have the CoverageResult <i>result</i>
     * 
     * @param result the CoverageResult beeing reached
     * @return  the CoverageStatus representing this situation
     */
    public static CoverageStatus calcCoverageStatus(CoverageResult result) {
        return calcCoverageStatus(result.getCoveredItems(),
                                  result.getTotalItems());
    }

    /**
     * Returns the CoverageStatus representing the coverage that is reached
     * if you have <i>covered</i> coveredItems and <i>total</i> coverable
     * items over all 
     * 
     * @param covered   the amount of coverableItems being covered 
     * @param total     the amount of all coverableItems 
     * @return  the CoverageStatus representing this situation
     */
    public static CoverageStatus calcCoverageStatus(int covered, int total) {
        if (total == 0) {
            return NONE;
        } else if (covered == total) {
            return FULLY;
        } else if (covered == 0) {
            return NOT;
        } else {
            return PARTLY;
        }
    }
}