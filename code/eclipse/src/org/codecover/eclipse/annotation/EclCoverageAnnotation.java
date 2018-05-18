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

package org.codecover.eclipse.annotation;

import org.codecover.metrics.coverage.CoverageMetric;
import org.codecover.report.highlighting.CoverageStatus;
import org.codecover.report.highlighting.annotation.CoverageAnnotation;
import org.eclipse.jface.text.Position;

/**
 * Annotation object that includes its position and all coverage information
 * to avoid further mappings in {@link org.codecover.eclipse.annotation}. This
 * is the presentation form of an {@link CoverageAnnotation} displayed in
 * Eclipse.
 * 
 * @author  Johannes Langauf
 * @version 1.0 ($Id: EclCoverageAnnotation.java 1 2007-12-12 17:37:26Z t-scheller $) 
 */
public class EclCoverageAnnotation extends EclPositionedAnnotation {

    /**
     * The generic Annotation that contains all relevant information for
     * this Eclipse specific Annotation. Currently only required for
     * getText().
     */
    private final CoverageAnnotation source;

    /**
     * Create a presentabel CoverageAnnotation from the more abstract model
     * from reporting.
     *
     * @param a
     * coverage information to generate presentation for, not null!
     */
    public EclCoverageAnnotation(CoverageAnnotation a) {
        super(getAnnotationID(a.getStatus(), a.getMetric()),
                new Position(a.getStartOffset(), a.getLength()));
        source = a;
    }

    @SuppressWarnings("nls")
    private static String getAnnotationID(CoverageStatus s, CoverageMetric metric) {
        final String metricID;
        if (metric.getName().equals("Branch Coverage")) {
            metricID = "Branch";
        } else if (metric.getName().equals("Strict Condition Coverage")) {
            metricID = "Condition";
        } else if (metric.getName().equals("Loop Coverage")) {
            metricID = "Loop";
        } else if (metric.getName().equals("Statement Coverage")) {
            metricID = "Statement";
        } else {
            metricID = "Other";
        }
        
        final String result;
        if (s == CoverageStatus.FULLY) {
            result = "org.codecover.eclipse.annotation.fullCoverage"
                + metricID + "Annotation";
        } else if (s == CoverageStatus.PARTLY) {
            result = "org.codecover.eclipse.annotation.partialCoverage" 
                + metricID + "Annotation";
        } else if (s == CoverageStatus.NOT) {
            result = "org.codecover.eclipse.annotation.noCoverage"
                + metricID + "Annotation";
        } else {
            throw new RuntimeException("Unknown status of Annotation: " + s);
        }

        // avoid duplicates in RAM and make result easy to compare
        return result.intern();
    }

    @Override
    public String getText() {
        //XXX: internationalization of this Tooltip
        /* not called extremely often. only on hover events */
        String ret = source.getNote();
        return ret;
    }

}
