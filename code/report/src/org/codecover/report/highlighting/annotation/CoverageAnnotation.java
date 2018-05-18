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

import java.util.Set;

import org.codecover.metrics.coverage.CoverageMetric;
import org.codecover.metrics.coverage.CoverageMetric.Hint;
import org.codecover.report.highlighting.CoverageStatus;

/**
 * Annotation about the coverage status of a MAST element.
 *
 * @author Johannes Langauf
 * @version 1.0 ($Id: CoverageAnnotation.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface CoverageAnnotation extends Annotation, ExecutionInformation {

    /**
     * Determine how often this Annotation's coverage criterion was fulfilled.
     * 
     * @return the executions
     */
    public abstract long getExecutions();

    /**
     * @return the metric that produced the annotation
     */
    public abstract CoverageMetric getMetric();

    /**
     * Get a hint on how to cover this fully or what this coverage means for 
     * this element.
     * 
     * @return the note, as text in English or null for none
     */
    public abstract String getNote();

    /**
     * Get a hint on how to cover this fully or what this coverage means for 
     * this element.
     * @return the hint
     */
    public abstract Set<Hint> getHints();
    
    /**
     * @return the status
     */
    public abstract CoverageStatus getStatus();

}
