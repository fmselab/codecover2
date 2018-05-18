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

package org.codecover.metrics.correlation;

import java.util.List;

import org.codecover.metrics.Metric;
import org.codecover.model.*;

/**
 * This interface is meant to be implemented by all the metrics, that calculate
 * correlation.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: CorrelationMetric.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface CorrelationMetric extends Metric {

    /**
     * Calculates the correlation of the given {@link TestCase}s to each other
     * <p>
     * Note: All the test case must belong to the same
     * {@link TestSessionContainer}
     * 
     * @param testCases
     *            the {@link TestCase}s, whose correlation is desired
     * @return the {@link CorrelationResult} which contains the results for all
     *         comparisons between all the {@link TestCase}s
     */
    public abstract CorrelationResult calculateCorrelation(
            List<TestCase> testCases);
}
