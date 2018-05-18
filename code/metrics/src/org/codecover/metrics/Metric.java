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

package org.codecover.metrics;

import java.util.Set;

import org.codecover.model.utils.criteria.Criterion;

/**
 * This interface should be implemented by all classes, that are used to analyze
 * parts of the data model.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: Metric.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface Metric {
    /**
     * Every metric has a unique name, that can be used to differeniate between
     * metrics.
     * 
     * @return Name of the metric.
     */
    public String getName();

    /**
     * Every metric has a description, that explains the purpose of the metric.
     * 
     * @return Description of the metric.
     */
    public String getDescription();

    /**
     * The returned set of Criterion denotes the criteria that had to have been
     * used during the instrumentation process. E.g. to calculate the statement
     * coverage of a test run of a SUT, said SUT must have been instrumented to
     * calculate statement coverage. It is only sensible to apply a Metric if
     * its required criteria are a subset of the criteria used in the
     * instrumentation process.
     * 
     * @return The criteria required for use of this metric.
     */
    public Set<Criterion> getRequiredCriteria();
}
