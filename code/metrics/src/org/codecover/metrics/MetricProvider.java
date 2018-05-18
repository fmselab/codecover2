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

import java.util.*;

import org.codecover.metrics.coverage.CoverageMetric;
import org.codecover.model.utils.*;
import org.codecover.model.utils.criteria.Criterion;
import org.codecover.model.extensions.*;

/**
 * This class is used to get an instance of all classes, that implement Metric.
 * 
 * @author Markus Wittlinger, Tilmann Scheller
 * @version 1.0 ($Id: MetricProvider.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MetricProvider {

    /**
     * This method returns a list of the instances of all classes that implement
     * Metric. This is done through reflection.
     * 
     * @param pluginManager
     *            the {@link PluginManager} to use.
     * @param logger
     *            the {@link Logger} to use.
     * @return A set of metrics.
     */
    public static Set<Metric> getAvailabeMetrics(PluginManager pluginManager,
            Logger logger) {
        return PluginUtils.getExtensionObjects(pluginManager, logger,
                Metric.class);
    }

    /**
     * This method returns a set of {@link Metric}s that are available when a
     * given set of {link Criterion Criteria} was used for instrumentation.
     * 
     * @param criteria
     *            the criteria used for instrumentation
     * @param pluginManager
     *            the {@link PluginManager} to use.
     * @param logger
     *            the {@link Logger} to use.
     * @return A set of metrics.
     */
    public static Set<Metric> getAvailabeMetrics(PluginManager pluginManager,
            Logger logger, Set<Criterion> criteria) {
        return getAvailabeMetrics(getAvailabeMetrics(pluginManager, logger),
                criteria);
    }

    /**
     * This method returns a set of {@link Metric}s that are available when a
     * given set of {link Criterion Criteria} was used for instrumentation.
     * 
     * @param metrics
     *            the {@link Set} of {@link Metric}s to select from
     * @param criteria
     *            the criteria used for instrumentation
     * @return A set of metrics.
     */
    public static Set<Metric> getAvailabeMetrics(Set<Metric> metrics,
            Set<Criterion> criteria) {
        final Set<Metric> result = new HashSet<Metric>();

        for (Metric metric : metrics) {
            if (criteria.containsAll(metric.getRequiredCriteria())) {
                result.add(metric);
            }
        }

        return Collections.unmodifiableSet(result);
    }

    /**
     * This method returns a list of the instances of all classes that implement
     * CoverageMetric.
     * 
     * @param pluginManager
     *            the {@link PluginManager} to use.
     * @param logger
     *            the {@link Logger} to use.
     * @return A set of coverage metrics.
     */
    public static Set<CoverageMetric> getAvailabeCoverageMetrics(
            PluginManager pluginManager, Logger logger) {
        final Set<CoverageMetric> metrics = new HashSet<CoverageMetric>();

        for (Metric metric : getAvailabeMetrics(pluginManager, logger)) {
            if (metric instanceof CoverageMetric) {
                metrics.add((CoverageMetric) metric);
            }
        }

        return Collections.unmodifiableSet(metrics);
    }

    /**
     * This method returns a set of {@link Metric}s that are available when a
     * given set of {link Criterion Criteria} was used for instrumentation.
     * 
     * @param criteria
     *            the criteria used for instrumentation
     * @param pluginManager
     *            the {@link PluginManager} to use.
     * @param logger
     *            the {@link Logger} to use.
     * @return A set of metrics.
     */
    public static Set<CoverageMetric> getAvailabeCoverageMetrics(
            PluginManager pluginManager, Logger logger, Set<Criterion> criteria) {
        return getAvailabeCoverageMetrics(getAvailabeMetrics(pluginManager,
                logger), criteria);
    }

    /**
     * This method returns a set of {@link Metric}s that are available when a
     * given set of {link Criterion Criteria} was used for instrumentation.
     * 
     * @param criteria
     *            the criteria used for instrumentation
     * @param metrics
     *            the {@link Set} of {@link Metric}s to select from
     * @return A set of metrics.
     */
    public static Set<CoverageMetric> getAvailabeCoverageMetrics(
            Set<Metric> metrics, Set<Criterion> criteria) {
        final Set<CoverageMetric> result = new HashSet<CoverageMetric>();

        for (Metric metric : metrics) {
            if (metric instanceof CoverageMetric
                    && criteria.containsAll(metric.getRequiredCriteria())) {
                result.add((CoverageMetric) metric);
            }
        }

        return Collections.unmodifiableSet(result);
    }
}
