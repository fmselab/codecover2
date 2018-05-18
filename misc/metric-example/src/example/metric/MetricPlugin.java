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

package example.metric;

import org.codecover.metrics.*;
import org.codecover.model.utils.*;
import org.codecover.model.extensions.*;

public class MetricPlugin extends AbstractPlugin {
    public MetricPlugin() {
        super("Example Metric Plugin", "This is a metric example plugin", new Extension<?>[] {
                new AbstractExtension<Metric>(Metric.class,
                                              "example.metric.MethodCoverage") {
                    public Metric getObject() {
                        return MethodCoverage.getInstance();
                    }
                }
            });
    }
}
