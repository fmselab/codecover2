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

package org.codecover.report.html;

import org.codecover.report.*;
import org.codecover.model.utils.*;
import org.codecover.model.extensions.*;

public class HtmlReportPlugin extends AbstractPlugin {
    public HtmlReportPlugin() {
        super("HTML Report", "This plugin contains a hierarchical HTML generator and a single file HTML generator.", new Extension<?>[] {
                new AbstractExtension<ReportGenerator>(ReportGenerator.class,
                    "org.codecover.report.html.SingleFileHTMLReportGenerator") {
                    public ReportGenerator getObject() {
                        return new SingleFileHTMLReportGenerator();
                    }
                },
                new AbstractExtension<ReportGenerator>(ReportGenerator.class,
                    "org.codecover.report.html.HierarchicalHTMLReportGenerator") {
                    public ReportGenerator getObject() {
                        return new HierarchicalHTMLReportGenerator();
                    }
                },
            });
    }
}
