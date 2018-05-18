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

package org.codecover.report.dummy;

import org.codecover.model.utils.Logger;
import org.codecover.report.Report;
import org.codecover.report.ReportGenerator;
import org.codecover.report.ReportTest;

/**
 * Just a dummy report generator used to test
 * <code>org.codecover.report.Report</code> and
 * <code>org.codecover.report.Template</code>.
 *
 * @author Robert Hanussek
 * @version 1.0 ($Id: DummyReportGenerator.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DummyReportGenerator implements ReportGenerator {

    /**
     * Doesn't do anything, this is a dummy report generator it never logs
     * anything.
     */
    public void setLogger(Logger logger) {
    }

    public void generateReport(Report report) {
        report.setDirectoryName(ReportTest.DUMMYREPORTGEN_LOAD_SUCCESS);
    }

    public String getContentType() {
        return null;
    }

}
