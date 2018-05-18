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

package org.codecover.report;

import org.codecover.report.exceptions.ReportException;
import org.codecover.report.exceptions.ReportIOException;

/**
 * A ReportGenerator is responsible for creating a report in a specific format,
 * e.g. hierarchical HTML.
 * <p>
 * Since the data model of CodeCover is designed to be able to save data
 * independently from programming languages this interface can be implemented so
 * that it generates reports for all programming languages supported by
 * CodeCover.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: ReportGenerator.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface ReportGenerator {

    /**
     * This method returns the output format of the report the
     * <code>ReportGenerator</code> produces as a MIME content type (e.g.
     * text/plain for plain text).
     * 
     * @return  the output format (of the report the
     *          <code>ReportGenerator</code> produces) as a MIME content type
     *          (e.g. &quot;text/plain&quot; for plain text).
     */
    public String getContentType();

    /**
     * This method generates the report based on the settings saved in the given
     * <code>Report</code>. These settings can be retrieved by the getters of
     * the given <code>Report</code>.
     * 
     * @param report    the <code>Report</code>-object which contains the
     *                  settings to generate the report
     * 
     * @throws ReportException      thrown on any error parsing or applying the
     *                              template
     * 
     * @throws ReportIOException    thrown on IO error during report generation
     */
    public void generateReport(Report report)
            throws ReportException, ReportIOException;

}
