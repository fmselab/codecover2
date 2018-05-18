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

/**
 * Provides classes and interfaces for generating reports in a manner
 * independent of programming languages.
 * 
 * The report component contains report generators whereas each of them is
 * responsible for the creation of a format, e.g.
 * {@linkplain org.codecover.report.html.HierarchicalHTMLReportGenerator
 *  hierarchical HTML}.
 * A specific report is generated based on a template which specifies the report
 * generator needed for the report generation.
 * <p>
 * The report generators are designed to be flexible in their output behavior
 * to be able to output the report in databases, files or a buffer in memory.
 * <p>
 * This flexibility in report generation makes chaining of report generators
 * possible, that is a report generator can write its output into a buffer in
 * memory and this output can be used as the template for another report
 * generator.
 * <p>
 * This flexibility is established by providing an <code>OutputStream</code> as
 * the target of the index file or the file for the single-file-report instead
 * of a path in the filesystem. Thus an <code>OutputStream</code> can be
 * provided to a report generator which in turn provides access to a database
 * for example.
 * <p>
 * The auxiliary files are created by a <code>FileCreationHandler</code>. If a
 * report generator needs to create an auxiliary file it requests a file from
 * the given <code>FileCreationHandler</code> which creates the file and returns
 * an <code>OutputStream</code> the report generator can write to.
 * 
 * <h3>Terminology</h3>
 * 
 * The following terms are used throughout the documentation of this package.
 * 
 * <dl>
 * 
 * <dt>multiple-files-report</dt>
 * <dd>A report which consists of multiple files (which possibly reference each
 * other through links). A m. consists of an <em>index file</em> and an
 * <em>output directory</em> which contains the rest of the files which are
 * called <em>auxiliary files</em> in the documentation of this package.</dd>
 * 
 * <dt>single-file-report</dt>
 * <dd>A report which consists of only one single file.</dd>
 *
 * <dt>basename, dirname</dt>
 * <dd>The basename of a path is its last component. In this case components are
 * files or directories. The dirname of a path is the path without its last
 * component. These definitions are borrowed from the POSIX standard and best
 * explained by way of example: The basename of the path
 * &quot;dir1/dir2/file&quot; is &quot;file&quot;, the dirname is
 * &quot;dir1/dir2&quot;. The basename of the path &quot;dir1/dir2/dir3&quot; is
 * &quot;dir3&quot;, the dirname is &quot;dir1/dir2&quot;.</dd>
 * 
 * </dl>
 * 
 * <h3>General procedure</h3>
 * 
 * Report generation works as follows:
 * <ol>
 * 
 * <li>A <code>Report</code> is instantiated.</li>
 * 
 * <li>The fields of <code>Report</code> are set via its setters. These fields
 * represent the options for report generation, e.g. the place to save the
 * report to or the location of the template.</li>
 * 
 * <li>The method <code>Report.generateReport</code> is called.</li>
 * 
 * <li><code>Report.generateReport</code> loads the <code>ReportGenerator</code>
 * via a <code>PluginManager</code>. A <code>ReportGenerator</code> is a class
 * which implements the interface <code>ReportGenerator</code>. One class which
 * implements <code>ReportGenerator</code> is responsible for exactly one output
 * format (respectively content type) of reports, e.g. HTML (text/html).</li>
 * 
 * <li><code>Report.generateReport</code> calls the method
 * <code>ReportGenerator.generateReport</code> and passes the
 * <code>Report</code>-object, it belongs to, as the parameter.
 * The <code>ReportGenerator</code> can read the fields of the
 * <code>Report</code>-object via the getters to find out the requested settings
 * for report generation.</li>
 * 
 * <li><code>ReportGenerator.generateReport</code> reads the template and
 * generates the report if it supports the required version.</li>
 * 
 * </ol>
 * 
 * @version $Id: package-info.java 1 2007-12-12 17:37:26Z t-scheller $
 */

package org.codecover.report;