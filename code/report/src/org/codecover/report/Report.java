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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.*;

import org.codecover.model.*;
import org.codecover.metrics.*;
import org.codecover.model.utils.*;
import org.codecover.model.extensions.*;
import org.codecover.report.exceptions.*;

/**
 * Holds the settings for report generation and initializes report generation.
 * After setting the options (e.g., just call
 * {@link #setFileSystemPath(String)}), the method {@link #generateReport()} can
 * be called which dispatches report generation to the required
 * <code>ReportGenerator</code>.
 *
 * @author Robert Hanussek
 * @version 1.0 ($Id: Report.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class Report {

    /**
     * The suffix which is appended to the the index filename to construct the
     * default name of the directory auxiliary files are written in.
     * 
     * @see #getDirectoryName()
     */
    public static final String OUTPUT_DIR_SUFFIX = "-files";

    /**
     * The <code>OutputStream</code> where either the index file of a
     * multiple-files report is written to or the file of the single-file-report
     * is written to.
     */
    private OutputStream indexOutputStream;

    /**
     * The name of the index file or the file of the single-file-report as a
     * <code>String</code>.
     */
    private String indexFilename;

    /**
     * The name of the output directory as a <code>String</code> where
     * auxiliary files are written to in case of a multiple-files-report. In
     * case of a single-file-report this field is ignored. If this field is not
     * set it defaults to <code>indexFilename</code> plus the suffix contained
     * in <code>OUTPUT_DIR_SUFFIX</code>.
     */
    private String directoryName;

    /**
     * A <code>FileCreationHandler</code> which is used to create auxiliary
     * files and thus is only needed for multiple-files-reports and is ignored
     * in case of single-file-reports.
     */
    private FileCreationHandler creationHandler;

    /**
     * A <code>ProgressHandler</code> which is periodically informed about the
     * progress of report generation.
     */
    private ProgressHandler progressHandler;

    /**
     * The template can either be set as a <code>String</code> which points to
     * the template in the filesystem or as a <code>org.w3c.dom.Document</code>
     * which allows template generation on the fly without writing it on disk.
     * Internally the template is saved as a <code>Template</code> object.
     */
    private Template template;

    /**
     * The content of the report as a List of <code>TestCase</code>s.
     */
    private List<TestCase> testCases;

    /**
     * The logger, mustn't be <code>null</code>, set to <code>Logger.NULL</code>
     * if you don't need a logger.
     */
    private Logger logger;

    private Set<Metric> metrics;

    private PluginManager pluginManager;

    /**
     * Generates an object of this class which holds the settings for report
     * generation and initializes report generation.
     * 
     * @param pluginManager the <code>PluginManager</code> used for loading the
     *                      <code>ReportGenerator</code>
     * 
     * @param metrics       the available <code>Metric</code>s
     * 
     * @param logger        the logger, mustn't be <code>null</code>, set to
     *                      <code>Logger.NULL</code> if you don't need logging
     * 
     * @throws NullPointerException
     * if a parameter is <code>null</code>
     */
    public Report(  PluginManager pluginManager,
                    Set<Metric> metrics,
                    Logger logger) {
        this.setLogger(logger);
        this.indexOutputStream = null;
        this.indexFilename = null;
        this.directoryName = null;
        this.creationHandler = null;
        this.progressHandler = ProgressHandler.NULL;
        this.template = new Template(this.getLogger());
        this.testCases = null;
        this.setPluginManager(pluginManager);
        this.setMetrics(metrics);
    }

    /**
     * Sets the <code>OutputStream</code> where either the index file of a
     * multiple-files report is written to or the file of the single-file-report
     * is written to.
     * 
     * @param indexOutputStream the <code>OutputStream</code> where either the
     *                          index file of a multiple-files report is written
     *                          to or the file of the single-file-report is
     *                          written to
     */
    public void setIndexOutputStream(OutputStream indexOutputStream) {
        this.indexOutputStream = indexOutputStream;
    }

    /**
     * Sets the name of the index file or the file of the single-file-report as
     * a <code>String</code>.
     * 
     * @param indexFilename the name of the index file or the file of the
     *                      single-file-report
     */
    public void setIndexFilename(String indexFilename) {
        this.indexFilename = indexFilename;
    }

    /**
     * Sets the name of the output directory as a <code>String</code> where
     * auxiliary files are written to in case of a multiple-files-report. In
     * case of a single-file-report this value is ignored. If this value is not
     * set it defaults to the value set with the method
     * {@link #setIndexFilename(String)} plus the suffix contained in
     * {@link #OUTPUT_DIR_SUFFIX}.
     * 
     * @param directoryName the name of the output directory where auxiliary
     *                      files are written to
     */
    public void setDirectoryName(String directoryName) {
        this.directoryName = directoryName;
    }

    /**
     * Sets the <code>FileCreationHandler</code> which is used to create
     * auxiliary files and thus is only needed for multiple-files-reports and is
     * ignored in case of single-file-reports.
     * <p>
     * The <code>FileCreationHandler</code> should create the auxiliary files in
     * a subdirectory of the directory which contains the index file. This
     * subdirectory should go by the name of the value returned by
     * {@link #getDirectoryName()}.
     * 
     * @param creationHandler   the <code>FileCreationHandler</code> which is
     *                          used to create auxiliary files
     */
    public void setCreationHandler(FileCreationHandler creationHandler) {
        this.creationHandler = creationHandler;
    }

    /**
     * Sets the <code>ProgressHandler</code> which is periodically informed
     * about the progress of report generation.
     * 
     * @param progressHandler   the <code>ProgressHandler</code> which is
     *                          periodically informed about the progress of
     *                          report generation
     */
    public void setProgressHandler(ProgressHandler progressHandler) {
        this.progressHandler = progressHandler;
    }

    /**
     * Sets the template which is represented by a DOM tree.
     *
     * @param template  the template represented by the root node of a DOM tree
     */
    public void setTemplate(org.w3c.dom.Document template) {
        this.template.setDocument(template);
    }

    /**
     * Sets the template.
     *
     * @param filepath  the path to the template file
     */
    public void setTemplate(String filepath) {
        this.template.setFile(new File(filepath));
    }

    /**
     * Sets the template.
     *
     * @param file  the template file
     */
    public void setTemplate(File file) {
        this.template.setFile(file);
    }

    /**
     * Sets the content of the report as a List of <code>TestCase</code>s.
     * 
     * @param testCases the test cases which form the content of this report
     */
    public void setTestCases(List<TestCase> testCases) {
        this.testCases = testCases;
    }

    /**
     * This method is provided for convenience and configures a Report to write
     * the report to the given path in the file system. It sets the following
     * fields of a Report:
     * <ul>
     * <li><code>indexFilename</code></li>
     * <li><code>indexOutputStream</code></li>
     * <li><code>directoryName</code> (implicitly set)</li>
     * <li><code>creationHandler</code></li>
     * </ul>
     * <p>
     * The parameter of this method is the path the index file of a
     * multiple-files-report or the file of a single-file-report is written to
     * and is referenced as the <em>given path</em> in the documentation of
     * this method.
     * <p>
     * This method sets <code>indexFilename</code> to the basename of the
     * given path. <code>indexOutputStream</code> is set to write to the given
     * path. <code>directoryName</code> is not set explicitly here but it
     * defaults to &quot;<code>indexFilename</code>-files&quot; (see
     * {@link #directoryName}). The output directory is a directory which name
     * is the value of <code>directoryName</code> and which resides in the
     * dirname of the given path. Thus creationHandler is set to a
     * {@link DefaultFileCreationHandler} with the path to the output directory
     * as the parameter of its constructor.
     *
     * @param filepath  the path the index file of a multiple-files-report or
     *                  the file of a single-file-report is written to
     *
     * @throws FileCreationException
     * if the index file or the output directory could not be created
     */
    public void setFileSystemPath(String filepath) throws FileCreationException{
        setFileSystemPath(new File(filepath));
    }


    /**
     * This method is provided for convenience and configures a Report to write
     * the report to the given path in the file system. It sets the following
     * fields of a Report:
     * <ul>
     * <li><code>indexFilename</code></li>
     * <li><code>indexOutputStream</code></li>
     * <li><code>directoryName</code> (implicitly set)</li>
     * <li><code>creationHandler</code></li>
     * </ul>
     * <p>
     * The parameter of this method is the path the index file of a
     * multiple-files-report or the file of a single-file-report is written to
     * and is referenced as the <em>given path</em> in the documentation of
     * this method.
     * <p>
     * This method sets <code>indexFilename</code> to the basename of the
     * given path. <code>indexOutputStream</code> is set to write to the given
     * path. <code>directoryName</code> is not set explicitly here but it
     * defaults to &quot;<code>indexFilename</code>-files&quot; (see
     * {@link #directoryName}). The output directory is a directory which name
     * is the value of <code>directoryName</code> and which resides in the
     * dirname of the given path. Thus creationHandler is set to a
     * {@link DefaultFileCreationHandler} with the path to the output directory
     * as the parameter of its constructor.
     *
     * @param indexFile the index file of a multiple-files-report, or the file
     *                  the single-file-report is written to
     *
     * @throws FileCreationException
     * if the index file or the output directory could not be created
     */
    public void setFileSystemPath(File indexFile) throws FileCreationException{
        File outdir;
        DefaultFileCreationHandler defFileCreatHandler;

        /* Set index filename and create output stream to index file.
         * index file = basename (indexFile)
         */
        setIndexFilename(indexFile.getName());
        try {
            setIndexOutputStream(new FileOutputStream(indexFile));
        } catch (FileNotFoundException e) {
            FileCreationException fce = new FileCreationException(
                    "Index file could not be created.", e);
            this.logger.error(fce.getMessage(),fce);
            throw fce;
        }
        // Create output directory and set the FileCreationHandler accordingly.
        outdir = new File(indexFile.getParent(), this.getDirectoryName());
        defFileCreatHandler =
                new DefaultFileCreationHandler(outdir.getAbsolutePath(), true,
                        this.logger);
        defFileCreatHandler.setLogger(this.logger);
        setCreationHandler(defFileCreatHandler);
    }

    /**
     * Sets the <code>PluginManager</code> used for loading the
     * {@code ReportGenerator}.
     *
     * @param pluginManager the <code>PluginManager</code> used for loading the
     *                      {@code ReportGenerator}
     *
     * @throws NullPointerException
     * if {@code pluginManager} is <code>null</code>
     */
    public void setPluginManager(PluginManager pluginManager) {
        if(pluginManager == null) {
            throw new NullPointerException("pluginManager == null");
        }
        this.pluginManager = pluginManager;
    }

    /**
     * Sets the available <code>Metric</code>s.
     *
     * @param metrics   the available <code>Metric</code>s
     *
     * @throws NullPointerException
     * if metrics is <code>null</code>
     */
    public void setMetrics(Set<Metric> metrics) {
        if(metrics == null) {
            throw new NullPointerException("metrics == null");
        }
        this.metrics = metrics;
    }

    /**
     * Sets the logger, mustn't be <code>null</code>, set to
     * <code>Logger.NULL</code> if you don't need logging.
     *
     * @param logger    the logger
     *
     * @throws NullPointerException
     * if logger is <code>null</code>
     */
    public void setLogger(Logger logger) {
        if(logger == null) {
            throw new NullPointerException("Logger mustn't be null.");
        }
        this.logger = logger;
    }

    /**
     * Returns the <code>OutputStream</code> where either the index file of a
     * multiple-files report is written to or the file of the single-file-report
     * is written to.
     * 
     * @return  the <code>OutputStream</code> where either the index file of a
     *          multiple-files report is written to or the file of the
     *          single-file-report is written to
     */
    public OutputStream getIndexOutputStream() {
        return this.indexOutputStream;
    }

    /**
     * Returns the name of the index file or the file of the single-file-report.
     * 
     * @return  the name of the index file or the file of the
     *          single-file-report.
     */
    public String getIndexFilename() {
        return this.indexFilename;
    }

    /**
     * Returns the name of the output directory where auxiliary files are
     * written to in case of a multiple-files-report. If this value was not
     * set explicitly it defaults to the value set with the method
     * {@link #setIndexFilename(String)} plus the suffix contained in
     * {@link #OUTPUT_DIR_SUFFIX}.
     * 
     * @return  the name of the output directory where auxiliary files are
     *          written to
     */
    public String getDirectoryName() {
        if(this.directoryName != null) {
            return this.directoryName;
        } else if(this.indexFilename != null) {
            return this.indexFilename + OUTPUT_DIR_SUFFIX;
        } else {
            return null;
        }
    }

    /**
     * Returns the <code>FileCreationHandler</code> which is used to create
     * auxiliary files and thus is only needed for multiple-files-reports.
     * <p>
     * When implementing a report generator you can assume that the
     * <code>FileCreationHandler</code> returned by this method creates files in
     * a subdirectory of the directory which contains the index file. Moreover
     * you can assume that this subdirectory goes by the name of the value
     * returned by {@link #getDirectoryName()}. This means that you can
     * link from the index file to an auxiliary file like this:
     * &quot;outputdirname/auxfilename&quot;, whereas &quot;outputdirname&quot;
     * is the value returned by {@link #getDirectoryName()} and
     * &quot;auxfilename&quot; is the name of the auxiliary file you want to
     * link to (and you created the auxiliary file via the
     * <code>FileCreationHandler</code> returned by this method).
     * 
     * @return  the <code>FileCreationHandler</code> which is used to create
     *          auxiliary files
     */
    public FileCreationHandler getCreationHandler() {
        return this.creationHandler;
    }

    /**
     * Returns the <code>ProgressHandler</code> which is periodically informed
     * about the progress of report generation.
     * 
     * @return  the <code>ProgressHandler</code> which is periodically informed
     *          about the progress of report generation
     */
    public ProgressHandler getProgressHandler() {
        return this.progressHandler;
    }

    /**
     * Returns the template.
     * 
     * @return  the template
     */
    public Template getTemplate() {
        return this.template;
    }

    /**
     * Returns the <code>List<code> of <code>TestCase</code>s which form the
     * content of the report.
     * 
     * @return  the <code>List<code> of <code>TestCase</code>s which form the
     *          content of the report
     */
    public List<TestCase> getTestCases() {
        return this.testCases;
    }

    /**
     * Returns the <code>Metric</code>s available for this report.
     * 
     * @return  the <code>Metric</code>s available for this report
     */
    public Set<Metric> getMetrics() {
        return metrics;
    }

    /**
     * Returns the logger or <code>null</code> if it wasn't set prior to the
     * call of this method.
     * 
     * @return  the logger or <code>null</code> if it wasn't set
     */
    public Logger getLogger() {
        return this.logger;
    }

    /**
     * This method dispatches the report generation to the
     * <code>ReportGenerator</code> the template requires. This works as
     * follows: First this method loads the <code>ReportGenerator</code> using
     * the previously set <code>PluginManager</code>. Then this method calls the
     * method <code>generateReport</code> of the <code>ReportGenerator</code>
     * and passes this <code>Report</code>-object as the parameter.
     *
     * @throws ReportException
     * if any error in generating the report happened
     * @throws TemplateException
     * if the template could not be read correctly
     * @throws LoadReportGeneratorException
     * if the plugin extension of the report generator the template requires
     * couldn't be loaded
     */
    public void generateReport() throws TemplateException,
                                        LoadReportGeneratorException,
                                        ReportException
    {
        // read template into Template object
        this.logger.info("Reading template...");
        this.template.setLogger(this.logger);
        this.template.read();   // may throw TemplateException
        this.logger.info("Done reading template.");

        // load class of the report generator the template requires
        this.logger.info("Loading report generator...");

        final String extensionName = template.getReportGeneratorName();
        final String pluginName = template.getPluginName();
        final ReportGenerator reportGenerator;
        try {
            reportGenerator = PluginUtils.getExtensionObjectByName(
                    pluginManager, logger, ReportGenerator.class, pluginName,
                    extensionName);
        } catch (FatalException fatal) {
            //some failure loading the plugin
            throw new LoadReportGeneratorException(fatal);
        }
        this.logger.info("Done loading report generator.");

        // generate the report
        this.logger.info("Running report generator...");
        reportGenerator.generateReport(this); // may throw ReportException
        this.logger.info("Done running report generator.");
    }

}
