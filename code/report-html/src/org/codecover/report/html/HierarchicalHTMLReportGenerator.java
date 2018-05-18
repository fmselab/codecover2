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

import java.io.*;
import java.text.*;
import java.util.*;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.exception.MethodInvocationException;
import org.apache.velocity.exception.ParseErrorException;
import org.codecover.metrics.Metric;
import org.codecover.metrics.MetricProvider;
import org.codecover.metrics.coverage.CoverageMetric;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.HierarchyLevelType;
import org.codecover.model.utils.Base64;
import org.codecover.model.utils.ProgressHandler;
import org.codecover.report.FileCreationHandler;
import org.codecover.report.Report;
import org.codecover.report.ReportGenerator;
import org.codecover.report.html.VelocityHelper;
import org.codecover.report.exceptions.FileCreationException;
import org.codecover.report.exceptions.ReportIOException;
import org.codecover.report.exceptions.ReportTemplateApplyException;
import org.codecover.report.exceptions.TemplateIncompatibleException;
import org.codecover.report.exceptions.TemplateParseException;
import org.codecover.report.html.HTMLCodeHighlighting;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;


/**
 * Generates a multi-file report in hierarchical HTML format.
 * 
 * @author Robert Hanussek, Johannes Langauf, Michael Starzmann
 * @version 1.0 ($Id: HierarchicalHTMLReportGenerator.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
//XXX don't refactor now, large parts will be put in a new father-class this one
//and SingleFileHTMLReportGenereator (and upcoming velocity reports) are childs of
public class HierarchicalHTMLReportGenerator implements ReportGenerator { 
    
    private Report report = null;

    private String titlePageTemplate = null;

    private String selectionPageTemplate = null;

    private Template selectionPageTemplateCache = null;

    private String codePageTemplate = null;

    private Template codePageTemplateCache = null;
    
    private int numberOfHierarchyLevels;
    private int currentHierarchyLevel;
    
    // gives a unique name to each HierarchyLevel, that can be used in a filename
    private HashMap<HierarchyLevel, String> uniqueNames = new HashMap<HierarchyLevel, String>();
    
    private List<NoPerHierarchyLevelType> noPerHLType =
                                          new Vector<NoPerHierarchyLevelType>();

    
    public class NoPerHierarchyLevelType {
        private int number;
        private HierarchyLevelType hierarchyLevelType;
        
        public NoPerHierarchyLevelType(HierarchyLevelType hlt, int number) {
            this.hierarchyLevelType = hlt;
            this.number = number;
        }
        
        /**
         * @return the hierarchyLevelType
         */
        public HierarchyLevelType getHierarchyLevelType() {
            return hierarchyLevelType;
        }
        /**
         * @param hierarchyLevelType the hierarchyLevelType to set
         */
        public void setHierarchyLevelType(HierarchyLevelType hierarchyLevelType) {
            this.hierarchyLevelType = hierarchyLevelType;
        }
        /**
         * @return the number
         */
        public int getNumber() {
            return number;
        }
        /**
         * @param number the number to set
         */
        public void setNumber(int number) {
            this.number = number;
        }
        
        public void incNumber() {
            number++;
        }
        
    }
    
    //TODO: Why is this called "templateEncoding"? should it be called "outputEncoding"?
    /**
     * Encoding for files generated from Velocity templates. 
     */
    private String templateEncoding = "utf-8";

    /**
     * language of given template as a two-letter code according to ISO-639
     */
    private String language = null;
    
    /*
     * constants
     */

    /**
     * The version of the report template this class is able to read.
     */
    private static final int READER_VERSION = 1;


    /*
     * getters
     */    
    
    //TODO: shouldn't this be "text/html; charset=UTF-8"? (same for the single file report generator)
    /**
     * Gets the content type of the hierarchical HTML report generator which is
     * &quot;text/html&quot;.
     */
    public String getContentType() {
        return "text/html";
    }
    
    /*
     * public methods
     */
    
    /**
     * This method generates the hierarchical HTML report based on the settings
     * saved in the given <code>Report</code>.
     * 
     * @param report
     *            the <code>Report</code>-object which contains the settings
     *            to generate the hierarchical HTML report
     * @throws TemplateIncompatibleException
     * if the given Template has an incompatible version in it's template-tag
     * @throws ReportIOException
     * if an IO error happens during report generation
     * @throws FileCreationException
     * if a file cannot be created during report generation
     * @throws TemplateParseException
     * if the template has syntactical errors
     * @throws ReportTemplateApplyException 
     * if something goes wrong while applying the template (no IOError here)
     */
    public void generateReport(Report report)
            throws TemplateIncompatibleException, ReportIOException,
            FileCreationException, TemplateParseException, ReportTemplateApplyException {
        this.report = report;
        
        //check cardinalities and more
        checkReport();
        
        //TODO: catch earlier and include info which template caused exceptions
        try {
            generatePages();
        } catch (ParseErrorException e) {
            TemplateParseException tpe = new TemplateParseException("Parse error in Velocity template", e);
            throw tpe;
        } catch (MethodInvocationException e) {
            //XXX: keep this for fun?
            /* In the finished product off course we never expect a broken
             *  model. But for now don't blindly blame the template. The
             *  exception probably isn't Micha's fault. */
            
            final StringWriter sw = new StringWriter();
            final PrintWriter out = new PrintWriter(sw);
            e.getWrappedThrowable().printStackTrace(out);
            final String stackTrace = new String(sw.getBuffer());
            
            String msg = "The method '" + e.getMethodName() + "' called via"
                         + " the VTL reference '" + e.getReferenceName()
                         + "' threw an exception (this might indicate an"
                         + " error in the template): " + stackTrace;
            /*Rewrap the exception to see the trace of the failed method.*/
            ReportTemplateApplyException rtae =
                new ReportTemplateApplyException(msg, e.getWrappedThrowable());
            throw rtae;
        } catch (IOException e) {
            ReportIOException ioe = new ReportIOException("IO error applying a" +
                    " template", e);
            throw ioe;
        } catch (FileCreationException e) {
            throw e;
        }
    }

    /*
     * internal methods
     */

    /**
     * Check all required cardinalities of the given <code>Report</code> and
     *  do other sanety checks with it.
     * @throws TemplateIncompatibleException 
     */
    private void checkReport() throws TemplateIncompatibleException {
        if (report == null) {
            throw new IllegalArgumentException("no report given");
        }
        //testcase missing
        if (report.getTestCases() == null || report.getTestCases().size() <= 0) {
            throw new IllegalArgumentException("no testcase given in report");
        }
        //template missing
        if (report.getTemplate() == null
                || report.getTemplate().getTemplate() == null) {
            throw new IllegalArgumentException(
                    "no <template>-Element given in report");
        }
        if (report.getCreationHandler() == null) {
            throw new IllegalArgumentException("no FileCreationHandler in report");
        }
        if (report.getIndexOutputStream() == null) {
            throw new IllegalArgumentException("no output stream in report");
        }
        if (report.getDirectoryName() == null) {
            throw new IllegalArgumentException("no directory name in report");
        }
        if (report.getIndexFilename() == null) {
            throw new IllegalArgumentException("no index file name in report");
        }
        if (report.getProgressHandler() == null) {
            throw new IllegalArgumentException("no progress handler in report");
        }
        
        //verify template version
        int version = Integer.parseInt(report.getTemplate().getTemplateVersion());
        if (version < 1 || READER_VERSION < version) {
            throw new TemplateIncompatibleException("Template version "
                    + version + "is not compatible with this ReportGenerator"
                    + " version " + READER_VERSION + ".",
                    TemplateIncompatibleException.VersionField.OUTER);
        }
    }

    /**
     * Extract velocity templates from the report specific part of the
     *  XML-template.
     * 
     * @param templateNode
     *                  the Node containing the &lt;template&gt;-Element
     */
    private void extractVelocityTemplates(Node templateNode) {
        NodeList nodes = templateNode.getChildNodes();
        
        for (int i = 0; i < nodes.getLength(); i++) {
            if (nodes.item(i).getNodeName().equals("title-page")) {
                titlePageTemplate = nodes.item(i).getTextContent();
            } else if (nodes.item(i).getNodeName().equals("selection-page")) {
                selectionPageTemplate = nodes.item(i).getTextContent();
            } else if (nodes.item(i).getNodeName().equals("code-page")) {
                codePageTemplate = nodes.item(i).getTextContent();
            } else if (nodes.item(i).getNodeName().equals("language")) {
                language = nodes.item(i).getTextContent();
            }
        }
    }
  
    
    private void makeChildrenNamesUnique(HierarchyLevel top) {
        HashSet<String> usedNames = new HashSet<String>();
        String name = null;
        for (HierarchyLevel level : top.getChildren()) {
            name = level.getName().toLowerCase();
            name = replaceSpecialChars(name, '_');
            if (usedNames.contains(name)) {
                name = name + "_";
                int i = 0;
                while (usedNames.contains(name + Integer.toString(i))) {
                    i++;
                }
                name = name + Integer.toString(i);
            }
            usedNames.add(name);
            uniqueNames.put(level, name);
            makeChildrenNamesUnique(level);
        }
    }

    /**
     * Get a modified version of a String without special characters. 
     * 
     * @param s
     * the string to remove special characters from
     * @param filler
     * the character to replace special characters
     * @return
     * s with special characters replaced by filler
     */
    private static String replaceSpecialChars(String s, char filler) {
        char buffer[] = s.toCharArray();
        
        for (int i = 0; i < buffer.length; i++) {
            if ((buffer[i] < 'a' || buffer[i] > 'z') &&
                    (buffer[i] < 'A' || buffer[i] > 'Z') &&
                    (buffer[i] < '0' || buffer[i] > '9')) {
                buffer[i] = filler; //replace special character
            }
        }
        
        return new String(buffer);
    }
    
    
    
    //TODO we still need localizable Strings of objects in the model and metrics: i.E. "Termüberdeckung", don't we?
    private void generatePages() throws  ParseErrorException,
                                         MethodInvocationException,
                                         IOException,
                                         FileCreationException
    {
        this.report.getLogger().debug("Generating pages of" +
                " HierarchicalHTMLReport...");
        
        final ProgressHandler progressHandler = report.getProgressHandler();
        
        progressHandler.setProgress(0.05f);
        
        VelocityHelper vh = new VelocityHelper();
        
        VelocityContext context = new VelocityContext();
        
        HierarchyLevel topmostLevel = report.getTestCases().get(0)
                .getTestSession().getTestSessionContainer().getCode();
        
        makeChildrenNamesUnique(topmostLevel);
        
        Node templateNode = report.getTemplate().getTemplate();
        
        numberOfHierarchyLevels = 0;
        topmostLevel.accept(new HierarchyLevel.Visitor() {
                public void visit(HierarchyLevel hierarchyLevel) {
                    numberOfHierarchyLevels++;
                    NoPerHierarchyLevelType nphlt = null;
                    int i = 0;
                    while (   i < noPerHLType.size()
                           && nphlt == null) {
                        if (noPerHLType.get(i).getHierarchyLevelType() == hierarchyLevel.getType()) {
                            nphlt = noPerHLType.get(i);
                        }
                        i++;
                    }
                    if (nphlt == null) {
                        noPerHLType.add(new NoPerHierarchyLevelType(hierarchyLevel.getType(), 1));
                    } else {
                        nphlt.incNumber();
                    }
                }
            }, null, null, null, null, null, null, null, null);
        
        //parse the template DOM
        extractVelocityTemplates(templateNode);
        
        context.put("dateFormatter", DateFormat.getDateTimeInstance(
                DateFormat.LONG, DateFormat.LONG, new Locale(this.language)));
        context.put("now", new Date());
        context.put("session", report.getTestCases().get(0).getTestSession());
        context.put("topmostHierarchyLevel", topmostLevel);
        context.put("nameOfFirstFile", report.getIndexFilename());
        context.put("currentLevel", topmostLevel);
        context.put("hierarchyLevelTypeCounters", noPerHLType);
        context.put("sorter", new Sorter());
         List<CoverageMetric> coverageMetrics = new Vector<CoverageMetric>();
        List<Metric> simpleMetrics = new Vector<Metric>();
        for(Metric metric : MetricProvider.getAvailabeMetrics(report.getMetrics(),
                                                              report
                                                              .getTestCases()
                                                              .get(0)
                                                              .getTestSession()
                                                              .getTestSessionContainer()
                                                              .getCriteria())) {
            if (metric instanceof CoverageMetric) {
                coverageMetrics.add((CoverageMetric)metric);
            } else {
                simpleMetrics.add(metric);
            }
        }
        context.put("coverageMetrics", coverageMetrics);
        context.put("simpleMetrics", simpleMetrics);
        context.put("testcases", report.getTestCases());
        context.put("outputDirectory", report.getDirectoryName());
        context.put("currentDepth", 0);
        
        context.put("html", new HTMLutils());
        context.put("filename", uniqueNames);

        progressHandler.setProgress(0.10f);
        
        OutputStreamWriter osw = null;
        
        //generate all additional text-files and resources defined in the template
        NodeList nodes = templateNode.getChildNodes();
        for (int i = 0; i < nodes.getLength(); i++) {
            progressHandler.setProgress(0.10f + (i * 1.0f / nodes.getLength()) * 0.10f);
            if (nodes.item(i).getNodeName().equals("text-file")) {
                
                //run velocity to generate a text file from the template
                //in the <text-file>-Tag
                Node textFile = nodes.item(i);
                NamedNodeMap atts = textFile.getAttributes();
                osw = new OutputStreamWriter(
                    report.getCreationHandler().createFile(
                        atts.getNamedItem("filename").getNodeValue(), 
                        atts.getNamedItem("content-type").getNodeValue()),
                    templateEncoding);
                vh.evaluate(context, osw, "hhr_text-file",
                        textFile.getTextContent(), null);
                osw.close();
            } else if (nodes.item(i).getNodeName().equals("resource")) {
            
                //extract the binary file given as Base64 in the <resource>-tag
                Node resource = nodes.item(i);
                NamedNodeMap atts = resource.getAttributes();
                java.io.OutputStream os;
                os = report.getCreationHandler().createFile(
                        atts.getNamedItem("filename").getNodeValue(), 
                           "application/octet-stream");
                os = new Base64.OutputStream(os,Base64.DECODE);
                os.write(resource.getTextContent().getBytes());
            }
        }
        
        progressHandler.setProgress(0.20f);
        
        vh = new VelocityHelper();
        osw = new OutputStreamWriter(report.getIndexOutputStream(),
                                     templateEncoding);
        vh.evaluate(context, osw, "hhr_titlePage", titlePageTemplate, null);
        osw.close();
 
        currentHierarchyLevel = 0;

        progressHandler.setProgress(0.25f);
        
        for (HierarchyLevel level : topmostLevel.getChildren()) {
            generateReportPage(context,
                               level,
                               "." + FileCreationHandler.SEPARATOR + uniqueNames.get(level),
                               new ArrayList<HierarchyLevel>());
        }
        progressHandler.setProgress(0.95f);
        this.report.getLogger().debug("Finished HierarchicalHTMLReport.");
    }

    
    private void generateReportPage (VelocityContext fathersContext,
                                     HierarchyLevel currentLevel,
                                     String path,
                                     List<HierarchyLevel> ancestors) 
        throws  ParseErrorException,
                MethodInvocationException,
                IOException,
                FileCreationException
    {
        currentHierarchyLevel++;
        report.getProgressHandler().setProgress(0.25f + ((currentHierarchyLevel - 1) * 1.0f / numberOfHierarchyLevels) * 0.70f);
        
        
        VelocityHelper vh = new VelocityHelper();
        VelocityContext context = new VelocityContext(fathersContext);
        
        context.put("currentLevel", currentLevel);
        context.put("ancestors", ancestors);
        
        OutputStreamWriter osw = new OutputStreamWriter(
                report.getCreationHandler().createFile(
                        path
                        + FileCreationHandler.SEPARATOR
                        + "index.html",
                        getContentType()),
                templateEncoding);
        if (currentLevel.getType().getInternalName().equals("method")) {
            HTMLCodeHighlighting codehighlight = new HTMLCodeHighlighting(
                    this.report.getLogger());
            context.put("code", codehighlight);
            codePageTemplateCache = vh.evaluate(context, osw,
                    "hhr_codePage", codePageTemplate, codePageTemplateCache);
        } else {
            selectionPageTemplateCache = vh.evaluate(context, osw,
                    "hhr_selectionPage", selectionPageTemplate,
                    selectionPageTemplateCache);
        }

        osw.close();
        ancestors.add(currentLevel);
        for (HierarchyLevel level : currentLevel.getChildren()) {
            generateReportPage(context, level, path + FileCreationHandler.SEPARATOR + uniqueNames.get(level),
                    new ArrayList<HierarchyLevel>(ancestors));
        }
    }
}
