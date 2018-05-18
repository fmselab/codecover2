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

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.text.DateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Vector;

import org.apache.velocity.VelocityContext;
import org.apache.velocity.exception.MethodInvocationException;
import org.apache.velocity.exception.ParseErrorException;
import org.codecover.metrics.Metric;
import org.codecover.metrics.MetricProvider;
import org.codecover.metrics.coverage.CoverageMetric;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.HierarchyLevelType;
import org.codecover.model.utils.ProgressHandler;
import org.codecover.report.Report;
import org.codecover.report.ReportGenerator;
import org.codecover.report.exceptions.FileCreationException;
import org.codecover.report.exceptions.ReportIOException;
import org.codecover.report.exceptions.ReportTemplateApplyException;
import org.codecover.report.exceptions.TemplateIncompatibleException;
import org.codecover.report.exceptions.TemplateParseException;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;


/**
 * Generates a single-file report in hierarchical HTML format.
 * 
 * @author Michael Starzmann
 * @version 1.0 ($Id: SingleFileHTMLReportGenerator.java 69 2010-01-27 19:31:18Z schmidberger $)
 */

//XXX don't refactor now, large parts will be put in a new father-class this one
//and HierarchicalHTMLReport (and upcoming velocity reports) are childs of

public class SingleFileHTMLReportGenerator implements ReportGenerator { 
    
    private Report report = null;

    private String htmlTemplate = null;
    
    private int numberOfHierarchyLevels;
    
    // gives a unique name to each HierarchyLevel, that can be used as anchor
    private HashMap<HierarchyLevel, String> uniqueNames = new HashMap<HierarchyLevel, String>();
    
    private HashSet<String> usedNames = new HashSet<String>();
    
    private List<NoPerHierarchyLevelType> noPerHLType =
                                          new Vector<NoPerHierarchyLevelType>();

    private HashMap<HierarchyLevel,Integer> depth = new HashMap<HierarchyLevel, Integer>();
    
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
     * This method generates the single-file HTML report based on the settings
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
               FileCreationException, TemplateParseException,
               ReportTemplateApplyException
    {
        this.report = report;
        
        //check cardinalities and more
        checkReport();
        
        //TODO: catch earlier and include info which template caused exceptions
        try {
            generateHTMLPage();
        } catch (ParseErrorException e) {
            TemplateParseException tpe = new TemplateParseException("Parse error in Velocity template", e);
            throw tpe;
        } catch (MethodInvocationException e) {
            /* In the finished product off course we never expect a broken
             *  model. But for now don't blindly blame the template. The
             *  exception probably isn't Micha's fault. */
            String msg = "The method '" + e.getMethodName() + "' called via"
                         + " the VTL reference '" + e.getReferenceName()
                         + "' threw an exception.";
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
    
    private void makeChildrenNamesUnique(HierarchyLevel top, boolean global) {
        if (!global) {
            usedNames.clear();
        }
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
            makeChildrenNamesUnique(level, global);
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
    
    public class Repeater {
        public String repeat(String s, int c) {
            String result = "";
            for (int i = 0; i < c; i++) {
                result += s;
            }
            return result;
        }
    }
    
    public class Checker {
        public boolean hasAnchor(HierarchyLevel level) {
            return (level.getLocation() != null && level.getLocation().getLocations().size() > 0);
        }
    }
    
    private void generateHTMLPage() throws  ParseErrorException,
                                            MethodInvocationException,
                                            IOException,
                                            FileCreationException
    {
        this.report.getLogger().debug("Generating page of" +
                " SingleFileHTMLReport...");
        
        final ProgressHandler progressHandler = report.getProgressHandler();
        
        progressHandler.setProgress(0.05f);
        
        VelocityHelper vh = new VelocityHelper();
        
        VelocityContext context = new VelocityContext();
        
        HierarchyLevel topmostLevel = report.getTestCases().get(0)
                .getTestSession().getTestSessionContainer().getCode();
        
        makeChildrenNamesUnique(topmostLevel, true);
        
        //parse the template DOM
        Node templateNode = report.getTemplate().getTemplate();
        NodeList nodes = templateNode.getChildNodes();
        for (int i = 0; i < nodes.getLength(); i++) {
            if (nodes.item(i).getNodeName().equals("html-page")) {
                htmlTemplate = nodes.item(i).getTextContent();
            } else if (nodes.item(i).getNodeName().equals("language")) {
                language = nodes.item(i).getTextContent();
            }
        }
                
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
        
        context.put("dateFormatter", DateFormat.getDateTimeInstance(
                DateFormat.LONG, DateFormat.LONG, new Locale(this.language)));
        context.put("now", new Date());
        context.put("String", new String());
        context.put("session", report.getTestCases().get(0).getTestSession());
        context.put("topmostHierarchyLevel", topmostLevel);
        context.put("hierarchyLevelTypeCounters", noPerHLType);
        context.put("sorter", new Sorter());
        context.put("checker", new Checker());
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
        context.put("html", new HTMLutils());
        context.put("anchornames", uniqueNames);
        List<HierarchyLevel> hierarchyLevels = new Vector<HierarchyLevel>();
        depth_first_search(topmostLevel, hierarchyLevels, new Integer(0));
        context.put("hierarchyLevels", hierarchyLevels);
        context.put("depth", depth);
        context.put("repeater", new Repeater());
        HTMLCodeHighlighting codehighlight = new HTMLCodeHighlighting(
                this.report.getLogger());
        context.put("code", codehighlight);
        context.put("currentHierarchyLevel", topmostLevel.getChildren().get(0));
        
        progressHandler.setProgress(0.10f);
        
        OutputStreamWriter osw = null;
        
        vh = new VelocityHelper();
        osw = new OutputStreamWriter(report.getIndexOutputStream(),
                                     templateEncoding);
        vh.evaluate(context, osw, "ofhr_HTMLPage", htmlTemplate, null);
        osw.close();
        //TODO refresh ProgressBar:
        //Template has to do something similar to
        //report.getProgressHandler().setProgress(0.25f + ((currentHierarchyLevel - 1) * 1.0f / numberOfHierarchyLevels) * 0.70f);
        progressHandler.setProgress(0.95f);
        this.report.getLogger().debug("Finished SingleFileHTMLReport.");
    }
    
    private void depth_first_search(HierarchyLevel level,
                                    List<HierarchyLevel> list,
                                    Integer depth) {
        list.add(level);
        this.depth.put(level, depth);
        for (HierarchyLevel child : level.getChildren()) {
            depth_first_search(child, list, new Integer(depth.intValue() + 1));
        }
    }
    
}
