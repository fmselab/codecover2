package org.codecover.report.csv;

import java.io.IOException;
import java.util.*;

import org.codecover.metrics.Metric;
import org.codecover.metrics.MetricProvider;
import org.codecover.metrics.coverage.CoverageMetric;
import org.codecover.metrics.coverage.CoverageResult;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.utils.ProgressHandler;
import org.codecover.report.Report;
import org.codecover.report.ReportGenerator;
import org.codecover.report.exceptions.ReportException;
import org.codecover.report.exceptions.ReportIOException;
import org.codecover.report.exceptions.TemplateIncompatibleException;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Generates a report in with comma seperated values (CSV).
 * Uses CSVWriter for the output. See there for information about the CSV-format
 * 
 * @author Michael Starzmann
 * @version 1.0 ($Id: CSVReportGenerator.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public class CSVReportGenerator implements ReportGenerator {

    /**
     * The version of the report template this class is able to read.
     */
    private static final int READER_VERSION = 1;
    
    private Report report = null;
    
    private List<CoverageMetric> coverageMetrics = new ArrayList<CoverageMetric>();
    
    private CSVWriter writer; 
    
    private NodeList template;
    
    private ProgressHandler progressHandler;
    
    private int totalLines = 0;
    
    // counts blocks of 1/32 of totalLines; after each block, the progress bar
    // is updated
    private int currentProgressStep = 0;
    
    private HierarchyLevel topLevel;
    
    /* (non-Javadoc)
     * @see org.codecover.report.ReportGenerator#generateReport(org.codecover.report.Report)
     */
    public void generateReport(Report report) throws ReportException, 
                                                             ReportIOException {
        this.report = report;
        checkReport();
        this.progressHandler = report.getProgressHandler();
        for(  Metric metric
            : MetricProvider.getAvailabeMetrics(report.getMetrics(),
                                                report.getTestCases()
                                                      .get(0)
                                                      .getTestSession()
                                                      .getTestSessionContainer()
                                                      .getCriteria())) {
            if (metric instanceof CoverageMetric) {
                coverageMetrics.add((CoverageMetric)metric);
            }
        }
        // If we have to metrics with the same name, they will be sorted
        // randomly (because we've got them from a HashSet). We don't care.
        Collections.sort(coverageMetrics, new Comparator<CoverageMetric>() {
                public int compare(CoverageMetric m1, CoverageMetric m2) {
                    return m1.getName().compareTo(m2.getName());
                }
            });
        template = report.getTemplate().getTemplate().getChildNodes();
        writer = new CSVWriter(report.getIndexOutputStream());
        topLevel = report.getTestCases().get(0).getTestSession().
        getTestSessionContainer().getCode();
        putOutHeader();
        putOutBody();
        this.progressHandler.setProgress(0.99f);
        try {
            writer.close();
        } catch (IOException e) {
            throw new ReportIOException(e);
        }
    }

    /* (non-Javadoc)
     * @see org.codecover.report.ReportGenerator#getContentType()
     */
    public String getContentType() {
        return "text/csv; charset=UTF-8,header=present";
    }
    
    /**
     * Check all required cardinalities of the given <code>Report</code> and
     *  do other sanity checks with it.
     *  
     * @throws TemplateIncompatibleException 
     */
    private void checkReport() throws TemplateIncompatibleException {
        if (report == null) {
            throw new IllegalArgumentException("no report given");
        }
        //testcase missing
        if (   report.getTestCases() == null
            || report.getTestCases().size() <= 0) {
            throw new IllegalArgumentException("no testcase given in report");
        }
        //template missing
        if (report.getTemplate() == null
                || report.getTemplate().getTemplate() == null) {
            throw new IllegalArgumentException(
                    "no <template>-Element given in report");
        }
        if (report.getIndexOutputStream() == null) {
            throw new IllegalArgumentException("no output stream in report");
        }
        if (report.getIndexFilename() == null) {
            throw new IllegalArgumentException("no index file name in report");
        }
        if (report.getProgressHandler() == null) {
            throw new IllegalArgumentException("no progress handler in report");
        }
        
        //verify template version
        int version = Integer.parseInt(report.getTemplate()
                                             .getTemplateVersion());
        if (version < 1 || READER_VERSION < version) {
            throw new TemplateIncompatibleException("Template version "
                    + version + "is not compatible with this ReportGenerator"
                    + " version " + READER_VERSION + ".",
                    TemplateIncompatibleException.VersionField.OUTER);
        }
    }

    /**
     * Puts out the headline of the CSV-File set in this.template
     */
    private void putOutHeader() {
        for (int i = 0; i < template.getLength(); i++) {
            if (template.item(i).getNodeName().equals("column")) {
                Node header = template.item(i).getAttributes()
                                              .getNamedItem("header");
                if (header != null) {
                    writer.print(header.getNodeValue());
                } else {
                    writer.print("");
                }
            } else if (template.item(i).getNodeName()
                                       .equals("columnsPerMetric")) {
                putOutHeaderPerMetric(template.item(i).getChildNodes());
            } else {
                // ignore, because it could be whitespaces = text instead of
                // an unknown tag which probably could deserve an exception 
            }
        }
        writer.newLine();
    }
    
    /**
     * Puts out the headers of the columns created for each CoverageMetric.
     * These columns are defined in nodes
     * 
     * @param nodes   the definition of the columns created per CoverageMetric
     */
    private void putOutHeaderPerMetric(NodeList nodes) {
        for (CoverageMetric cm : coverageMetrics) {
       
            for (int i = 0; i < nodes.getLength(); i++) {
                if (nodes.item(i).getNodeName().equals("column")) {
                    Node header = nodes.item(i).getAttributes()
                                               .getNamedItem("header");
                    if (header != null) {
                        writer.printf(header.getNodeValue(),
                                cm.getName()); // 1$ Name of CoverageMetric
                    } else {
                        writer.print("");
                    } 
                } else {
                    // ignore, because it could be whitespaces = text instead of
                    // an unknown tag which probably could deserve an exception 
                }
            }
        }
    }
    
    private int countHierarchyLevels(HierarchyLevel top) {
        class IntContainer {
            public int counter = 0;
        }
        final IntContainer i = new IntContainer();
        top.accept(new HierarchyLevel.Visitor() {
            public void visit(HierarchyLevel hierarchyLevel) {
                i.counter++;
            }
        }, null, null, null, null, null, null, null, null);
        return i.counter;
    }
    
    /**
     * puts out the coverage data per HierarchyLevel according to the columns
     * set in this.template
     */
    private void putOutBody () {
        class FloatContainer {
            public float counter = 0;
        }
        final FloatContainer i = new FloatContainer();
        final FloatContainer progress = new FloatContainer();
        progress.counter = 0.03f;
        this.totalLines = countHierarchyLevels(this.topLevel);
        this.progressHandler.setProgress(progress.counter);
        if (this.totalLines < 96) { //too less for good progressBar
            progress.counter = 0.99f - (this.totalLines / 100.0f);
            topLevel.accept(new HierarchyLevel.Visitor() {
                public void visit(HierarchyLevel hierarchyLevel) {
                    putOutOneLineOfBody(hierarchyLevel);
                    progress.counter = progress.counter + 0.01f;
                    progressHandler.setProgress(progress.counter);
                }
            }, null, null, null, null, null, null, null, null);
        } else {
          //now, 32x3% are left. currentProgressStep counts 1/32 of totalLines
            progress.counter = 0.03f;
            topLevel.accept(new HierarchyLevel.Visitor() {
                public void visit(HierarchyLevel hierarchyLevel) {
                    putOutOneLineOfBody(hierarchyLevel);
                    if (currentProgressStep > 0) {
                        currentProgressStep--;
                    } else {
                        currentProgressStep = totalLines >> 5; // = /32
                        progress.counter = progress.counter + 0.03f;
                        progressHandler.setProgress(progress.counter);
                    }
                }
            }, null, null, null, null, null, null, null, null);
        }
    }
    
    /**
     * puts out one line to the CSV file, containing the columns set in
     * this.template
     * 
     * @param level   the HierarchyLevel this line belongs to
     */
    private void putOutOneLineOfBody (HierarchyLevel level) {
        for (int i = 0; i < template.getLength(); i++) {
            if (template.item(i).getNodeName().equals("column")) {
                writer.printf(template.item(i).getTextContent(),
                              null, // %1$ = CoverageMetricName is not available
                              level.getHeaderString(),                        // 2$ name of method, class, ...
                              level.getType().getEnglishName(),               // 3$ type of 2$ (method, class, ...)
                              packageOf(level));                              // 4$ package 2$ is part of
            } else if (template.item(i).getNodeName()
                                       .equals("columnsPerMetric")) {
                putOutBodyLinePerMetric(template.item(i).getChildNodes(),
                                        level);
            } else {
                // ignore, because it could be whitespaces = text instead of
                // an unknown tag which probably could deserve an exception 
            }
        }
        writer.newLine();
    }
    
    private HashMap<HierarchyLevel, String> cache = new HashMap<HierarchyLevel, String>();
    
    private String packageOf (HierarchyLevel level) {
        if (cache.containsKey(level)) {
            return cache.get(level);
        } else {
            String result;
            if (level == topLevel) {
                result = "";
            } else {
                if (packageOf(topLevel.getParent(level)).length() < 1) {
                    result = level.getName();
                } else {
                    result = packageOf(topLevel.getParent(level)) + "." + level.getName();
                }
            }
            cache.put(level, result);
            return result;
        }
    }
    
    /**
     * Puts out the columns cerated for each CoverageMetric.
     * These columns are defined in nodes
     * 
     * @param nodes   the definition of the columns created per CoverageMetric
     * @param level   the HierarchyLevel this line belongs to
     */
    private void putOutBodyLinePerMetric (NodeList nodes, HierarchyLevel level){
        for (CoverageMetric cm : coverageMetrics) {
            for (int i = 0; i < nodes.getLength(); i++) {
                if (nodes.item(i).getNodeName().equals("column")) {
                    CoverageResult cr = cm.getCoverage(report.getTestCases(),
                                                       level);
                    int covered = cr.getCoveredItems();
                    int total = cr.getTotalItems();
                    String levelname = (level.getName().length() >= level.getHeaderString().length())
                                       ? level.getName()
                                       : level.getHeaderString();
                    writer.printf(nodes.item(i).getTextContent(),
                                  cm.getName(),                                   // 1$ coveragemetric
                                  levelname,                                      // 2$ name of method, class, ...
                                  level.getType().getEnglishName(),               // 3$ type of 2$ (method, class, ...)
                                  packageOf(level),                               // 4$ package 2$ is part of
                                  covered,                                        // 5$ covered
                                  total - covered,                                // 6$ uncovered
                                  total,                                          // 7$ = 5$ + 6$
                                  total == 0 ? 1.0 : (1.0 * covered / total),     // 8$ = 5$ / 7$
                                  total == 0 ? 100.0 : (100.0 * covered / total));// 9$ = $8 * 100
                                           
                } else {
                    // ignore, because it could be whitespaces = text instead of
                    // an unknown tag which probably could deserve an exception 
                }
            }
        }
    }
}
