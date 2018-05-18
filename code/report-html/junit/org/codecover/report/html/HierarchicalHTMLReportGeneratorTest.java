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
 * 
 */
package org.codecover.report.html;

import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.xml.parsers.ParserConfigurationException;

import org.codecover.DatabaseExample;
import org.codecover.model.*;
import org.codecover.model.exceptions.*;
import org.codecover.model.utils.*;
import org.codecover.metrics.*;
import org.codecover.report.*;
import org.codecover.report.exceptions.*;
import org.codecover.model.extensions.*;

import junit.framework.TestCase;

/**
 * @author Johannes Langauf
 * @version 1.0 $Id: HierarchicalHTMLReportGeneratorTest.java 1 2007-12-12 17:37:26Z t-scheller $
 */
public class HierarchicalHTMLReportGeneratorTest extends TestCase {

    public static String MODULE_BASE = ".";

    public static String TEST_DIR = MODULE_BASE + File.separator + "testreport"
            + File.separator;

    public Logger logger = new SimpleLogger();

    ProgressHandler dummyProgressHandler = ProgressHandler.NULL;

    private String templatePath = "HTML_Report_hierarchic.xml";

    /*
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();

        // create empty test directory
        File testDir = new File(TEST_DIR);
        org.codecover.UtilsForTesting.deleteFileRecursively(testDir);
        testDir.mkdir();
    }

    /*
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#tearDown()
     */
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public void testSmallDefaultReport() throws FileCreationException,
            ReportException {
        //FIXME: this testcase is broken since the model changed, fix DatabaseExamle or remove TestCase
//        // set output file path
//        String outPath = TEST_DIR + File.separator + "fulldefault.html";
//
//        // generate Database
//        DatabaseExample db = new DatabaseExample();
//
//        generateReport(outPath, templatePath, db.getTestSessionContainer());
    }

    public void testMediumDefaultReport() throws Throwable {
        // set output file path
        String outPath = TEST_DIR + File.separator + "mediumDefault.html";
        String dbPath = "../instrumentation/testsource/codeExampleTestSessionContainer.snapshot.xml";
        
        /*
        try {
            generateReport(outPath, templatePath, dbPath);
        } catch (ReportException e) {
            assertNull("Report exception happened", e);
        } catch (FileLoadException e) {
            throw new RuntimeException("Failed to load DB (either a bug in " +
                    "the model or outdated test session container, maybe " +
                    "rebuild with 'ant -f build-measurement.xml " +
                    "snapshotTestdata' in trunk/code/):", e);
        }
        */
        // Catching exceptions only makes debugging more difficult
        generateReport(outPath, templatePath, dbPath);
    }
    
// a testcase for fred (FIXME: unknown where to find the TSC))))
// may be useful as a template for other test cases as well
//    public void testFredDefaultReport() throws FileCreationException,
//    ReportException {
//        /* set output file path */
//        String outPath = TEST_DIR + File.separator + "fredDefault.html";
//        String dbPath = "test-session-container.xml";
//
//        try {
//            generateReport(outPath, templatePath, dbPath);
//        } catch (ReportException e) {
//            assertNull("Report exception happened", e);
//        } catch (FileLoadException e) {
//            throw new RuntimeException("Failed to load DB", e);
//        }
//    }

    private void generateReport(String outPath, String template, String dbPath)
            throws  FileLoadException,
                    FileCreationException,
                    ReportException
    {

        // prepare db
        MASTBuilder builder = new MASTBuilder(logger);
        TestSessionContainer testSessionContainer = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder, dbPath);
        
        generateReport(outPath, template, testSessionContainer);
    }
    
    private void generateReport(String outPath, String templatePath,
                                TestSessionContainer testSessionContainer)
            throws FileCreationException, ReportException {

        List<org.codecover.model.TestCase> testCases = null;
        try {
            testCases = testSessionContainer.getTestSessions()
                                            .get(0)
                                            .getTestCases();
        } catch (Exception e) {
            logger.fatal("Failed to extract test case from db.", e);
        }

        if (testCases == null) {
            throw new NullPointerException("No test cases found in DB.");
        }
        
        /*
        Report r = new Report();
        r.setLogger(logger); // log with level WANRING to stderr
        r.setMetrics(MetricProvider.getAvailabeMetrics(PluginManager.create(), logger));
        r.setFileSystemPath(outPath);
        r.setTemplate(templatePath);
        r.setTestCases(testCases);
        r.setProgressHandler(dummyProgressHandler);
        */

        // TODO: this won't work anymore, since
        // we would need a real plugin for the generator
        // r.generateReport();
    }

}
