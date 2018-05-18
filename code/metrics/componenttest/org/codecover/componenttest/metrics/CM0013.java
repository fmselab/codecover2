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

package org.codecover.componenttest.metrics;

import java.util.List;

import org.codecover.metrics.coverage.BranchCoverage;
import org.codecover.metrics.coverage.CoverageResult;
import org.codecover.metrics.coverage.LoopCoverage;
import org.codecover.metrics.coverage.StatementCoverage;
import org.codecover.metrics.coverage.TermCoverage;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;

import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.SimpleLogger;

/**
 * 
 * @author Tilmann Scheller
 * @version 1.0 ($Id: CM0013.java 71 2010-04-14 18:28:46Z schmidberger $)
 */
public class CM0013 extends junit.framework.TestCase {

    public void testCM0013() throws FileLoadException {
        String containerLocation = "../../qa/testdata/containers/metrics/metrics-loop-partial.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        TestSessionContainer tsc = null;
        TestSession ts = null;
        List<TestCase> testCases = null;
        CoverageResult result = null;
        HierarchyLevel code = null;
        
        // load test session container
        tsc = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder, containerLocation);
        
        // get MAST root node
        code = tsc.getCode();
        assertNotNull(code);
        
        // get test session named "ts1"
        ts = tsc.getTestSessionWithName("ts1");
        assertNotNull(ts);
        // get all of its test cases
        testCases = ts.getTestCases();
        assertNotNull(testCases);
        
        // get coverage metrics
        StatementCoverage statementCoverage = StatementCoverage.getInstance();
        BranchCoverage branchCoverage = BranchCoverage.getInstance();
        TermCoverage termCoverage = TermCoverage.getInstance();
        LoopCoverage loopCoverage = LoopCoverage.getInstance();
        
        // check statement coverage
        result = statementCoverage.getCoverage(testCases, code);
        assertEquals(0, result.getCoveredItems());

        // check branch coverage
        result = branchCoverage.getCoverage(testCases, code);
        assertEquals(0, result.getCoveredItems());
        
        // check strict condition coverage
        result = termCoverage.getCoverage(testCases, code);
        assertEquals(0, result.getCoveredItems());
        
        // check loop coverage
        result = loopCoverage.getCoverage(testCases, code);
        assertEquals(2, result.getCoveredItems());
    }
    
}
