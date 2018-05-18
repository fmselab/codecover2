///////////////////////////////////////////////////////////////////////////////
//
// $Id: CobolInstrumenterTest.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 27.03.2007 01:07:36
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Queue;

import org.gbt2.instrumentation.criteria.BranchCoverage;
import org.gbt2.instrumentation.criteria.ConditionCoverage;
import org.gbt2.instrumentation.criteria.LoopCoverage;
import org.gbt2.instrumentation.criteria.StatementCoverage;

import junit.framework.TestCase;

/**
 * @author Stefan Franke
 *
 */
public class CobolInstrumenterTest extends TestCase {
    
    Instrumenter instrumenter;

    /**
     *
     * @throws java.lang.Exception
     */
    protected void setUp() throws Exception {
        this.instrumenter = new org.gbt2.instrumentation.cobol85.Instrumenter();
    }

    /**
     *
     * @throws java.lang.Exception
     */
    protected void tearDown() throws Exception {
        this.instrumenter = null;
    }
    
    public void testExample1() {
        String srcPath = "testsource/org/gbt2/instrument/cobol85/test/test1/test1.cob";
        File source = new File(srcPath);
        String targetPath = "testtarget/org/gbt2/instrument/cobol85/test/test1/test1.cob";
        File target = new File(targetPath);

        this.instrumenter.addCriterion(StatementCoverage.getInstance());
        this.instrumenter.addCriterion(BranchCoverage.getInstance());
        this.instrumenter.addCriterion(ConditionCoverage.getInstance());
        this.instrumenter.addCriterion(LoopCoverage.getInstance());
        this.instrumenter.setCharset(Charset.forName("UTF-8"));
        Queue<InstrumentationError> errors = this.instrumenter.instrument(source, target);
        for (InstrumentationError error : errors) {
            error.getException().printStackTrace();
        }
        assertTrue(errors.isEmpty());
    }
    
}
