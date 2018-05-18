///////////////////////////////////////////////////////////////////////////////
//
// $Id: JavaBranchTest.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 19:08:36
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation;

import java.io.File;
import java.nio.charset.Charset;
import java.util.Queue;

import junit.framework.TestCase;

import org.gbt2.instrumentation.criteria.BranchCoverage;
import org.gbt2.instrumentation.criteria.StatementCoverage;

/**
 * @author Christoph Müller
 * 
 */
public class JavaBranchTest extends TestCase {

    Instrumenter instrumenter;

    /**
     * 
     * @throws java.lang.Exception
     */
    protected void setUp() throws Exception {
        this.instrumenter = new org.gbt2.instrumentation.java15.Instrumenter();
    }

    /**
     * 
     * @throws java.lang.Exception
     */
    protected void tearDown() throws Exception {
        this.instrumenter = null;
    }

    public void testTest1() {
        String srcPath = "testsource/org/gbt2/instrument/java1_5/test/test1/TestClass.java";
        File source = new File(srcPath);
        String targetPath = "testtarget/org/gbt2/instrument/java1_5/test/test1/TestClass.java";
        File target = new File(targetPath);

        this.instrumenter.addCriterion(BranchCoverage.getInstance());
        this.instrumenter.setCharset(Charset.forName("UTF-8"));
        Queue<InstrumentationError> errors = this.instrumenter.instrument(source, target);
        for (InstrumentationError error : errors) {
            System.out.println(error.getException().getMessage());
            error.getException().printStackTrace();
            System.out.printf("%n########################%n");
        }
        assertTrue(errors.isEmpty());
    }

    public void testTest2() {
        String srcPath = "testsource/org/gbt2/instrument/java1_5/test/test2/";
        File source = new File(srcPath);
        String targetPath = "testtarget/org/gbt2/instrument/java1_5/test/test2/";
        File target = new File(targetPath);

        this.instrumenter.addCriterion(BranchCoverage.getInstance());
        this.instrumenter.setCharset(Charset.forName("UTF-8"));
        Queue<InstrumentationError> errors = this.instrumenter.instrumentFolder(source, true, null, target);
        for (InstrumentationError error : errors) {
            System.out.println(error.getException().getMessage());
            error.getException().printStackTrace();
            System.out.printf("%n########################%n");
        }
        assertTrue(errors.isEmpty());
    }

    public void testTest3() {
        String srcPath = "testsource/org/gbt2/instrument/java1_5/test/test3/CodeExample.java";
        File source = new File(srcPath);
        String targetPath = "testtarget/org/gbt2/instrument/java1_5/test/test3/CodeExample.java";
        File target = new File(targetPath);

        this.instrumenter.addCriterion(BranchCoverage.getInstance());
        this.instrumenter.setCharset(Charset.forName("UTF-8"));
        Queue<InstrumentationError> errors = this.instrumenter.instrument(source, target);
        for (InstrumentationError error : errors) {
            System.out.println(error.getException().getMessage());
            error.getException().printStackTrace();
            System.out.printf("%n########################%n");
        }
        assertTrue(errors.isEmpty());
    }
}
