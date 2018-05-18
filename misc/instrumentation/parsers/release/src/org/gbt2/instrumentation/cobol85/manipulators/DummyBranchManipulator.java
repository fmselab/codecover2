///////////////////////////////////////////////////////////////////////////////
//
// $Id: DummyBranchManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 27.03.2007 00:05:45
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;

import org.gbt2.instrumentation.cobol85.syntaxtree.ProgramUnit;

/**
 * This is a dummy implementation of the branch manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 - 27.03.2007
 *
 */
public class DummyBranchManipulator implements BranchManipulator {

    public void generateBranchCounter(ProgramUnit programUnit, PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateElseBranch(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateEndEvaluate(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateEndIf(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateWhenOtherBranch(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void manipulate(PrintWriter printWriter) {
        // The dummy does not write
    }
}
