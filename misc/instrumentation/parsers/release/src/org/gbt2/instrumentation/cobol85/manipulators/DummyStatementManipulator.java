///////////////////////////////////////////////////////////////////////////////
//
// $Id: DummyStatementManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 27.03.2007 00:04:54
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;

import org.gbt2.instrumentation.cobol85.syntaxtree.ProgramUnit;

/**
 * This is a dummy implementation of the statement manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 - 27.03.2007
 *
 */
public class DummyStatementManipulator implements StatementManipulator {

    public void manipulate(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateStatementCounter(ProgramUnit programUnit, PrintWriter printWriter) {
        // The dummy does not write
    }

}
