///////////////////////////////////////////////////////////////////////////////
//
// $Id: DummyLoopManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 27.03.2007 00:07:22
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;

import org.gbt2.instrumentation.cobol85.syntaxtree.ProgramUnit;

/**
 * This is a dummy implementation of the loop manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 - 27.03.2007
 *
 */
public class DummyLoopManipulator implements LoopManipulator {

    public void generateLoopCounter(ProgramUnit programUnit, PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateAuxiliaryLoopCounter(ProgramUnit programUnit, PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateAuxiliaryLoopCounterHeader(PrintWriter printWriter) {
        // The dummy does not write
    }

}
