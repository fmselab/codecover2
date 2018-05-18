///////////////////////////////////////////////////////////////////////////////
//
// $Id: ConditionManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 23:49:39
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;

import org.gbt2.instrumentation.cobol85.syntaxtree.ProgramUnit;

/**
 * This is an interface for condition manipulators. A condition manipulator 
 * should be used to insert counters into the source code. The counter 
 * variable declaration is also placed in a statement manipulator.
 * 
 * @author Stefan Franke
 * @version 1.0 - 27.03.2007
 *
 */
public interface ConditionManipulator {
    
    /**
     * Generates all condition counter that appears in the given program unit.
     * Writes results to the output writer.
     * 
     * @param programUnit the active program unit
     * @param printWriter the output writer
     */
    void generateConditionCounter(ProgramUnit programUnit, PrintWriter printWriter);

    /**
     * Manipulates the source code for condition coverage criteria.
     * 
     * @param printWriter the output writer
     */
    void manipulate(PrintWriter printWriter);

}
