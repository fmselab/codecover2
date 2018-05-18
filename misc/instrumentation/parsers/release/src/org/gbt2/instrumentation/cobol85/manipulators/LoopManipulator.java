///////////////////////////////////////////////////////////////////////////////
//
// $Id: LoopManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 23:49:53
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;

import org.gbt2.instrumentation.cobol85.syntaxtree.ProgramUnit;

/**
 * This is an interface for loop manipulators. A loop manipulator 
 * should be used to insert counters into the source code. The counter 
 * variable declaration is also placed in a statement manipulator.
 * 
 * @author Stefan Franke
 * @version 1.0 - 27.03.2007
 *
 */
public interface LoopManipulator {
    
    /**
     * Generates all loop counter that appears in the given program unit.
     * Writes results to the output writer.
     * 
     * @param programUnit the active program unit
     * @param printWriter the output writer
     */
    void generateLoopCounter(ProgramUnit programUnit, PrintWriter printWriter);
    
    /**
     * Generates all auxiliary loop counter that appears in the given program unit.
     * Writes results to the output writer.
     * 
     * @param programUnit the active program unit
     * @param printWriter the output writer
     */
    void generateAuxiliaryLoopCounter(ProgramUnit programUnit, PrintWriter printWriter);
    
    /**
     * Generates auxiliary loop counter head.
     * 
     * @param printWriter the output writer
     */
    void generateAuxiliaryLoopCounterHeader(PrintWriter printWriter);
    
}
