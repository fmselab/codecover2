///////////////////////////////////////////////////////////////////////////////
//
// $Id: BranchManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 23:49:21
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;

import org.gbt2.instrumentation.cobol85.syntaxtree.ProgramUnit;

/**
 * This is an interface for branch manipulators. A branch manipulator 
 * should be used to insert counters into the source code. The counter 
 * variable declaration is also placed in a statement manipulator.
 * 
 * @author Stefan Franke
 * @version 1.0 - 27.03.2007
 *
 */
public interface BranchManipulator {
    
    /**
     * Generates all branch counter that appears in the given program unit.
     * Writes results to the output writer.
     * 
     * @param programUnit the active program unit
     * @param printWriter the output writer
     */
    void generateBranchCounter(ProgramUnit programUnit, PrintWriter printWriter);
    
    /**
     * Generates an else-branch which includes a branch counter.
     * 
     * @param printWriter the output writer
     */
    void generateElseBranch(PrintWriter printWriter);
    
    /**
     * Generates the end evaluate keyword.
     * 
     * @param printWriter the output writer
     */
    void generateEndEvaluate(PrintWriter printWriter);
    
    /**
     * Generates the end if keyword.
     * 
     * @param printWriter the output writer
     */
    void generateEndIf(PrintWriter printWriter);
    
    /**
     * Generates an when-other-branch which includes a branch counter.
     * 
     * @param printWriter the output writer
     */
    void generateWhenOtherBranch(PrintWriter printWriter);

    /**
     * Manipulates the source code for branch coverage criteria.
     * 
     * @param printWriter the output writer
     */
    void manipulate(PrintWriter printWriter);
    
}
