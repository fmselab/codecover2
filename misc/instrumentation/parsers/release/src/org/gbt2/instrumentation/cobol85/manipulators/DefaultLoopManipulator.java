///////////////////////////////////////////////////////////////////////////////
//
// $Id: DefaultLoopManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 27.03.2007 00:08:44
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;

import org.gbt2.instrumentation.cobol85.CounterProvider;
import org.gbt2.instrumentation.cobol85.syntaxtree.ProgramUnit;

/**
 * This is a default implementation of the loop manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 - 27.03.2007
 *
 */
public class DefaultLoopManipulator implements LoopManipulator {
    
    private static final String LOOPING_COVERAGE_COUNTER = "  05 LOOPING-COVERAGE-COUNTER.%n";
    
    private CounterProvider counterProvider;
    
    /**
     * Constructor
     *
     * @param counterProvider the counter provider
     */
    public DefaultLoopManipulator(CounterProvider counterProvider) {
        this.counterProvider = counterProvider;
    }

    public void generateLoopCounter(ProgramUnit programUnit, PrintWriter printWriter) {
        // 
    }

    public void generateAuxiliaryLoopCounterHeader(PrintWriter printWriter) {
        printWriter.printf(LOOPING_COVERAGE_COUNTER);
    }

    public void generateAuxiliaryLoopCounter(ProgramUnit programUnit, PrintWriter printWriter) {
        // 
    }

}
