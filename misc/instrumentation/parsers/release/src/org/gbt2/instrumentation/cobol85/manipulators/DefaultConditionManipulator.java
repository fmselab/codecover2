///////////////////////////////////////////////////////////////////////////////
//
// $Id: DefaultConditionManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 27.03.2007 00:09:30
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;

import org.gbt2.instrumentation.cobol85.CounterProvider;
import org.gbt2.instrumentation.cobol85.syntaxtree.ProgramUnit;

/**
 * This is a default implementation of the condition manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 - 27.03.2007
 *
 */
public class DefaultConditionManipulator implements ConditionManipulator {
    
    private CounterProvider counterProvider;
    
    /**
     * Constructor
     *
     * @param counterProvider the counter provider
     */
    public DefaultConditionManipulator(CounterProvider counterProvider) {
        this.counterProvider = counterProvider;
    }

    public void generateConditionCounter(ProgramUnit programUnit, PrintWriter printWriter) {
        this.counterProvider.generateConditionCounter(programUnit, printWriter);
    }

    public void manipulate(PrintWriter printWriter) {
        this.counterProvider.generateConditionCoverageBlock(printWriter);
    }

}
