///////////////////////////////////////////////////////////////////////////////
//
// $Id: DefaultStatementManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 27.03.2007 00:02:47
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;

import org.gbt2.instrumentation.cobol85.CounterProvider;
import org.gbt2.instrumentation.cobol85.syntaxtree.ProgramUnit;

/**
 * This is a default implementation of the statement manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 - 27.03.2007
 *
 */
public class DefaultStatementManipulator implements StatementManipulator {
    
    private CounterProvider counterProvider;
    
    /**
     * Constructor
     *
     * @param counterProvider the counter provider
     */
    public DefaultStatementManipulator(CounterProvider counterProvider) {
        this.counterProvider = counterProvider;
    }

    public void manipulate(PrintWriter printWriter) {
        printWriter.printf(this.counterProvider.newStatementCounter());
    }

    public void generateStatementCounter(ProgramUnit programUnit, PrintWriter printWriter) {
        this.counterProvider.generateStatementCounter(programUnit, printWriter);
    }

}
