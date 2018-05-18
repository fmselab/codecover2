///////////////////////////////////////////////////////////////////////////////
//
// $Id: DefaultBranchManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 27.03.2007 00:09:58
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;

import org.gbt2.instrumentation.cobol85.CounterProvider;
import org.gbt2.instrumentation.cobol85.syntaxtree.ProgramUnit;

/**
 * This is a default implementation of the branch manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 - 27.03.2007
 *
 */
public class DefaultBranchManipulator implements BranchManipulator {
    
    private static final String ELSE = "%nELSE";
    
    private static final String WHEN_OTHER = "%nWHEN OTHER";
    
    private static final String END_IF = "%nEND-IF";
    
    private static final String END_EVALUATE = "%nEND-EVALUATE";
    
    private CounterProvider counterProvider;
    
    /**
     * Constructor
     *
     * @param counterProvider the counter provider
     */
    public DefaultBranchManipulator(CounterProvider counterProvider) {
        this.counterProvider = counterProvider;
    }
    
    public void generateBranchCounter(ProgramUnit programUnit, PrintWriter printWriter) {
        this.counterProvider.generateBranchCounter(programUnit, printWriter);
    }

    public void generateElseBranch(PrintWriter printWriter) {
        printWriter.printf(ELSE);
        this.manipulate(printWriter);
    }

    public void generateEndEvaluate(PrintWriter printWriter) {
        printWriter.printf(END_EVALUATE);
    }

    public void generateEndIf(PrintWriter printWriter) {
        printWriter.printf(END_IF);
    }

    public void generateWhenOtherBranch(PrintWriter printWriter) {
        printWriter.printf(WHEN_OTHER);
        this.manipulate(printWriter);
    }
    
    public void manipulate(PrintWriter printWriter) {
        printWriter.printf(this.counterProvider.newBranchCounter());
    }

}
