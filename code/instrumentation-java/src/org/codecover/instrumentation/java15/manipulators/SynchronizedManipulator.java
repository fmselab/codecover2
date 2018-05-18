package org.codecover.instrumentation.java15.manipulators;

import java.io.IOException;

import org.codecover.instrumentation.java15.syntaxtree.SynchronizedStatement;

/**
 * @author 
 *
 * @version 1.0 ($Id$)
 */

public interface SynchronizedManipulator extends Manipulator {

    /**
     * 
     * @param n
     * @param statementID
     * @throws IOException
     */
    public void manipulatePart1(SynchronizedStatement n, String statementID)
        throws IOException;

    /**
     * 
     * @param n
     * @param statementID
     * @throws IOException
     */
    public void manipulatePart2(SynchronizedStatement n, String statementID)
        throws IOException;
    
    /**
     * 
     * @param n
     * @param statementID
     * @throws IOException
     */
    public void manipulatePart3(SynchronizedStatement n, String statementID)
        throws IOException;
    

    /**
     * 
     * @param n
     * @param statementID
     * @throws IOException
     */
    public void manipulateBefore(SynchronizedStatement n, String statementID)
        throws IOException;

    /**
     * 
     * @param n
     * @param statementID
     * @throws IOException
     */
    public void manipulateInnerBefore(SynchronizedStatement n, String statementID)
        throws IOException;

    /**
     * 
     * @param n
     * @param statementID
     * @throws IOException
     */
    public void manipulateInnerAfter(SynchronizedStatement n, String statementID)
        throws IOException;
}
