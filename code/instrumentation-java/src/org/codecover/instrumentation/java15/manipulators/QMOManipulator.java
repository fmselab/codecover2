package org.codecover.instrumentation.java15.manipulators;

import java.io.IOException;

import org.codecover.instrumentation.java15.syntaxtree.ConditionalOrExpression;

/**
 * @author 
 *
 * @version 1.0 ($Id$)
 */

public interface QMOManipulator extends Manipulator {
    /**
     * 
     * @param n the Condition of the ?-Op
     * @param qmoID
     * @throws IOException
     */
    public void manipulateBefore(ConditionalOrExpression n, String qmotID)
        throws IOException;
    public void manipulateAfter(ConditionalOrExpression n, String qmotID)
        throws IOException;

}
