///////////////////////////////////////////////////////////////////////////////
//
// $Id: AbstractDefaultManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 31.03.2007 12:47:40
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.manipulators;

import java.io.Writer;

import org.gbt2.instrumentation.java15.counter.CounterIDManager;

/**
 * An abstract {@link Manipulator} which can be inherited by Default
 * Manipulators.
 * 
 * @author Christoph Müller
 * @see Manipulator
 * @see DefaultStatementManipulator
 * @see DefaultBranchManipulator
 * @see DefaultConditionManipulator
 * @see DefaultLoopManipulator
 */
public abstract class AbstractDefaultManipulator implements Manipulator {
    private CounterIDManager counterIDManager = null;

    private Writer writer = null;

    public void setCounterIDManager(CounterIDManager counterIDManager) {
        this.counterIDManager = counterIDManager;
    }

    public void setWriter(Writer writer) {
        this.writer = writer;
    }

    /**
     * @return The counterIDManager.
     */
    protected CounterIDManager getCounterIDManager() {
        return this.counterIDManager;
    }

    /**
     * @return The writer.
     */
    protected Writer getWriter() {
        return this.writer;
    }
}
