///////////////////////////////////////////////////////////////////////////////
//
// $Id: AbstractDummyManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 31.03.2007 12:47:40
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.manipulators;

import java.io.Writer;

import org.gbt2.instrumentation.java15.counter.CounterIDManager;

/**
 * An abstract {@link Manipulator} which can be inherited by Dummy Manipulators.<br>
 * <br>
 * All required methods do nothing and all boolean functions return false.
 * 
 * @author Christoph Müller
 * @see Manipulator
 * @see DummyStatementManipulator
 * @see DummyBranchManipulator
 * @see DummyConditionManipulator
 * @see DummyLoopManipulator
 */
public abstract class AbstractDummyManipulator implements Manipulator {

    public void setCounterIDManager(CounterIDManager counterIDManager) {
        // null
    }

    public void setWriter(Writer writer) {
        // null
    }

    /**
     * Always false.
     * 
     * @return false
     */
    public boolean requiresBlockExpansionsForBranches() {
        return false;
    }

    /**
     * Always false.
     * 
     * @return false
     */
    public boolean requiresBlockExpansionsForLoops() {
        return false;
    }
}
