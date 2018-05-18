///////////////////////////////////////////////////////////////////////////////
//
// $Id: DefaultConditionManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 17:31:25
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.manipulators;


/**
 * @author Christoph Müller
 */
public class DefaultConditionManipulator extends AbstractDefaultManipulator
        implements ConditionManipulator {

    /**
     * Always false.
     * 
     * @return false;
     */
    public boolean requiresBlockExpansionsForBranches() {
        return false;
    }

    /**
     * Always false.
     * 
     * @return false;
     */
    public boolean requiresBlockExpansionsForLoops() {
        return false;
    }
}
