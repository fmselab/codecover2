///////////////////////////////////////////////////////////////////////////////
//
// $Id: LoopCoverage.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 15:39:11
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.criteria;

/**
 * This is an {@link Criterion} describing LoopCoverage.
 * 
 * @author Christoph Müller
 */
public class LoopCoverage extends Criterion {

    /** the String "<b>LoopCoverage</b>" */
    public static final String NAME = "LoopCoverage";

    private static final LoopCoverage instance = new LoopCoverage();

    /**
     * @return The single instance of LoopCoverage;
     */
    public static LoopCoverage getInstance() {
        return instance;
    }

    private LoopCoverage() {
        // just to set the constructor private
    }

    /**
     * @return the String "<b>LoopCoverage</b>"
     */
    @Override
    public String getName() {
        return NAME;
    }
}
