///////////////////////////////////////////////////////////////////////////////
//
// $Id: BranchCoverage.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 15:39:11
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.criteria;

/**
 * This is an {@link Criterion} describing BranchCoverage.
 * 
 * @author Christoph Müller
 */
public class BranchCoverage extends Criterion {

    /** the String "<b>BranchCoverage</b>" */
    public static final String NAME = "BranchCoverage";

    private static final BranchCoverage instance = new BranchCoverage();

    /**
     * @return The single instance of BranchCoverage;
     */
    public static BranchCoverage getInstance() {
        return instance;
    }

    private BranchCoverage() {
        // just to set the constructor private
    }

    /**
     * @return the String "<b>BranchCoverage</b>"
     */
    @Override
    public String getName() {
        return NAME;
    }
}
