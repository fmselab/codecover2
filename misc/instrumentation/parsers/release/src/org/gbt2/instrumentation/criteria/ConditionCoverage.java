///////////////////////////////////////////////////////////////////////////////
//
// $Id: ConditionCoverage.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 15:39:11
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.criteria;

/**
 * This is an {@link Criterion} describing ConditionCoverage.
 * 
 * @author Christoph Müller
 */
public class ConditionCoverage extends Criterion {

    /** the String "<b>ConditionCoverage</b>" */
    public static final String NAME = "ConditionCoverage";

    private static final ConditionCoverage instance = new ConditionCoverage();

    /**
     * @return The single instance of ConditionCoverage;
     */
    public static ConditionCoverage getInstance() {
        return instance;
    }

    private ConditionCoverage() {
        // just to set the constructor private
    }

    /**
     * @return the String "<b>ConditionCoverage</b>"
     */
    @Override
    public String getName() {
        return NAME;
    }
}
