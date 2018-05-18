///////////////////////////////////////////////////////////////////////////////
//
// $Id: StatementCoverage.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 15:39:11
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.criteria;

/**
 * This is an {@link Criterion} describing StatementCoverage.
 * 
 * @author Christoph Müller
 * 
 */
public class StatementCoverage extends Criterion {

    /** the String "<b>StatementCoverage</b>" */
    public static final String NAME = "StatementCoverage";
    
    private static final StatementCoverage instance = new StatementCoverage();

    /**
     * @return The single instance of StatementCoverage;
     */
    public static StatementCoverage getInstance() {
        return instance;
    }
    
    private StatementCoverage() {
        // just to set the constructor private
    }
    
    /**
     * @return the String "<b>StatementCoverage</b>"
     */
    @Override
    public String getName() {
        return NAME;
    }
}
