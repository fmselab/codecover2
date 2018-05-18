///////////////////////////////////////////////////////////////////////////////
//
// $Id: Criterion.java 1 2007-12-12 17:37:26Z t-scheller $
//
// created at: 26.03.2007 15:33:46
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.criteria;

/**
 * This interface describes a criterion for code coverage.
 * 
 * @author Christoph Müller
 */
public abstract class Criterion implements Comparable<Criterion> {

    /**
     * @return The name of the criterion.
     */
    public abstract String getName();
    
    /**
     * Compares this object with another {@link Criterion}.
     * 
     * @param otherCriterion
     *            the other criterion
     * @return true &rarr; the objects describe the same criteria; false &rarr;
     *         the objects describe different criteria.
     */
    public int compareTo(Criterion otherCriterion) {
        return this.getClass().getName().compareTo(
                otherCriterion.getClass().getName());
    }
}
