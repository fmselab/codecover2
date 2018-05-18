///////////////////////////////////////////////////////////////////////////////
//
// $Id: CoverageCounterLog.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 22.03.2007 22:47:35
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.measurement;

/**
 * This is an interface where coverage counters can be written to.<br>
 * <br>
 * For example, this interface is expanded to the {@link CoverageResultLog}<br>
 * <br>
 * This log file accepts the the start of a section and long counters.
 * 
 * @author Christoph Müller
 */
public interface CoverageCounterLog {
    /** This is the name of {@link #passCounter()}. */
    public static final String PASS_COUNTER_METHOD_NAME = "passCounter";
    
    /** This is the name of {@link #startNamedSection()}. */
    public static final String START_SECTION_METHOD_NAME = "startNamedSection";

    /**
     * Notifies the start of a section.
     * 
     * @param sectionName
     *            The name of the section.
     */
    public void startNamedSection(String sectionName);

    /**
     * Passes the ID and the value of a counter.
     * 
     * @param counterID
     *            The ID of the counter
     * @param counterValue
     *            The value of the counter.
     */
    public void passCounter(String counterID, long counterValue);
}
