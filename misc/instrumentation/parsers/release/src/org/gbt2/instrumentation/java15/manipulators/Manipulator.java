///////////////////////////////////////////////////////////////////////////////
//
// $Id: Manipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 31.03.2007 12:43:59
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.manipulators;

import java.io.Writer;

import org.gbt2.instrumentation.java15.counter.CounterIDManager;
import org.gbt2.instrumentation.java15.visitor.InstrumentationVisitorWithException;

/**
 * This is a interface for manipulators.<br>
 * <br>
 * It allows to set a {@link Writer} and a {@link CounterIDManager}.
 * 
 * @author Christoph Müller
 * @see StatementManipulator
 * @see BranchManipulator
 * @see ConditionManipulator
 * @see LoopManipulator
 * @see AbstractDummyManipulator
 * @see AbstractDefaultManipulator
 */
public interface Manipulator {
    /**
     * Sets the {@link CounterIDManager} for ID generation usage.
     * 
     * @param counterIDManager
     */
    public void setCounterIDManager(CounterIDManager counterIDManager);

    /**
     * Sets the writer for incrementing statements and modifications.
     * 
     * @param writer
     */
    public void setWriter(Writer writer);

    /**
     * Whether or whether not statements should occur in Block Statements.<br>
     * <br>
     * This information is needed by the {@link InstrumentationVisitorWithException} to
     * expand single line statements - e.g.
     * 
     * <pre>
     * if (a == 1)
     *     a++;
     * </pre>
     * 
     * into
     * 
     * <pre>
     * if (a == 1) {
     *     a++;
     * }
     * </pre>
     * 
     * that a++ can be instrumented. Here statements in if / else are
     * considered.
     * 
     * @return true &rarr; Statements must be expanded to Block Statements if
     *         necessary.
     */
    public boolean requiresBlockExpansionsForBranches();

    /**
     * Whether or whether not statements should occur in Block Statements.<br>
     * <br>
     * This information is needed by the {@link InstrumentationVisitorWithException} to
     * expand single line statements - e.g.
     * 
     * <pre>
     * while ( a <= 10 )
     *     a++;
     * </pre>
     * 
     * into
     * 
     * <pre>
     * while ( a <= 10 ) {
     *     a++;
     * }
     * </pre>
     * 
     * that a++ can be instrumented. Here statements in loops are
     * considered.
     * 
     * @return true &rarr; Statements must be expanded to Block Statements if
     *         necessary.
     */
    public boolean requiresBlockExpansionsForLoops();
}
