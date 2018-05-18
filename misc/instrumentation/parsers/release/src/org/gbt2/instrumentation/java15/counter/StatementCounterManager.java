///////////////////////////////////////////////////////////////////////////////
//
// $Id: StatementCounterManager.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 15.04.2007 15:51:06
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.counter;

import java.io.IOException;
import java.io.Writer;

/**
 * This class represents a manager for statement counters.<br>
 * <br>
 * This manager can generate statement IDs, write a counter of a statement ID to
 * the instrumented source file, write the declaration(s), write a block for
 * resetting all counters and write a block for serializing and resetting all
 * counters. This interface is used by the {@link CounterIDManager}.<br>
 * There are varoius variants of implementation:
 * <ul>
 * <li>{@link LongStatementCounterManager}</li>
 * </ul>
 * 
 * @author Christoph Müller
 * 
 * @see CounterIDManager
 */
interface StatementCounterManager {
    /**
     * Increments the StatementID.
     * 
     * @return The incremented and formatted StatementID.
     */
    public String nextStatementID();

    /**
     * Writes a statement for incrementing the counter of a statementID.<br>
     * <br>
     * The statementID is saved for {@link #writeInnerClass()}.
     * 
     * @param statementID
     *            The ID of the statement.
     * 
     * @throws IOException
     *             For IO exceptions at the use of {@link Writer#write(String)}.
     */
    public void writeCounterIncrementingForStatement(String statementID)
            throws IOException;

    /**
     * Writes the declarations for the statement counter(s).
     * 
     * @throws IOException
     */
    public void writeDeclarations() throws IOException;

    /**
     * Writes the block to reset the statement counters.
     * 
     * @throws IOException
     */
    public void writeReset() throws IOException;

    /**
     * Writes the block to serialize and reset the statement counters.
     * 
     * @throws IOException
     */
    public void writeSerialzeAndReset() throws IOException;
}