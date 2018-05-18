///////////////////////////////////////////////////////////////////////////////
//
// $Id: InstrumentationError.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 30.03.2007 15:07:30
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation;

import java.io.File;

/**
 * This class represents an exception that occured during the instrumentation
 * process.<br>
 * <br>
 * It contains the source file, the target file and the exception that occurs
 * during the instrumentation process.
 * 
 * @author Christoph Müller
 * @see #getSource()
 * @see #getTarget()
 * @see #getException()
 */
public class InstrumentationError {

    private final File source;

    private final File target;

    private final Exception exception;

    /**
     * Constructs a new {@link InstrumentationError}.
     * 
     * @param source
     *            The source file for which the exception occurs.
     * @param target
     *            The target file for which the exception occurs.
     * @param exception
     *            The exception that occurs.
     */
    public InstrumentationError(final File source, final File target,
            final Exception exception) {
        this.source = source;
        this.target = target;
        this.exception = exception;
    }
    
    /**
     * Constructs a new {@link InstrumentationError}.
     * 
     * @param job
     *            The {@link InstrumentationJob} for which the exception occurs.
     * @param exception
     *            The exception that occurs.
     */
    public InstrumentationError(InstrumentationJob job,
            final Exception exception) {
        this.source = job.getSource();
        this.target = job.getTarget();
        this.exception = exception;
    }

    /**
     * @return The source file for which the exception occurs.
     */
    public File getSource() {
        return this.source;
    }

    /**
     * @return The target file for which the exception occurs.
     */
    public File getTarget() {
        return this.target;
    }

    /**
     * @return The exception that occurs.
     */
    public Exception getException() {
        return this.exception;
    }
}
