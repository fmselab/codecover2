/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This file may be used, modifies and redistributed     *
 * under the terms of either the Eclipse Public License v1.0 which            *
 * accompanies this distribution and is available at                          *
 * http://www.eclipse.org/legal/epl-v10.html or the MIT license, available at *
 * http://www.opensource.org/licenses/mit-license.php                         *
 ******************************************************************************/

package org.codecover.batch;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.codecover.model.utils.ChangeEvent;
import org.codecover.model.utils.ChangeListener;
import org.codecover.model.utils.ChangeType;
import org.codecover.model.utils.ListenerHandle;
import org.codecover.model.utils.LogLevel;
import org.codecover.model.utils.LogMessage;
import org.codecover.model.utils.Logger;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: BatchLogger.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class BatchLogger extends Logger {
    /**
     * The default log level.
     */
    public static final LogLevel DEFAULT_LOG_LEVEL = LogLevel.WARNING;

    private LogLevel logLevel;

    private final Object lock = new Object();

    private final ChangeEvent<String> preWriteEvent = new ChangeEvent<String>();

    private final ChangeEvent<String> postWriteEvent = new ChangeEvent<String>();
    
    private boolean showStackTraces = false;
    
    /**
     * Constructor.<br>
     * <br>
     * Sets the output to {@link System#err}.
     * 
     * @param logLevel
     *            the {@link LogLevel} of the Logger
     */
    public BatchLogger(LogLevel logLevel) {
        this.logLevel = logLevel;
    }

    /**
     * Constructor.<br>
     * <br>
     * Sets the output to {@link System#err}. The logLevel is the
     * {@link #DEFAULT_LOG_LEVEL}.
     */
    public BatchLogger() {
        this(DEFAULT_LOG_LEVEL);
    }
    
    /**
     * Sets whether or not stack traces are shown.
     * 
     * @param showStackTraces
     *                the indicator.
     */
    public void setShowStackTraces(boolean showStackTraces) {
        this.showStackTraces = showStackTraces;
    }
    
    /**
     * Sets the {@link LogLevel} of the {@link BatchLogger}
     * 
     * @param logLevel
     *            the given {@link LogLevel}
     */
    public void setLogLevel(LogLevel logLevel) {
        if (logLevel == null) {
            throw new NullPointerException("logLevel == null");
        }

        synchronized (this.lock) {
            this.logLevel = logLevel;
        }
    }

    /**
     * Gets the {@link LogLevel} of the {@link BatchLogger}
     * 
     * @return the {@link LogLevel}
     */
    public LogLevel getLogLevel() {
        synchronized (this.lock) {
            return this.logLevel;
        }
    }

    /**
     * Writes a given {@link LogMessage}, if its LogLevel is &lt;=
     * {@link #getLogLevel()}
     * 
     * @param message
     *            the given {@link LogMessage}
     */
    @Override
    public void writeLogMessage(LogMessage message) {
        if (message == null) {
            throw new NullPointerException("message == null");
        }

        synchronized (this.lock) {
            if (message.getLevel().compareTo(getLogLevel()) > 0) {
                return;
            }

            StringBuilder builder = new StringBuilder();
            // we construct the message
            builder.append('[');
            builder.append(message.getLevel().toString());
            builder.append("] ");
            builder.append(message.getMessage());
            
            /*
             * Never write the caller of this function or the stacktrace:
             * Both is internal information and the user probably can not use
             * it. Assume (or hope), that the error is the user's fault
             * (e.g. missing file).
             */

            // if the log lever is error or worse, than put out the caller of
            // this message
            if (this.showStackTraces && message.getLevel().compareTo(LogLevel.ERROR) <= 0
                    && !message.getStackTrace().isEmpty()) {
                builder.append(" (at ");
                builder.append(message.getStackTrace().get(0).toString());
                builder.append(')');
                
                if (message.getException() != null) {
                    builder.append('\n');
                    final StringWriter sw = new StringWriter();
                    final PrintWriter out = new PrintWriter(sw);
                    message.getException().printStackTrace(out);
                    builder.append(sw.toString());
                }
            }

            this.preWriteEvent.emitChanged(ChangeType.ADD, builder.toString());

            System.err.println(builder.toString());

            this.postWriteEvent.emitChanged(ChangeType.ADD, builder.toString());
        }
    }

    /**
     * Adds a {@link ChangeListener} to this logger, that is notified before a
     * message is written
     * 
     * @param listener
     *            the given {@link ChangeListener}
     * @return a {@link ListenerHandle} to remove the {@link ChangeListener}
     */
    public ListenerHandle addPreWriteListener(ChangeListener<String> listener) {
        return this.preWriteEvent.addListener(listener);
    }

    /**
     * Adds a {@link ChangeListener} to this logger, that is notified after a
     * message is written
     * 
     * @param listener
     *            the given {@link ChangeListener}
     * @return a {@link ListenerHandle} to remove the {@link ChangeListener}
     */
    public ListenerHandle addPostWriteListener(ChangeListener<String> listener) {
        return this.postWriteEvent.addListener(listener);
    }
}
