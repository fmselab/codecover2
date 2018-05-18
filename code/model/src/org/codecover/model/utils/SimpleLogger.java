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

package org.codecover.model.utils;

import java.io.PrintStream;
import java.util.Collections;
import java.util.List;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: SimpleLogger.java 186 2013-05-20 19:18:39Z langaujs $)
 */
public class SimpleLogger extends Logger {
    private static LogLevel defaultLogLevel = LogLevel.INFO;
    
    private static final List<StackTraceElement> emptyStackTrace = Collections.emptyList();

    private final PrintStream output;

    private LogLevel logLevel;

    private boolean showPosition = true;

    private final Object lock = new Object();

    /**
     * Constructor
     * 
     * @param output
     *            the PrintStream used for writing output
     * @param logLevel
     *            the {@link LogLevel} of the Logger
     */
    public SimpleLogger(PrintStream output, LogLevel logLevel) {
        if (output == null) {
            throw new NullPointerException("output == null");
        }

        if (logLevel == null) {
            throw new NullPointerException("logLevel == null");
        }

        this.output = output;
        this.logLevel = logLevel;
    }

    /**
     * Constructor
     * 
     * @param output
     *            the PrintStream used for writing output
     */
    public SimpleLogger(PrintStream output) {
        this(output, defaultLogLevel);
    }

    /**
     * Constructor
     * 
     * @param logLevel
     *            the {@link LogLevel} of the Logger
     */
    public SimpleLogger(LogLevel logLevel) {
        this(System.err, logLevel);
    }

    /**
     * Constructor
     * 
     */
    public SimpleLogger() {
        this(defaultLogLevel);
    }

    /**
     * Sets the {@link LogLevel} of the {@link SimpleLogger}
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
     * Gets the {@link LogLevel} of the {@link SimpleLogger}
     * 
     * @return the {@link LogLevel}
     */
    public LogLevel getLogLevel() {
        synchronized (this.lock) {
            return this.logLevel;
        }
    }

    /**
     * Set, whether or not the position is to be shown
     * 
     * @param showPosition
     *            the given boolean value
     */
    public void setShowPosition(boolean showPosition) {
        synchronized (this.lock) {
            this.showPosition = showPosition;
        }
    }

    /**
     * Gets, whether or not the position is shown.
     * 
     * @return <code>true</code> &rarr; position is shown <br>
     *         <code>false</code> &rarr; position is not shown
     */
    public boolean getShowPosition() {
        synchronized (this.lock) {
            return this.showPosition;
        }
    }

    @Override
    public void log(LogLevel level, String message, Exception exception) {
        synchronized (this.lock) {
        	if (getShowPosition()) {
        		log(new LogMessage(level, message, exception));
        	} else {
        		log(new LogMessage(level, message, exception, emptyStackTrace));
        	}
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
    public void writeLogMessage(LogMessage message) { //MAYDO: make this protected as in super class
        if (message == null) {
            throw new NullPointerException("message == null");
        }

        synchronized (this.lock) {
            if (message.getLevel().compareTo(getLogLevel()) <= 0) {
                this.output.println(message.toString(getShowPosition()));
            }
        }
    }
}
