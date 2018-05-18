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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A message that can be logged using a {@link Logger}.
 *
 * @author Steffen Kieß
 * @version 1.0 ($Id: LogMessage.java 186 2013-05-20 19:18:39Z langaujs $)
 */
public final class LogMessage {
    private final LogLevel level;

    private final String message;

    private final Exception exception;

    private final List<StackTraceElement> stackTrace;

    /**
     * Constructor
     *
     * @param level
     *            the {@link LogLevel} of the {@link LogMessage}
     * @param message
     *            the message of the {@link LogMessage}
     * @param exception
     *            the {@link Exception} of the {@link LogMessage}
     * @param stackTrace
     *            the list of {@link StackTraceElement}s of the
     *            {@link LogMessage}
     */
    public LogMessage(LogLevel level, String message, Exception exception,
            List<StackTraceElement> stackTrace) {
        if (level == null) {
            throw new NullPointerException("level == null");
        }

        if (message == null) {
            throw new NullPointerException("message == null");
        }

        if (stackTrace == null) {
            throw new NullPointerException("stackTrace == null");
        }

        this.level = level;
        this.message = message;
        this.exception = exception;
        this.stackTrace = stackTrace;
    }

    /**
     * Calls {@link #LogMessage(LogLevel, String, Exception, List)}
     *
     * @param level
     *            the {@link LogLevel} of the {@link LogMessage}
     * @param message
     *            the message of the {@link LogMessage}
     * @param stackTrace
     *            the list of {@link StackTraceElement}s of the
     *            {@link LogMessage}
     */
    public LogMessage(LogLevel level, String message,
            List<StackTraceElement> stackTrace) {
        this(level, message, null, stackTrace);
    }

    /**
     * Calls {@link #LogMessage(LogLevel, String, Exception, List)}
     *
     * @param level
     *            the {@link LogLevel} of the {@link LogMessage}
     * @param message
     *            the message of the {@link LogMessage}
     * @param exception
     *            the {@link Exception} of the {@link LogMessage}
     */
    public LogMessage(LogLevel level, String message, Exception exception) {
        this(level, message, exception, createStackTrace());
    }

    /**
     * Calls {@link #LogMessage(LogLevel, String, Exception, List)}
     *
     * @param level
     *            the {@link LogLevel} of the {@link LogMessage}
     * @param message
     *            the message of the {@link LogMessage}
     */
    public LogMessage(LogLevel level, String message) {
        this(level, message, (Exception) null);
    }

    private static List<StackTraceElement> createStackTrace() {
        final StackTraceElement[] stackTrace = Thread.currentThread()
                .getStackTrace();
        final List<StackTraceElement> stackTraceList = new ArrayList<StackTraceElement>();

        int localStackFramesToDiscard = 0;
        while (localStackFramesToDiscard < stackTrace.length
                 && (stackTrace[localStackFramesToDiscard].getClassName().equals(LogMessage.class.getName())
                     || stackTrace[localStackFramesToDiscard].getClassName().equals(SimpleLogger.class.getName())
                     || stackTrace[localStackFramesToDiscard].getClassName().equals(Logger.class.getName())
                     || stackTrace[localStackFramesToDiscard].getClassName().equals(Thread.class.getName()))) {
	    localStackFramesToDiscard++;
            /* MAYDO tricky loop find out if we want to do more here ... */
        }

        for (int i = localStackFramesToDiscard; i < stackTrace.length; i++) {
            stackTraceList.add(stackTrace[i]);
        }

        return Collections.unmodifiableList(stackTraceList);
    }

    /**
     * Gets the {@link LogLevel} of the {@link LogMessage}
     *
     * @return the {@link LogLevel}
     */
    public LogLevel getLevel() {
        return this.level;
    }

    /**
     * Gets the message of the {@link LogMessage}
     *
     * @return the message.
     */
    public String getMessage() {
        return this.message;
    }

    /**
     * Gets the {@link Exception} of the {@link LogMessage}
     *
     * @return the {@link Exception}
     */
    public Exception getException() {
        return this.exception;
    }

    /**
     * Gets the stackTrace of the {@link LogMessage}
     *
     * @return the list of {@link StackTraceElement}s
     */
    public List<StackTraceElement> getStackTrace() {
        return this.stackTrace;
    }

    /**
     * Calls {@link #toString(boolean)} with true as the parameter
     *
     * @see #toString(boolean)
     * @return the value of toString(true)
     */
    @Override
    public String toString() {
        return toString(true);
    }

    /**
     * Gets a string of the following format<br>
     * <br>
     * [Loglevel] message (at position) <br>
     * <br>
     * as well as the stackTrace of the exception, if it is not
     * <code>null</code>
     *
     * @param showPosition
     *            <br>
     *            <code>true</code> &rarr; (at position) is shown <br>
     *            <code>false</code> &rarr; (at position) is not shown
     * @return the formatted string.
     */
    public String toString(boolean showPosition) {
        String position = "";
        if (showPosition && !getStackTrace().isEmpty()) {
            position = " (at " + getStackTrace().get(0) + ")";
        }

        if (getException() == null) {
            return "[" + getLevel() + "] " + getMessage() + position;
        } else {
            final StringWriter sw = new StringWriter();
            final PrintWriter out = new PrintWriter(sw);
            getException().printStackTrace(out);
            final StringBuffer sb = sw.getBuffer();
            for (int i = sb.length() - 1; i > 0; i--) {
            	char c = sb.charAt(i);
            	if (c == '\n' || c == '\r')
            		continue;
            	else {
            		sb.setLength(i);
            		break;
            	}
            }
            
            final String stackTrace = new String(sb);

            return "[" + getLevel() + "] " + getMessage() + position + ": "
                    + stackTrace;
        }
    }
}
