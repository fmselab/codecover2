/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.eclipse;

import org.codecover.model.utils.LogLevel;
import org.codecover.model.utils.LogMessage;
import org.codecover.model.utils.Logger;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;

/**
 * Hands logging messages over to the Logger of Eclipse. Eclipse sometimes
 *  displays them in a different order!
 * 
 * @author Steffen Kieß
 * @author Robert Hanussek
 * @version 1.0 ($Id: EclipseLogger.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class EclipseLogger extends Logger {
    private final ILog output;

    private LogLevel logLevel;

    private final String pluginID;

    private boolean showPosition = true;

    private final Object lock = new Object();

    /**
     * Constructor
     * 
     * @param output
     *            the PrintStream used for writing output
     * @param logLevel
     *            the {@link LogLevel} of the Logger
     * @param pluginID
     *            the id of the plugin
     */
    public EclipseLogger(ILog output, LogLevel logLevel, String pluginID) {
        if (output == null) {
            throw new NullPointerException("output == null"); //$NON-NLS-1$
        }

        if (logLevel == null) {
            throw new NullPointerException("logLevel == null"); //$NON-NLS-1$
        }

        if (pluginID == null) {
            throw new NullPointerException("pluginID == null"); //$NON-NLS-1$
        }

        this.output = output;
        this.logLevel = logLevel;
        this.pluginID = pluginID;
    }

    /**
     * Sets the {@link LogLevel} of the {@link EclipseLogger}
     * 
     * @param logLevel
     *            the given {@link LogLevel}
     */
    public void setLogLevel(LogLevel logLevel) {
        if (logLevel == null) {
            throw new NullPointerException("logLevel == null"); //$NON-NLS-1$
        }

        synchronized (this.lock) {
            this.logLevel = logLevel;
        }
    }

    /**
     * Gets the {@link LogLevel} of the {@link EclipseLogger}
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
            throw new NullPointerException("message == null"); //$NON-NLS-1$
        }

        if (message.getLevel().compareTo(this.getLogLevel()) <= 0) {
            // map LogLevels to Status types
            switch (message.getLevel()) {
                case DEBUG:
                    this.output.log(new Status(IStatus.INFO,
                            this.pluginID,
                            IStatus.OK,
                            "[DEBUG] " + message.getMessage(), //$NON-NLS-1$
                            message.getException()));
                    break;
                case INFO:
                    this.output.log(new Status(IStatus.INFO,
                            this.pluginID,
                            IStatus.OK,
                            message.getMessage(),
                            message.getException()));
                    break;
                case WARNING:
                    this.output.log(new Status(IStatus.WARNING,
                            this.pluginID,
                            IStatus.OK,
                            message.getMessage(),
                            message.getException()));
                    break;
                case ERROR:
                    this.output.log(new Status(IStatus.ERROR,
                            this.pluginID,
                            IStatus.OK,
                            message.getMessage(),
                            message.getException()));
                    break;
                case FATAL:
                    final Status st = new Status(IStatus.ERROR,
                            this.pluginID,
                            IStatus.OK,
                            "[FATAL] " + message.getMessage(), //$NON-NLS-1$
                            message.getException());
                    this.output.log(st);
                    Display.getDefault().asyncExec(new Runnable() {
                        @Override
						public void run() {
                            ErrorDialog.openError(null,
                                    "CodeCover Fatal Error", null, //$NON-NLS-1$
                                    st);
                        }
                    });
                    break;
                default:
                    /* LogLevel unknown */
                    this.output.log(new Status(IStatus.ERROR,
                            this.pluginID,
                            IStatus.OK,
                            "[Unknown LogLevel] " //$NON-NLS-1$
                                    + message.getMessage(),
                            message.getException()));
            }
        }
    }
}
