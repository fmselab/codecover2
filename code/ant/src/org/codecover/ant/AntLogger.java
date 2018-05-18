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

package org.codecover.ant;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.codecover.model.utils.LogLevel;
import org.codecover.model.utils.LogMessage;
import org.codecover.model.utils.Logger;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: AntLogger.java 50 2009-06-01 09:59:34Z ahija $)
 */
public class AntLogger extends Logger {
    private final Project project;

    /**
     * Converts the {@link LogLevel} to the respective ant level
     *
     * @param level
     *                the {@link LogLevel} to convert.
     * @return the ant level.
     */
    public static int convertLevel(LogLevel level) {
        switch (level) {
        case FATAL:
        case ERROR:
            return Project.MSG_ERR;

        case WARNING:
            return Project.MSG_WARN;

        case INFO:
            return Project.MSG_INFO;

        case DEBUG:
            return Project.MSG_DEBUG;

        default:
            return Project.MSG_INFO;
        }
    }

    /**
     * Constructor
     *
     * @param project
     *                the given {@link Project} to log.
     *
     */
    public AntLogger(Project project) {
        this.project = project;
    }

    @Override
    public void writeLogMessage(LogMessage message) {
        if (message == null) {
            throw new NullPointerException("message == null");
        }

        if (message.getLevel() == LogLevel.FATAL) {
            throw new BuildException(message.getMessage(), message.getException());
        }

        this.project.log(message.toString(false), convertLevel(message.getLevel()));
    }
}
