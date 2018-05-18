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

package org.codecover.model.exceptions;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: FileVersionException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class FileVersionException extends ModelException {
    private static final long serialVersionUID = 6562542662675523971L;

    private final int major;

    private final int minor;

    /**
     * Construct a new FileVersionException object with <code>message</code>
     * as its detail message, as well as the version components.
     * 
     * @param message
     *            The message for this exception.
     * @param major
     *            the major component of the version.
     * @param minor
     *            the minor component of the version.
     */
    public FileVersionException(String message, int major, int minor) {
        super(message);
        this.major = major;
        this.minor = minor;
    }

    /**
     * Constructs a new FileVersionException with the specified
     * <code>message</code> and <code>cause</code>, as well as the version
     * components.
     * 
     * @param message
     *            The message for this exception.
     * @param cause
     *            The cause of this exception.
     * @param major
     *            the major component of the version.
     * @param minor
     *            the minor component of the version.
     */
    public FileVersionException(String message, Throwable cause, int major,
            int minor) {
        super(message, cause);
        this.major = major;
        this.minor = minor;
    }

    /**
     * Gets the major.
     * 
     * @return the major
     */
    public final int getMajor() {
        return this.major;
    }

    /**
     * Gets the minor.
     * 
     * @return the minor
     */
    public final int getMinor() {
        return this.minor;
    }
}
