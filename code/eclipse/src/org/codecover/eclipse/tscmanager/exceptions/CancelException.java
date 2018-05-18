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

package org.codecover.eclipse.tscmanager.exceptions;

import org.eclipse.core.runtime.IProgressMonitor;

/**
 * Thrown when a task was canceled during execution as a consequence of
 * {@link IProgressMonitor#isCanceled()} being <code>true</code>.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: CancelException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
@SuppressWarnings("serial")
public class CancelException extends Exception {

    /**
     * Constructs a <code>CancelException</code> with a detail message which
     * should contain a description of the canceled task and the changes the
     * task could already perform.
     * 
     * @param message   the detail message which should contain a description of
     *                  the canceled task and the changes the task could already
     *                  perform
     */
    public CancelException(String message) {
        super(message);
    }

    /**
     * Constructs a <code>CancelException</code> with a detail message which
     * should contain a description of the canceled task and the changes the
     * task could already perform or if none were directly performed by the
     * task, a reference to the cause (which should contain the changes or a
     * reference to another cause).
     * 
     * @param message   the detail message which should contain a description of
     *                  the canceled task and the changes the task could already
     *                  perform or if none were directly performed by the
     *                  task, a reference to the cause which was received from a
     *                  canceled subtask
     * 
     * @param cause     the cause of canceling the task which was received from
     *                  a canceled subtask
     */
    public CancelException(String message, CancelException cause) {
        super(message, cause);
    }

}
