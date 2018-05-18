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

package org.codecover.eclipse.tscmanager;

import java.lang.reflect.InvocationTargetException;

import org.codecover.eclipse.tscmanager.exceptions.CancelException;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * This interface is designed to provide a common protocol for objects that wish to execute a task on the
 * active test session container.
 * 
 * @see TSContainerManager#setActiveTSContainer(TSContainerInfo, ActiveTSContainerRunnable,
 *      org.eclipse.core.runtime.IProgressMonitor)
 * @author Robert Hanussek
 * @version 1.0 ($Id: ActiveTSContainerRunnable.java 49 2009-06-01 08:52:19Z ahija $)
 */
public interface ActiveTSContainerRunnable {

  /**
   * Returns a description which describes the task this runnable executes. This description will be display
   * in a progress monitor for example.
   * 
   * @return a description which describes the task this runnable executes or <code>null</code> if the
   *         description of the method, this runnable will be executed in, shall be used.
   */
  public String getDescription();

  /**
   * This method contains the code of the task to execute on the active test session container.
   * 
   * @param activeTSCInfo the <code>ActiveTSContainerInfo</code>-representation of the active test session
   *        container or <code>null</code> if none is active
   * @param monitor the progress monitor to use to display progress and receive requests for cancelation
   *        (mustn't be <code>null</code>)
   * @throws InvocationTargetException if the run method must propagate an exception, it should wrap it inside
   *         an <code>InvocationTargetException</code>; runtime exceptions are automatically wrapped in an
   *         <code>InvocationTargetException</code> by the calling context
   * @throws CancelException if the operation detects a request to cancel, using
   *         {@link IProgressMonitor#isCanceled()}, it should exit by throwing <code>CancelException</code>
   */
  public void run(ActiveTSContainerInfo activeTSCInfo, IProgressMonitor monitor)
    throws InvocationTargetException, CancelException;

}
