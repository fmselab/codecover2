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
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.exceptions.CancelException;
import org.codecover.eclipse.tscmanager.exceptions.TSCFileCreateException;
import org.codecover.eclipse.tscmanager.exceptions.TSCQueuedSaveException;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileSaveException;
import org.codecover.model.utils.CollectionUtil;
import org.codecover.model.utils.Logger;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;

/**
 * The queue which contains the save operations which are queued because they can't be performed instantly
 * when they are a reaction on a project-close event. The reason for this is that the workspace is locked
 * during this event. The save operation is represented as a pair of the <code>TestSessionContainer</code>
 * to save and its <code>TSContainerInfo</code>-representation.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: TSContainerSaveQueue.java 49 2009-06-01 08:52:19Z ahija $)
 */
public class TSContainerSaveQueue {

  private static final String MONITOR_SAVING_QUEUED_TEST_SESSION_CONTAINER =
    Messages.getString("TSContainerSaveQueue.MONITOR_SAVING_QUEUED_TEST_SESSION_CONTAINER"); //$NON-NLS-1$

  private final TSContainerStorage tscStorage;

  private final Map<TSContainerInfo, TestSessionContainer> queue;

  private final Logger logger;

  TSContainerSaveQueue(TSContainerStorage tscStorage) {
    if (tscStorage == null) {
      throw new NullPointerException("tscStorage mustn't be null"); //$NON-NLS-1$
    }

    this.tscStorage = tscStorage;
    this.queue = Collections.synchronizedMap(new HashMap<TSContainerInfo, TestSessionContainer>());
    this.logger = this.tscStorage.getLogger();
  }

  void queueSave(TSContainerInfo tscInfo, TestSessionContainer tsc) {
    if (tscInfo == null) {
      throw new NullPointerException("tscInfo mustn't be null"); //$NON-NLS-1$
    }
    if (tsc == null) {
      throw new NullPointerException("tsc mustn't be null"); //$NON-NLS-1$
    }

    /*
     * if the test session container is already in the queue its entry is replaced with this one
     */
    this.queue.put(tscInfo, tsc);
  }

  /* public */void removeSaveFromQueue(TSContainerInfo tscInfo) {
    this.queue.remove(tscInfo);
  }

  /**
   * Locking of <code>TSContainerManager</code> must be done by the calling context.
   */
  boolean saveQueued(IProgressMonitor monitor) throws CancelException, TSCQueuedSaveException {
    Set<TSContainerInfo> queuedTSCInfos;
    List<Throwable> exceptions = new ArrayList<Throwable>();
    String cancelDescr = "Canceled activation of test " + //$NON-NLS-1$
      " session container.\nChanges" + //$NON-NLS-1$
      " happened so far:\n"; //$NON-NLS-1$
    boolean performedSave = false;
    final int monitorScale = 1000;
    monitor = (monitor != null) ? monitor : new NullProgressMonitor();
    /*
     * copy keyset to avoid problems due to modifications of queue while iterating over its keyset. if we
     * wouldn't copy it, we would have to put the whole for-loop inside the sync-block, which would block the
     * save queue until every pending save is performed
     */
    synchronized (this.queue) {
      queuedTSCInfos = CollectionUtil.copy(this.queue.keySet());
    }
    try { // this try only ensures that the monitor is left done()
      monitor.beginTask(MONITOR_SAVING_QUEUED_TEST_SESSION_CONTAINER, queuedTSCInfos.size() * 1
        * monitorScale);
      if (monitor.isCanceled()) {
        throw new CancelException(cancelDescr + "None."); //$NON-NLS-1$
      }
      for (TSContainerInfo tscInfo : queuedTSCInfos) {
        try {
          this.saveQueued(tscInfo, new SubProgressMonitor(monitor, 1 * monitorScale));
        }
        /*
         * catch TSCFileCreateException, FileSaveException and OutOfMemoryError, log them and collect them in
         * a list which will be thrown as a TSCQueuedSaveException
         */
        catch (Throwable t) {
          exceptions.add(t);
          logger.error("Error while performing queued" + //$NON-NLS-1$
            " save of test session container: " //$NON-NLS-1$
            + tscInfo.getPath().toString(), new InvocationTargetException(t));
        }
        cancelDescr += "Saved test session container: " //$NON-NLS-1$
          + tscInfo.getPath().toString() + "\n"; //$NON-NLS-1$
        if (monitor.isCanceled()) {
          throw new CancelException(cancelDescr);
        }
        performedSave = true;
      }
      // throw all occurred exceptions subsumed in a TSCQueuedSaveException
      if (!exceptions.isEmpty()) {
        throw new TSCQueuedSaveException(exceptions);
      }
    } finally {
      monitor.done();
    }
    return performedSave;
  }

  /**
   * Locking of <code>TSContainerManager</code> must be done by the calling context.
   */
  boolean saveQueued(IProject project, IProgressMonitor monitor) throws TSCQueuedSaveException {
    Set<TSContainerInfo> queuedTSCInfos;
    List<Throwable> exceptions = new ArrayList<Throwable>();
    boolean performedSave = false;
    final int monitorScale = 1000;
    monitor = (monitor != null) ? monitor : new NullProgressMonitor();
    /*
     * copy keyset to avoid problems due to modifications of queue while iterating over its keyset. if we
     * wouldn't copy it, we would have to put the whole for-loop inside the sync-block, which would block the
     * save queue until every pending save is performed
     */
    synchronized (this.queue) {
      queuedTSCInfos = CollectionUtil.copy(this.queue.keySet());
    }
    try { // this try only ensures that the monitor is left done()
      monitor.beginTask(MONITOR_SAVING_QUEUED_TEST_SESSION_CONTAINER, queuedTSCInfos.size() * 1
        * monitorScale);
      for (TSContainerInfo tscInfo : queuedTSCInfos) {
        if (tscInfo.getFile().getProject().equals(project)) {
          try {
            /*
             * perform the pending save of the test session container, if there is one
             */
            this.saveQueued(tscInfo, new SubProgressMonitor(monitor, 1 * monitorScale));
          }
          /*
           * catch TSCFileCreateException, FileSaveException and OutOfMemoryError, log them and collect them
           * in a list which will be thrown as a TSCQueuedSaveException
           */
          catch (Throwable t) {
            exceptions.add(t);
            logger.error("Error while performing queued" + //$NON-NLS-1$
              " save of test session container: " //$NON-NLS-1$
              + tscInfo.getFile().getFullPath().toString(), new InvocationTargetException(t));
          }
          performedSave = true;
        } else {
          monitor.worked(1 * monitorScale);
        }
      }
      // throw all occurred exceptions subsumed in a TSCQueuedSaveException
      if (!exceptions.isEmpty()) {
        throw new TSCQueuedSaveException(exceptions);
      }
    } finally {
      monitor.done();
    }
    return performedSave;
  }

  /**
   * Performs the pending save (if there is any) of a test session container. There won't be any retries if
   * the saving fails (the save is removed from the queue in any case, success or failure). Locking of
   * <code>TSContainerManager</code> must be done by the calling context.
   * 
   * @param tscInfo the <code>TSContainerInfo</code>-representation of the test session container to save
   * @throws TSCFileCreateException (see {@link TSContainerStorage#saveTSContainer(TestSessionContainer, 
   *         TSContainerInfo, boolean, IProgressMonitor)}
   * @throws FileSaveException (see {@link TSContainerStorage#saveTSContainer(TestSessionContainer, 
   *         TSContainerInfo, boolean, IProgressMonitor)}
   */
  void saveQueued(TSContainerInfo tscInfo, IProgressMonitor monitor)
    throws TSCFileCreateException, FileSaveException, OutOfMemoryError {
    TestSessionContainer tsc;
    final int monitorScale = 1000;
    monitor = (monitor != null) ? monitor : new NullProgressMonitor();
    try { // this try only ensures that the monitor is left done()
      monitor.beginTask(MONITOR_SAVING_QUEUED_TEST_SESSION_CONTAINER, 1 * monitorScale);
      if (tscInfo == null || (tsc = this.queue.get(tscInfo)) == null) {
        monitor.worked(1 * monitorScale); // #1
        return;
      }
      logger.debug("Performing queued save: " //$NON-NLS-1$
        + tscInfo.getFile().getFullPath().toString());
      /*
       * may throw TSCFileCreateException, FileSaveException, OutOfMemoryError
       */
      this.tscStorage.saveTSContainer(tsc, tscInfo, false, new SubProgressMonitor(monitor, 1 * monitorScale)); // #1
    } finally {
      monitor.done();
    }
  }

}
