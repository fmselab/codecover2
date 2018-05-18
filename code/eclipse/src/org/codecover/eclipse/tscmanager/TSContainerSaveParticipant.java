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

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.tscmanager.exceptions.CancelException;
import org.codecover.eclipse.tscmanager.exceptions.TSCQueuedSaveException;
import org.codecover.model.utils.Logger;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/**
 * Saves the known test session containers if Eclipse requests to do so.
 * 
 * @see TSContainerManagerSaveParticipantHandler
 * @author Robert Hanussek
 * @version 1.0 ($Id: TSContainerSaveParticipant.java 49 2009-06-01 08:52:19Z ahija $)
 */
public class TSContainerSaveParticipant
  implements SaveParticipant {

  private final TSContainerManager tscManager;

  private final TSContainerStorage tscStorage;

  private final Logger logger;

  /**
   * Constructor.
   * 
   * @param tscManager the {@link TSContainerManager}
   * @param tscStorage the storage class of the test session container.
   * @param logger the given {@link Logger} to use.
   */
  public TSContainerSaveParticipant(TSContainerManager tscManager, TSContainerStorage tscStorage,
    Logger logger) {
    this.tscManager = tscManager;
    this.tscStorage = tscStorage;
    this.logger = logger;
  }

  @Override
public void prepareToSave(ISaveContext context) throws CoreException {
    // nothing to do here, we're always prepared ;)
  }

  @Override
public boolean saving(ISaveContext context) throws CoreException {
    switch (context.getKind()) {
      case ISaveContext.FULL_SAVE:
        return this.performFullSave(context);
      case ISaveContext.PROJECT_SAVE:
        return this.performProjectSave(context);
      case ISaveContext.SNAPSHOT:
        /*
         * snapshot savings must be very fast thus saving a test session container (which is a long-running
         * operation) can't be done here
         */
        return false;
    }
    return false;
  }

  @Override
public void doneSaving(ISaveContext context) {
    // nothing to do here
  }

  @Override
public void rollback(ISaveContext context) {
    // nothing to do here
  }

  private boolean performFullSave(ISaveContext context) throws CoreException {
    CoreException exception = null;
    boolean performedSaveOfActive = false;
    boolean performedQueuedSave = false;
    // save active test session container
    try {
      performedSaveOfActive = this.tscManager.saveActiveTSContainer(false, null);
    }
    /*
     * catch TSCFileCreateException, FileSaveException and OutOfMemoryError and wrap them into a CoreException
     */
    catch (Throwable t) {
      exception =
        new CoreException(new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID, IStatus.OK,
          "Error while saving active" + //$NON-NLS-1$
            " test session container", //$NON-NLS-1$
          t));
    }
    // perform queued saves
    try {
      synchronized (this.tscManager.getWriteLock()) {
        performedQueuedSave = this.tscStorage.getSaveQueue().saveQueued(null);
      }
    } catch (TSCQueuedSaveException e) {
      logger.error("Error while performing queued" + //$NON-NLS-1$
        " saves of test session" + //$NON-NLS-1$
        " containers", //$NON-NLS-1$
        e);
      if (exception == null) {
        exception =
          new CoreException(new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID, IStatus.OK,
            "Error while performing queued" + //$NON-NLS-1$
              " saves of test session" + //$NON-NLS-1$
              " containers", //$NON-NLS-1$
            e));
      }
    } catch (CancelException e) {
      /*
       * ignore because it can't happen if no progress monitor was passed
       */
    }
    if (exception != null) {
      throw exception;
    }
    return performedSaveOfActive || performedQueuedSave;
  }

  private boolean performProjectSave(ISaveContext context) throws CoreException {
    IProject project = context.getProject(); // the saved project
    ActiveTSContainerInfo activeTSCInfo;
    CoreException exception = null;
    boolean performedSaveOfActive = false;
    boolean performedQueuedSave = false;
    // save active test session container if it belongs to the saved project
    synchronized (this.tscManager.getWriteLock()) {
      activeTSCInfo = this.tscManager.getActiveTSContainer();
      if (activeTSCInfo != null && activeTSCInfo.getFile().getProject().equals(project)) {
        try {
          performedSaveOfActive = this.tscManager.saveActiveTSContainer(false, null);
        }
        /*
         * catch TSCFileCreateException, FileSaveException and OutOfMemoryError and wrap them into a
         * CoreException
         */
        catch (Throwable t) {
          exception =
            new CoreException(new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID, IStatus.OK,
              "Error while saving active" + //$NON-NLS-1$
                " test session container", //$NON-NLS-1$
              t));
        }
      }
    }
    // save queued test session containers which belong to the saved project
    try {
      synchronized (this.tscManager.getWriteLock()) {
        performedQueuedSave = this.tscStorage.getSaveQueue().saveQueued(project, null);
      }
    } catch (TSCQueuedSaveException e) {
      logger.error("Error while performing" + //$NON-NLS-1$
        " queued saves of" + //$NON-NLS-1$
        " test session containers", e); //$NON-NLS-1$
      if (exception == null) {
        exception =
          new CoreException(new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID, IStatus.OK,
            "Error while performing queued" + //$NON-NLS-1$
              " saves of test session" + //$NON-NLS-1$
              " containers", //$NON-NLS-1$
            e));
      }
    }
    if (exception != null) {
      throw exception;
    }
    return performedSaveOfActive || performedQueuedSave;
  }

}
