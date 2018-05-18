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
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.exceptions.CancelException;
import org.codecover.eclipse.tscmanager.exceptions.TSCFileCreateException;
import org.codecover.eclipse.tscmanager.exceptions.TSContainerManagerModifyException;
import org.codecover.model.TestCase;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.exceptions.FileSaveException;
import org.codecover.model.utils.CollectionUtil;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.NameReoccurenceHelper;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.resources.ISavedState;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;

/**
 * A <code>TSContainerManager</code> contains all known test session containers and informs
 * <code>TSContainerManagerListener</code>s of changes to the known test session containers.
 * <h3>Terminology</h3>
 * A <em>known test session container</em> is a test session container (TSC) which is contained in the
 * <code>TSContainerManager</code> and which is represented by a <code>TSContainerInfo</code>. Every
 * known TSC belongs to exactly one specific project.
 * 
 * @see TestSessionContainer
 * @author Robert Hanussek, Markus Wittlinger
 * @version 1.0 ($Id: TSContainerManager.java 49 2009-06-01 08:52:19Z ahija $)
 */
public class TSContainerManager {

  private static final String MONITOR_ADDING_TEST_SESSION_CONTAINER =
    Messages.getString("TSContainerManager.MONITOR_ADDING_TEST_SESSION_CONTAINER"); //$NON-NLS-1$

  private static final String MONITOR_REMOVING_TEST_SESSION_CONTAINERS =
    Messages.getString("TSContainerManager.MONITOR_REMOVING_TEST_SESSION_CONTAINERS"); //$NON-NLS-1$

  private static final String MONITOR_ACTIVATING_TEST_SESSION_CONTAINER =
    Messages.getString("TSContainerManager.MONITOR_ACTIVATING_TEST_SESSION_CONTAINER"); //$NON-NLS-1$

  private static final String MONITOR_LOADING_AN_OTHER_TEST_SESSION_CONTAINER =
    Messages.getString("TSContainerManager.MONITOR_LOADING_AN_OTHER_TEST_SESSION_CONTAINER"); //$NON-NLS-1$

  private static final String MONITOR_REMOVING_TSCS_INCL_UNLOADABLE =
    Messages.getString("TSContainerManager.MONITOR_REMOVING_TSCS_INCL_UNLOADABLE"); //$NON-NLS-1$

  private static final String MONITOR_SAVING_ACTIVE_TEST_SESSION_CONTAINER =
    Messages.getString("TSContainerManager.MONITOR_SAVING_TEST_SESSION_CONTAINER"); //$NON-NLS-1$

  private static final String MONITOR_DELETING_TEST_SESSION_CONTAINERS =
    Messages.getString("TSContainerManager.MONITOR_DELETING_TEST_SESSION_CONTAINERS"); //$NON-NLS-1$

  /**
   * List of known test session containers represented as <code>TSContainerInfo</code>s.
   */
  final List<TSContainerInfo> tscInfos;

  /**
   * The <code>ActiveTSContainerInfo</code>-representation of the active test session container which
   * <code>TestCase</code>s are visualized in the plugin's views. (The active test session container is the
   * one which is selected in the combo box of the Test Sessions view.)
   */
  private ActiveTSContainerInfo activeTSCInfo;

  private final TSContainerStorage tscStorage;

  private final ActiveTestCasesStorage testCasesStorage;

  private final TSContainerManagerSaveParticipantHandler saveParticipantHandler;

  private final TSContainerManagerListenerHandler listenerHandler;

  final private Object writeLock;

  final private Object readLock;

  boolean locked;

  /**
   * The logger.
   */
  private final Logger logger;

  /**
   * Constructs a <code>TSContainerManager</code>.
   * 
   * @param logger the logger, mustn't be <code>null</code>
   * @throws CoreException if the save participant couldn't be registered
   */
  public TSContainerManager(Logger logger) throws CoreException {
    ISavedState lastState = null;
    String pathToLastActiveTSC = null;
    TSContainerInfo lastActiveTSC;
    CoreException registerSaveParticipantException = null;

    if (logger == null) {
      throw new NullPointerException("logger mustn't be null"); //$NON-NLS-1$
    }
    this.logger = logger;

    this.tscInfos = new LinkedList<TSContainerInfo>();
    this.activeTSCInfo = null;

    this.tscStorage = new TSContainerStorage(this);
    this.testCasesStorage = new ActiveTestCasesStorage(logger);

    this.saveParticipantHandler = new TSContainerManagerSaveParticipantHandler(logger);
    this.saveParticipantHandler.add(new TSContainerSaveParticipant(this, this.tscStorage, logger));
    this.saveParticipantHandler.add(new ActiveTestCasesSaveParticipant(this, this.testCasesStorage));

    this.listenerHandler = new TSContainerManagerListenerHandler(this);

    this.writeLock = new Object();
    this.readLock = new Object();
    this.setLocked(false);

    try {
      lastState =
        ResourcesPlugin.getWorkspace().addSaveParticipant(CodeCoverPlugin.getDefault(),
          this.getSaveParticipant());
    } catch (CoreException e) {
      registerSaveParticipantException = e;
      lastState = null;
    }

    /*
     * must happen before the test session containers are loaded so that method this.add (which is called by
     * method this.loadAllTestSessionContainers) can unbuffer the active test cases (via method
     * this.testCasesStorage.unbufferActiveTestCases)
     */
    if (lastState != null) {
      pathToLastActiveTSC = this.testCasesStorage.loadAllActiveTestCases(lastState);
    }

    // load all test session containers of open projects
    this.loadAllTestSessionContainers();

    // set last active test session container
    if (pathToLastActiveTSC != null && (lastActiveTSC = this.getTSContainer(pathToLastActiveTSC)) != null) {
      try {
        this.setActiveTSContainer(lastActiveTSC, null, null);
      } catch (FileLoadException e) {
        logger.error("Couldn't load last active test session" + //$NON-NLS-1$
          " container: " + pathToLastActiveTSC, e); //$NON-NLS-1$
      } catch (OutOfMemoryError e) {
        logger.error("Out of memory while loading last active" + //$NON-NLS-1$
          " test session container: " //$NON-NLS-1$
          + pathToLastActiveTSC, new InvocationTargetException(e));
      } catch (InvocationTargetException e) {
        // can't be thrown if no monitor is passed
      } catch (CancelException e) {
        // can't be thrown if no monitor is passed
      }
    }

    /*
     * register for changes on the workspace, including: - adding of test session container files - removing
     * of test session container files (ignored) - adding/removing of projects - opening/closing of projects
     * (handled the same way as the prev. one) - changing of test session container files (ignored, for reason
     * see Javadoc of method WorkspaceListener)
     */
    new WorkspaceListener(this, this.testCasesStorage);

    if (registerSaveParticipantException != null) {
      throw registerSaveParticipantException;
    }
  }

  /**
   * Returns (always the same) logger of this <code>TSContainerManager</code>.
   * 
   * @return the logger of this <code>TSContainerManager</code>, which doesn't change over time
   */
  Logger getLogger() {
    return this.logger;
  }

  /**
   * Sets the active <code>TestSessionContainer</code>, that is the <code>TestSessionContainer</code>
   * which <code>TestCase</code>s are visualized in the plugin's views.
   * 
   * @param tscInfo the <code>TSContainerInfo</code> which represents the <code>TestSessionContainer</code>
   *        to set as the active one or <code>null</code> if none shall be active
   * @param runnable the runnable to execute after the <code>TestSessionContainer</code> was activated or
   *        <code>null</code> if no actions are to be done
   * @param monitor a progress monitor or <code>null</code> if progress reporting is not desired
   * @return the <code>ActiveTSContainerInfo</code>-representation of the new active test session container
   * @throws TSContainerManagerModifyException if a listener calls this method during a change event
   * @throws FileLoadException if the file which contains the <code>TestSessionContainer</code> to activate
   *         can't be loaded
   * @throws OutOfMemoryError if the virtual machine runs out of memory while loading the
   *         <code>TestSessionContainer</code>
   * @throws InvocationTargetException if the runnable threw an exception, use
   *         {@link InvocationTargetException#getCause()} to get the exception of the runnable
   * @throws CancelException if a request to cancel is detected (with the given progress monitor)
   */
  public ActiveTSContainerInfo setActiveTSContainer(TSContainerInfo tscInfo,
    ActiveTSContainerRunnable runnable, IProgressMonitor monitor)
    throws TSContainerManagerModifyException, FileLoadException, OutOfMemoryError, InvocationTargetException,
    CancelException {
    String monitorName;
    final int monitorScale = 1000;
    synchronized (this.getWriteLock()) {
      if (this.isLocked()) {
        throw new TSContainerManagerModifyException("TSContainerManager is locked for" + //$NON-NLS-1$
          " modifications during change event"); //$NON-NLS-1$
      }
      monitor = (monitor != null) ? monitor : new NullProgressMonitor();
      try { // this try only ensures that the monitor is left done()
        if (runnable != null && runnable.getDescription() != null) {
          monitorName = runnable.getDescription();
        } else {
          monitorName = MONITOR_ACTIVATING_TEST_SESSION_CONTAINER;
        }
        monitor.beginTask(monitorName, 3 * monitorScale);

        // if test session container is not already activated, perform saves
        if ((tscInfo == null && this.activeTSCInfo != null)
          || (tscInfo != null && !tscInfo.equals(this.activeTSCInfo))) {
          /*
           * save active test session container (if necessary)
           */
          try {
            this.saveActiveTSContainer(false, new SubProgressMonitor(monitor, 1 * monitorScale)); // #1
          }
          // catch TSCFileCreateException and FileSaveException
          catch (Exception e) {
            if (this.activeTSCInfo != null) {
              logger.error("Error while saving" + //$NON-NLS-1$
                " test session container: " //$NON-NLS-1$
                + this.activeTSCInfo.getFile().getFullPath().toString(), e);
            } else {
              logger.error("Error while saving" + //$NON-NLS-1$
                " test session container" + //$NON-NLS-1$
                " (which is null!?)", //$NON-NLS-1$
                e);
            }
          } catch (OutOfMemoryError e) {
            if (this.activeTSCInfo != null) {
              logger.error("Out of memory while saving" + //$NON-NLS-1$
                " test session container: " //$NON-NLS-1$
                + this.activeTSCInfo.getFile().getFullPath().toString(), new InvocationTargetException(e));
            } else {
              logger.error("Out of memory while saving" + //$NON-NLS-1$
                " test session container" + //$NON-NLS-1$
                " (which is null!?)", //$NON-NLS-1$
                new InvocationTargetException(e));
            }
          }

          /*
           * perform pending save of test session container to activate before loading it
           */
          if (tscInfo != null) {
            try {
              this.tscStorage.getSaveQueue().saveQueued(tscInfo,
                new SubProgressMonitor(monitor, 1 * monitorScale)); // #2
            }
            /*
             * catch TSCFileCreateException, FileSaveException and OutOfMemoryError
             */
            catch (Throwable t) {
              logger.error("Error while saving" + //$NON-NLS-1$
                " test session container: " //$NON-NLS-1$
                + tscInfo.getFile().getFullPath().toString(), (t instanceof Exception) ? (Exception) t
                : new InvocationTargetException(t));
            }
          } else {
            monitor.worked(1 * monitorScale); // #2
          }
        } else {
          monitor.worked(2 * monitorScale); // #1-#2
        }

        /*
         * may throw TSContainerManagerModifyException, FileLoadException, OutOfMemoryError,
         * InvocationTargetException, CancelException
         */
        return this.setActiveTSContainerButDontSave(tscInfo, runnable, new SubProgressMonitor(monitor,
          1 * monitorScale)); // #3
      } finally {
        monitor.done();
      }
    }
  }

  /**
   * Sets the active <code>TestSessionContainer</code>, that is the <code>TestSessionContainer</code>
   * which <code>TestCase</code>s are visualized in the plugin's views.
   * 
   * @param tscInfo the <code>TSContainerInfo</code> which represents the <code>TestSessionContainer</code>
   *        to set as the active one or <code>null</code> if none shall be active
   * @param runnable the runnable to execute after the <code>TestSessionContainer</code> was activated or
   *        <code>null</code> if no actions are to be done; the runnable is only executed if a test session
   *        container is active
   * @param monitor a progress monitor or <code>null</code> if progress reporting is not desired
   * @return the <code>ActiveTSContainerInfo</code>-representation of the new active test session container
   * @throws TSContainerManagerModifyException if a listener calls this method during a change event
   * @throws FileLoadException if the file which contains the <code>TestSessionContainer</code> to activate
   *         can't be loaded
   * @throws OutOfMemoryError if the virtual machine runs out of memory while loading the
   *         <code>TestSessionContainer</code>
   * @throws InvocationTargetException if the runnable threw an exception, use
   *         {@link InvocationTargetException#getCause()} to get the exception of the runnable
   * @throws CancelException if a request to cancel is detected (with the given progress monitor)
   */
  private ActiveTSContainerInfo setActiveTSContainerButDontSave(TSContainerInfo tscInfo,
    ActiveTSContainerRunnable runnable, IProgressMonitor monitor)
    throws TSContainerManagerModifyException, FileLoadException, OutOfMemoryError, InvocationTargetException,
    CancelException {
    TestSessionContainer tsc = null;
    String cancelDescr = "Canceled activation of test " + //$NON-NLS-1$
      " session container.\nChanges" + //$NON-NLS-1$
      " happened so far:\n"; //$NON-NLS-1$
    String monitorName;
    final int monitorScale = 1000;
    monitor = (monitor != null) ? monitor : new NullProgressMonitor();
    try { // this try only ensures that the monitor is left done()
      if (runnable != null && runnable.getDescription() != null) {
        monitorName = runnable.getDescription();
      } else {
        monitorName = MONITOR_ACTIVATING_TEST_SESSION_CONTAINER;
      }
      monitor.beginTask(monitorName, 7 * monitorScale);
      if (monitor.isCanceled()) {
        throw new CancelException(cancelDescr + "None."); //$NON-NLS-1$
      }
      synchronized (this.getWriteLock()) {
        if (this.isLocked()) {
          throw new TSContainerManagerModifyException("TSContainerManager is locked for" + //$NON-NLS-1$
            " modifications during change event"); //$NON-NLS-1$
        }
        if (tscInfo != null && !this.tscInfos.contains(tscInfo)) {
          throw new IllegalStateException("Test session container to activate is" + //$NON-NLS-1$
            " not in list of test session" + //$NON-NLS-1$
            " containers."); //$NON-NLS-1$
        }

        if (tscInfo == null && this.activeTSCInfo == null) {
          monitor.worked(7 * monitorScale); // #1-#7
          return null;
        }
        /*
         * if the given test session container is already active, just execute the runnable
         */
        if (tscInfo != null && tscInfo.equals(this.activeTSCInfo)) {
          monitor.worked(6 * monitorScale); // #1-#6
          try {
            this.executeRunnable(runnable, new SubProgressMonitor(monitor, 1 * monitorScale)); // #7
          } catch (InvocationTargetException e) {
            throw e;
          } catch (CancelException e) {
            throw new CancelException(cancelDescr + "See the cause.", e); //$NON-NLS-1$
          }
          return this.activeTSCInfo;
        }

        monitor.worked(1 * monitorScale); // #1

        monitor.worked(1 * monitorScale); // #2

        if (tscInfo != null) {
          if (monitor.isCanceled()) {
            throw new CancelException(cancelDescr);
          }
          // load test session container from file
          try {
            tsc =
              TSContainerStorage.load(tscInfo.getFile(), true, new SubProgressMonitor(monitor,
                1 * monitorScale)); // #4
          } catch (FileLoadException e) {
            logger.error("Couldn't load test session" + //$NON-NLS-1$
              " container: " //$NON-NLS-1$
              + tscInfo.getFile().getFullPath(), e);
            throw e;
          } catch (OutOfMemoryError e) {
            logger.error("Out of memory while loading" + //$NON-NLS-1$
              " test session container: " //$NON-NLS-1$
              + tscInfo.getFile().getFullPath(), new InvocationTargetException(e));
            throw e;
          }
        } else {
          monitor.worked(2 * monitorScale); // #3, #4
        }
        if (monitor.isCanceled()) {
          throw new CancelException(cancelDescr);
        }
        synchronized (this.getReadLock()) {
          if (tscInfo != null) {
            /*
             * passing null instead of a set of test cases makes the constructor restore the (active) test
             * cases from tscInfo
             */
            this.activeTSCInfo = new ActiveTSContainerInfo(tscInfo, tsc, null);
          } else {
            this.activeTSCInfo = null;
          }
        }
        monitor.worked(1 * monitorScale); // #5

        this.listenerHandler.fireTestSessionContainerActivated(this.activeTSCInfo);
        monitor.worked(1 * monitorScale); // #6

        if (runnable != null && this.activeTSCInfo != null) {
          try {
            this.executeRunnable(runnable, new SubProgressMonitor(monitor, 1 * monitorScale)); // #7
          } catch (InvocationTargetException e) {
            throw e;
          } catch (CancelException e) {
            throw new CancelException(cancelDescr + "See the cause.", e); //$NON-NLS-1$
          }
        }
      }
    } finally {
      monitor.done();
    }

    return this.activeTSCInfo;
  }

  /*
   * methods to add and get known test session containers
   */

  /**
   * Returns the <code>ActiveTSContainerInfo</code>-representation of the active
   * <code>TestSessionContainer</code>, that is the <code>TestSessionContainer</code> which
   * <code>TestCase</code>s are visualized in the plugin's views. Use
   * {@link ActiveTSContainerInfo#getTestSessionContainer()} to get the active
   * <code>TestSessionContainer</code>, but first check the <code>ActiveTSContainerInfo</code> for being
   * non-<code>null</code>!
   * 
   * @return the <code>ActiveTSContainerInfo</code>-representation of the active
   *         <code>TestSessionContainer</code> or <code>null</code> if none is active.
   */
  public ActiveTSContainerInfo getActiveTSContainer() {
    synchronized (this.getReadLock()) {
      return this.activeTSCInfo;
    }
  }

  /**
   * Gets the (known) test session container with the given path.
   * 
   * @param path the path of the test session container to return
   * @return the (known) test session container with the given path
   */
  TSContainerInfo getTSContainer(IPath path) {
    List<TSContainerInfo> tscInfos;
    synchronized (this.getReadLock()) {
      tscInfos = this.getTestSessionContainers();
    }
    for (TSContainerInfo tscInfo : tscInfos) {
      if (tscInfo.getPath().equals(path)) {
        return tscInfo;
      }
    }
    return null;
  }

  /**
   * Gets the (known) test session container with the given path.
   * 
   * @param path the path of the test session container to return, must have been produced by a previous call
   *        to {@link IPath#toPortableString()}.
   * @return the (known) test session container with the given path
   */
  TSContainerInfo getTSContainer(String path) {
    IPath givenPath = Path.fromPortableString(path);
    List<TSContainerInfo> tscInfos;
    synchronized (this.getReadLock()) {
      tscInfos = this.getTestSessionContainers();
    }
    for (TSContainerInfo tscInfo : tscInfos) {
      if (tscInfo.getPath().equals(givenPath)) {
        return tscInfo;
      }
    }
    return null;
  }

  /**
   * Returns an immutable copy of the list of the <code>TSContainerInfo</code>-representations of the known
   * test session containers.
   * 
   * @return an immutable copy of the list of the <code>TSContainerInfo</code>-representations of the known
   *         test session containers.
   */
  public List<TSContainerInfo> getTestSessionContainers() {
    List<TSContainerInfo> tscInfosCopy;
    synchronized (this.getReadLock()) {
      tscInfosCopy = CollectionUtil.copy(this.tscInfos);
    }
    return tscInfosCopy;
  }

  /**
   * Gets the list of {@link TSContainerInfo}s associated with the given {@link IProject}
   * 
   * @param project the given {@link IProject}
   * @return the list of {@link TSContainerInfo}s
   */
  public List<TSContainerInfo> getTestSessionContainers(IProject project) {
    LinkedList<TSContainerInfo> tscInfosByProject = new LinkedList<TSContainerInfo>();
    synchronized (this.getReadLock()) {
      for (TSContainerInfo tscInfo : this.tscInfos) {
        if (tscInfo.getFile().getProject().equals(project)) {
          tscInfosByProject.add(tscInfo);
        }
      }
    }
    return tscInfosByProject;
  }

  /**
   * Gets the list of {@link TSContainerInfo}s associated with the given ID
   * 
   * @param ID the given Id.
   * @return the list of {@link TSContainerInfo}s
   */
  public List<TSContainerInfo> getTestSessionContainers(String ID) {
    LinkedList<TSContainerInfo> tscInfosByProject = new LinkedList<TSContainerInfo>();
    synchronized (this.getReadLock()) {
      for (TSContainerInfo tscInfo : this.tscInfos) {
        if (tscInfo.getId().equals(ID)) {
          tscInfosByProject.add(tscInfo);
        }
      }
    }
    return tscInfosByProject;
  }

  /**
   * Saves the given <code>TestSessionContainer</code> to the CodeCover-folder of the given project, adds it
   * to the list of known <code>TestSessionContainer</code> and activates it if desired or if none is active
   * already. If the test session container is already known it is just activated if desired or if none is
   * active already.
   * 
   * @param tsc the <code>TestSessionContainer</code> to add
   * @param project the project the given <code>TestSessionContainer</code> will be associated with
   * @param forceActivation if <code>true</code> the <code>TestSessionContainer</code> is activated after
   *        being added, if <code>false</code> it is only activated if none is active already
   * @param runnable the runnable to execute after the added <code>TestSessionContainer</code> was activated
   *        or <code>null</code> if no actions are to be done
   * @param monitor a progress monitor or <code>null</code> if progress reporting is not desired
   * @return the <code>TSContainerInfo</code>-representation of the added test session container.
   * @throws TSCFileCreateException if the file to write to can't be created
   * @throws FileSaveException if the test session container can't be written to the file
   * @throws OutOfMemoryError if the java virtual machine is out of memory
   * @throws FileLoadException if the <code>TestSessionContainer</code> can't be read from the contents of
   *         the file
   * @throws InvocationTargetException if the runnable threw an exception, use
   *         {@link InvocationTargetException#getCause()} to get the exception of the runnable
   * @throws CancelException if a request to cancel is detected (with the given progress monitor)
   */
  public TSContainerInfo addTestSessionContainer(TestSessionContainer tsc, IProject project,
    boolean forceActivation, ActiveTSContainerRunnable runnable, IProgressMonitor monitor)
    throws TSCFileCreateException, FileSaveException, OutOfMemoryError, FileLoadException,
    InvocationTargetException, CancelException {
    IFile file;
    synchronized (this.getWriteLock()) {
      file = this.tscStorage.writeTestSessionContainer(tsc, null, project, null, monitor);
    }
    return this.addTestSessionContainer(file, forceActivation, runnable, monitor);
  }

  /**
   * Adds the <code>TestSessionContainer</code> contained in the given file to the list of known test
   * session containers and activates it if desired or if none is active already. If the
   * <code>TestSessionContainer</code> is already known it is just activated if desired or if none is active
   * already. The caller must ensure that the given file is contained in a CodeCover-folder.
   * 
   * @param file the file which contains the <code>TestSessionContainer</code> to add
   * @param forceActivation if <code>true</code> the <code>TestSessionContainer</code> is activated after
   *        being added, if <code>false</code> it is only activated if none is active already
   * @param runnable the runnable to execute after the added <code>TestSessionContainer</code> was activated
   *        or <code>null</code> if no actions are to be done
   * @param monitor a progress monitor or <code>null</code> if progress reporting is not desired
   * @return the <code>TSContainerInfo</code>-representation of the added test session container.
   * @throws FileLoadException if the <code>TestSessionContainer</code> can't be read from the contents of
   *         the file
   * @throws OutOfMemoryError if the virtual machine runs out of memory while loading the
   *         <code>TestSessionContainer</code>
   * @throws InvocationTargetException if the runnable threw an exception, use
   *         {@link InvocationTargetException#getCause()} to get the exception of the runnable
   * @throws CancelException if a request to cancel is detected (with the given progress monitor)
   */
  private TSContainerInfo addTestSessionContainer(IFile file, boolean forceActivation,
    ActiveTSContainerRunnable runnable, IProgressMonitor monitor)
    throws FileLoadException, OutOfMemoryError, InvocationTargetException, CancelException {
    TSContainerInfo tscInfo;
    String monitorName;
    final int monitorScale = 1000;
    monitor = (monitor != null) ? monitor : new NullProgressMonitor();
    try { // this try only ensures that the monitor is left done()
      if (runnable != null && runnable.getDescription() != null) {
        monitorName = runnable.getDescription();
      } else {
        monitorName = MONITOR_ADDING_TEST_SESSION_CONTAINER;
      }
      monitor.beginTask(monitorName, 3 * monitorScale);
      if (monitor.isCanceled()) {
        throw new CancelException("Canceled adding of test session container." + //$NON-NLS-1$
          " Nothing changed."); //$NON-NLS-1$
      }
      synchronized (this.getWriteLock()) {
        if (this.isLocked()) {
          throw new TSContainerManagerModifyException("TSContainerManager is locked for" + //$NON-NLS-1$
            " modifications during change event"); //$NON-NLS-1$
        }
        tscInfo = this.getTSContainer(file.getFullPath());

        // check if test session container of file is not already known
        if (tscInfo == null) {
          try {
            // may throw FileLoadException
            tscInfo = this.add(file, new SubProgressMonitor(monitor, 1 * monitorScale)); // #1
          } catch (CancelException e) {
            throw new CancelException("Canceled adding of test session" + //$NON-NLS-1$
              " container. See the cause for" + //$NON-NLS-1$
              " changes.", e); //$NON-NLS-1$
          }
          monitor.worked(1 * monitorScale); // #2
        } else {
          monitor.worked(2 * monitorScale); // #1-#2
        }
        if (forceActivation) {
          try {
            /*
             * may throw FileLoadException, OutOfMemoryError, InvocationTargetException
             */
            this.setActiveTSContainer(tscInfo, runnable, new SubProgressMonitor(monitor, 1 * monitorScale)); // #3
          } catch (CancelException e) {
            throw new CancelException("Canceled adding of test" + //$NON-NLS-1$
              " session container. See the" + //$NON-NLS-1$
              " cause for changes.", e); //$NON-NLS-1$
          }
        } else {
          monitor.worked(1 * monitorScale); // #3
        }
      }
    } finally {
      monitor.done();
    }

    return tscInfo;
  }

  /**
   * Deletes test session containers, that is the test session containers are removed from the list of known
   * test session containers and their files are deleted.
   * 
   * @param tscInfos the list of test session containers to delete
   * @param monitor a progress monitor or <code>null</code> if progress reporting is not desired
   * @throws NullPointerException if the given list of test session containers is null
   * @throws IllegalArgumentException if the given list of test session containers is empty
   * @throws CoreException if deletion of one or more files failed (see {@link IResource#delete(boolean,
   *         IProgressMonitor)} for details); only the <code>CoreException</code> of the last failed
   *         deletion is thrown
   * @throws CancelException if a request to cancel is detected (with the given progress monitor)
   */
  public void deleteTestSessionContainers(List<TSContainerInfo> tscInfos, IProgressMonitor monitor)
    throws CoreException, CancelException {
    IProgressMonitor deleteMonitor;
    final int monitorScale = 1000;
    monitor = (monitor != null) ? monitor : new NullProgressMonitor();
    final String cancelDescr = "Deleting test session containers canceled." + //$NON-NLS-1$
      " See the cause for changes that already" + //$NON-NLS-1$
      " happened."; //$NON-NLS-1$

    if (tscInfos == null) {
      throw new NullPointerException("tscInfos mustn't be null"); //$NON-NLS-1$
    }
    if (tscInfos.isEmpty()) {
      throw new IllegalArgumentException("tscInfos mustn't be empty"); //$NON-NLS-1$
    }

    try { // this try only ensures that the monitor is left done()
      monitor.beginTask(MONITOR_DELETING_TEST_SESSION_CONTAINERS, 2 * monitorScale);
      try {
        this.remove(tscInfos, new SubProgressMonitor(monitor, 1 * monitorScale)); // #1
      } catch (CancelException e) {
        throw new CancelException(cancelDescr, e);
      }
      deleteMonitor = new SubProgressMonitor(monitor, 1 * monitorScale); // #2
      try {
        this.tscStorage.deleteTSCFiles(tscInfos, deleteMonitor);
      } catch (CancelException e) {
        throw new CancelException(cancelDescr, e);
      }
    } finally {
      monitor.done();
    }
  }

  /**
   * Deletes the given test session container, that is the test session container is removed from the list of
   * known test session containers and its file is deleted. A call to
   * <code>deleteTestSessionContainer(tscInfo, monitor)</code> is equal to calling
   * <code>deleteTestSessionContainer(new ArrayList(tscInfo), monitor)</code>. See
   * {@link #deleteTestSessionContainers(List, IProgressMonitor)} for details like exceptions.
   * 
   * @param tscInfo the test session container to delete
   * @param monitor a progress monitor or <code>null</code> if progress reporting is not desired
   * @throws CoreException if deletion of one or more files failed (see
   *         {@link IResource#delete(boolean, IProgressMonitor)} for details); only the
   *         <code>CoreException</code> of the last failed deletion is thrown
   * @throws CancelException if a request to cancel is detected (with the given progress monitor)
   * @see #deleteTestSessionContainers(List, IProgressMonitor)
   */
  public void deleteTestSessionContainer(TSContainerInfo tscInfo, IProgressMonitor monitor)
    throws CoreException, CancelException {
    this.deleteTestSessionContainers(Collections.singletonList(tscInfo), monitor);
  }

  /*
   * method to set the active test cases
   */

  /**
   * Sets the active <code>TestCase</code>s of the active <code>TestSessionContainer</code> which are
   * visualized in the plugin's views.
   * 
   * @param testCases the active <code>TestCase</code>s or an empty <code>Set</code> if none are active
   * @throws NullPointerException if <code>testCases</code> is <code>null</code> (pass an empty set to
   *         deactivate all test cases)
   * @throws IllegalStateException if there is no active test session container
   * @throws TSContainerManagerModifyException if the <code>TSContainerManager</code> is locked (because of
   *         a change event which is currently propagated)
   */
  public void setActiveTestCases(Set<TestCase> testCases) {
    ActiveTSContainerInfo newActiveTSCInfo;

    if (testCases == null) {
      throw new NullPointerException("testCases mustn't be null"); //$NON-NLS-1$
    }

    synchronized (this.getWriteLock()) {
      if (this.isLocked()) {
        throw new TSContainerManagerModifyException("TSContainerManager is locked for" + //$NON-NLS-1$
          " modifications during change event"); //$NON-NLS-1$
      }

      // check if there is an active test session container
      if (this.activeTSCInfo == null) {
        throw new IllegalStateException("Active test cases can't be set if there" + //$NON-NLS-1$
          " is no active test session container."); //$NON-NLS-1$
      }
      // check if given test cases belong to the active test session cont.
      for (TestCase testCase : testCases) {
        if (testCase.getTestSession().getTestSessionContainer() != this.activeTSCInfo
          .getTestSessionContainer()) {
          throw new IllegalArgumentException("Test case to activate doesn't" + //$NON-NLS-1$
            " belong to the active test session" + //$NON-NLS-1$
            " container."); //$NON-NLS-1$
        }
      }

      /*
       * the passed set of test cases is saved in the TSContainerInfo, too by the constructor
       */
      newActiveTSCInfo =
        new ActiveTSContainerInfo(this.activeTSCInfo.getTSContainerInfo(), this.activeTSCInfo
          .getTestSessionContainer(), testCases);
      synchronized (this.getReadLock()) {
        this.activeTSCInfo = newActiveTSCInfo;
      }

      this.listenerHandler.fireTestCasesActivated(this.activeTSCInfo);
    }
  }

  /**
   * Sets the redundant <code>TestCase</code>s of the active <code>TestSessionContainer</code> which are
   * visualized in the plugin's views.
   * 
   * @param testCases the redundant <code>TestCase</code>s or an empty <code>Set</code> if none are
   *        redundant
   * @throws NullPointerException if <code>testCases</code> is <code>null</code> (pass an empty set to
   *         deactivate all test cases)
   * @throws IllegalStateException if there is no redundant test session container
   * @throws TSContainerManagerModifyException if the <code>TSContainerManager</code> is locked (because of
   *         a change event which is currently propagated)
   */
  public void setRedundantTestCases(Set<TestCase> testCases, Set<TestCase> redundantTestCases) {
    ActiveTSContainerInfo newActiveTSCInfo;

    if (testCases == null) {
      throw new NullPointerException("testCases mustn't be null"); //$NON-NLS-1$
    }
    if (redundantTestCases == null) {
      throw new NullPointerException("testCases mustn't be null"); //$NON-NLS-1$
    }

    synchronized (this.getWriteLock()) {
      if (this.isLocked()) {
        throw new TSContainerManagerModifyException("TSContainerManager is locked for" + //$NON-NLS-1$
          " modifications during change event"); //$NON-NLS-1$
      }

      // check if there is an active test session container
      if (this.activeTSCInfo == null) {
        throw new IllegalStateException("Active test cases can't be set if there" + //$NON-NLS-1$
          " is no active test session container."); //$NON-NLS-1$
      }
      // check if given test cases belong to the active test session cont.
      for (TestCase testCase : testCases) {
        if (testCase.getTestSession().getTestSessionContainer() != this.activeTSCInfo
          .getTestSessionContainer()) {
          throw new IllegalArgumentException("Test case to activate doesn't" + //$NON-NLS-1$
            " belong to the active test session" + //$NON-NLS-1$
            " container."); //$NON-NLS-1$
        }
      }

      /*
       * the passed set of test cases is saved in the TSContainerInfo, too by the constructor
       */
      newActiveTSCInfo =
        new ActiveTSContainerInfo(this.activeTSCInfo.getTSContainerInfo(), this.activeTSCInfo
          .getTestSessionContainer(), testCases, redundantTestCases);
      synchronized (this.getReadLock()) {
        this.activeTSCInfo = newActiveTSCInfo;
      }

      this.listenerHandler.fireTestCasesActivated(this.activeTSCInfo);
    }
  }

  /*
   * private methods to add and remove test session containers
   */

  /**
   * Adds the test session container of the given file to the list of known test session containers. If the
   * test session container of the given file is already known, nothing is done. If currently no test session
   * container is active, the added one is activated.
   * 
   * @param file the file which contains the <code>TestSessionContainer</code> to add
   * @param monitor a progress monitor or <code>null</code> if progress reporting is not desired
   * @return the <code>TSContainerInfo</code>-representation of the added test session container.
   * @throws FileLoadException if the <code>TestSessionContainer</code> can't be read from the contents of
   *         the file
   * @throws CancelException if a request to cancel is detected (with the given progress monitor)
   */
  TSContainerInfo add(IFile file, IProgressMonitor monitor) throws FileLoadException, CancelException {
    TestSessionContainer tsc;
    TSContainerInfo addTSCInfo;
    boolean noTSCactive;
    int insertPos;
    final int monitorScale = 1000;
    monitor = (monitor != null) ? monitor : new NullProgressMonitor();

    try { // this try only ensures that the monitor is left done()
      monitor.beginTask(MONITOR_ADDING_TEST_SESSION_CONTAINER, 5 * monitorScale);
      if (monitor.isCanceled()) {
        throw new CancelException("Canceled adding of test session container." + //$NON-NLS-1$
          " Nothing changed."); //$NON-NLS-1$
      }
      synchronized (this.getWriteLock()) {
        if (this.isLocked()) {
          throw new TSContainerManagerModifyException("TSContainerManager is locked for" + //$NON-NLS-1$
            " modifications during change event"); //$NON-NLS-1$
        }

        // if currently no TSC is active then the added one is activated
        noTSCactive = (this.activeTSCInfo == null);

        // if the test session container is not already known then add it
        if ((addTSCInfo = this.getTSContainer(file.getFullPath())) == null) {
          try {
            tsc = TSContainerStorage.load(file, false, new SubProgressMonitor(monitor, 1 * monitorScale));// #1
          } catch (FileLoadException e) {
            throw e;
          }

          if (monitor.isCanceled()) {
            throw new CancelException("Canceled adding of test session" + //$NON-NLS-1$
              " container. Nothing changed."); //$NON-NLS-1$
          }

          /*
           * Remember to copy everything you take out of a test session container, else the garbage collector
           * won't remove it from memory.
           */
          addTSCInfo =
            new TSContainerInfo(file, new String(tsc.getId()), (Date) tsc.getDate().clone(), this
              .generateTSContainerName(tsc, file));
          this.testCasesStorage.unbufferActiveTestCases(addTSCInfo);
          monitor.worked(1 * monitorScale); // #2

          if (monitor.isCanceled()) {
            throw new CancelException("Canceled adding of test session" + //$NON-NLS-1$
              " container. Nothing changed."); //$NON-NLS-1$
          }

          /*
           * Since test session containers can only be removed from the list of known test session containers
           * by the method remove, which performs pending saves and removes them from the queue, there can't
           * be any pending saves of newly added test session containers. Thus the following call to
           * removeSaveFromQueue is just for safety.
           */
          this.tscStorage.getSaveQueue().removeSaveFromQueue(addTSCInfo);

          // find position to insert the new TSC into the list
          for (insertPos = 0; insertPos < this.tscInfos.size()
            && addTSCInfo.compareTo(this.tscInfos.get(insertPos)) >= 0; insertPos++)
            ;

          if (monitor.isCanceled()) {
            throw new CancelException("Canceled adding of test session" + //$NON-NLS-1$
              " container. Nothing changed."); //$NON-NLS-1$
          }

          synchronized (this.getReadLock()) {
            this.tscInfos.add(insertPos, addTSCInfo);
          }

          monitor.worked(1 * monitorScale); // #3

          this.listenerHandler.fireTestSessionContainerAdded(addTSCInfo, insertPos);

          monitor.worked(1 * monitorScale); // #4
        } else {
          /*
           * if the test session container is already known, just activate it (if none is activated already)
           */
          monitor.worked(4 * monitorScale); // #1-#4
        }
        if (noTSCactive) {
          try {
            this.setActiveTSContainerButDontSave(addTSCInfo, null, new SubProgressMonitor(monitor,
              1 * monitorScale)); // #5
          } catch (FileLoadException e) {
            throw e;
          } catch (InvocationTargetException e) {
            /*
             * ignore because it can't be thrown if no runnable was passed
             */
          } catch (CancelException e) {
            throw new CancelException("Canceled adding of test" + //$NON-NLS-1$
              " session container. See the" + //$NON-NLS-1$
              " cause for changes.", //$NON-NLS-1$
              e);
          }
        } else {
          monitor.worked(1 * monitorScale); // #5
        }
      }
    } finally {
      monitor.done();
    }

    return addTSCInfo;
  }

  /**
   * Removes the given test session containers from the list of known test session containers.
   * 
   * @param tscInfos the test session containers to remove (must be in the list of known test session
   *        containers)
   * @param monitor a progress monitor or <code>null</code> if progress reporting is not desired
   * @throws NullPointerException if the given list is <code>null</code>
   * @throws IllegalArgumentException if the given list is empty
   * @throws TSContainerManagerModifyException if a listener calls this method during a change event
   * @throws IllegalArgumentException if one of the given test session containers is unknown
   * @throws CancelException if a request to cancel is detected (with the given progress monitor)
   */
  void remove(List<TSContainerInfo> tscInfos, IProgressMonitor monitor) throws CancelException {
    boolean activateSuccess;
    // list for requested removals and removals of unloadable TSCs
    List<TSContainerInfo> allTSCInfosToRemove = new LinkedList<TSContainerInfo>(tscInfos);
    IProgressMonitor loadTSCMonitor;
    IProgressMonitor removeMonitor;
    final int monitorScale = 1000;
    monitor = (monitor != null) ? monitor : new NullProgressMonitor();
    String cancelDescr = "Canceled removal of test " + //$NON-NLS-1$
      " session containers.\n" + //$NON-NLS-1$
      "Changes happened so far:\n"; //$NON-NLS-1$

    if (tscInfos == null) {
      throw new NullPointerException("tscInfos mustn't be null"); //$NON-NLS-1$
    }
    if (tscInfos.isEmpty()) {
      throw new IllegalArgumentException("tscInfos mustn't be empty"); //$NON-NLS-1$
    }

    try { // this try only ensures that the monitor is left done()
      monitor.beginTask(MONITOR_REMOVING_TEST_SESSION_CONTAINERS, 3 * monitorScale);
      synchronized (this.getWriteLock()) {
        if (this.isLocked()) {
          throw new TSContainerManagerModifyException("TSContainerManager is locked for" + //$NON-NLS-1$
            " modifications during change event"); //$NON-NLS-1$
        }
        if (!this.tscInfos.containsAll(tscInfos)) {
          throw new IllegalArgumentException("The given list of test session" + //$NON-NLS-1$
            " containers to remove contains" + //$NON-NLS-1$
            " an unknown test session container."); //$NON-NLS-1$
        }
        // if the active TSC is to be removed...
        if (tscInfos.contains(this.activeTSCInfo.getTSContainerInfo())) {
          /*
           * this for-loop tries to load an other test session container than the active one
           */
          loadTSCMonitor = new SubProgressMonitor(monitor, 1 * monitorScale);// #1
          try {
            loadTSCMonitor.beginTask(MONITOR_LOADING_AN_OTHER_TEST_SESSION_CONTAINER, this.tscInfos.size()
              * monitorScale);
            activateSuccess = false;
            for (TSContainerInfo curTSCInfo : this.tscInfos) {
              if (!tscInfos.contains(curTSCInfo)) {
                activateSuccess = true;
                try {
                  this.setActiveTSContainerButDontSave(curTSCInfo, null, new SubProgressMonitor(
                    loadTSCMonitor, 1 * monitorScale));
                } catch (FileLoadException e) {
                  activateSuccess = false;
                  allTSCInfosToRemove.add(curTSCInfo);
                } catch (InvocationTargetException e) {
                  /*
                   * ignore because it can't be thrown if no runnable was passed
                   */
                } catch (CancelException e) {
                  throw new CancelException("Canceled removal of (active)" + //$NON-NLS-1$
                    " test session container. For" + //$NON-NLS-1$
                    " changes happend so far, see" + //$NON-NLS-1$
                    " the cause.", e); //$NON-NLS-1$
                }
                if (activateSuccess) {
                  break;
                }
              }
            }
          } finally {
            loadTSCMonitor.done();
          }

          /*
           * if there was no success in activating an other test session container, all other test session
           * containers are unloadable and will be removed or there were no other test session containers in
           * the first place. Thus the active test session container can safely be set to null.
           */
          if (!activateSuccess) {
            try {
              this.setActiveTSContainerButDontSave(null, null, new SubProgressMonitor(monitor,
                1 * monitorScale)); // #2
            } catch (FileLoadException e) {
              /*
               * ignore because it can't be thrown if active TSC is set to null
               */
            } catch (InvocationTargetException e) {
              /*
               * ignore because it can't be thrown if no runnable was passed
               */
            } catch (CancelException e) {
              throw new CancelException("Canceled removal of (active)" + //$NON-NLS-1$
                " test session container. For" + //$NON-NLS-1$
                " changes happend so far, see" + //$NON-NLS-1$
                " the cause.", e); //$NON-NLS-1$
            }
          }
        } else {
          monitor.worked(2 * monitorScale); // #1-#2
        }

        // perform requested removals and remove unloadable TSCs, too
        removeMonitor = new SubProgressMonitor(monitor, 1 * monitorScale); // #3
        try {
          removeMonitor.beginTask(MONITOR_REMOVING_TSCS_INCL_UNLOADABLE, allTSCInfosToRemove.size()
            * monitorScale);
          if (removeMonitor.isCanceled()) {
            throw new CancelException(cancelDescr + "No TSCs removed (from list of" + //$NON-NLS-1$
              " known TSCs)."); //$NON-NLS-1$
          }
          for (TSContainerInfo tscToRemove : allTSCInfosToRemove) {
            synchronized (this.getReadLock()) {
              this.tscInfos.remove(tscToRemove);
            }
            this.listenerHandler.fireTestSessionContainerRemoved(tscToRemove);
            cancelDescr += tscToRemove.getPath().toString() + " removed (from list of known" + //$NON-NLS-1$
              " TSCs)\n"; //$NON-NLS-1$
            if (removeMonitor.isCanceled()) {
              throw new CancelException(cancelDescr);
            }
            removeMonitor.worked(1 * monitorScale);
          }
        } finally {
          removeMonitor.done();
        }
      }
    } finally {
      monitor.done();
    }
  }

  /**
   * Executes the given runnable which is passed the <code>ActiveTSContainerInfo</code>-representation of
   * the currently active test session container. This method throws the exceptions the runnable produces,
   * whereas <code>RuntimeException</code>s of the runnable are wrapped in
   * <code>InvocationTargetException</code>s.
   * 
   * @param runnable the runnable to execute
   * @param monitor the progress monitor which is passed to the runnable, so that it can report its progress
   * @throws InvocationTargetException if the runnable throws an <code>InvocationTargetException</code> or
   *         if the runnable throws a <code>RuntimeException</code> (which is wrapped in an
   *         <code>InvocationTargetException</code> by this method)
   * @throws CancelException if the runnable throws a <code>CancelException</code>
   */
  private void executeRunnable(ActiveTSContainerRunnable runnable, IProgressMonitor monitor)
    throws InvocationTargetException, CancelException {
    if (runnable != null) {
      try {
        if (this.activeTSCInfo != null) {
          // may throw InvocationTargetException, CancelException
          runnable.run(this.activeTSCInfo, monitor);
        } else {
          // may throw InvocationTargetException, CancelException
          runnable.run(null, monitor);
        }
      } catch (RuntimeException e) {
        throw new InvocationTargetException(e);
      }
    }
  }

  /**
   * Returns the save participant which handles save operations of test session containers (see
   * {@link TSContainerSaveParticipant}) and handles saving of the selection of active test cases and the
   * selected active test session container, to be able to restore the selection of the active test cases and
   * active test session container after a restart of Eclipse.
   * 
   * @return the save participant
   */
  public ISaveParticipant getSaveParticipant() {
    return this.saveParticipantHandler;
  }

  /*
   * methods to save/load test session containers
   */

  /**
   * Saves the active test session container (to stable storage).
   * 
   * @param monitor a progress monitor or <code>null</code> if progress reporting is not desired
   * @throws TSCFileCreateException if the file, to write the active test test session container to, couldn't
   *         be created
   * @throws FileSaveException if the active test session container couldn't be written to file
   * @throws OutOfMemoryError if the virtual machine runs out of memory while saving the active test session
   *         container
   */
  public void saveActiveTSContainer(IProgressMonitor monitor)
    throws TSCFileCreateException, FileSaveException, OutOfMemoryError {
    this.saveActiveTSContainer(false, monitor);
  }

  /**
   * Saves the active test session container (to stable storage) or queues the save operation.
   * 
   * @param queue set to <code>true</code> if the save operation must be queued
   * @param monitor a progress monitor or <code>null</code> if progress reporting is not desired
   * @return <code>true</code> if a save operation was performed/queued, <code>false</code> otherwise
   * @throws TSCFileCreateException if the file, to write the active test test session container to, couldn't
   *         be created
   * @throws FileSaveException if the active test session container couldn't be written to file
   * @throws OutOfMemoryError if the virtual machine runs out of memory while saving the active test session
   *         container
   */
  boolean saveActiveTSContainer(boolean queue, IProgressMonitor monitor)
    throws TSCFileCreateException, FileSaveException, OutOfMemoryError {
    ActiveTSContainerInfo activeTSCInfo;
    boolean performedSave = false;
    final int monitorScale = 1000;
    monitor = (monitor != null) ? monitor : new NullProgressMonitor();
    try { // this try only ensures that the monitor is left done()
      monitor.beginTask(MONITOR_SAVING_ACTIVE_TEST_SESSION_CONTAINER, 1 * monitorScale);
      synchronized (this.getWriteLock()) {
        if (this.isLocked()) {
          throw new TSContainerManagerModifyException("TSContainerManager is locked for" + //$NON-NLS-1$
            " modifications during change event"); //$NON-NLS-1$
        }
        activeTSCInfo = this.getActiveTSContainer();
        if (activeTSCInfo != null && !activeTSCInfo.isSynchronized()) {
          try {
            this.tscStorage.saveTSContainer(activeTSCInfo.getTestSessionContainer(), activeTSCInfo
              .getTSContainerInfo(), queue, new SubProgressMonitor(monitor, 1 * monitorScale));
          } catch (TSCFileCreateException e) {
            throw e;
          } catch (FileSaveException e) {
            throw e;
          } catch (OutOfMemoryError e) {
            throw e;
          }
          performedSave = true;
        } else {
          monitor.worked(1 * monitorScale);
        }
      }
    } finally {
      monitor.done();
    }
    return performedSave;
  }

  /**
   * Loads a known test session container.
   * 
   * @param tscInfo the <code>TSContainerInfo</code>-representation of the test session container to load
   * @param monitor a progress monitor or <code>null</code> if progress reporting is not desired
   * @return the loaded <code>TestSessionContainer</code>.
   * @throws FileLoadException if the <code>TestSessionContainer</code> can't be read from the contents of
   *         the file
   */
  public static TestSessionContainer load(TSContainerInfo tscInfo, IProgressMonitor monitor)
    throws FileLoadException {
    return TSContainerStorage.load(tscInfo, monitor);
  }

  /**
   * Loads all <code>TestSessionContainer</code>s of all open projects <em>and</em> adds them to the
   * <code>TSContainerManager</code> (makes them known).
   * <p>
   * Only use for initialization of CodeCover!
   * </p>
   */
  public void loadAllTestSessionContainers() {
    // get all TSCs in workspace
    List<IFile> tscFiles = TSContainerStorage.fetchTSCFiles();
    synchronized (this.getWriteLock()) {
      if (this.isLocked()) {
        throw new TSContainerManagerModifyException("TSContainerManager is locked for" + //$NON-NLS-1$
          " modifications during change event"); //$NON-NLS-1$
      }
      synchronized (this.getReadLock()) {
        for (IFile file : tscFiles) {
          // only load if file was not already loaded
          if (this.getTSContainer(file.getFullPath()) == null) {
            try {
              this.add(file, null);
            } catch (FileLoadException e) {
              logger.error("Couldn't load test session" + //$NON-NLS-1$
                " container: " //$NON-NLS-1$
                + file.getFullPath(), e);
            } catch (CancelException e) {
              /*
               * ignore because it can't happen if no progress monitor was passed
               */
            }
          }
        }
      }
    }
  }

  /*
   * method to generate a name for a test session container
   */

  /**
   * Generates a unique name for a known test session container. The name is unique in the current session of
   * Eclipse. The <code>TSContainerInfo</code> this name is used for must be added to the
   * <code>TSContainerManager</code> before the write lock is released, else another thread could generate
   * the same name.
   * 
   * @param tsc the test session container the name is generated for
   * @param file the file of the test session container the name is generated for
   * @return a (session-)unique name for the given test session container
   */
  private String generateTSContainerName(TestSessionContainer tsc, IFile file) {
    List<String> existingNames = new ArrayList<String>();
    // build the name for the TSC
    String name = file.getProject().getName() + " " //$NON-NLS-1$
      + DateFormat.getDateTimeInstance().format(tsc.getDate());
    String escapedName;
    synchronized (this.writeLock) {
      // build the list of existing names for the known TSCs
      for (TSContainerInfo tscInfo : this.getTestSessionContainers()) {
        existingNames.add(tscInfo.getName());
      }
      escapedName = NameReoccurenceHelper.escapeName(existingNames, name);
    }
    return escapedName;
  }

  /*
   * methods to add/remove TSContainerManagerListeners
   */

  /**
   * Adds the given listener to receive events from this <code>TSContainerManager</code>.
   * 
   * @param listener the listener
   * @throws NullPointerException if the specified listener is <code>null</code>
   */
  public void addListener(TSContainerManagerListener listener) {
    this.listenerHandler.addListener(listener);
  }

  /**
   * Removes the given listener so that it no longer receives events from this <code>TSContainerManager</code>.
   * 
   * @param listener the listener
   */
  public void removeListener(TSContainerManagerListener listener) {
    this.listenerHandler.removeListener(listener);
  }

  /*
   * methods for locking of the TSContainerManager
   */

  /**
   * Returns the object used for locking this <code>TSContainerManager</code> for writing. If a thread owns
   * this lock (by using a &quot;<code>synchronized(tscManager.getWriteLock())
   * { ... }</code>&quot;-block)
   * no thread may manipulate this <code>TSContainerManager</code> including the one which owns the lock.
   * See {@link #getReadLock()} if you want to change this <code>TSContainerManager</code>.
   * 
   * @return the object used for locking this <code>TSContainerManager</code> for writing
   */
  Object getWriteLock() {
    return this.writeLock;
  }

  /**
   * Returns the object used for locking this <code>TSContainerManager</code> for reading. If a thread owns
   * this lock (by using a &quot;<code>synchronized(tscManager.getWriteLock())
   * { ... }</code>&quot;-block)
   * no other thread may read this <code>TSContainerManager</code>.
   * <p>
   * If you only want to read from this <code>TSContainerManager</code> you have to acquire the read lock
   * (returned by this method).
   * <p>
   * If you want to actually change this <code>TSContainerManager</code>, you have to acquire both locks,
   * the read and the write lock. Before acquiring the read lock, the write lock must be acquired:
   * 
   * <pre>
   * synchronized(tscManager.getWriteLock()) {
   *     synchronized(tscManager.getReadLock()) {
   *         // in here the TSContainerManager is locked for reading and
   *         // writing
   *     }
   * }
   * </pre>
   * 
   * </p>
   * 
   * @return the object used for locking this <code>TSContainerManager</code> for reading
   */
  Object getReadLock() {
    return this.readLock;
  }

  /**
   * Sets the locking state of this <code>TSContainerManager</code> which can be locked due to change
   * events.
   * 
   * @param locked <code>true</code> if this <code>TSContainerManager</code> is locked due to a running
   *        change event, <code>false</code> otherwise
   */
  void setLocked(boolean locked) {
    this.locked = locked;
  }

  /**
   * Returns the locking state of this <code>TSContainerManager</code> which can be locked due to change
   * events.
   * 
   * @return <code>true</code> if this <code>TSContainerManager</code> is locked due to a running change
   *         event, <code>false</code> otherwise
   */
  boolean isLocked() {
    return this.locked;
  }

}
