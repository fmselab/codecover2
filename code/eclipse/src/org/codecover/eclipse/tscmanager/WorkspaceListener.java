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
import java.util.LinkedList;
import java.util.List;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.tscmanager.exceptions.CancelException;
import org.codecover.eclipse.tscmanager.exceptions.TSCFileCreateException;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.exceptions.FileSaveException;
import org.codecover.model.utils.Logger;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;

/**
 * Listens for changes in the workspace. The task is to filter for events affecting files of test session
 * containers. This only includes adding of unknown files in the <code>CodeCoverPlugin.CODECOVER_FOLDER</code>.
 * There is no reaction on removal of files because while saving we overwrite existing files which includes
 * removing the existing file and thus cannot be distinguished from a real removal. Moreover there is no
 * reaction on changes of files since this is done by the listeners on the loaded test session containers. The
 * only reaction that could have been done here would have been to completely reload the test session
 * container. This would be a rather heavy resource-consuming operation and would slow down the work with the
 * plug-in too much. As a consequence of this behavior no plug-in-external changes of test session containers
 * are detected during execution.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: WorkspaceListener.java 49 2009-06-01 08:52:19Z ahija $)
 */
class WorkspaceListener
  implements IResourceChangeListener {

  private final TSContainerManager tscManager;

  private final ActiveTestCasesStorage testCasesStorage;

  private final Logger logger;

  /**
   * Constructs a workspace listener and registers it to listen for resource changes.
   * 
   * @param tscManager the <code>TSContainerManager</code>, which is to be updated on changes
   * @param testCasesStorage the <code>ActiveTestCasesStorage</code> to use for saving
   */
  public WorkspaceListener(TSContainerManager tscManager, ActiveTestCasesStorage testCasesStorage) {
    this.tscManager = tscManager;
    this.testCasesStorage = testCasesStorage;
    this.logger = tscManager.getLogger();

    ResourcesPlugin.getWorkspace().addResourceChangeListener(this,
      IResourceChangeEvent.POST_CHANGE | IResourceChangeEvent.PRE_CLOSE | IResourceChangeEvent.PRE_DELETE);
  }

  /**
   * This method notifies the <code>WorkspaceListener</code> about changes in the workspace's resources
   * (files, folders). The task is to filter for events affecting files of test session containers. This only
   * includes adding of unknown files in the <code>CodeCoverPlugin.CODECOVER_FOLDER</code>. There is no
   * reaction on removal of files because while saving we overwrite existing files which includes removing the
   * existing file and thus cannot be distinguished from a real removal. Moreover there is no reaction on
   * changes of files since this is done by the listeners on the loaded test session containers. The only
   * reaction that could have been done here would have been to completely reload the test session container.
   * This would be a rather heavy resource-consuming operation and would slow down the work with the plug-in
   * too much. As a consequence of this behavior no plug-in-external changes of test session containers are
   * detected during execution.
   * 
   * @param event the event to handle
   */
  @Override
public void resourceChanged(final IResourceChangeEvent event) {
    if (event.getType() == IResourceChangeEvent.POST_CHANGE) {
      List<IResourceDelta> changedTSCFiles = fetchChangedTSCFiles(event.getDelta());
      IPath filepath;
      /*
       * getDelta() returns the resource delta tree rooted at the workspace root (which is a precondition for
       * method fetchChangedTSCFiles)
       */
      for (IResourceDelta fileDelta : changedTSCFiles) {
        filepath = fileDelta.getResource().getFullPath();
        if (fileDelta.getKind() == IResourceDelta.ADDED) {
          logger.debug("Unknown file added to a" + //$NON-NLS-1$
            " CodeCover-Folder: " //$NON-NLS-1$
            + filepath.toString());

          try {
            /*
             * method add checks if the test session container of this file is already loaded, so no worries
             * method add queues saves thus workspace-locked- exceptions can't arise
             */
            this.tscManager.add((IFile) fileDelta.getResource(), null);
          } catch (FileLoadException e) {
            logger.error("Couldn't load newly added" + //$NON-NLS-1$
              " test session container." //$NON-NLS-1$
              + ((IFile) fileDelta.getResource()).getFullPath().toString(), e);
          } catch (CancelException e) {
            /*
             * ignore because it can't happen if no progress monitor was passed
             */
          }
        }
        /*
         * IResourceDelta.REMOVED and IResourceDelta.CHANGED are ignored
         */
      }
    } else if (event.getType() == IResourceChangeEvent.PRE_CLOSE
      || event.getType() == IResourceChangeEvent.PRE_DELETE) {
      // remove the test session containers of the closed/deleted project
      IProject project = (IProject) event.getResource();
      List<TSContainerInfo> tscsToRemove;
      ActiveTSContainerInfo activeTSCInfo;
      synchronized (this.tscManager.getWriteLock()) {
        tscsToRemove = this.tscManager.getTestSessionContainers(project);
        activeTSCInfo = this.tscManager.getActiveTSContainer();
        if (!tscsToRemove.isEmpty()) {
          /*
           * first all active test cases of all TSCs of the closed project are buffered and the active TSC is
           * saved if it belongs to the closed project
           */
          if (event.getType() == IResourceChangeEvent.PRE_CLOSE) {
            for (TSContainerInfo tscToRemove : tscsToRemove) {
              if (tscToRemove.equals(activeTSCInfo) && !activeTSCInfo.isSynchronized()) {
                /*
                 * saves have to be queued because the workspace is locked during a resource change event
                 */
                try {
                  this.tscManager.saveActiveTSContainer(true, null);
                } catch (TSCFileCreateException e) {
                  /*
                   * can't be thrown because the save is just queued
                   */
                } catch (FileSaveException e) {
                  /*
                   * can't be thrown because the save is just queued
                   */
                } catch (OutOfMemoryError e) {
                  logger.error("Out of memory while" + //$NON-NLS-1$
                    " queueing save operation" + //$NON-NLS-1$
                    " of test session" + //$NON-NLS-1$
                    " container: " //$NON-NLS-1$
                    + activeTSCInfo.getFile().getFullPath().toString(), new InvocationTargetException(e));
                }
                logger.debug("Queued save of " //$NON-NLS-1$
                  + activeTSCInfo.getFile().getFullPath().toString());
              }
              this.testCasesStorage.bufferActiveTestCases(tscToRemove);
            }
          }
          /*
           * then all TSCs of the closed/deleted project are removed from the list of known test session
           * containers
           */
          try {
            this.tscManager.remove(tscsToRemove, null);
          } catch (CancelException e) {
            /*
             * ignore because it can't happen if no progress monitor was passed
             */
          }
        }
      }
    }
  }

  /*
   * methods to fetch changed or all test session container files of the workspace
   */

  /**
   * Convenience-method for retrieving all changed test session container files of the workspace.
   * 
   * @param workspace the workspace root
   * @return all changed test session container files of the workspace.
   * @see #fetchChangedProjects(IResourceDelta)
   * @see #fetchChangedTSCFilesByProject(IResourceDelta)
   */
  static List<IResourceDelta> fetchChangedTSCFiles(IResourceDelta workspace) {
    List<IResourceDelta> changedProjects;
    List<IResourceDelta> allChangedTSCFiles = new LinkedList<IResourceDelta>();
    try {
      changedProjects = fetchChangedProjects(workspace);
    } catch (ClassCastException e) {
      throw e;
    }
    for (IResourceDelta project : changedProjects) {
      allChangedTSCFiles.addAll(fetchChangedTSCFilesByProject(project));
    }
    return allChangedTSCFiles;
  }

  /**
   * Retrieves all changed projects of the resource delta of a workspace. The projects are represented by
   * <code>IResourceDelta</code>s which contain information about the type of change that happened to the
   * project, the changed children of the project (if any) and a reference to the project itself. It is
   * guaranteed that the returned list only contains <code>IResourceDelta</code>s of projects (<code>IProject</code>s).
   * 
   * @param workspace the workspace root
   * @return all changed projects of the resource delta of a workspace.
   */
  private static List<IResourceDelta> fetchChangedProjects(IResourceDelta workspace) {
    List<IResourceDelta> projects = new LinkedList<IResourceDelta>();
    for (IResourceDelta project : workspace.getAffectedChildren()) {
      if (project.getResource() instanceof IProject) {
        projects.add(project);
      }
    }
    return projects;
  }

  /**
   * Retrieves all changed files of test session containers of the resource delta of a project. The test
   * session container files are represented by <code>IResourceDelta</code>s which contain information
   * about the type of change that happened to the file and a reference to the file itself. It is guaranteed
   * that the returned list only contains <code>IResourceDelta</code>s of files (<code>IFile</code>s),
   * however these files don't have to be test session containers.
   * 
   * @param project the project
   * @return all changed test session container files of the resource delta of a project.
   */
  private static List<IResourceDelta> fetchChangedTSCFilesByProject(IResourceDelta project) {
    LinkedList<IResourceDelta> tscFiles = new LinkedList<IResourceDelta>();
    for (IResourceDelta child : project.getAffectedChildren()) {
      if (child.getResource() instanceof IFolder
        && ((IFolder) child.getResource()).getName().equals(CodeCoverPlugin.CODECOVER_FOLDER)) {
        for (IResourceDelta tscFile : child.getAffectedChildren()) {
          if (tscFile.getResource() instanceof IFile) {
            tscFiles.add(tscFile);
          }
        }
      }
    }
    return tscFiles;
  }

}
