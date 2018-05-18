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

import java.io.ByteArrayInputStream;
import java.lang.reflect.InvocationTargetException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.exceptions.CancelException;
import org.codecover.eclipse.tscmanager.exceptions.TSCFileCreateException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.exceptions.FileSaveException;
import org.codecover.model.utils.Logger;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceRuleFactory;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.MultiRule;

/**
 * Handles the saving and loading of test session containers. Locking of the stored and related objects (i.e.,
 * the method parameters) must be done by the calling context.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: TSContainerStorage.java 49 2009-06-01 08:52:19Z ahija $)
 */
class TSContainerStorage {

  private static final String MONITOR_WRITING_TEST_SESSION_CONTAINER =
    Messages.getString("TSContainerStorage.MONITOR_WRITING_TEST_SESSION_CONTAINER"); //$NON-NLS-1$

  private static final String MONITOR_LOADING_TEST_SESSION_CONTAINER_FROM =
    Messages.getString("TSContainerManager.MONITOR_LOADING_TEST_SESSION_CONTAINER_FROM"); //$NON-NLS-1$

  private static final String MONITOR_DELETING_TSC_FILES =
    Messages.getString("TSContainerStorage.MONITOR_DELETING_TSC_FILES"); //$NON-NLS-1$

  private static final String TSC_FILENAME_PREFIX = "test-session-container"; //$NON-NLS-1$

  private static final String TSC_FILENAME_SEPERATOR = "-"; //$NON-NLS-1$

  private static final DateFormat TSC_FILENAME_DATE_SUFFIX = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss-SSS"); //$NON-NLS-1$

  private static final String TSC_FILENAME_EXTENSION = ".xml"; //$NON-NLS-1$

  private static final String OLD_TSC_FILENAME_EXTENSION = ".old";//$NON-NLS-1$

  private final TSContainerSaveQueue saveQueue;

  private final Logger logger;

  TSContainerStorage(TSContainerManager tscManager) {
    if (tscManager == null) {
      throw new NullPointerException("tscManager mustn't be null"); //$NON-NLS-1$
    }

    this.logger = tscManager.getLogger();
    this.saveQueue = new TSContainerSaveQueue(this);
  }

  /**
   * Returns (always the same) logger.
   * 
   * @return the logger, which doesn't change over time
   */
  Logger getLogger() {
    return this.logger;
  }

  /*
   * methods to save test session containers
   */

  TSContainerSaveQueue getSaveQueue() {
    return this.saveQueue;
  }

  /**
   * Locking of <code>TSContainerManager</code> must be done by the calling context.
   */
  void saveTSContainer(final TestSessionContainer tsc, final TSContainerInfo tscInfo, boolean queue,
    IProgressMonitor monitor) throws TSCFileCreateException, FileSaveException, OutOfMemoryError {
    if (queue) {
      this.saveQueue.queueSave(tscInfo, tsc);
    } else {
      this.writeTestSessionContainer(tsc, null, null, tscInfo, monitor);
    }
  }

  /**
   * Writes a test session container to a file in the CodeCover-folder of a project. Locking of
   * <code>TSContainerManager</code> must be done by the calling context if required (i.e., if a known test
   * session container is written).
   * 
   * @param tsc the test session container to write
   * @param reqFname the requested filename for the test session container; If it is <code>null</code>, a
   *        filename in a default format is used which incorporates the current date and time. If the
   *        requested or generated filename already exists in the folder and parameter
   *        <code>findAlternative</code> is <code>true</code>, the suffix <code>.N</code> is appended
   *        to the given filename. Whereas <code>N</code> is a number so that the extended
   *        <code>filename.N</code> is unique in the CodeCover-folder of the given project.
   * @param destProj the associated project, i.e. the project the test session container belongs to, its file
   *        is stored in the CodeCover-folder of the project which is created if it doesn't already exist
   * @param tscInfo if the test session container is already known and just has to be written to update its
   *        file (i.e., to save it), this parameter can be passed; the parameters for the requested filename
   *        and the project are ignored if this one is passed (i.e. if this one is non-<code>null</code>)
   * @param monitor a progress monitor or <code>null</code> if progress reporting is not desired
   * @return the file of the written test session container
   * @throws TSCFileCreateException if the file to write to couldn't be created
   * @throws FileSaveException if the test session container couldn't be written to the file
   * @throws OutOfMemoryError if the java virtual machine is out of memory
   */
  IFile writeTestSessionContainer(final TestSessionContainer tsc, final String reqFname,
    final IProject destProj, final TSContainerInfo tscInfo, IProgressMonitor monitor)
    throws TSCFileCreateException, FileSaveException, OutOfMemoryError {
    final String filename = (tscInfo != null) ? tscInfo.getFile().getName() : reqFname;
    final IProject project = (tscInfo != null) ? tscInfo.getProject() : destProj;
    IWorkspace workspace = ResourcesPlugin.getWorkspace();
    IResourceRuleFactory ruleFactory = workspace.getRuleFactory();
    // needs to be a list to get the new IFile out of the IWorkspaceRunnable
    final List<IFile> newFile = new ArrayList<IFile>();
    if (tsc == null) {
      throw new NullPointerException("tsc mustn't be null"); //$NON-NLS-1$
    }
    if (project == null && tscInfo == null) {
      throw new NullPointerException("a project or a TSContainerInfo must be" + //$NON-NLS-1$
        " passed (non-null)"); //$NON-NLS-1$
    }

    monitor = (monitor != null) ? monitor : new NullProgressMonitor();
    IWorkspaceRunnable writeRunnable = new IWorkspaceRunnable() {

      @Override
	public void run(IProgressMonitor monitor) throws CoreException {
        // the temporary file the test session container is saved to
        IFile tempFile;
        // the old file after it was moved to the temp folder
        IFile oldFile = null;
        IFolder tempFolder;
        IFolder codeCoverFolder;
        logger.debug("Trying to write test session container" + //$NON-NLS-1$
          " with ID " + tsc.getId() + //$NON-NLS-1$
          " to project " + project.getName()); //$NON-NLS-1$
        try { // this try only ensures that the monitor is left done()
          monitor.beginTask(MONITOR_WRITING_TEST_SESSION_CONTAINER, 6);

          if (tscInfo != null) {
            /*
             * remove pending saves from queue to avoid overwriting of this save by a queued one
             */
            TSContainerStorage.this.saveQueue.removeSaveFromQueue(tscInfo);
          }

          // open project if necessary
          if (!project.isOpen()) {
            // may throw CoreException
            project.open(null);
          }

          // save to temporary file
          try {
            tempFile = generateTSCFile(filename, project, true);
          } catch (TSCFileCreateException e) {
            throw new CoreException(new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID, IStatus.OK, e
              .getMessage(), e));
          }
          monitor.worked(1); // #1
          try {
            tsc.save(tempFile.getLocation().toFile());
          } catch (FileSaveException fse) {
            logger.error("Error while writing test session" + //$NON-NLS-1$
              " container to temporary file", fse); //$NON-NLS-1$
            try {
              tempFile.delete(true, null);
            } catch (CoreException ce) {
              logger.error("Error while deleting temporary" + //$NON-NLS-1$
                " file.", ce); //$NON-NLS-1$
            }
            throw new CoreException(new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID, IStatus.OK, fse
              .getMessage(), fse));
          } catch (OutOfMemoryError oome) {
            logger.error("Out of memory while writing" + //$NON-NLS-1$
              " test session container to" + //$NON-NLS-1$
              " temporary file", //$NON-NLS-1$
              new InvocationTargetException(oome));
            try {
              tempFile.delete(true, null);
            } catch (CoreException ce) {
              logger.error("Error while deleting temporary" + //$NON-NLS-1$
                " file.", ce); //$NON-NLS-1$
            }
            throw new CoreException(new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID, IStatus.OK, oome
              .getMessage(), oome));
          }
          monitor.worked(1); // #2

          // sync workspace with temporary file in the local file system
          try {
            tempFile.refreshLocal(IResource.DEPTH_ZERO, null);
          } catch (CoreException e) {
            logger.error("Couldn't refresh workspace for" + //$NON-NLS-1$
              " (written) temporary file: " //$NON-NLS-1$
              + tempFile.getFullPath().toString(), e);
          }
          monitor.worked(1); // #3

          /*
           * move old file to temp folder to be able to restore it if something goes wrong
           */
          if (tscInfo != null && tscInfo.getFile().exists()) {
            try {
              tempFolder = createTempFolder(project);
              oldFile = findNewFilename(filename + OLD_TSC_FILENAME_EXTENSION, tempFolder, true);
            } catch (TSCFileCreateException e) {
              throw new CoreException(new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID, IStatus.OK, e
                .getMessage(), e));
            }
            tscInfo.getFile().move(oldFile.getFullPath(), false, null);
          }

          // move new file to (the root of) the CodeCover-folder
          try {
            codeCoverFolder = createCodeCoverFolder(project);
            newFile.add(findNewFilename(filename, codeCoverFolder, tscInfo == null));
          } catch (TSCFileCreateException e) {
            try {
              /*
               * restore old file (move it from temp folder back to the CodeCover-folder)
               */
              if (tscInfo != null && oldFile != null) {
                oldFile.move(tscInfo.getFile().getFullPath(), false, null);
              }
            } catch (Exception exception) {
              /*
               * catch all exceptions to be able to throw the CoreException
               */
            }
            throw new CoreException(new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID, IStatus.OK, e
              .getMessage(), e));
          }
          // may throw CoreException
          tempFile.move(newFile.get(0).getFullPath(), false, null);
          if (tscInfo != null) {
            /*
             * just done for safety. the path of the file doesn't (and mustn't) change, but maybe an IFile
             * isn't just defined through its path
             */
            tscInfo.setFile(newFile.get(0));
            tscInfo.setSynchronized(true);
          }
          monitor.worked(1); // #4

          // delete old file from temp folder
          if (oldFile != null) {
            try {
              oldFile.delete(true, null);
            } catch (CoreException ce) {
              logger.error("Error while deleting old" + //$NON-NLS-1$
                " file from temporary folder.", ce); //$NON-NLS-1$
            }
          }
          monitor.worked(1); // #5

          // sync workspace with saved file in the local file system
          try {
            newFile.get(0).refreshLocal(IResource.DEPTH_ZERO, null);
          } catch (CoreException e) {
            logger.warning("Couldn't refresh workspace for" + //$NON-NLS-1$
              " written file: " //$NON-NLS-1$
              + newFile.get(0).toString(), e);
          }
          monitor.worked(1); // #6

          logger.info("Wrote test session container: " //$NON-NLS-1$
            + newFile.get(0).getFullPath().toString());
        } finally {
          monitor.done();
        }
      }
    };

    try {
      /*
       * combine the rule for opening the project and the rule (which is the project itself) for modifying the
       * content of the project
       */
      workspace.run(writeRunnable, MultiRule.combine(ruleFactory.modifyRule(project), project),
        IWorkspace.AVOID_UPDATE, monitor);
    } catch (CoreException e) {
      if (e.getCause() != null) {
        if (e.getCause() instanceof TSCFileCreateException) {
          throw (TSCFileCreateException) e.getCause();
        } else if (e.getCause() instanceof FileSaveException) {
          throw (FileSaveException) e.getCause();
        } else if (e.getCause() instanceof OutOfMemoryError) {
          throw (OutOfMemoryError) e.getCause();
        }
      }
      throw new FileSaveException("Error while writing test session container: " //$NON-NLS-1$
        + filename, e);
    }
    return newFile.get(0);
  }

  /**
   * Generates a file in the CodeCover-folder or the folder for temporary files of CodeCover.
   * 
   * @param requestedFilename the requested filename, if <code>null</code> or empty a default format is
   *        used; If this filename already exists in the folder, the suffix <code>.N</code> is appended to
   *        the given filename. Whereas <code>N</code> is a number so that the extended
   *        <code>filename.N</code> is unique in the CodeCover-folder of the given project.
   * @param project the project the created file belongs to
   * @param temporary <code>true</code> if a temporary file is to be create, <code>false</code> otherwise
   * @return a new file in the CodeCover-folder or the folder for temporary files of CodeCover, whereas the
   *         file is associated with the given project by CodeCover
   * @throws TSCFileCreateException if the file or a containing folder could not be created
   */
  private static IFile generateTSCFile(String requestedFilename, IProject project, boolean temporary)
    throws TSCFileCreateException {
    IFolder codeCoverFolder;
    IFolder tempFolder;
    IFolder folder;
    IFile reqFile;

    // create CodeCover-folder if it doesn't exist
    // may throw TSCFileCreateException
    codeCoverFolder = createCodeCoverFolder(project);
    if (temporary) {
      // create folder for temporary files if it doesn't exist
      // may throw TSCFileCreateException
      tempFolder = createTempFolder(project);
      folder = tempFolder;
    } else {
      folder = codeCoverFolder;
    }

    // ensure that the file doesn't already exist
    reqFile = findNewFilename(requestedFilename, folder, true);

    // create the file
    try {
      reqFile.create(new ByteArrayInputStream(new byte[0]), false, null);
    } catch (CoreException e) {
      throw new TSCFileCreateException("Couldn't create file", e); //$NON-NLS-1$
    }

    return reqFile;
  }

  /**
   * Creates the CodeCover-folder of the given project if it doesn't exist already.
   * 
   * @param project the project the folder to create belongs to
   */
  private static IFolder createCodeCoverFolder(IProject project) throws TSCFileCreateException {
    IFolder codeCoverFolder = project.getFolder(CodeCoverPlugin.CODECOVER_FOLDER);
    if (!codeCoverFolder.exists()) {
      try {
        codeCoverFolder.create(false, true, null);
      } catch (CoreException e) {
        throw new TSCFileCreateException("Couldn't create CodeCover-folder", e); //$NON-NLS-1$
      }
    }
    return codeCoverFolder;
  }

  /**
   * Creates the folder for temporary files if it doesn't exist.
   * 
   * @param project the project the folder to create belongs to
   */
  private static IFolder createTempFolder(IProject project) throws TSCFileCreateException {
    IFolder codeCoverFolder;
    IFolder tempFolder;

    // may throw TSCFileCreateException
    codeCoverFolder = createCodeCoverFolder(project);

    tempFolder = codeCoverFolder.getFolder(CodeCoverPlugin.TEMP_FOLDER);

    // create folder for temporary files if it doesn't exist
    if (!tempFolder.exists()) {
      try {
        tempFolder.create(false, true, null);
      } catch (CoreException e) {
        throw new TSCFileCreateException("Couldn't create temporary folder", e);//$NON-NLS-1$
      }
    }
    return tempFolder;
  }

  /**
   * Finds an unused filename in the given folder.
   * 
   * @param requestedFilename the requested filename; If it is <code>null</code>, a filename in a default
   *        format is used which incorporates the current date and time. If the requested or generated
   *        filename already exists in the folder and parameter <code>findAlternative</code> is
   *        <code>true</code>, the suffix <code>.N</code> is appended to the given filename. Whereas
   *        <code>N</code> is a number so that the extended <code>filename.N</code> is unique in the given
   *        folder.
   * @param folder the folder to find an unused filename in
   * @param findAlternative if <code>true</code> and the requested filename already exists in the folder,
   *        this method tries to find an alternative name; if <code>false</code>, this method just checks
   *        if this filename is not already in use and throws an exception if this is the case
   * @return an unused filename in the given folder as an <code>IFile</code> object (the object just points
   *         to the filename, the file doesn't exist yet)
   * @throws TSCFileCreateException if the requested filename already exists or if this method exceeded the
   *         maximum number of tries in finding an unused filename in the given folder
   */
  private static IFile findNewFilename(String requestedFilename, IFolder folder, boolean findAlternative)
    throws TSCFileCreateException {
    IFile reqFile;
    IFolder folderWithSameName;
    String suffixedFilename;
    IFile suffixedFile;
    // maximum number of tries to find a non-existing filename
    final int MAX_TRIES = 1024;
    int nbr;

    // if no specific filename was requested a default one is used
    if (requestedFilename == null || requestedFilename.length() == 0) {
      requestedFilename =
        TSC_FILENAME_PREFIX + TSC_FILENAME_SEPERATOR + TSC_FILENAME_DATE_SUFFIX.format(new Date())
          + TSC_FILENAME_EXTENSION;
    }

    reqFile = folder.getFile(requestedFilename);
    folderWithSameName = folder.getFolder(reqFile.getName());

    if (findAlternative && (reqFile.exists() || folderWithSameName.exists())) {
      nbr = 1;
      do {
        suffixedFilename = reqFile.getName() + TSC_FILENAME_SEPERATOR + nbr++;
        suffixedFile = folder.getFile(suffixedFilename);
        folderWithSameName = folder.getFolder(suffixedFilename);
      } while ((suffixedFile.exists() || folderWithSameName.exists()) && nbr < MAX_TRIES);

      if (suffixedFile.exists() || folderWithSameName.exists()) {
        throw new TSCFileCreateException("exceeded maximum number of tries to" + //$NON-NLS-1$
          " find a unique filename"); //$NON-NLS-1$
      }

      reqFile = suffixedFile;
    } else if (!findAlternative && (reqFile.exists() || folderWithSameName.exists())) {
      throw new TSCFileCreateException("filename already in use"); //$NON-NLS-1$
    }
    return reqFile;
  }

  /*
   * methods to load test session containers
   */

  /**
   * Loads a known test session container.
   * 
   * @param tscInfo the <code>TSContainerInfo</code>-representation of the test session container to load
   * @param monitor a progress monitor or <code>null</code> if progress reporting is not desired
   * @return the loaded <code>TestSessionContainer</code>
   * @throws FileLoadException if the <code>TestSessionContainer</code> can't be read from the contents of
   *         the file
   */
  static TestSessionContainer load(TSContainerInfo tscInfo, IProgressMonitor monitor)
    throws FileLoadException {
    monitor = (monitor != null) ? monitor : new NullProgressMonitor();
    // may throw FileLoadException
    return TSContainerStorage.load(tscInfo.getFile(), true, monitor);
  }

  /**
   * Loads the <code>TestSessionContainer</code> from the given file.
   * 
   * @param file the file which contains the test session container to load
   * @param fully if <code>true</code> fully loads the test session container, else only loads some infos
   *        about the test session container (see {@link TestSessionContainer#loadInfoOnly(
   *        org.codecover.model.extensions.PluginManager, Logger, MASTBuilder, String)} for details)
   * @param monitor a progress monitor or <code>null</code> if progress reporting is not desired
   * @return the <code>TestSessionContainer</code> of the given file or <code>null</code> if the given
   *         file doesn't exist.
   * @throws FileLoadException if the <code>TestSessionContainer</code> can't be read from the contents of
   *         the file
   */
  static TestSessionContainer load(final IFile file, final boolean fully, IProgressMonitor monitor)
    throws FileLoadException {
    Logger logger = CodeCoverPlugin.getDefault().getLogger();
    MASTBuilder builder = new MASTBuilder(logger);
    TestSessionContainer tsc = null;
    monitor = (monitor != null) ? monitor : new NullProgressMonitor();

    try {
      monitor.beginTask(String.format(MONITOR_LOADING_TEST_SESSION_CONTAINER_FROM, file.getName()), 2);
      monitor.worked(1);
      if (fully) {
        // may throw FileLoadException
        tsc =
          TestSessionContainer.load(
            CodeCoverPlugin.getDefault().getEclipsePluginManager().getPluginManager(), logger, builder, file
              .getLocation().toOSString());
      } else {
        // may throw FileLoadException
        tsc =
          TestSessionContainer.loadInfoOnly(CodeCoverPlugin.getDefault().getEclipsePluginManager()
            .getPluginManager(), logger, builder, file.getLocation().toOSString());
      }
      monitor.worked(1);
    } finally {
      monitor.done();
    }

    return tsc;
  }

  /**
   * Fetches all files of test session containers of all open projects which are currently in the workspace.
   * It is guaranteed that the returned list only contains files (<code>IFile</code>s), however these
   * files don't have to contain test session containers.
   * 
   * @return all files of test session containers of all open projects which are currently in the workspace.
   */
  static List<IFile> fetchTSCFiles() {
    return TSContainerStorage.fetchTSCFiles(ResourcesPlugin.getWorkspace().getRoot().getProjects());
  }

  /**
   * Fetches all files of test session containers of the given projects. It is guaranteed that the returned
   * list only contains files (<code>IFile</code>s), however these files don't have to contain test
   * session containers.
   * 
   * @param projects the projects to fetch the files from
   * @return all files of test session containers of the given projects which are currently in the workspace.
   */
  private static List<IFile> fetchTSCFiles(IProject[] projects) {
    List<IFile> tscFiles = new LinkedList<IFile>();
    IFolder codeCoverFolder;
    IResource[] directChildren;
    for (IProject project : projects) {
      if (project.isOpen()) {
        codeCoverFolder = project.getFolder(CodeCoverPlugin.CODECOVER_FOLDER);
        if (codeCoverFolder.exists()) {
          try {
            directChildren = codeCoverFolder.members();
          } catch (CoreException e) {
            // can't happen, just to be safe
            continue;
          }
          for (IResource file : directChildren) {
            if (file instanceof IFile) {
              tscFiles.add((IFile) file);
            }
          }
        }
      }
    }
    return tscFiles;
  }

  /**
   * Deletes files of test session containers.
   * 
   * @param tscInfos the list of test session containers which files are to be deleted
   * @param monitor a progress monitor or <code>null</code> if progress reporting is not desired
   * @throws NullPointerException if the given list of test session containers is null
   * @throws IllegalArgumentException if the given list of test session containers is empty
   * @throws CoreException if deletion of one or more files failed (see {@link IResource#delete(boolean,
   *         IProgressMonitor)} for details); only the <code>CoreException</code> of the last failed
   *         deletion is thrown
   * @throws CancelException if a request to cancel is detected (with the given progress monitor)
   */
  void deleteTSCFiles(List<TSContainerInfo> tscInfos, IProgressMonitor monitor)
    throws CoreException, CancelException {
    CoreException lastFileDeleteException;
    final int monitorScale = 1000;
    monitor = (monitor != null) ? monitor : new NullProgressMonitor();
    String cancelDescr = "Canceled deletion of test " + //$NON-NLS-1$
      " session container files.\n" + //$NON-NLS-1$
      "Changes happened so far:\n"; //$NON-NLS-1$

    if (tscInfos == null) {
      throw new NullPointerException("tscInfos mustn't be null"); //$NON-NLS-1$
    }
    if (tscInfos.isEmpty()) {
      throw new IllegalArgumentException("tscInfos mustn't be empty"); //$NON-NLS-1$
    }

    try { // this try only ensures that the monitor is left done()
      monitor.beginTask(MONITOR_DELETING_TSC_FILES, tscInfos.size() * monitorScale);
      if (monitor.isCanceled()) {
        throw new CancelException(cancelDescr + "No files deleted."); //$NON-NLS-1$
      }
      lastFileDeleteException = null;
      for (TSContainerInfo tscToDelete : tscInfos) {
        try {
          tscToDelete.getFile().delete(true, new SubProgressMonitor(monitor, 1 * monitorScale));
        } catch (CoreException e) {
          lastFileDeleteException = e;
        }
        cancelDescr += tscToDelete.getPath().toString() + " deleted\n"; //$NON-NLS-1$
        if (monitor.isCanceled()) {
          throw new CancelException(cancelDescr);
        }
      }
      if (lastFileDeleteException != null) {
        throw lastFileDeleteException;
      }
    } finally {
      monitor.done();
    }
  }

}
