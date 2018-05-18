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

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.codecover.eclipse.CodeCoverPlugin;
import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.XMLMemento;

/**
 * Saves which test cases are active and which test session container is active, if Eclipse requests to do so.
 * 
 * @see TSContainerManagerSaveParticipantHandler
 * @author Robert Hanussek
 * @version 1.0 ($Id: ActiveTestCasesSaveParticipant.java 49 2009-06-01 08:52:19Z ahija $)
 */
public class ActiveTestCasesSaveParticipant
  implements SaveParticipant {

  private final TSContainerManager tscManager;

  private final ActiveTestCasesStorage testCasesStorage;

  /**
   * Constructor.
   * 
   * @param tscManager the {@link TSContainerManager}
   * @param testCasesStorage the to be saved test cases.
   */
  public ActiveTestCasesSaveParticipant(TSContainerManager tscManager, ActiveTestCasesStorage testCasesStorage) {
    this.tscManager = tscManager;
    this.testCasesStorage = testCasesStorage;
  }

  @Override
public void prepareToSave(ISaveContext context) throws CoreException {
    // nothing to do here
  }

  @Override
public boolean saving(ISaveContext context) throws CoreException {
    switch (context.getKind()) {
      case ISaveContext.PROJECT_SAVE:
      case ISaveContext.FULL_SAVE:
        try {
          this.saveAllActiveTestCases(context);
        } catch (CoreException e) {
          throw e; // throw exception to signal failure in saving
        }
        return true; // it's always saved something

      case ISaveContext.SNAPSHOT:
        /*
         * Snapshot savings must be very fast but saving the active test cases can be a long-running operation
         * if there are many of them. Thus nothing is done here.
         */
    }
    return false;
  }

  @Override
public void doneSaving(ISaveContext context) {
    String oldFileName;
    File f;
    switch (context.getKind()) {
      case ISaveContext.PROJECT_SAVE:
      case ISaveContext.FULL_SAVE:
        // delete the old file since it is not necessary anymore
        oldFileName =
          ActiveTestCasesStorage.ACTIVE_TEST_CASES_SAVE_FILENAME
            + Integer.toString(context.getPreviousSaveNumber());
        f = CodeCoverPlugin.getDefault().getStateLocation().append(oldFileName).toFile();
        f.delete();
    }
  }

  @Override
public void rollback(ISaveContext context) {
    String saveFileName;
    File f;
    /*
     * Since the save operation has failed, delete the saved active test cases that were just written. We
     * don't have to worry about the fact that we mapped the following file name into the ISaveContext. The
     * platform will discard the context when a save operation fails
     */
    switch (context.getKind()) {
      case ISaveContext.PROJECT_SAVE:
      case ISaveContext.FULL_SAVE:
        saveFileName =
          ActiveTestCasesStorage.ACTIVE_TEST_CASES_SAVE_FILENAME + Integer.toString(context.getSaveNumber());
        f = CodeCoverPlugin.getDefault().getStateLocation().append(saveFileName).toFile();
        f.delete();
    }
  }

  /**
   * This method saves which test cases are currently active and which test session container is currently
   * active. Not only the active test cases of the currently active test session container are saved but also
   * the active test cases of all other known test session containers.
   * <p>
   * A write lock of the <code>TSContainerManager</code> is acquired during the execution of this method.
   * 
   * @param context the save context
   * @throws IOException if the save failed
   */
  private void saveAllActiveTestCases(ISaveContext context) throws CoreException {
    String saveFileName =
      ActiveTestCasesStorage.ACTIVE_TEST_CASES_SAVE_FILENAME + Integer.toString(context.getSaveNumber());
    File saveFile = CodeCoverPlugin.getDefault().getStateLocation().append(saveFileName).toFile();
    FileWriter writer;
    XMLMemento rootMemento =
      XMLMemento.createWriteRoot(ActiveTestCasesStorage.ACTIVE_TEST_CASES_MEMENTO_ROOT);
    ActiveTSContainerInfo activeTSCInfo;
    List<TSContainerInfo> tscInfos;
    Map<String, List<TestCaseInfo>> bufferedTestCases;

    synchronized (this.tscManager.getWriteLock()) {
      activeTSCInfo = this.tscManager.getActiveTSContainer();
      tscInfos = this.tscManager.getTestSessionContainers();
      bufferedTestCases = this.testCasesStorage.getBufferedActiveTestCases();
    }
    ActiveTestCasesStorage.storeActiveTestSessionContainer(activeTSCInfo, rootMemento);
    ActiveTestCasesStorage.storeAllActiveTestCases(tscInfos, bufferedTestCases, rootMemento);

    /*
     * if writing fails, an exception is thrown and we do not update the path
     */
    try {
      writer = new FileWriter(saveFile);
      rootMemento.save(writer);
    } catch (IOException e) {
      throw new CoreException(new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID, IStatus.OK,
        "Error while saving active test cases", //$NON-NLS-1$
        e));
    }
    try {
      writer.close();
    } catch (IOException e) {
      throw new CoreException(new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID, IStatus.OK,
        "Error while saving active test cases", //$NON-NLS-1$
        e));
    }
    // update the path of the file with the saved active test cases
    context.map(new Path(ActiveTestCasesStorage.ACTIVE_TEST_CASES_SAVE_FILENAME), new Path(saveFileName));
  }

}
