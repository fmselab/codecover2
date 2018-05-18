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
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.model.utils.CollectionUtil;
import org.codecover.model.utils.Logger;
import org.eclipse.core.resources.ISavedState;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.XMLMemento;

/**
 * Contains methods for storing and restoring which test cases are active and which test session container is
 * active. Locking of the processed objects (the methods' parameters) must be done by the calling context.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: ActiveTestCasesStorage.java 49 2009-06-01 08:52:19Z ahija $)
 */
class ActiveTestCasesStorage {

  static final String ACTIVE_TEST_CASES_SAVE_FILENAME = "activetestcases.xml"; //$NON-NLS-1$

  static final String ACTIVE_TEST_CASES_MEMENTO_ROOT = "activetestcases"; //$NON-NLS-1$

  private static final String ACTIVE_TEST_CASES_MEMENTO_ACTIVE_TSC = "activetsc"; //$NON-NLS-1$

  private static final String ACTIVE_TEST_CASES_MEMENTO_TSC = "tsc"; //$NON-NLS-1$

  private static final String ACTIVE_TEST_CASES_MEMENTO_TEST_CASE = "tc"; //$NON-NLS-1$

  private static final String ACTIVE_TEST_CASES_MEMENTO_TEST_SESSION = "ts"; //$NON-NLS-1$   

  /**
   * Contains buffered active test cases (of test session containers (identified by their path ({@link IPath#toPortableString()}))
   * which reside in currently closed projects or generally are currently unknown).
   */
  private final Map<String, List<TestCaseInfo>> activeTestCasesOfUnknownTSCs;

  private final Logger logger;

  private final Object lock;

  ActiveTestCasesStorage(Logger logger) {
    this.lock = new Object();
    this.activeTestCasesOfUnknownTSCs = new HashMap<String, List<TestCaseInfo>>();
    this.logger = logger;
  }

  /**
   * Loads the information which test cases were active at the given saved state and returns the path to the
   * test session container which was active at the given saved state.
   * 
   * @param lastState the saved state
   * @return the path to the test session container which was active at the given saved state or
   *         <code>null</code> if none was active or it is unknown which one was active (the path was
   *         produces by {@link IPath#toPortableString()})
   */
  String loadAllActiveTestCases(ISavedState lastState) {
    IPath lastSaveFilePath;
    String lastSaveFileName;
    File lastSaveFile;
    FileReader reader = null;
    IMemento rootMemento;
    String lastTSCPath = null;
    // check if there is anything saved at all
    if ((lastSaveFilePath = lastState.lookup(new Path(ACTIVE_TEST_CASES_SAVE_FILENAME))) == null) {
      return null;
    }
    lastSaveFileName = lastSaveFilePath.toString();
    lastSaveFile = CodeCoverPlugin.getDefault().getStateLocation().append(lastSaveFileName).toFile();
    try {
      reader = new FileReader(lastSaveFile);
      rootMemento = XMLMemento.createReadRoot(reader);
      this.restoreAllActiveTestCases(rootMemento);
      lastTSCPath = fetchLastActiveTestSessionContainer(rootMemento);
    }
    /* catch FileNotFoundException and WorkbenchException */
    catch (Exception e) {
      logger.error("Error loading active test cases from " //$NON-NLS-1$
        + lastSaveFileName, e);
      return null;
    } finally {
      if (reader != null) {
        try {
          reader.close();
        } catch (IOException e) {
          // pfff... who cares?
        }
      }
    }
    return lastTSCPath;
  }

  /**
   * Restores (from memento) which test cases are active (into the buffer).
   * 
   * @param rootMemento the root memento to get the saved information from
   */
  void restoreAllActiveTestCases(IMemento rootMemento) {
    List<TestCaseInfo> activeTestCases;
    synchronized (this.lock) {
      for (IMemento tscMemento : rootMemento.getChildren(ACTIVE_TEST_CASES_MEMENTO_TSC)) {
        activeTestCases = fetchActiveTestCaseInfos(tscMemento);
        this.bufferActiveTestCases(tscMemento.getID(), activeTestCases);
      }
    }
  }

  /**
   * Generates <code>TestCaseInfo</code>s from the given memento.
   */
  private static List<TestCaseInfo> fetchActiveTestCaseInfos(IMemento tscMemento) {
    String curTestSessionName;
    List<TestCaseInfo> tscInfos = new LinkedList<TestCaseInfo>();
    for (IMemento testSessionMemento : tscMemento.getChildren(ACTIVE_TEST_CASES_MEMENTO_TEST_SESSION)) {
      curTestSessionName = testSessionMemento.getID();
      for (IMemento testCaseMemento : testSessionMemento.getChildren(ACTIVE_TEST_CASES_MEMENTO_TEST_CASE)) {
        tscInfos.add(new TestCaseInfo(testCaseMemento.getID(), curTestSessionName));
      }
    }
    return tscInfos;
  }

  /**
   * Returns the path of the last active test session container.
   * 
   * @param rootMemento the root memento to get the saved information from
   * @return the path of the active test session container or <code>null</code> if none is stored in the
   *         given memento (the path was produces by {@link IPath#toPortableString()})
   */
  private static String fetchLastActiveTestSessionContainer(IMemento rootMemento) {
    IMemento activeTSCMemento = rootMemento.getChild(ACTIVE_TEST_CASES_MEMENTO_ACTIVE_TSC);
    if (activeTSCMemento != null) {
      return activeTSCMemento.getID();
    } else {
      return null;
    }
  }

  /**
   * Buffers the active test cases of the given test session container. This method is used to buffer active
   * test cases of currently closed test session containers.
   * 
   * @param tscInfo the <code>TSContainerInfo</code>-representation of the test session container which
   *        active test cases are to be buffered
   */
  void bufferActiveTestCases(TSContainerInfo tscInfo) {
    List<TestCaseInfo> activeTestCaseInfos = tscInfo.getActiveTestCases();
    if (!activeTestCaseInfos.isEmpty()) {
      this.bufferActiveTestCases(tscInfo.getPath().toPortableString(), activeTestCaseInfos);
    }
  }

  /**
   * Buffer the given active test cases of the test session container which file is located at the given path.
   */
  private void bufferActiveTestCases(String path, List<TestCaseInfo> testCases) {
    if (!testCases.isEmpty()) {
      synchronized (this.lock) {
        this.activeTestCasesOfUnknownTSCs.put(path, testCases);
      }
    }
  }

  Map<String, List<TestCaseInfo>> getBufferedActiveTestCases() {
    synchronized (this.lock) {
      return CollectionUtil.copy(this.activeTestCasesOfUnknownTSCs);
    }
  }

  void unbufferActiveTestCases(TSContainerInfo tscInfo) {
    List<TestCaseInfo> testCaseInfos;
    synchronized (this.lock) {
      testCaseInfos = this.activeTestCasesOfUnknownTSCs.remove(tscInfo.getPath().toPortableString());
    }
    if (testCaseInfos != null) {
      tscInfo.setActiveTestCases(testCaseInfos);
    }
  }

  static void storeActiveTestSessionContainer(ActiveTSContainerInfo activeTSCInfo, IMemento rootMemento) {
    if (activeTSCInfo != null) {
      rootMemento.createChild(ACTIVE_TEST_CASES_MEMENTO_ACTIVE_TSC, activeTSCInfo.getPath()
        .toPortableString());
    }
  }

  /**
   * This method stores in the given memento which test cases of the given test session containers are active.
   * <p>
   * The active test cases of the given test session containers mustn't change during the execution of this
   * method. Thus it is recommended to pass immutable copies as the parameters.
   * </p>
   * 
   * @param tscInfos the test session containers which active test cases are to be stored
   * @param rootMemento the memento
   */
  static void storeAllActiveTestCases(List<TSContainerInfo> tscInfos, Map<String, List<TestCaseInfo>> bufTCs,
    IMemento rootMemento) {
    IMemento curTSCMemento;
    List<String> storedTestSessions = new LinkedList<String>();
    List<TestCaseInfo> curActiveTestCases;
    /*
     * store active test cases of given (known) test session containers
     */
    for (TSContainerInfo tscInfo : tscInfos) {
      // don't store that no test cases are active
      if (tscInfo.getActiveTestCases().isEmpty()) {
        continue;
      }
      storedTestSessions.clear();
      // create memento child for current test session container
      curTSCMemento =
        rootMemento.createChild(ACTIVE_TEST_CASES_MEMENTO_TSC, tscInfo.getPath().toPortableString());
      /*
       * create memento child for each test session which has active test cases
       */
      for (TestCaseInfo tcInfo : tscInfo.getActiveTestCases()) {
        if (!storedTestSessions.contains(tcInfo.getTestSessionName())) {
          storeTestSession(tcInfo.getTestSessionName(), tscInfo.getActiveTestCases(), curTSCMemento);
          storedTestSessions.add(tcInfo.getTestSessionName());
        }
      }
    }
    /*
     * store buffered active test cases (of test session containers which reside in currently closed projects)
     */
    for (String tscPath : bufTCs.keySet()) {
      storedTestSessions.clear();
      // create memento child for current test session container
      curTSCMemento = rootMemento.createChild(ACTIVE_TEST_CASES_MEMENTO_TSC, tscPath);
      /*
       * create memento child for each test session which has active test cases
       */
      curActiveTestCases = bufTCs.get(tscPath);
      for (TestCaseInfo tcInfo : curActiveTestCases) {
        if (!storedTestSessions.contains(tcInfo.getTestSessionName())) {
          storeTestSession(tcInfo.getTestSessionName(), curActiveTestCases, curTSCMemento);
          storedTestSessions.add(tcInfo.getTestSessionName());
        }
      }
    }
  }

  /**
   * Stores the subset of given test cases which belongs to a test session with the given name in the given
   * memento.
   * 
   * @param testSessionName the name of the test session which test cases are to be stored
   * @param tcInfos all <code>TestCaseInfo</code>s (including the ones to be stored)
   * @param tscMemento the memento
   */
  private static void storeTestSession(String testSessionName, List<TestCaseInfo> tcInfos, IMemento tscMemento) {
    List<TestCaseInfo> tcInfosByTestSession = fetchTestCaseInfos(testSessionName, tcInfos);
    IMemento testSessionMemento =
      tscMemento.createChild(ACTIVE_TEST_CASES_MEMENTO_TEST_SESSION, testSessionName);
    for (TestCaseInfo tcInfo : tcInfosByTestSession) {
      testSessionMemento.createChild(ACTIVE_TEST_CASES_MEMENTO_TEST_CASE, tcInfo.getName());
    }
  }

  /**
   * Returns the test cases which belong to a test session with the given name.
   */
  private static List<TestCaseInfo> fetchTestCaseInfos(String testSessionName, List<TestCaseInfo> tcInfos) {
    List<TestCaseInfo> tcInfosByTestSession = new LinkedList<TestCaseInfo>();
    for (TestCaseInfo tcInfo : tcInfos) {
      if (tcInfo.getTestSessionName().equals(testSessionName)) {
        tcInfosByTestSession.add(tcInfo);
      }
    }
    return tcInfosByTestSession;
  }

}
