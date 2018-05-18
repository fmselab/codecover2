package org.codecover.eclipse.tscmanager;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.utils.CollectionUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

/**
 * Represents an active <code>TestSessionContainer</code> and contains a reference to the represented test
 * session container and a list of the active test cases of the represented test session container. An object
 * of this class isn't updated after being created, e.g. the set of active test cases contained in an object
 * of this class isn't adapted to later changes in the selection of active test cases.
 * <p>
 * This class doesn't extend <code>TSContainerInfo</code> because it should be used as a throw-away-object,
 * it is only intended to be used as long as the represented test session container is active and not as a
 * replacement for a TSContainerInfo. The reason for this is that objects of this class contain a reference to
 * a test session container and as long as this reference exists the test session container resides in main
 * memory. This problem is also the reason why <code>TSContainerHandle</code> isn't public.
 * <p>
 * In the documentation of this class the term <em>represented test session
 * container</em> is used for the
 * active <code>TestSessionContainer</code> which is represented by an object of this class.
 */
public class ActiveTSContainerInfo
  implements TSContainerHandle {

  private final TSContainerInfo tscInfo;

  private final TestSessionContainer tsc;

  private final Set<TestCase> testCases;

  private final Set<TestCase> RedundantTestCases;

  /**
   * Constructs a new <code>ActiveTSContainerInfo</code>-representation of a test session container.
   * 
   * @param tscInfo the <code>TSContainerInfo</code>-representation of the represented test session
   *        container
   * @param tsc the represented test session container
   * @param testCases the active test cases of the represented test session container, the given set is copied
   *        into a new immutable set
   * @throws NullPointerException if one of the given parameters is <code>null</code>
   */
  ActiveTSContainerInfo(TSContainerInfo tscInfo, TestSessionContainer tsc, Set<TestCase> testCases) {
    if (tscInfo == null) {
      throw new NullPointerException("tscInfo mustn't be null"); //$NON-NLS-1$
    }
    if (tsc == null) {
      throw new NullPointerException("tsc mustn't be null"); //$NON-NLS-1$
    }
    this.tscInfo = tscInfo;
    this.tsc = tsc;
    if (testCases != null) {
      this.testCases = CollectionUtil.copy(testCases);
      this.storeActiveTestCases();
    } else {
      this.testCases = restoreActiveTestCases(this.tscInfo, this.tsc);
    }
    this.RedundantTestCases = null;
  }

  /**
   * Constructs a new <code>ActiveTSContainerInfo</code>-representation of a test session container.
   * 
   * @param tscInfo the <code>TSContainerInfo</code>-representation of the represented test session
   *        container
   * @param tsc the represented test session container
   * @param testCases the active test cases of the represented test session container, the given set is copied
   *        into a new immutable set
   * @param RedundantTestCases the redundant test cases identified by human tester
   * @throws NullPointerException if one of the given parameters is <code>null</code>
   */
  ActiveTSContainerInfo(TSContainerInfo tscInfo, TestSessionContainer tsc, Set<TestCase> testCases,
    Set<TestCase> RedundantTestCases) {
    if (tscInfo == null) {
      throw new NullPointerException("tscInfo mustn't be null"); //$NON-NLS-1$
    }
    if (tsc == null) {
      throw new NullPointerException("tsc mustn't be null"); //$NON-NLS-1$
    }
    this.tscInfo = tscInfo;
    this.tsc = tsc;
    if (testCases != null) {
      this.testCases = CollectionUtil.copy(testCases);
      this.storeActiveTestCases();
    } else {
      this.testCases = restoreActiveTestCases(this.tscInfo, this.tsc);
    }

    if (RedundantTestCases != null) {
      this.RedundantTestCases = CollectionUtil.copy(RedundantTestCases);
      this.storeRedundantTestCases();
    } else {
      this.RedundantTestCases = restoreRedundantTestCases(this.tscInfo, this.tsc);
    }
  }

  /**
   * Stores the test cases in the <code>TSContainerInfo</code>.
   */
  private void storeActiveTestCases() {
    this.tscInfo.setActiveTestCases(this.testCases);
  }

  /**
   * Stores the redundant test cases in the <code>TSContainerInfo</code>.
   */
  private void storeRedundantTestCases() {
    this.tscInfo.setRedundantTestCases(this.testCases);
  }

  /**
   * Returns the restored active test cases of the represented test session container, i.e. translates the
   * <code>TestCaseInfo</code>s of the <code>TSContainerInfo</code>-representation in real
   * <code>TestCase</code>s and returns them. If a corresponding <code>TestCase</code> can't be found,
   * the <code>TestCaseInfo</code> is just ignored and removed from the <code>TSContainerInfo</code>.
   * 
   * @param tscInfo {@link TSContainerInfo} holding the {@link TestCaseInfo}s
   * @param tsc the {@link TestSessionContainer} holding the {@link TestCase}s
   * @return the restored active test cases of the represented test session container.
   */
  public static Set<TestCase> restoreActiveTestCases(TSContainerInfo tscInfo, TestSessionContainer tsc) {
    Set<TestCase> activeTestCases = new HashSet<TestCase>();
    TestSession testSession;
    TestCase testCase;
    /*
     * iterating over the list can't rise concurrency problems because tscInfo.getActiveTestCases returns a
     * immutable copy of the list of TestCaseInfos
     */
    for (TestCaseInfo tcInfo : tscInfo.getActiveTestCases()) {
      testSession = tsc.getTestSessionWithName(tcInfo.getTestSessionName());
      if (testSession == null) {
        /*
         * can't rise concurrency problems because if tcInfo is already removed it is just ignored by method
         * removeActiveTestCase
         */
        tscInfo.removeActiveTestCase(tcInfo);
        continue;
      }
      testCase = testSession.getTestCaseWithName(tcInfo.getName());
      if (testCase == null) {
        /*
         * can't rise concurrency problems because if tcInfo is already removed it is just ignored by method
         * removeActiveTestCase
         */
        tscInfo.removeActiveTestCase(tcInfo);
        continue;
      }
      activeTestCases.add(testCase);
    }
    return activeTestCases;
  }

  /**
   * Returns the restored redundant test cases of the represented test session container, i.e. translates the
   * <code>TestCaseInfo</code>s of the <code>TSContainerInfo</code>-representation in real
   * <code>TestCase</code>s and returns them. If a corresponding <code>TestCase</code> can't be found,
   * the <code>TestCaseInfo</code> is just ignored and removed from the <code>TSContainerInfo</code>.
   * 
   * @param tscInfo {@link TSContainerInfo} holding the {@link TestCaseInfo}s
   * @param tsc the {@link TestSessionContainer} holding the {@link TestCase}s
   * @return the restored active test cases of the represented test session container.
   */
  public static Set<TestCase> restoreRedundantTestCases(TSContainerInfo tscInfo, TestSessionContainer tsc) {
    Set<TestCase> redundantTestCases = new HashSet<TestCase>();
    TestSession testSession;
    TestCase testCase;
    /*
     * iterating over the list can't rise concurrency problems because tscInfo.getActiveTestCases returns a
     * immutable copy of the list of TestCaseInfos
     */
    for (TestCaseInfo tcInfo : tscInfo.getRedundantTestCases()) {
      testSession = tsc.getTestSessionWithName(tcInfo.getTestSessionName());
      if (testSession == null) {
        /*
         * can't rise concurrency problems because if tcInfo is already removed it is just ignored by method
         * removeActiveTestCase
         */
        tscInfo.removeRedundantTestCase(tcInfo);
        continue;
      }
      testCase = testSession.getTestCaseWithName(tcInfo.getName());
      if (testCase == null) {
        /*
         * can't rise concurrency problems because if tcInfo is already removed it is just ignored by method
         * removeActiveTestCase
         */
        tscInfo.removeRedundantTestCase(tcInfo);
        continue;
      }
      redundantTestCases.add(testCase);
    }
    return redundantTestCases;
  }

  /**
   * Returns the <code>TSContainerInfo</code>-representation of the represented test session container.
   * 
   * @return the <code>TSContainerInfo</code>-representation of the represented test session container
   */
  public TSContainerInfo getTSContainerInfo() {
    return this.tscInfo;
  }

  /**
   * Returns the represented test session container.
   * 
   * @return the represented test session container
   */
  public TestSessionContainer getTestSessionContainer() {
    return this.tsc;
  }

  /**
   * Returns an immutable copy of the set of active <code>TestCase</code>s of the represented test session
   * container.
   * 
   * @return an immutable copy of the set of active <code>TestCase</code>s of the represented test session
   *         container or an (immutable) empty set if none are active.
   */
  public Set<TestCase> getActiveTestCases() {
    return this.testCases;
  }

  /**
   * Returns an immutable copy of the set of redundant <code>TestCase</code>s of the represented test
   * session container.
   * 
   * @return an immutable copy of the set of redundant <code>TestCase</code>s of the represented test
   *         session container or an (immutable) empty set if none are redundant.
   */
  public Set<TestCase> getRedundantTestCases() {
    return this.RedundantTestCases;
  }

  /**
   * Returns the file of the represented test session container.
   * <p>
   * A call to this method is equal to calling
   * <code>ActiveTSContainerInfo.getTSContainerInfo().getFile()</code>.
   * 
   * @return the file of the represented test session container
   */
  @Override
public IFile getFile() {
    return this.tscInfo.getFile();
  }

  /**
   * Returns the ID of the represented test session container, do <em>not</em> use this ID as a unique
   * identifier for this representation of a test session container or for the represented test session
   * container itself, because the same test session container can appear twice in the workspace (e.g., if the
   * user imported it two times).
   * <p>
   * A call to this method is equal to calling <code>ActiveTSContainerInfo.getTSContainerInfo().getId()</code>.
   * 
   * @return the ID of the represented test session container
   */
  @Override
public String getId() {
    return this.tscInfo.getId();
  }

  /**
   * Returns the date of the represented test session container.
   * <p>
   * A call to this method is equal to calling
   * <code>ActiveTSContainerInfo.getTSContainerInfo().getDate()</code>.
   * 
   * @return the date of the represented test session container
   */
  @Override
public Date getDate() {
    return this.tscInfo.getDate();
  }

  /**
   * Returns the path of the file of the represented test session container relative to the containing
   * workspace. This path is used as the unique identifier of this representation of a test session container
   * and the represented test session container itself.
   * <p>
   * A call to this method is equal to calling
   * <code>ActiveTSContainerInfo.getTSContainerInfo().getPath()</code>.
   * 
   * @return the path of the file of the represented test session container.
   */
  @Override
public IPath getPath() {
    return this.tscInfo.getPath();
  }

  /**
   * Returns the name of the represented test session container. This name doesn't have to be unique (use the
   * path of the file ({@link #getPath()}), if you need a unique identifier for the represented test session
   * container or this representation of a test session container).
   * <p>
   * A call to this method is equal to calling
   * <code>ActiveTSContainerInfo.getTSContainerInfo().getName()</code>.
   * 
   * @return the name of the represented test session container
   */
  @Override
public String getName() {
    return this.tscInfo.getName();
  }

  /**
   * Returns the project the represented test session container belongs to.
   * <p>
   * A call to this method is equal to calling
   * <code>ActiveTSContainerInfo.getTSContainerInfo().getProject()</code>.
   * 
   * @return the project the represented test session container belongs to.
   */
  @Override
public IProject getProject() {
    return this.tscInfo.getProject();
  }

  /**
   * Returns whether the represented test session container is in sync with its file in the workspace.
   * <p>
   * A call to this method is equal to calling
   * <code>ActiveTSContainerInfo.getTSContainerInfo().isSynchronized()</code>.
   * 
   * @return <code>true</code> if the represented test session container is synchronized with its
   *         <code>IFile</code>, <code>false</code> if there are unsaved changes
   */
  @Override
public boolean isSynchronized() {
    return this.tscInfo.isSynchronized();
  }

  /**
   * Returns whether this object equals the given object. A call to this method is equivalent to calling
   * <code>ActiveTSContainerInfo.getTSContainerInfo().equals(Object)</code>.
   * 
   * @param o the object to compare to
   * @return the comparison result
   */
  @Override
  public boolean equals(Object o) {
    return this.tscInfo.equals(o);
  }

  /**
   * A call to this method is equal to calling
   * <code>ActiveTSContainerInfo.getTSContainerInfo().hashCode()</code>.
   */
  @Override
  public int hashCode() {
    return this.tscInfo.hashCode();
  }

  /**
   * Compares to represented test session containers by the name of the project they belong to and by their
   * dates.
   * <p>
   * A call to this method is equal to calling <code>ActiveTSContainerInfo.getTSContainerInfo().compareTo(
   * TSContainerHandle)</code>.
   */
  @Override
public int compareTo(TSContainerHandle tscHandle) {
    return this.tscInfo.compareTo(tscHandle);
  }

  @Override
  public String toString() {
    return String.format("%s (%2$td.%2$tm.%2$tY %2$tH:%2$tM:%2$tS)", //$NON-NLS-1$
      getId(), getDate());
  }

}