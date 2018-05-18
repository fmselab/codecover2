package org.codecover.eclipse.tscmanager;

import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.codecover.model.TestCase;
import org.codecover.model.utils.CollectionUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

/**
 * Represents a known test session container to save memory, only the location of its file, its ID, and its
 * date are saved.
 * <p>
 * In the documentation of this class the term <em>represented test session
 * container</em> is used for a
 * <code>TestSessionContainer</code> which is represented by an object of this class.
 */
public class TSContainerInfo
  implements TSContainerHandle {

  /**
   * File of the <code>TestSessionContainer</code> represented by this object. Its path is used as the
   * unique identifier for this object and the corresponding <code>TestSessionContainer</code>.
   */
  private IFile file;

  /**
   * The ID of the represented test session container.
   */
  private final String id;

  /**
   * The date of the represented test session container.
   */
  private final Date date;

  /**
   * The name of this represented test session container, which is unique during a session of Eclipse.
   */
  private final String name;

  /**
   * List of active <code>TestCase</code>s as <code>TestCaseInfo</code>s.
   */
  private List<TestCaseInfo> activeTestCases;

  private List<TestCaseInfo> redundantTestCases;

  /**
   * <code>true</code> if the represented test session container is synchronized with its <code>IFile</code>,
   * <code>false</code> if there are unsaved changes
   */
  private boolean sync;

  private final TSContainerInfoListenerHandler listenerHandler;

  private final Object lockTestCases;

  private final Object lockFile;

  /**
   * Constructs a <code>TSContainerInfo</code>-representation of a test session container with the given
   * ID, date and name and which is contained in the given file.
   * 
   * @param file the file the represented test session container is stored in
   * @param id the ID of the represented test session container
   * @param date the date of the represented test session container
   * @param name the name of the represented test session container
   */
  TSContainerInfo(IFile file, String id, Date date, String name) {
    if (file == null) {
      throw new NullPointerException("file mustn't be null");//$NON-NLS-1$
    }
    if (id == null) {
      throw new NullPointerException("id mustn't be null"); //$NON-NLS-1$
    }
    if (date == null) {
      throw new NullPointerException("date mustn't be null");//$NON-NLS-1$
    }
    if (name == null) {
      throw new NullPointerException("name mustn't be null");//$NON-NLS-1$
    }
    this.lockTestCases = new Object();
    this.lockFile = new Object();
    this.file = file;
    this.id = new String(id);
    this.date = (Date) date.clone();
    this.name = name;
    this.activeTestCases = new LinkedList<TestCaseInfo>();
    this.sync = true;
    this.listenerHandler = new TSContainerInfoListenerHandler(this);
  }

  void setFile(IFile file) {
    if (file == null) {
      throw new NullPointerException("file mustn't be null");//$NON-NLS-1$
    }
    synchronized (this.lockFile) {
      if (!file.getFullPath().equals(this.file.getFullPath())) {
        throw new IllegalArgumentException("file must have the same path"); //$NON-NLS-1$
      }
      this.file = file;
    }
  }

  /**
   * Stores the given (active) test cases represented as a list of <code>TestCaseInfo</code>s.
   * 
   * @param testCases the test cases to save
   */
  void setActiveTestCases(Set<TestCase> testCases) {
    this.setActiveTestCases(TestCaseInfo.generateTestCaseInfos(testCases));
  }

  void setActiveTestCases(List<TestCaseInfo> testCaseInfos) {
    if (testCaseInfos == null) {
      throw new NullPointerException("testCaseInfos mustn't be null"); //$NON-NLS-1$
    }
    synchronized (this.lockTestCases) {
      this.activeTestCases = new LinkedList<TestCaseInfo>(testCaseInfos);
    }
  }

  /**
   * Stores the given (redundant) test cases represented as a list of <code>TestCaseInfo</code>s.
   * 
   * @param testCases the test cases to save
   */
  void setRedundantTestCases(Set<TestCase> RedundantTestCases) {
    this.setRedundantTestCases(TestCaseInfo.generateTestCaseInfos(RedundantTestCases));
  }

  void setRedundantTestCases(List<TestCaseInfo> testCaseInfos) {
    if (testCaseInfos == null) {
      throw new NullPointerException("testCaseInfos mustn't be null"); //$NON-NLS-1$
    }
    synchronized (this.lockTestCases) {
      this.activeTestCases = new LinkedList<TestCaseInfo>(testCaseInfos);
    }
  }

  /**
   * Removes/Deactivates the given active test case. If the given test case isn't active, nothing is changed.
   * 
   * @param tcInfo the <code>TestCaseInfo</code>-representation of the active test cases to
   *        remove/deactivate
   */
  void removeActiveTestCase(TestCaseInfo tcInfo) {
    synchronized (this.lockTestCases) {
      this.activeTestCases.remove(tcInfo);
    }
  }

  /**
   * Removes/Deactivates the given redundant test case. If the given test case isn't active, nothing is
   * changed.
   * 
   * @param tcInfo the <code>TestCaseInfo</code>-representation of the redundant test cases to
   *        remove/deactivate
   */
  void removeRedundantTestCase(TestCaseInfo tcInfo) {
    synchronized (this.lockTestCases) {
      this.redundantTestCases.remove(tcInfo);
    }
  }

  /**
   * Sets whether the represented test session container is in sync with its file in the workspace.
   * 
   * @param sync <code>true</code> if the represented test session container is synchronized with its
   *        <code>IFile</code>, <code>false</code> if there are unsaved changes
   */
  void setSynchronized(boolean sync) {
    this.sync = sync;
    this.listenerHandler.fireSynchronizedStateChanged(sync);
  }

  /**
   * Adds the given listener to receive events from the associated <code>TSContainerInfo</code>.
   * 
   * @param listener the listener
   * @throws NullPointerException if the specified listener is <code>null</code>
   */
  void addListener(TSContainerInfoListener listener) {
    this.listenerHandler.addListener(listener);
  }

  /**
   * Removes the given listener so that it no longer receives events from the associated
   * <code>TSContainerInfo</code>.
   * 
   * @param listener the listener
   */
  void removeListener(TSContainerInfoListener listener) {
    this.listenerHandler.removeListener(listener);
  }

  /**
   * Returns the file of the represented test session container.
   * 
   * @return the file of the represented test session container
   */
  @Override
public IFile getFile() {
    synchronized (this.lockFile) {
      return this.file;
    }
  }

  /**
   * Returns the ID of the represented test session container, do <em>not</em> use this ID as a unique
   * identifier for this representation of a test session container or for the represented test session
   * container itself, because the same test session container can appear twice in the workspace (e.g., if the
   * user imported it two times).
   * 
   * @return the ID of the represented test session container
   */
  @Override
public String getId() {
    return this.id;
  }

  /**
   * Returns the date of the represented test session container.
   * 
   * @return the date of the represented test session container
   */
  @Override
public Date getDate() {
    return this.date;
  }

  /**
   * Returns the path of the file of the represented test session container relative to the containing
   * workspace. This path is used as the unique identifier of this representation of a test session container
   * and the represented test session container itself.
   * 
   * @return the path of the file of the represented test session container.
   */
  @Override
public IPath getPath() {
    synchronized (this.lockFile) {
      return this.file.getFullPath();
    }
  }

  /**
   * Returns the name of the represented test session container. This name doesn't have to be unique. Use the
   * path of the file ({@link #getPath()}), if you need a unique identifier for the represented test session
   * container or this representation of a test session container.
   * 
   * @return the name of the represented test session container
   */
  @Override
public String getName() {
    return this.name;
  }

  /**
   * Returns the project the represented test session container belongs to.
   * 
   * @return the project the represented test session container belongs to.
   */
  @Override
public IProject getProject() {
    synchronized (this.lockFile) {
      return this.file.getProject();
    }
  }

  /**
   * Returns the active test cases represented by an immutable copy of the list of <code>TestCaseInfo</code>s.
   * 
   * @return the active test cases represented by a list of <code>TestCaseInfo</code>s.
   */
  List<TestCaseInfo> getActiveTestCases() {
    synchronized (this.lockTestCases) {
      return CollectionUtil.copy(this.activeTestCases);
    }
  }

  /**
   * Returns the redundant test cases represented by an immutable copy of the list of
   * <code>TestCaseInfo</code>s.
   * 
   * @return the redundant test cases represented by a list of <code>TestCaseInfo</code>s.
   */
  List<TestCaseInfo> getRedundantTestCases() {
    synchronized (this.lockTestCases) {
      return CollectionUtil.copy(this.redundantTestCases);
    }
  }

  @Override
  public String toString() {
    return String.format("%s (%2$td.%2$tm.%2$tY %2$tH:%2$tM:%2$tS)", //$NON-NLS-1$
      this.getId(), this.getDate());
  }

  /**
   * Returns whether the represented test session container is in sync with its file in the workspace.
   * 
   * @return <code>true</code> if the represented test session container is synchronized with its
   *         <code>IFile</code>, <code>false</code> if there are unsaved changes
   */
  @Override
public boolean isSynchronized() {
    return this.sync;
  }

  /**
   * Returns whether this <code>TSContainerInfo</code> equals the given object.
   * <p>
   * This <code>TSContainerInfo</code> is equal to the given object if the given object is a
   * <code>TSContainerHandle</code> and both have the same path ({@link #getPath()}). See
   * {@link IPath#equals(Object)} for equality of paths.
   * 
   * @param o the object to compare to
   * @return the comparison result
   */
  @Override
  public boolean equals(Object o) {
    if (o != null && o instanceof TSContainerHandle
      && ((TSContainerHandle) o).getPath().equals(this.getPath())) {
      return true;
    } else {
      return false;
    }
  }

  @Override
  public int hashCode() {
    return this.getPath().toString().hashCode();
  }

  /**
   * Compares this object with the given represented test session container by the name of the project they
   * belong to, by their dates and by their names.
   */
  @Override
public int compareTo(TSContainerHandle tscHandle) {
    int cProjectName, cDate;
    if ((cProjectName = this.getProject().getName().compareTo(tscHandle.getProject().getName())) != 0) {
      return cProjectName;
    } else if ((cDate = this.getDate().compareTo(tscHandle.getDate())) != 0) {
      return cDate;
    } else {
      return this.getName().compareTo(tscHandle.getName());
    }
  }
}