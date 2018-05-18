package org.codecover.eclipse.tscmanager;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.codecover.model.TestCase;

/**
 * Represents a <code>TestCase</code> of a <code>TestSession</code> of a <code>TestSessionContainer</code>
 * that is not currently loaded (else we wouldn't need a <code>TestCaseInfo</code> representation because we
 * could use the <code>TestCase</code> reference directly). The parent <code>TSContainerInfo</code>-representation
 * of the represented test case is not saved in an object of this class because it is not always available,
 * e.g. if the project of the test session container is currently closed.
 * <p>
 * In the documentation of this class the term <em>represented test
 * case</em> is used for a
 * <code>TestCase</code> which is represented by an object of this class.
 */
public class TestCaseInfo {

  /**
   * The name of the parent <code>TestSession</code>.
   */
  private String testSessionName;

  /**
   * The name of the represented test case.
   */
  private String name;

  /**
   * Constructs a representation of a <code>TestCase</code> by it's name and the name of its parent
   * <code>TestSession</code>.
   * 
   * @param name the name of the <code>TestCase</code> to create a representation for
   * @param testSessionName the name of the parent <code>TestSession</code> of the <code>TestCase</code>
   *        to create a representation for
   */
  public TestCaseInfo(String name, String testSessionName) {
    if (name == null) {
      throw new NullPointerException("name of test case mustn't be null"); //$NON-NLS-1$
    }
    if (testSessionName == null) {
      throw new NullPointerException("name of test session mustn't be null"); //$NON-NLS-1$
    }
    this.name = new String(name);
    this.testSessionName = new String(testSessionName);
  }

  /**
   * Generates <code>TestCaseInfo</code>-representations of the given <code>TestCase</code>s.
   * 
   * @param testCases the <code>TestCase</code>s the <code>TestCaseInfo</code>-representations will be
   *        generated for
   * @return <code>TestCaseInfo</code>-representations of the given <code>TestCase</code>s.
   */
  public static List<TestCaseInfo> generateTestCaseInfos(Set<TestCase> testCases) {
    List<TestCaseInfo> testCaseInfos = new LinkedList<TestCaseInfo>();
    for (TestCase testCase : testCases) {
      testCaseInfos.add(new TestCaseInfo(testCase.getName(), testCase.getTestSession().getName()));
    }
    return testCaseInfos;
  }

  /**
   * Returns the name of the parent <code>TestSession</code> of the represented test case.
   * 
   * @return the name of the parent <code>TestSession</code> of the represented test case.
   */
  public String getTestSessionName() {
    return this.testSessionName;
  }

  /**
   * Returns the name of the represented test case.
   * 
   * @return the name of the represented test case.
   */
  public String getName() {
    return this.name;
  }

  /**
   * Returns whether this <code>TestCaseInfo</code> equals the given object.
   * <p>
   * This <code>TestCaseInfo</code> is equal to the given object if the given object is a
   * <code>TestCaseInfo</code> and both have the same name and name of the test session. The parent
   * <code>TSContainerInfo</code>s are <em>not</em> compared (because this information is not available).
   */
  @Override
  public boolean equals(Object o) {
    if (o != null && o instanceof TestCaseInfo) {
      TestCaseInfo tcInfo = (TestCaseInfo) o;
      if (tcInfo.getName().equals(this.getName())
        && tcInfo.getTestSessionName().equals(this.getTestSessionName())) {
        return true;
      }
    }
    return false;
  }

  @Override
  public int hashCode() {
    return this.getTestSessionName().hashCode() ^ this.getName().hashCode();
  }

}