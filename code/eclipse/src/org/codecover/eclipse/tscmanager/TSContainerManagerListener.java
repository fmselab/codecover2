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

import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.utils.ChangeType;

/**
 * The listener interface for receiving events on changes in <code>TSContainerManager</code>. The class
 * that is interested in processing such events implements this interface. The listener object created from
 * that class is then registered with the <code>TSContainerManager</code> using the <code>addListener</code>
 * method of <code>TSContainerManager</code>. When the content of the <code>TSContainerManager</code>
 * changes, the relevant method in the listener object is invoked.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: TSContainerManagerListener.java 49 2009-06-01 08:52:19Z ahija $)
 */
public interface TSContainerManagerListener {

  /**
   * Invoked when a test session container has been added to the list of known test session containers.
   * 
   * @param tscInfo the <code>TSContainerInfo</code>-representation of the added test session container
   * @param index the position of the added test session container in the list of all known test session
   *        containers
   */
  public void testSessionContainerAdded(TSContainerInfo tscInfo, int index);

  /**
   * Invoked when a test session container has been removed from the list of known test session containers.
   * 
   * @param tscInfo the <code>TSContainerInfo</code>-representation of the removed test session container
   */
  public void testSessionContainerRemoved(TSContainerInfo tscInfo);

  /**
   * Invoked when the active test session container was changed internally, that is if a test session was
   * added or deleted.
   * 
   * @param changeType the type of the change event
   * @param tscInfo the <code>ActiveTSContainerInfo</code>-representation of the changed (active) test
   *        session container
   */
  public void testSessionContainerChanged(ChangeType changeType, ActiveTSContainerInfo tscInfo);

  /**
   * Invoked when a (other) test session container has been activated.
   * 
   * @param tscInfo the <code>ActiveTSContainerInfo</code>-representation of the active test session
   *        container or <code>null</code> if none is active
   */
  public void testSessionContainerActivated(ActiveTSContainerInfo tscInfo);

  /**
   * Invoked when the list of active test cases (of the active test session container) has been changed. This
   * method is not only invoked if the active test cases have been set explicitly but also when the list of
   * test cases changed implicitly due to deletions of active test cases. This method is <em>not</em>
   * invoked when an other test session container has been activated and thus the list of active test cases
   * has been changed, use {@link #testSessionContainerActivated(ActiveTSContainerInfo)} for this.
   * 
   * @param tscInfo the <code>ActiveTSContainerInfo</code>-representation of the active test session
   *        container the activated test cases belong to; First check if your view/component visualizes the
   *        test session container the activated test cases belong to. If it does, use
   *        {@link ActiveTSContainerInfo#getActiveTestCases()} to retrieve the activated test cases. Else you
   *        can ignore this event.
   */
  public void testCasesActivated(ActiveTSContainerInfo tscInfo);

  /**
   * Invoked when a test session of the active test session container was added, deleted or changed.
   * 
   * @param tscInfo the <code>ActiveTSContainerInfo</code>-representation of the (active) test session
   *        container the changed test session belongs to; Use this parameter to check if your view/component
   *        visualizes the test session container the changed test session belongs to. If it doesn't, you can
   *        ignore this event.
   * @param changeType the type of the change event
   * @param testSession the added, deleted or changed test session
   */
  public void testSessionChanged(ActiveTSContainerInfo tscInfo, ChangeType changeType, TestSession testSession);

  /**
   * Invoked when a test case of the active test session container was added, deleted or changed.
   * 
   * @param tscInfo the <code>ActiveTSContainerInfo</code>-representation of the (active) test session
   *        container the changed test session belongs to; Use this parameter to check if your view/component
   *        visualizes the test session container the changed test case belongs to. If it doesn't, you can
   *        ignore this event.
   * @param changeType the type of the change event
   * @param testCase the added, deleted or changed test case
   */
  public void testCaseChanged(ActiveTSContainerInfo tscInfo, ChangeType changeType, TestCase testCase);

  /**
   * Invoked when the sync-flag (see {@link TSContainerInfo#isSynchronized()}) of the active test session
   * container changed. This method is <em>not</em> invoked when an other test session container has been
   * activated and thus the sync-flag has been changed use
   * {@link #testSessionContainerActivated(ActiveTSContainerInfo)} for this.
   * 
   * @param tscInfo the <code>TSContainerInfo</code>-representation of the (active) test session container
   *        this event is associated with
   * @param isSynchronized <code>true</code> if the active test session container is in sync with its file
   *        in the workspace, <code>false</code> otherwise
   */
  public void synchronizedStateChanged(TSContainerInfo tscInfo, boolean isSynchronized);

}
