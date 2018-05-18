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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.tscmanager.exceptions.TSContainerManagerModifyException;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.utils.ChangeListener;
import org.codecover.model.utils.ChangeType;
import org.codecover.model.utils.ListenerHandle;
import org.codecover.model.utils.Logger;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

/**
 * Handles the <code>TSContainerManagerListener</code>s and contains methods to fire events which signalize
 * change of the <code>TSContainerManager</code> or of the active test session container. Moreover if a
 * change of the active test session container is detected the {@link TSContainerInfo#isSynchronized()}-flag
 * of its <code>TSContainerInfo</code> is set to <code>false</code>. If a test session was added, the
 * Test Sessions view is created and made visible. If it would hide the currently active view, it is not made
 * visible (but still created if necessary).
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: TSContainerManagerListenerHandler.java 49 2009-06-01 08:52:19Z ahija $)
 */
class TSContainerManagerListenerHandler {

  final TSContainerManager tscManager;

  private final List<TSContainerManagerListener> listeners;

  private ActiveTSCInfoListener activeTSCInfoListener;

  private ListenerHandle testSessionContainerListenerHandle;

  private ListenerHandle testSessionListenerHandle;

  private ListenerHandle testCaseListenerHandle;

  final Logger logger;

  TSContainerManagerListenerHandler(TSContainerManager tscManager) {
    this.tscManager = tscManager;
    this.listeners = new ArrayList<TSContainerManagerListener>();
    this.logger = tscManager.getLogger();
    this.activeTSCInfoListener = null;
    this.testSessionContainerListenerHandle = null;
    this.testSessionListenerHandle = null;
    this.testCaseListenerHandle = null;
  }

  /**
   * Adds the given listener to receive events from the <code>TSContainerManager</code>.
   * 
   * @param listener the listener
   * @throws NullPointerException if the specified listener is <code>null</code>
   */
  void addListener(TSContainerManagerListener listener) {
    if (listener == null) {
      throw new NullPointerException("listener mustn't be null"); //$NON-NLS-1$
    }
    synchronized (this.tscManager.getWriteLock()) {
      this.listeners.add(listener);
    }
  }

  /**
   * Removes the given listener so that it no longer receives events from the <code>TSContainerManager</code>.
   * 
   * @param listener the listener
   */
  void removeListener(TSContainerManagerListener listener) {
    if (listener != null) {
      synchronized (this.tscManager.getWriteLock()) {
        this.listeners.remove(listener);
      }
    }
  }

  /**
   * Registers listeners on the given test session container which mainly just redirect the change events of
   * the given test session containers to the <code>TSContainerManagerListener</code>s.
   * 
   * @param tscInfo the test session container to register the redirection listeners on
   */
  private void registerTSCListeners(final ActiveTSContainerInfo tscInfo) {
    final TestSessionContainer tsc = tscInfo.getTestSessionContainer();

    /*
     * dispose existing internal listeners
     */
    this.unregisterTSCListeners();

    this.activeTSCInfoListener = new ActiveTSCInfoListener(tscInfo.getTSContainerInfo());

    /*
     * register internal listeners which dispatch the events to the TSContainerManagerListeners
     */
    this.testSessionContainerListenerHandle =
      tsc.addTestSessionContainerListener(new ChangeListener<TestSessionContainer>() {

        @Override
		public void changed(ChangeType ct, TestSessionContainer ctsc) {
          ActiveTSContainerInfo activeTSCInfo =
            TSContainerManagerListenerHandler.this.tscManager.getActiveTSContainer();
          if (activeTSCInfo == null || ctsc != activeTSCInfo.getTestSessionContainer()) {
            logger.warning("Irrelevant TSC change event" + //$NON-NLS-1$
              " received (and ignored)"); //$NON-NLS-1$
            return;
          }
          activeTSCInfo.getTSContainerInfo().setSynchronized(false);

          TSContainerManagerListenerHandler.this.fireTestSessionContainerChanged(ct, activeTSCInfo);
        }
      });
    this.testSessionListenerHandle = tsc.addTestSessionListener(new ChangeListener<TestSession>() {

      @Override
	public void changed(ChangeType ct, TestSession cts) {
        ActiveTSContainerInfo activeTSCInfo;
        synchronized (TSContainerManagerListenerHandler.this.tscManager.getWriteLock()) {
          if (TSContainerManagerListenerHandler.this.tscManager.isLocked()) {
            throw new TSContainerManagerModifyException("TSContainerManager is locked" + //$NON-NLS-1$
              " for modifications during" + //$NON-NLS-1$
              " change event"); //$NON-NLS-1$
          }
          activeTSCInfo = TSContainerManagerListenerHandler.this.tscManager.getActiveTSContainer();
          if (activeTSCInfo == null
            || cts.getTestSessionContainer() != activeTSCInfo.getTestSessionContainer()) {
            logger.warning("Irrelevant TSC change event" + //$NON-NLS-1$
              " received (and ignored)"); //$NON-NLS-1$
            return;
          }
          activeTSCInfo.getTSContainerInfo().setSynchronized(false);
          /*
           * If a test session was deleted which contains test cases which are currently active, they are
           * removed from the active test cases. This removal fires a change event of the active test cases
           * before the change event of the deleted test session is propagated.
           */

          if (ct == ChangeType.REMOVE) {
            boolean removed = false;
            boolean curRemoved;
            Set<TestCase> newActiveTestCases = new HashSet<TestCase>(activeTSCInfo.getActiveTestCases());
            for (TestCase tc : cts.getTestCases()) {
              curRemoved = newActiveTestCases.remove(tc);
              removed = removed || curRemoved;
            }
            if (removed) {
              TSContainerManagerListenerHandler.this.tscManager.setActiveTestCases(newActiveTestCases);
            }
          }
        }

        TSContainerManagerListenerHandler.this.fireTestSessionChanged(activeTSCInfo, ct, cts);

        /*
         * If a test session was added, the Test Sessions view is created and made visible. If it would hide
         * the currently active view, it is not made visible (but still created if necessary). This is done
         * after all events were fired to not bother the view with events shortly after creation.
         */
        if (ct == ChangeType.ADD) {
          TSContainerManagerListenerHandler.showTestSessionsView();
        }
      }
    });
    this.testCaseListenerHandle = tsc.addTestCaseListener(new ChangeListener<TestCase>() {

      @Override
	public void changed(ChangeType ct, TestCase ctc) {
        ActiveTSContainerInfo activeTSCInfo;
        synchronized (TSContainerManagerListenerHandler.this.tscManager.getWriteLock()) {
          activeTSCInfo = TSContainerManagerListenerHandler.this.tscManager.getActiveTSContainer();
          if (TSContainerManagerListenerHandler.this.tscManager.isLocked()) {
            throw new TSContainerManagerModifyException("TSContainerManager is locked" + //$NON-NLS-1$
              " for modifications during" + //$NON-NLS-1$
              " change event"); //$NON-NLS-1$
          }

          if (activeTSCInfo == null
            || ctc.getTestSession().getTestSessionContainer() != activeTSCInfo.getTestSessionContainer()) {
            logger.warning("Irrelevant TSC change event" + //$NON-NLS-1$
              " received (and ignored)"); //$NON-NLS-1$
            return;
          }
          activeTSCInfo.getTSContainerInfo().setSynchronized(false);

          /*
           * If a test case was deleted which is currently active, it is removed from the active test cases.
           * This removal fires a change event of the active test cases before the change event of the deleted
           * test case is propagated.
           */
          if (ct == ChangeType.REMOVE && activeTSCInfo.getActiveTestCases().contains(ctc)) {
            Set<TestCase> newActiveTestCases = new HashSet<TestCase>(activeTSCInfo.getActiveTestCases());
            newActiveTestCases.remove(ctc);
            TSContainerManagerListenerHandler.this.tscManager.setActiveTestCases(newActiveTestCases);
          }
        }

        TSContainerManagerListenerHandler.this.fireTestCaseChanged(activeTSCInfo, ct, ctc);
      }
    });
  }

  private void unregisterTSCListeners() {
    if (this.activeTSCInfoListener != null) {
      this.activeTSCInfoListener.dispose();
      this.activeTSCInfoListener = null;
    }
    if (this.testSessionContainerListenerHandle != null) {
      this.testSessionContainerListenerHandle.dispose();
      this.testSessionContainerListenerHandle = null;
    }
    if (this.testSessionListenerHandle != null) {
      this.testSessionListenerHandle.dispose();
      this.testSessionListenerHandle = null;
    }
    if (this.testCaseListenerHandle != null) {
      this.testCaseListenerHandle.dispose();
      this.testCaseListenerHandle = null;
    }
  }

  /**
   * Reports that a new (known) test session container was added.
   * 
   * @param tscInfo the <code>TSContainerInfo</code>-representation of the added test session container
   * @param index the position of the added test session container in the list of all known test session
   *        containers
   */
  void fireTestSessionContainerAdded(TSContainerInfo tscInfo, int index) {
    HashSet<TSContainerManagerListener> listeners = new HashSet<TSContainerManagerListener>(this.listeners);
    synchronized (this.tscManager.getWriteLock()) {
      try {
        this.tscManager.setLocked(true);
        for (TSContainerManagerListener listener : listeners) {
          try {
            listener.testSessionContainerAdded(tscInfo, index);
          } catch (Throwable t) {
            logger.error("Error while notifying" + //$NON-NLS-1$
              " TSContainerManagerListener",//$NON-NLS-1$
              new InvocationTargetException(t));
          }
        }
      } finally {
        this.tscManager.setLocked(false);
      }
    }
  }

  /**
   * Reports that a (known) test session container was removed.
   * 
   * @param tscInfo the <code>TSContainerInfo</code>-representation of the removed test session container
   */
  void fireTestSessionContainerRemoved(TSContainerInfo tscInfo) {
    HashSet<TSContainerManagerListener> listeners = new HashSet<TSContainerManagerListener>(this.listeners);
    synchronized (this.tscManager.getWriteLock()) {
      try {
        this.tscManager.setLocked(true);
        for (TSContainerManagerListener listener : listeners) {
          try {
            listener.testSessionContainerRemoved(tscInfo);
          } catch (Throwable t) {
            logger.error("Error while notifying" + //$NON-NLS-1$
              " TSContainerManagerListener",//$NON-NLS-1$
              new InvocationTargetException(t));
          }
        }
      } finally {
        this.tscManager.setLocked(false);
      }
    }
  }

  /**
   * Reports that a (other) test session container was activated and registers change listeners on the newly
   * activated test session container.
   * 
   * @param tscInfo the <code>ActiveTSContainerInfo</code>-representation of the activated test session
   *        container or <code>null</code> if none is active
   */
  void fireTestSessionContainerActivated(ActiveTSContainerInfo tscInfo) {
    HashSet<TSContainerManagerListener> listeners = new HashSet<TSContainerManagerListener>(this.listeners);
    synchronized (this.tscManager.getWriteLock()) {
      try {
        this.tscManager.setLocked(true);
        for (TSContainerManagerListener listener : listeners) {
          try {
            listener.testSessionContainerActivated(tscInfo);
          } catch (Throwable t) {
            logger.error("Error while notifying" + //$NON-NLS-1$
              " TSContainerManagerListener",//$NON-NLS-1$
              new InvocationTargetException(t));
          }
        }
      } finally {
        this.tscManager.setLocked(false);
      }
      /*
       * register the test session container listeners after the activated event was fired to prevent
       * populating change events before the listening components have noticed that an other test session
       * container has been activated
       */
      if (tscInfo != null) {
        this.registerTSCListeners(tscInfo);
      } else {
        this.unregisterTSCListeners();
      }
    }
  }

  /**
   * Reports a change in the list of active test cases.
   * <p>
   * Be sure to pass an <em>immutable copy</em> of the original set of active test cases!
   * <p>
   * Do <em>not</em> call this method if another test session container was activated (which changes the
   * list of active test cases, too)! Use {@link #fireTestSessionContainerActivated(ActiveTSContainerInfo)}
   * for this.
   * 
   * @param tscInfo the <code>ActiveTSContainerInfo</code>-representation of the active test session
   *        container the activated test cases belong to
   */
  void fireTestCasesActivated(ActiveTSContainerInfo tscInfo) {
    HashSet<TSContainerManagerListener> listeners = new HashSet<TSContainerManagerListener>(this.listeners);
    synchronized (this.tscManager.getWriteLock()) {
      try {
        this.tscManager.setLocked(true);
        logger.debug("Firing event: test cases activated");//$NON-NLS-1$
        for (TSContainerManagerListener listener : listeners) {
          try {
            listener.testCasesActivated(tscInfo);
          } catch (Throwable t) {
            logger.error("Error while notifying" + //$NON-NLS-1$
              " TSContainerManagerListener",//$NON-NLS-1$
              new InvocationTargetException(t));
          }
        }
      } finally {
        this.tscManager.setLocked(false);
      }
    }
  }

  private void fireTestSessionContainerChanged(ChangeType ct, ActiveTSContainerInfo tscInfo) {
    HashSet<TSContainerManagerListener> listeners = new HashSet<TSContainerManagerListener>(this.listeners);
    synchronized (this.tscManager.getWriteLock()) {
      try {
        this.tscManager.setLocked(true);
        for (TSContainerManagerListener listener : listeners) {
          try {
            listener.testSessionContainerChanged(ct, tscInfo);
          } catch (Throwable t) {
            logger.error("Error while notifying" + //$NON-NLS-1$
              " TSContainerManagerListener",//$NON-NLS-1$
              new InvocationTargetException(t));
          }
        }
      } finally {
        this.tscManager.setLocked(false);
      }
    }
  }

  private void fireTestSessionChanged(ActiveTSContainerInfo tscInfo, ChangeType ct, TestSession cts) {
    HashSet<TSContainerManagerListener> listeners =
      new HashSet<TSContainerManagerListener>(TSContainerManagerListenerHandler.this.listeners);
    synchronized (this.tscManager.getWriteLock()) {
      try {
        this.tscManager.setLocked(true);
        for (TSContainerManagerListener listener : listeners) {
          try {
            listener.testSessionChanged(tscInfo, ct, cts);
          } catch (Throwable t) {
            logger.error("Error while notifying" + //$NON-NLS-1$
              " TSContainerManagerListener",//$NON-NLS-1$
              new InvocationTargetException(t));
          }
        }
      } finally {
        this.tscManager.setLocked(false);
      }
    }
  }

  private void fireTestCaseChanged(ActiveTSContainerInfo tscInfo, ChangeType ct, TestCase ctc) {
    HashSet<TSContainerManagerListener> listeners = new HashSet<TSContainerManagerListener>(this.listeners);
    synchronized (this.tscManager.getWriteLock()) {
      try {
        this.tscManager.setLocked(true);
        for (TSContainerManagerListener listener : listeners) {
          try {
            listener.testCaseChanged(tscInfo, ct, ctc);
          } catch (Throwable t) {
            logger.error("Error while notifying" + //$NON-NLS-1$
              " TSContainerManagerListener",//$NON-NLS-1$
              new InvocationTargetException(t));
          }
        }
      } finally {
        this.tscManager.setLocked(false);
      }
    }
  }

  private void fireSynchronizedStateChanged(TSContainerInfo tscInfo, boolean isSynchronized) {
    HashSet<TSContainerManagerListener> listeners = new HashSet<TSContainerManagerListener>(this.listeners);
    synchronized (this.tscManager.getWriteLock()) {
      try {
        this.tscManager.setLocked(true);
        for (TSContainerManagerListener listener : listeners) {
          try {
            listener.synchronizedStateChanged(tscInfo, isSynchronized);
          } catch (Throwable t) {
            logger.error("Error while notifying" + //$NON-NLS-1$
              " TSContainerManagerListener",//$NON-NLS-1$
              new InvocationTargetException(t));
          }
        }
      } finally {
        this.tscManager.setLocked(false);
      }
    }
  }

  private class ActiveTSCInfoListener
    implements TSContainerInfoListener {

    private final TSContainerInfo tscInfo;

    public ActiveTSCInfoListener(TSContainerInfo tscInfo) {
      this.tscInfo = tscInfo;
      this.tscInfo.addListener(this);
    }

    @Override
	public void synchronizedStateChanged(boolean isSynchronized) {
      TSContainerManagerListenerHandler.this.fireSynchronizedStateChanged(this.tscInfo, isSynchronized);
    }

    public void dispose() {
      this.tscInfo.removeListener(this);
    }
  }

  /**
   * Creates the Test Sessions view and makes it visible (only if that wouldn't hide the currently active
   * view).
   */
  private static void showTestSessionsView() {
    Display.getDefault().asyncExec(new Runnable() {

      @Override
	public void run() {
        IWorkbenchWindow activeWindow;
        IWorkbenchPage activePage;
        if ((activeWindow = PlatformUI.getWorkbench().getActiveWorkbenchWindow()) != null
          && (activePage = activeWindow.getActivePage()) != null) {
          try {
            activePage.showView("org.codecover.eclipse.views" + //$NON-NLS-1$
              ".TestSessionsView", //$NON-NLS-1$
              null, IWorkbenchPage.VIEW_VISIBLE);
          } catch (PartInitException e) {
            CodeCoverPlugin.getDefault().getLogger().error("Test Sessions view couldn't" + //$NON-NLS-1$
              " be created.", e); //$NON-NLS-1$
          }
        }
      }
    });
  }

}
