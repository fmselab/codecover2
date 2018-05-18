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
import java.util.List;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.model.utils.CollectionUtil;

/**
 * Handles the <code>TSContainerInfoListener</code>s of a <code>TSContainerInfo</code> and contains
 * methods to fire events which signalize change of the associated <code>TSContainerInfo</code>.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: TSContainerInfoListenerHandler.java 49 2009-06-01 08:52:19Z ahija $)
 */
class TSContainerInfoListenerHandler {

  private final List<TSContainerInfoListener> listeners;

  private final Object lock;

  TSContainerInfoListenerHandler(TSContainerInfo tscInfo) {
    if (tscInfo == null) {
      throw new NullPointerException("tscInfo mustn't be null"); //$NON-NLS-1$
    }
    this.listeners = new ArrayList<TSContainerInfoListener>();
    this.lock = new Object();
  }

  /**
   * Adds the given listener to receive events from the associated <code>TSContainerInfo</code>.
   * 
   * @param listener the listener
   * @throws NullPointerException if the specified listener is <code>null</code>
   */
  void addListener(TSContainerInfoListener listener) {
    if (listener == null) {
      throw new NullPointerException("listener mustn't be null"); //$NON-NLS-1$
    }
    synchronized (this.lock) {
      this.listeners.add(listener);
    }
  }

  /**
   * Removes the given listener so that it no longer receives events from the associated
   * <code>TSContainerInfo</code>.
   * 
   * @param listener the listener
   */
  void removeListener(TSContainerInfoListener listener) {
    if (listener != null) {
      synchronized (this.lock) {
        this.listeners.remove(listener);
      }
    }
  }

  void fireSynchronizedStateChanged(boolean isSynchronized) {
    List<TSContainerInfoListener> listenersCopy;
    synchronized (this.lock) {
      listenersCopy = CollectionUtil.copy(this.listeners);
    }
    for (TSContainerInfoListener listener : listenersCopy) {
      try {
        listener.synchronizedStateChanged(isSynchronized);
      } catch (Throwable t) {
        CodeCoverPlugin.getDefault().getLogger().error("Error while notifying" + //$NON-NLS-1$
          " TSContainerInfoListener", //$NON-NLS-1$
          new InvocationTargetException(t));
      }
    }
  }

}
