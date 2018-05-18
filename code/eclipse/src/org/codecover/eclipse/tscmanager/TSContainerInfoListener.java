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

/**
 * The listener interface for receiving events on changes in a <code>TSContainerInfo</code>. The class that
 * is interested in processing such events implements this interface. The listener object created from that
 * class is then registered with a <code>TSContainerInfo</code> using the <code>addListener</code> method
 * of the <code>TSContainerInfo</code>. When the <code>TSContainerInfo</code> changes, the relevant
 * method in the listener object is invoked.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: TSContainerInfoListener.java 49 2009-06-01 08:52:19Z ahija $)
 */
public interface TSContainerInfoListener {

  /**
   * Invoked when the sync-flag (see {@link TSContainerInfo#isSynchronized()}) of the associated
   * <code>TSContainerInfo</code> changed.
   * 
   * @param isSynchronized <code>true</code> if the test session container, represented by the
   *        <code>TSContainerInfo</code> this listener is registered to, is in sync with its file in the
   *        workspace, <code>false</code> otherwise
   */
  public void synchronizedStateChanged(boolean isSynchronized);

}
