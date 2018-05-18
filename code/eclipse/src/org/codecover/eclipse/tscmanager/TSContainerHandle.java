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

import java.util.Date;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

/**
 * Represents a known <code>TestSessionContainer</code>.
 * <p>
 * This interface is not public because this would allow using <code>TSContainerInfo</code> and
 * <code>ActiveTSContainerInfo</code> interchangeably which isn't intended, for reasons see
 * {@link ActiveTSContainerInfo}.
 * <p>
 * In the documentation of this interface the term <em>represented test
 * session container</em> is used for a
 * <code>TestSessionContainer</code> which is represented by an object of a class which implements this
 * interface.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: TSContainerHandle.java 49 2009-06-01 08:52:19Z ahija $)
 */
interface TSContainerHandle
  extends Comparable<TSContainerHandle> {

  /**
   * Returns the file of the represented test session container.
   * 
   * @return the file of the represented test session container
   */
  public IFile getFile();

  /**
   * Returns the ID of the represented test session container, do <em>not</em> use this ID as a unique
   * identifier for this representation of a test session container or for the represented test session
   * container itself, because the same test session container can appear twice in the workspace (e.g., if the
   * user imported it two times).
   * 
   * @return the ID of the represented test session container
   */
  public String getId();

  /**
   * Returns the date of the represented test session container.
   * 
   * @return the date of the represented test session container
   */
  public Date getDate();

  /**
   * Returns the path of the file of the represented test session container relative to the containing
   * workspace. This path is used as the unique identifier of this representation of a test session container
   * and the represented test session container itself.
   * 
   * @return the path of the file of the represented test session container.
   */
  public IPath getPath();

  /**
   * Returns the name of the represented test session container. This name doesn't have to be unique (use the
   * path of the file (<code>getPath()</code>), if you need a unique identifier for the represented test
   * session container or this representation of a test session container).
   * 
   * @return the name of the represented test session container
   */
  public String getName();

  /**
   * Returns the project the represented test session container belongs to.
   * 
   * @return the project the represented test session container belongs to.
   */
  public IProject getProject();

  /**
   * Returns whether the represented test session container is in sync with its file in the workspace.
   * 
   * @return <code>true</code> if the represented test session container is synchronized with its
   *         <code>IFile</code>, <code>false</code> if there are unsaved changes
   */
  public boolean isSynchronized();

  /**
   * Returns whether this <code>TSContainerHandle</code> equals the given object.
   * <p>
   * This <code>TSContainerHandle</code> is equal to the given object if the given object is a
   * <code>TSContainerHandle</code> and has the same path (<code>getPath()</code>). See
   * {@link IPath#equals(Object)} for equality of paths.
   * <p>
   * The following conditions are (and must be) always <code>true</code>:
   * <code>Object x; ActiveTSContainerInfo activeTSCInfo;
   * TSContainerInfo tscInfo;</code><br>
   * <code>activeTSCInfo.equals(x)
   * == activeTSCInfo.getTSContainerInfo().equals(x)</code>
   * <code>tscInfo.equals(activeTSCInfo)
   * == tscInfo.equals(activeTSCInfo.getTSContainerInfo())</code>
   * 
   * @param o the object to compare to
   * @return the comparison result
   */
  @Override
public boolean equals(Object o);

}