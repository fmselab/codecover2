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

import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.runtime.CoreException;

/**
 * A participant in the saving of the workspace.
 * 
 * @see TSContainerManagerSaveParticipantHandler
 * @author Robert Hanussek
 * @version 1.0 ($Id: SaveParticipant.java 49 2009-06-01 08:52:19Z ahija $)
 */
public interface SaveParticipant {

  /**
   * @param context the save context object
   * @see org.eclipse.core.resources.ISaveParticipant#doneSaving(ISaveContext)
   */
  public void doneSaving(ISaveContext context);

  /**
   * @param context the save context object
   * @exception CoreException if this method fails to snapshot the state of this workspace.
   * @see org.eclipse.core.resources.ISaveParticipant#prepareToSave(ISaveContext)
   */
  public void prepareToSave(ISaveContext context) throws CoreException;

  /**
   * This method is only called if the method
   * {@link org.eclipse.core.resources.ISaveParticipant#saving(ISaveContext)} (of the
   * {@link org.eclipse.core.resources.ISaveParticipant} which uses this <code>SaveParticpant</code>)
   * completed without exceptions and the save operation failed because of an <em>other</em>
   * <code>ISaveParticipant</code>.
   * 
   * @param context the save context object
   * @see org.eclipse.core.resources.ISaveParticipant#rollback(ISaveContext)
   */
  public void rollback(ISaveContext context);

  /**
   * See {@link org.eclipse.core.resources.ISaveParticipant#saving(ISaveContext)}, but do <em>not</em> call
   * {@link ISaveContext#needSaveNumber()} and <em>do</em> return the appropriate return value if saving was
   * successful or didn't take place. If saving wasn't successful throw a <code>CoreException</code>.
   * 
   * @param context the save context object
   * @exception CoreException if this method fails to snapshot the state of this workspace.
   * @return <code>true</code> if something was saved by this <code>SaveParticipant</code>,
   *         <code>false</code> otherwise
   */
  public boolean saving(ISaveContext context) throws CoreException;
}
