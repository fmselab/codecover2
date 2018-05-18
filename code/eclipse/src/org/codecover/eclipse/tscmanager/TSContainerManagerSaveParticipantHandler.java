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

import java.util.ArrayList;
import java.util.List;

import org.codecover.model.utils.Logger;
import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.runtime.CoreException;

/**
 * This handler can be registered as an Eclipse save participant (see
 * {@link org.eclipse.core.resources.IWorkspace#addSaveParticipant( org.eclipse.core.runtime.Plugin,
 * ISaveParticipant)}), it then redirects the received save requests of Eclipse to the {@link SaveParticipant}s
 * which have been previously registered (via {@link #add(SaveParticipant)}) to itself. This procedure is
 * needed to be able to split up the save participant into several independent classes. Eclipse doesn't
 * support this natively since it only allows to register one save participant. The save participants
 * registered to this handler are executed in the order they were registered.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: TSContainerManagerSaveParticipantHandler.java 49 2009-06-01 08:52:19Z ahija $)
 */
public class TSContainerManagerSaveParticipantHandler
  implements ISaveParticipant {

  /**
   * The <code>SaveParticipant</code>s in the order they are executed.
   */
  private final List<SaveParticipant> saveParticipants;

  private final Logger logger;

  /**
   * Constructor.
   * 
   * @param logger the {@link Logger} to be used.
   */
  public TSContainerManagerSaveParticipantHandler(Logger logger) {
    this.saveParticipants = new ArrayList<SaveParticipant>();
    this.logger = logger;
  }

  /**
   * Adds the given {@link SaveParticipant}.
   * 
   * @param saveParticipant the {@link SaveParticipant} to be added.
   */
  public void add(SaveParticipant saveParticipant) {
    if (!this.saveParticipants.contains(saveParticipant)) {
      this.saveParticipants.add(saveParticipant);
    }
  }

  @Override
public void prepareToSave(ISaveContext context) throws CoreException {
    for (SaveParticipant saveParticipant : this.saveParticipants) {
      saveParticipant.prepareToSave(context);
    }
  }

  @Override
public void saving(ISaveContext context) throws CoreException {
    boolean performedSave = false;
    boolean currentSave;
    // output some debug messages
    switch (context.getKind()) {
      case ISaveContext.FULL_SAVE:
        logger.debug("Full save requested" + //$NON-NLS-1$
          " (by Eclipse)"); //$NON-NLS-1$
        break;
      case ISaveContext.PROJECT_SAVE:
        logger.debug("Project (" //$NON-NLS-1$
          + context.getProject().getName() + ") save requested (by Eclipse)"); //$NON-NLS-1$
        break;
      /*
       * case ISaveContext.SNAPSHOT: logger.debug("Snapshot save requested" + //$NON-NLS-1$ " (by Eclipse)");
       * //$NON-NLS-1$ break;
       */
    }
    // perform the saves
    for (SaveParticipant saveParticipant : this.saveParticipants) {
      currentSave = saveParticipant.saving(context);
      performedSave = performedSave || currentSave;
    }
    // tell that we actively participated in this save
    if (performedSave) {
      context.needSaveNumber();
    }
  }

  @Override
public void doneSaving(ISaveContext context) {
    for (SaveParticipant saveParticipant : this.saveParticipants) {
      saveParticipant.doneSaving(context);
    }
  }

  @Override
public void rollback(ISaveContext context) {
    /*
     * This method is only called if method this.saving completed without exceptions and the save operation
     * failed because of an _other_ save participant.
     */
    for (SaveParticipant saveParticipant : this.saveParticipants) {
      saveParticipant.rollback(context);
    }
  }

}
