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

package org.codecover.eclipse.views.controls;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.exceptions.TSCFileCreateException;
import org.codecover.model.exceptions.FileSaveException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;

/**
 * An action which saves the active test session container (to stable storage),
 * if there are changes since the last save operation.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: SaveActiveTSContainerAction.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class SaveActiveTSContainerAction extends Action {
    
    private static final String TEXT = Messages.getString("SaveActiveTSContainerAction.TEXT"); //$NON-NLS-1$
    private static final String TOOLTIP = Messages.getString("SaveActiveTSContainerAction.TOOLTIP"); //$NON-NLS-1$
    private static final String ERROR_CREATING_FILE = Messages.getString("SaveActiveTSContainerAction.ERROR_CREATING_FILE"); //$NON-NLS-1$
    private static final String ERROR_SAVING_TSC = Messages.getString("SaveActiveTSContainerAction.ERROR_SAVING_TSC"); //$NON-NLS-1$
    
    /**
     * Creates an action which saves the active test session container. The user
     * may send the save operation to background.
     */
    public SaveActiveTSContainerAction() {
        this.setText(TEXT);
        this.setToolTipText(TOOLTIP);
        this.setImageDescriptor(CodeCoverPlugin.getDefault()
                .getImageRegistry().getDescriptor(CodeCoverPlugin.Image
                        .SESSION_CONTAINER_SAVE.getPath()));
    }
    
    /**
     * Creates a job which saves the active test session container. The user may
     * send this job to background.
     */
    @Override
    public void run() {
        (new SaveActiveTSCJob()).schedule();
    }
    
    private class SaveActiveTSCJob extends Job {
        
        /**
         * Constructs a (user) job which saves the active test session
         * container.
         */
        public SaveActiveTSCJob() {
            super(TEXT);
            this.setUser(true);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            CodeCoverPlugin plugin = CodeCoverPlugin.getDefault();
            try {
                plugin.getTSContainerManager().saveActiveTSContainer(monitor);
            } catch(TSCFileCreateException e) {
                plugin.getLogger().error(
                        "Couldn't creating the file to write" +    //$NON-NLS-1$
                        " the active test session container to.",  //$NON-NLS-1$
                        e);
                Display.getDefault().asyncExec(new Runnable() {
                    @Override
					public void run() {
                        MessageDialog.openError(null, TEXT,ERROR_CREATING_FILE);
                    }
                });
                return Status.CANCEL_STATUS;
            } catch(FileSaveException e) {
                plugin.getLogger().error(
                        "Error while saving active test" +         //$NON-NLS-1$
                        " session container.",                     //$NON-NLS-1$
                        e);
                Display.getDefault().asyncExec(new Runnable() {
                    @Override
					public void run() {
                        MessageDialog.openError(null, TEXT,ERROR_SAVING_TSC);
                    }
                });
                return Status.CANCEL_STATUS;
            }
            return Status.OK_STATUS;
        }

    }
    
}
