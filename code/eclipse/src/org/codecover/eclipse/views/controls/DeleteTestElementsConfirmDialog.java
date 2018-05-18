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

import org.codecover.eclipse.Messages;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * Prompts the user to confirm a deletion of test elements.
 * 
 * @see DeleteTestElementsSelectDialog
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: DeleteTestElementsConfirmDialog.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DeleteTestElementsConfirmDialog extends MessageDialog {
    
    private static final String TITLE = Messages.getString("TestSessionsView.ELEMENTS_DELETE_QUESTION_DIALOG_TITLE"); //$NON-NLS-1$
    private static final String MSG = Messages.getString("TestSessionsView.ELEMENTS_DELETE_QUESTION_DIALOG_MSG"); //$NON-NLS-1$
    
    /**
     * The index of the &quot;Yes&quot;-button. See the following example if you
     * want to check whether the user pressed this button:
     * <pre>
     * DeleteTestElementsConfirmDialog confirmDialog
     *         = new DeleteTestElementsConfirmDialog(shell, elementsToDelete);
     * if(confirmDialog.open()
     *         == DeleteTestElementsConfirmDialog.YES_BUTTON_INDEX) {
     *     // User pressed the "Yes"-button, i.e. confirmed the deletion
     * } else {
     *     // User didn't press the "Yes"-button,
     *     // i.e. he doesn't want to perform the deletion
     * }
     * </pre>
     */
    // adapt call to super constructor if you change this constant
    public static final int YES_BUTTON_INDEX = 0;
    
    private Object[] selection;
    
    /**
     * Constructs a dialog which asks the user to confirm a deletion of test
     * elements.
     * 
     * @param shell     the parent shell
     * @param selection the test elements the user selected for deletion
     */
    public DeleteTestElementsConfirmDialog(Shell shell, Object[] selection) {
        super(  shell,
                TITLE,
                null,
                MSG,
                MessageDialog.QUESTION,
                // adapt YES_BUTTON_INDEX if you change the following!
                new String[] { IDialogConstants.YES_LABEL,
                        IDialogConstants.NO_LABEL },
                1);
        this.selection = selection;
        this.setShellStyle(this.getShellStyle() | SWT.RESIZE);
    }

    @Override
    protected Point getInitialSize() {
        return new Point(400,300);
    }
    
    @Override
    protected Control createCustomArea(Composite parent) {
        parent.setLayout(new FillLayout());

        // create tree viewer which lists test sessions and test cases
        TreeViewer viewer = TestSessionsViewerFactory.newTreeViewer(
                parent,
                true,
                new Runnable() { @Override
				public void run() {} });
        viewer.setInput(this.selection);
        
        return parent;
    }

}
