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

import java.util.ArrayList;
import java.util.List;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.TSContainerInfo;
import org.codecover.eclipse.tscmanager.exceptions.CancelException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;

/**
 * A dialog which allows the user to select multiple test session containers for
 * deletion. The user must confirm the selection before the deletion is
 * performed.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: DeleteTSCsSelectDialog.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DeleteTSCsSelectDialog extends Dialog {

    private static final String TITLE = Messages
            .getString("DeleteTSCsSelectDialog.TITLE"); //$NON-NLS-1$

    private static final String MESSAGE = Messages
            .getString("DeleteTSCsSelectDialog.MESSAGE"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_TITLE = Messages
            .getString("DeleteTSCsSelectDialog.DIALOG_ERROR_TITLE"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_MSG = Messages
            .getString("DeleteTSCsSelectDialog.DIALOG_ERROR_MSG"); //$NON-NLS-1$

    private static final String ACTION_SELECT_ALL_TEXT = Messages
            .getString("DeleteTSCsSelectDialog.ACTION_SELECT_ALL_TEXT"); //$NON-NLS-1$

    private static final String ACTION_SELECT_ALL_TOOLTIP = Messages
            .getString("DeleteTSCsSelectDialog.ACTION_SELECT_ALL_TOOLTIP"); //$NON-NLS-1$

    private static final String ACTION_DESELECT_ALL_TEXT = Messages
            .getString("DeleteTSCsSelectDialog.ACTION_DESELECT_ALL_TEXT"); //$NON-NLS-1$

    private static final String ACTION_DESELECT_ALL_TOOLTIP = Messages
            .getString("DeleteTSCsSelectDialog.ACTION_DESELECT_ALL_TOOLTIP"); //$NON-NLS-1$

    CheckboxTableViewer viewer;

    /**
     * Constructs a dialog which allows the user to select multiple test session
     * containers for deletion.
     * 
     * @param shell the parent shell of the dialog
     */
    public DeleteTSCsSelectDialog(Shell shell) {
        super(shell);

        this.setShellStyle(this.getShellStyle() | SWT.RESIZE);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText(TITLE);
    }

    @Override
    protected Point getInitialSize() {
        return new Point(400,300);
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite)super.createDialogArea(parent);
        GridData gridData;
        GridLayout layout = new GridLayout();
        layout.numColumns = 1;
        composite.setLayout(layout);

        // create top label
        Label lblViewer = new Label(composite, SWT.NONE);
        lblViewer.setText(MESSAGE);

        // create list viewer which lists the test session containers
        this.viewer = DeleteTSCsSelectDialog.createTSCList(composite);
        this.viewer.setInput(CodeCoverPlugin.getDefault()
                .getTSContainerManager().getTestSessionContainers().toArray());
        this.viewer.setSelection(new StructuredSelection());

        // layout top label
        lblViewer.setLayoutData(new GridData());
        // layout viewer
        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.verticalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        this.viewer.getControl().setLayoutData(gridData);

        return composite;
    }

    private static CheckboxTableViewer createTSCList(Composite parent) {
        /*
         * with SWT.MULTI the selection mark (usually the selected row has a
         * blue background) can be hidden by setting an empty selection
         */
        CheckboxTableViewer viewer = CheckboxTableViewer.newCheckList(parent,
                SWT.BORDER | SWT.MULTI);
        viewer.setContentProvider(new ArrayContentProvider());
        viewer.setLabelProvider(new LabelProvider() {
            @Override
            public Image getImage(Object element) {
                return CodeCoverPlugin.getDefault().getImageRegistry()
                        .get(CodeCoverPlugin.Image.SESSION_CONTAINER.getPath());
            }
            @Override
            public String getText(Object element) {
                return ((TSContainerInfo)element).getName();
            }
        });
        // create context menu
        MenuManager menuMgr = new MenuManager(null);
        menuMgr.add(DeleteTSCsSelectDialog.makeSelectAllAction(viewer));
        menuMgr.add(DeleteTSCsSelectDialog.makeDeselectAllAction(viewer));
        Menu menu = menuMgr.createContextMenu(viewer.getControl());
        viewer.getControl().setMenu(menu);
        return viewer;
    }

    private static Action makeSelectAllAction(final CheckboxTableViewer vwr) {
        Action action = new Action() {
            @Override
            public void run() {
                Object[] allTSCs = (Object[])vwr.getInput();
                vwr.setCheckedElements(allTSCs);
            }
        };
        action.setText(ACTION_SELECT_ALL_TEXT);
        action.setToolTipText(ACTION_SELECT_ALL_TOOLTIP);
        return action;
    }

    private static Action makeDeselectAllAction(final CheckboxTableViewer vwr) {
        Action action = new Action() {
            @Override
            public void run() {
                vwr.setCheckedElements(new Object[0]);
            }
        };
        action.setText(ACTION_DESELECT_ALL_TEXT);
        action.setToolTipText(ACTION_DESELECT_ALL_TOOLTIP);
        return action;
    }

    @Override
    protected void okPressed() {
        List<TSContainerInfo> tscsToDelete = new ArrayList<TSContainerInfo>();
        Dialog confirmDialog;
        for(Object tsc : this.viewer.getCheckedElements()) {
            tscsToDelete.add((TSContainerInfo)tsc);
        }
        this.close();
        if(tscsToDelete.isEmpty()) {
            return;
        }
        confirmDialog = new DeleteTSCsConfirmDialog(
                this.getParentShell(), tscsToDelete);
        confirmDialog.setBlockOnOpen(true);
        if(confirmDialog.open() == DeleteTSCsConfirmDialog.YES_BUTTON_INDEX) {
            try {
                CodeCoverPlugin.getDefault().getTSContainerManager()
                        .deleteTestSessionContainers(tscsToDelete, null);
            } catch(CoreException e) {
                ErrorDialog.openError(
                        this.getParentShell(),
                        DIALOG_ERROR_TITLE,
                        null,
                        new Status( IStatus.ERROR,
                                    CodeCoverPlugin.PLUGIN_ID,
                                    IStatus.OK,
                                    DIALOG_ERROR_MSG,
                                    e));
            } catch(CancelException e) {
                // ignore because it can't be thrown if no monitor was passed
            }
        }
    }

}
