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

import java.util.List;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.TSContainerInfo;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * A dialog which allows the user to confirm the deletion of a previously made
 * selection of test session containers.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: DeleteTSCsConfirmDialog.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DeleteTSCsConfirmDialog extends MessageDialog {

    private static final String TITLE = Messages
            .getString("DeleteTSCsConfirmDialog.TITLE"); //$NON-NLS-1$

    private static final String MESSAGE = Messages
            .getString("DeleteTSCsConfirmDialog.MESSAGE"); //$NON-NLS-1$

    static final int YES_BUTTON_INDEX = 0;

    private List<TSContainerInfo> selection;

    DeleteTSCsConfirmDialog(Shell shell,
            List<TSContainerInfo> selection) {
        super(  shell,
                TITLE,
                null,
                MESSAGE,
                MessageDialog.QUESTION,
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

        // create tree viewer which lists the previously selected TSCs
        TableViewer viewer
            = DeleteTSCsConfirmDialog.createTSCList(parent);
        viewer.setInput(this.selection);
        viewer.setSelection(new StructuredSelection());

        return parent;
    }

    private static TableViewer createTSCList(Composite parent) {
        /*
         * with SWT.MULTI the selection mark (usually the selected row has a
         * blue background) can be hidden by setting an empty selection
         */
        TableViewer viewer = new TableViewer(parent, SWT.BORDER | SWT.MULTI);
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
        return viewer;
    }

}
