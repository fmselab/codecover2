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

package org.codecover.eclipse.exportWizards;

import java.io.File;

import org.codecover.eclipse.Messages;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: ReportExportPage.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ReportExportPage extends WizardPage {
    private static final String LABEL_TEMPLATE = Messages
            .getString("ReportExportPage.LABEL_TEMPLATE"); //$NON-NLS-1$

    private static final String LABEL_TEMPLATE_GROUP = Messages
            .getString("ReportExportPage.LABEL_TEMPLATE_GROUP"); //$NON-NLS-1$

    private static final String DESCRIPTION = Messages
            .getString("ReportExportPage.DESCRIPTION"); //$NON-NLS-1$

    private static final String FILE_ERROR_NONE_SPECIFIED = Messages
            .getString("ReportExportPage.FILE_ERROR_NONE_SPECIFIED"); //$NON-NLS-1$

    private static final String FILE_ERROR_NOT_A_FILE = Messages
            .getString("ReportExportPage.FILE_ERROR_NOT_A_FILE"); //$NON-NLS-1$

    private static final String FILE_ERROR_NOT_READABLE = Messages
            .getString("ReportExportPage.FILE_ERROR_NOT_READABLE"); //$NON-NLS-1$

    private FileFieldEditor templateLocation;

    /**
     * @param pageName
     */
    protected ReportExportPage(String pageName) {
        super(pageName);
        setTitle(pageName);
        setDescription(DESCRIPTION);
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
     */
    @Override
	public void createControl(Composite parent) {
        Composite mainComposite = new Composite(parent, SWT.NONE);
        mainComposite.setLayout(new GridLayout());
        setControl(mainComposite);

        mainComposite.setLayoutData(new GridData(GridData.FILL_BOTH
                | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL));

        createTemplateSelection(mainComposite);

        handleErrors();
    }

    /**
     * @param mainComposite
     */
    private void createTemplateSelection(Composite mainComposite) {
        Group group = new Group(mainComposite, SWT.NONE);
        group.setText(LABEL_TEMPLATE_GROUP);

        GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = 2;
        gridLayout.makeColumnsEqualWidth = false;
        group.setLayout(gridLayout);
        group.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        Label templateLabel = new Label(group, SWT.NONE);
        templateLabel.setText(LABEL_TEMPLATE);

        Composite fileFieldComposite = new Composite(group, SWT.NONE);
        fileFieldComposite.setLayout(new FillLayout());

        this.templateLocation = new FileFieldEditor("", "", fileFieldComposite); //$NON-NLS-1$ //$NON-NLS-2$
        this.templateLocation.getTextControl(fileFieldComposite)
                .addModifyListener(new ModifyListener() {

                    @Override
					public void modifyText(ModifyEvent e) {
                        onTemplateLocationTextChanged(e);
                    }
                });
        this.templateLocation.getTextControl(fileFieldComposite)
                .clearSelection();
        this.templateLocation.setFileExtensions(new String[] {"*.xml", "*.*"}); //$NON-NLS-1$ //$NON-NLS-2$

        fileFieldComposite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL
                | GridData.GRAB_HORIZONTAL));
    }

    /**
     * Gets the selected {@link File}
     * 
     * @return the {@link File}
     */
    public File getSelectedTemplate() {
        return new File(this.templateLocation.getStringValue());
    }

    /**
     * @param e
     */
    protected void onTemplateLocationTextChanged(ModifyEvent e) {
        handleErrors();
    }

    private String checkFile() {
        String error = null;
        String filePath = this.templateLocation.getStringValue();
        File file = new File(filePath);
        if (filePath.length() == 0) {
            error = FILE_ERROR_NONE_SPECIFIED;
        } else if (!file.isFile()) {
            error = FILE_ERROR_NOT_A_FILE;
        } else if (!file.canRead()) {
            error = FILE_ERROR_NOT_READABLE;
        }
        return error;
    }

    private void handleErrors() {
        String fileError = checkFile();
        if (fileError != null) {
            setErrorMessage(fileError);
            setPageComplete(false);
        } else {
            setErrorMessage(null);
            setPageComplete(true);
        }
    }
}
