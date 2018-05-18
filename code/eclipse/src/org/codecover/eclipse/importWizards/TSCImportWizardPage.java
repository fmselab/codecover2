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

package org.codecover.eclipse.importWizards;

import java.io.File;
import java.util.Arrays;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.utils.Logger;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;

/**
 * The (one and only) page of the wizard to import test session containers.
 *
 * @see TSCImportWizard
 *
 * @author Robert Hanussek
 * @version 1.0 ($Id: TSCImportWizardPage.java 54 2009-07-20 11:25:13Z ahija $)
 */
public class TSCImportWizardPage extends WizardPage implements Listener {

    private Composite fileFieldComposite;
    private FileFieldEditor fileField;
    private List list;
    private IStructuredSelection selection;
    private String filepath;
    private java.util.List<IProject> projects;
    private IProject selectedProject;
    private String fileError;
    private String projectError;

    private static final String ERROR_TSC_LOAD = Messages
            .getString("TSCImportWizardPage.ERROR_TSC_LOAD"); //$NON-NLS-1$
    private static final String ERROR_TSC_WRITE = Messages
            .getString("TSCImportWizardPage.ERROR_TSC_WRITE"); //$NON-NLS-1$
    private static final String LABEL_SELECT_PROJECT = Messages
            .getString("TSCImportWizardPage.LABEL_SELECT_PROJECT"); //$NON-NLS-1$
    private static final String LABEL_TEST_SESSION_CONTAINER = Messages
            .getString("TSCImportWizardPage.LABEL_TEST_SESSION_CONTAINER"); //$NON-NLS-1$
    private static final String DESCRIPTION = Messages
            .getString("TSCImportWizardPage.DESCRIPTION"); //$NON-NLS-1$
    private static final String FILE_ERROR_NONE_SPECIFIED = Messages
            .getString("TSCImportWizardPage.FILE_ERROR_NONE_SPECIFIED"); //$NON-NLS-1$
    private static final String FILE_ERROR_FILE_NOT_FOUND = Messages
            .getString("TSCImportWizardPage.FILE_ERROR_FILE_NOT_FOUND"); //$NON-NLS-1$
    private static final String FILE_ERROR_NOT_A_FILE = Messages
            .getString("TSCImportWizardPage.FILE_ERROR_NOT_A_FILE"); //$NON-NLS-1$
    private static final String FILE_ERROR_NOT_READABLE = Messages
            .getString("TSCImportWizardPage.FILE_ERROR_NOT_READABLE"); //$NON-NLS-1$
    private static final String PROJECT_ERROR_NONE_SPECIFIED = Messages
            .getString("TSCImportWizardPage.PROJECT_ERROR_NONE_SPECIFIED"); //$NON-NLS-1$

    /**
     * Constructor
     *
     * @param pageName
     *            the name of the page
     * @param selection
     *            the initial selection.
     */
    protected TSCImportWizardPage(String pageName,
            IStructuredSelection selection) {
        super(pageName);
        this.setTitle(pageName);
        this.setDescription(DESCRIPTION);
        this.selection = selection;
        this.fileError = FILE_ERROR_NONE_SPECIFIED;
        this.projectError = PROJECT_ERROR_NONE_SPECIFIED;
    }

    /**
     * (non-Javadoc)
     *
     * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
     */
    @Override
	public void createControl(Composite parent) {
        Composite mainComposite = new Composite(parent, SWT.NONE);
        mainComposite.setLayout(new FormLayout());
        this.fileFieldComposite = new Composite(mainComposite, SWT.NONE);
        this.fileFieldComposite.setLayout(new FillLayout());

        // create control to select the file to import
        this.fileField = new FileFieldEditor("fileSelect", //$NON-NLS-1$
                LABEL_TEST_SESSION_CONTAINER, this.fileFieldComposite);
        this.fileField.getTextControl(this.fileFieldComposite).addListener(
                SWT.Modify, this);
        this.fileField.getTextControl(this.fileFieldComposite).clearSelection();
        this.fileField.setFileExtensions(new String[] {"*.xml", "*.*"}); //$NON-NLS-1$//$NON-NLS-2$

        // create label for (following) list control
        Label label = new Label(mainComposite, SWT.NONE);
        label.setText(LABEL_SELECT_PROJECT);

        // create list control to select project
        this.list = new List(mainComposite, SWT.BORDER | SWT.SINGLE);

        this.projects = Arrays.asList(ResourcesPlugin.getWorkspace().getRoot()
                .getProjects());
        for(IProject project : this.projects) {
            this.list.add(project.getName());
        }
        /*
         * If the user selected a Java project before starting the import wizard
         * this project is selected in the list control. (doesn't work yet)
         */
        if(!this.selection.isEmpty()
                && this.selection.getFirstElement() instanceof IJavaProject) {
            IJavaProject selPrj = (IJavaProject) this.selection
                    .getFirstElement();
            if(this.projects.contains(selPrj)) {
                this.list.select(this.projects.indexOf(selPrj));
            }
        }
        this.list.addListener(SWT.Selection, this);

        // layout
        FormData formData = new FormData();
        formData.left = new FormAttachment(0, 5);
        formData.right = new FormAttachment(100, -5);
        formData.top = new FormAttachment(0, 5);
        // formData.bottom = new FormAttachment(100,-5);
        this.fileFieldComposite.setLayoutData(formData);

        formData = new FormData();
        formData.left = new FormAttachment(0, 5);
        formData.top = new FormAttachment(this.fileFieldComposite, 5);
        label.setLayoutData(formData);

        formData = new FormData();
        formData.left = new FormAttachment(0, 5);
        formData.right = new FormAttachment(100, -5);
        formData.top = new FormAttachment(label, 5);
        formData.bottom = new FormAttachment(100, -5);
        this.list.setLayoutData(formData);

        this.setControl(mainComposite);
        this.applyErrorMessage();
    }

    /**
     * Imports the test session container
     *
     * @return true; because scheduling always works.
     */
    boolean importTestSessionContainer() {
        (new ImportTSCJob(this.filepath, this.selectedProject)).schedule();
        return true; // because scheduling always works
    }

    private class ImportTSCJob extends Job {

        /**
         * The path (in the file system) to the test session container to
         * import.
         */
        private final String filepath;

        /**
         * The project to import the test session container into.
         */
        private final IProject project;

        /**
         * Constructs a job which imports a test session container
         *
         * @param filepath  the path (in the file system) to the test session
         *                  container to import
         *
         * @param project   the project to import the test session container
         *                  into
         */
        public ImportTSCJob(String filepath, IProject project) {
            super(TSCImportWizardPage.this.getTitle());
            this.setUser(true);
            this.filepath = filepath;
            this.project = project;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            TestSessionContainer tsc;
            Logger logger = CodeCoverPlugin.getDefault().getLogger();
            File tscFile = new File(this.filepath);
            if(this.project != null && tscFile.exists()
                    && tscFile.isFile()) {
                // load the test session container to import
                try {
                    tsc = TestSessionContainer.load(
                            CodeCoverPlugin.getDefault()
                                .getEclipsePluginManager().getPluginManager(),
                            logger,
                            new MASTBuilder(logger),
                            tscFile);
                } catch(FileLoadException e) {
                    logger.error("Error while importing test" +    //$NON-NLS-1$
                            " session container: "                 //$NON-NLS-1$
                            + tscFile.getAbsolutePath(), e);
                    Display.getDefault().asyncExec(new Runnable() {
                        @Override
						public void run() {
                            MessageDialog.openError(null,
                                    TSCImportWizardPage.this.getTitle(),
                                    ERROR_TSC_LOAD);
                        }
                    });
                    return Status.CANCEL_STATUS;
                }

                // write test session container
                try {
                    CodeCoverPlugin.getDefault().getTSContainerManager()
                            .addTestSessionContainer(tsc, this.project,
                                    false, null, null);
                } catch(Exception e) {
                    logger.error("Error while importing test" +    //$NON-NLS-1$
                            " session container: "                 //$NON-NLS-1$
                            + tscFile.getAbsolutePath(), e);
                    Display.getDefault().asyncExec(new Runnable() {
                        @Override
						public void run() {
                            MessageDialog.openError(null,
                                    TSCImportWizardPage.this.getTitle(),
                                    ERROR_TSC_WRITE);
                        }
                    });
                    return Status.CANCEL_STATUS;
                }
            }

            return Status.OK_STATUS;
        }

    }

    /**
     * (non-Javadoc)
     *
     * @see org.eclipse.swt.widgets.Listener#handleEvent(org.eclipse.swt.widgets.Event)
     */
    @Override
	public void handleEvent(Event event) {
        String error = null;
        File tscFile;
        if (event.widget == this.fileField.getTextControl(this.fileFieldComposite)) {
            this.filepath = this.fileField.getStringValue();
            tscFile = new File(this.filepath);
            if (this.filepath.length() == 0) {
                error = FILE_ERROR_NONE_SPECIFIED;
            } else if (!tscFile.exists()) {
                error = FILE_ERROR_FILE_NOT_FOUND;
            } else if (!tscFile.isFile()) {
                error = FILE_ERROR_NOT_A_FILE;
            } else if (!tscFile.canRead()) {
                error = FILE_ERROR_NOT_READABLE;
            }
            this.fileError = error;
        } else if (event.widget == this.list) {
            int selectedIndex = this.list.getSelectionIndex();
            if (selectedIndex < 0 || selectedIndex >= this.projects.size()) {
                this.selectedProject = null;
                error = PROJECT_ERROR_NONE_SPECIFIED;
            } else {
                this.selectedProject = this.projects.get(selectedIndex);
            }
            this.projectError = error;
        }
        this.applyErrorMessage();
    }

    private void applyErrorMessage() {
        if(this.fileError != null) {
            this.setErrorMessage(this.fileError);
            this.setPageComplete(false);
        } else if(this.projectError != null) {
            this.setErrorMessage(this.projectError);
            this.setPageComplete(false);
        } else {
            this.setErrorMessage(null);
            this.setPageComplete(true);
        }
    }

}
