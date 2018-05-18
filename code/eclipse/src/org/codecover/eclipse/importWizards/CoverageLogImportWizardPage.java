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
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.Charset;
import java.nio.charset.IllegalCharsetNameException;
import java.nio.charset.UnsupportedCharsetException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.tscmanager.ActiveTSContainerRunnable;
import org.codecover.eclipse.tscmanager.TSContainerInfo;
import org.codecover.eclipse.tscmanager.exceptions.CancelException;
import org.codecover.instrumentation.measurement.CoverageResultLogReader;
import org.codecover.instrumentation.measurement.MeasurementConstants;
import org.codecover.instrumentation.measurement.parser.CoverageLogParser;
import org.codecover.instrumentation.measurement.parser.ParseException;
import org.codecover.instrumentation.measurement.parser.TokenMgrError;
import org.codecover.instrumentation.measurement.parser.WrongUIDException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.utils.file.FileTool;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

/**
 * The (one and only) page of the wizard to import coverage logs.
 *
 * @see CoverageLogImportWizard
 *
 * @author Robert Hanussek
 * @version 1.0 ($Id: CoverageLogImportWizardPage.java 54 2009-07-20 11:25:13Z ahija $)
 */
public class CoverageLogImportWizardPage extends WizardPage {

    private static final String PAGE_NAME = Messages
            .getString("CoverageLogImportWizardPage.PAGE_NAME"); //$NON-NLS-1$

    private static final String DESCRIPTION = Messages
            .getString("CoverageLogImportWizardPage.DESCRIPTION"); //$NON-NLS-1$

    private static final String GROUP_COVERAGE_LOG = Messages
            .getString("CoverageLogImportWizardPage.GROUP_COVERAGE_LOG"); //$NON-NLS-1$

    private static final String LABEL_COVERAGE_LOG = Messages
            .getString("CoverageLogImportWizardPage.LABEL_COVERAGE_LOG"); //$NON-NLS-1$

    private static final String LABEL_CHARSET = Messages
            .getString("CoverageLogImportWizardPage.LABEL_CHARSET"); //$NON-NLS-1$

    private static final String LABEL_SELECT_TEST_SESSION_CONTAINER = Messages
            .getString("CoverageLogImportWizardPage.LABEL_SELECT_TEST_SESSION_CONTAINER"); //$NON-NLS-1$

    private static final String GROUP_TEST_SESSION = Messages
            .getString("CoverageLogImportWizardPage.GROUP_TEST_SESSION"); //$NON-NLS-1$

    private static final String LABEL_TEST_SESSION_NAME = Messages
            .getString("CoverageLogImportWizardPage.LABEL_TEST_SESSION_NAME"); //$NON-NLS-1$

    private static final String LABEL_TEST_SESSION_COMMENT = Messages
            .getString("CoverageLogImportWizardPage.LABEL_TEST_SESSION_COMMENT"); //$NON-NLS-1$

    private static final String ERROR_FILE_NONE_SPECIFIED = Messages
            .getString("CoverageLogImportWizardPage.ERROR_FILE_NONE_SPECIFIED"); //$NON-NLS-1$

    private static final String ERROR_FILE_FILE_NOT_FOUND = Messages
            .getString("CoverageLogImportWizardPage.ERROR_FILE_FILE_NOT_FOUND"); //$NON-NLS-1$

    private static final String ERROR_FILE_NOT_A_FILE = Messages
            .getString("CoverageLogImportWizardPage.ERROR_FILE_NOT_A_FILE"); //$NON-NLS-1$

    private static final String ERROR_FILE_NOT_READABLE = Messages
            .getString("CoverageLogImportWizardPage.ERROR_FILE_NOT_READABLE"); //$NON-NLS-1$

    private static final String ERROR_CHARSET_NONE_SPECIFIED = Messages
            .getString("CoverageLogImportWizardPage.ERROR_CHARSET_NONE_SPECIFIED"); //$NON-NLS-1$

    private static final String ERROR_CHARSET_UNSUPPORTED = Messages
            .getString("CoverageLogImportWizardPage.ERROR_CHARSET_UNSUPPORTED"); //$NON-NLS-1$

    private static final String ERROR_TSC_NONE_SPECIFIED = Messages
            .getString("CoverageLogImportWizardPage.ERROR_TSC_NONE_SPECIFIED"); //$NON-NLS-1$

    private static final String ERROR_TEST_SESSION_NAME_NONE_SPECIFIED = Messages
            .getString("CoverageLogImportWizardPage.ERROR_TEST_SESSION_NAME_NONE_SPECIFIED"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_UNSUPPORTED_CHARSET_TITLE = Messages
            .getString("CoverageLogImportWizardPage.DIALOG_ERROR_UNSUPPORTED_CHARSET_TITLE"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_UNSUPPORTED_CHARSET_MSG = Messages
            .getString("CoverageLogImportWizardPage.DIALOG_ERROR_UNSUPPORTED_CHARSET_MSG"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_LOAD_TSC_TITLE = Messages
            .getString("CoverageLogImportWizardPage.DIALOG_ERROR_LOAD_TSC_TITLE"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_LOAD_TSC_MSG = Messages
            .getString("CoverageLogImportWizardPage.DIALOG_ERROR_LOAD_TSC_MSG"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_ACTIVATE_TSC_OUT_OF_MEM_TITLE = Messages
            .getString("CoverageLogImportWizardPage.DIALOG_ERROR_ACTIVATE_TSC_OUT_OF_MEM_TITLE"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_ACTIVATE_TSC_OUT_OF_MEM_MSG = Messages
            .getString("CoverageLogImportWizardPage.DIALOG_ERROR_ACTIVATE_TSC_OUT_OF_MEM_MSG"); //$NON-NLS-1$

    private static final String DIALOG_WARNING_TEST_SESSION_RENAMED_TITLE = Messages
            .getString("CoverageLogImportWizardPage.DIALOG_WARNING_TEST_SESSION_RENAMED_TITLE"); //$NON-NLS-1$

    private static final String DIALOG_WARNING_TEST_SESSION_RENAMED_MSG = Messages
            .getString("CoverageLogImportWizardPage.DIALOG_WARNING_TEST_SESSION_RENAMED_MSG"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_COVERAGE_LOG_READ_TITLE = Messages
            .getString("CoverageLogImportWizardPage.DIALOG_ERROR_COVERAGE_LOG_READ_TITLE"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_COVERAGE_LOG_ID_MISMATCH_TITLE = Messages
            .getString("CoverageLogImportWizardPage.DIALOG_ERROR_COVERAGE_LOG_ID_MISMATCH_TITLE"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_COVERAGE_LOG_PARSE_TITLE = Messages
            .getString("CoverageLogImportWizardPage.DIALOG_ERROR_COVERAGE_LOG_PARSE_TITLE"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_COVERAGE_LOG_READ_MSG = Messages
            .getString("CoverageLogImportWizardPage.DIALOG_ERROR_COVERAGE_LOG_READ_MSG"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_COVERAGE_LOG_ID_MISMATCH_MSG = Messages
            .getString("CoverageLogImportWizardPage.DIALOG_ERROR_COVERAGE_LOG_ID_MISMATCH_MSG"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_COVERAGE_LOG_PARSE_MSG = Messages
            .getString("CoverageLogImportWizardPage.DIALOG_ERROR_COVERAGE_LOG_PARSE_MSG"); //$NON-NLS-1$

    private static final String[] standardCharsets = {
        "UTF-8",        //$NON-NLS-1$
        "US-ASCII",     //$NON-NLS-1$
        "ISO-8859-1",   //$NON-NLS-1$
        "UTF-16BE",     //$NON-NLS-1$
        "UTF-16LE",     //$NON-NLS-1$
        "UTF-16" };     //$NON-NLS-1$

    private FileFieldEditor fldFile;
    private Combo cmbCharset;
    private TreeViewer viewer;
    private Text txtTestSessionName;
    private Text txtTestSessionComment;

    /**
     * The path to the coverage log file.
     */
    private String filepath;

    private String charsetName;

    /**
     * The test session container the user selected.
     */
    private TSContainerInfo selectedTSCInfo;

    private String testSessionName;
    private String testSessionComment;

    private String fileError;
    private String charsetError;
    private String tscError;
    private String testSessionNameError;

    private boolean importSuccessfull;

    /**
     * Constructor.
     */
    protected CoverageLogImportWizardPage() {
        super(PAGE_NAME);
        this.setTitle(PAGE_NAME);
        this.setDescription(DESCRIPTION);
        this.selectedTSCInfo = null;
        this.testSessionName = "";                                 //$NON-NLS-1$
        this.testSessionComment = "";                              //$NON-NLS-1$
        this.fileError = ERROR_FILE_NONE_SPECIFIED;
        this.tscError = ERROR_TSC_NONE_SPECIFIED;
        this.testSessionNameError = ERROR_TEST_SESSION_NAME_NONE_SPECIFIED;
    }

    @Override
	public void createControl(Composite parent) {
        Composite mainComposite = new Composite(parent, SWT.NONE);
        GridLayout gridLayout;
        GridData gridData;
        gridLayout = new GridLayout();
        gridLayout.numColumns = 1;
        mainComposite.setLayout(gridLayout);

        // create group which will host the widgets for the coverage log
        Group grpCoverageLog = new Group(mainComposite, SWT.NONE);
        grpCoverageLog.setText(GROUP_COVERAGE_LOG);
        gridLayout = new GridLayout();
        gridLayout.numColumns = 2;
        grpCoverageLog.setLayout(gridLayout);
        // create widget to select the file to import
        Composite fileFieldComposite = new Composite(grpCoverageLog, SWT.NONE);
        fileFieldComposite.setLayout(new FillLayout());
        this.fldFile = new FileFieldEditor("fileSelect",           //$NON-NLS-1$
                LABEL_COVERAGE_LOG, fileFieldComposite);
        this.fldFile.getTextControl(fileFieldComposite)
                .addModifyListener(new FileFieldListener());
        this.fldFile.getTextControl(fileFieldComposite).clearSelection();
        this.fldFile.setFileExtensions(new String[] {
                "*.clf", "*.*"});                    //$NON-NLS-1$ //$NON-NLS-2$
        // layout file widget
        gridData = new GridData();
        gridData.horizontalSpan = 2;
        gridData.horizontalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        fileFieldComposite.setLayoutData(gridData);
        // create label for the charset combo
        Label charsetLabel = new Label(grpCoverageLog, SWT.NONE);
        charsetLabel.setText(LABEL_CHARSET);
        // layout charset label
        charsetLabel.setLayoutData(new GridData());
        // create combo for charset selection
        this.cmbCharset = new Combo(grpCoverageLog, SWT.DROP_DOWN | SWT.SINGLE);
        Charset defaultCharset = MeasurementConstants.CHARSET;
        this.cmbCharset.add(defaultCharset.name());
        for(String charsetName : standardCharsets) {
            if(!charsetName.equals(defaultCharset.name())) {
                this.cmbCharset.add(charsetName);
            }
        }
        this.cmbCharset.select(0);
        this.charsetName = this.cmbCharset.getText();
        this.cmbCharset.addModifyListener(new CharsetComboListener());
        // layout charset combo
        this.cmbCharset.setLayoutData(new GridData());

        // create label for (following) tree viewer
        Label viewerLabel = new Label(mainComposite, SWT.NONE);
        viewerLabel.setText(LABEL_SELECT_TEST_SESSION_CONTAINER);

        // create tree viewer which lists the known test session containers
        this.viewer = new TreeViewer(mainComposite, SWT.SINGLE
                | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
        // set the viewer's input to all projects (including closed ones)
        this.viewer.setContentProvider(new ViewerContentProvider());
        this.viewer.setLabelProvider(new ViewerLabelProvider());
        this.viewer.setInput(
                ResourcesPlugin.getWorkspace().getRoot().getProjects());
        this.viewer.addSelectionChangedListener(new ViewerListener());
        // per default, we select the current test session container
        ActiveTSContainerInfo activeTSCInfo = CodeCoverPlugin.getDefault().getTSContainerManager().getActiveTSContainer();
        if (activeTSCInfo != null)
        {
            IProject testSessionContainerProject = activeTSCInfo.getProject();
            TreePath selectedTreePath = new TreePath(
                    new Object[] { testSessionContainerProject, activeTSCInfo });
            TreeSelection selection = new TreeSelection(selectedTreePath);
            this.viewer.setSelection(selection, true);
        }

        // create group which will host the test session name and comment field
        Group grpTestSession = new Group(mainComposite, SWT.NONE);
        grpTestSession.setText(GROUP_TEST_SESSION);
        gridLayout = new GridLayout();
        gridLayout.numColumns = 2;
        grpTestSession.setLayout(gridLayout);
        Label lblTSName = new Label(grpTestSession, SWT.LEAD);
        lblTSName.setText(LABEL_TEST_SESSION_NAME);
        // create text field for test session name
        this.txtTestSessionName = new Text(grpTestSession,
                SWT.SINGLE | SWT.BORDER);
        // layout text field for test session name
        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        this.txtTestSessionName.setLayoutData(gridData);
        // create label of text field for test session comment
        Label lblTSComment = new Label(grpTestSession, SWT.LEAD);
        lblTSComment.setText(LABEL_TEST_SESSION_COMMENT);
        // layout label of text field for test session comment
        gridData = new GridData();
        gridData.verticalAlignment = GridData.BEGINNING;
        lblTSComment.setLayoutData(gridData);
        // create text field for test session comment
        this.txtTestSessionComment = new Text(grpTestSession,
                SWT.MULTI | SWT.WRAP | SWT.V_SCROLL | SWT.BORDER);
        // layout text field for test session comment
        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        this.txtTestSessionComment.setLayoutData(gridData);
        // register listeners for test session name/comment
        ModifyListener tsListener = new TestSessionFieldsListener();
        this.txtTestSessionName.addModifyListener(tsListener);
        this.txtTestSessionComment.addModifyListener(tsListener);

        // layout mainComposite
        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        grpCoverageLog.setLayoutData(gridData);

        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        viewerLabel.setLayoutData(gridData);

        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.verticalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        this.viewer.getControl().setLayoutData(gridData);

        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        grpTestSession.setLayoutData(gridData);

        this.setControl(mainComposite);
        this.applyErrorMessage();
    }

    /** Overwrites the currently selected file and may be used to select a file intialy. */
    public void selectCoverageLogFile(String path)
    {
        this.fldFile.setStringValue(path);
    }

    boolean importCoverageLog() {
        CoverageLogImportWizardPage.this.importSuccessfull = true;
        ActiveTSContainerRunnable tscRunnable = new ActiveTSContainerRunnable(){
            @Override
			public String getDescription() {
                return DESCRIPTION;
            }
            @Override
			public void run(ActiveTSContainerInfo activeTSCInfo,
                    IProgressMonitor monitor)
                            throws CancelException {
                TestSessionContainer activeTSC
                        = activeTSCInfo.getTestSessionContainer();
                Charset coverageLogCharset;
                MASTBuilder builder;
                CoverageLogParser logParser;
                CoverageResultLogReader coverageResultLogReader;
                int monitorScale = 1000;
                try { // this try only ensure that the monitor is left done()
                monitor.beginTask(DESCRIPTION, 3 * monitorScale);
                try {
                    coverageLogCharset = Charset.forName(
                            CoverageLogImportWizardPage.this.charsetName);
                } catch(UnsupportedCharsetException e) {
                    ErrorDialog.openError(
                            CodeCoverPlugin.getDefault()
                                    .getWorkbench().getDisplay()
                                    .getActiveShell(),
                            DIALOG_ERROR_UNSUPPORTED_CHARSET_TITLE,
                            null,
                            new Status( IStatus.ERROR,
                                        CodeCoverPlugin.PLUGIN_ID,
                                        IStatus.OK,
                                        DIALOG_ERROR_UNSUPPORTED_CHARSET_MSG,
                                        e));
                    CoverageLogImportWizardPage.this.importSuccessfull = false;
                    return;
                }

                monitor.worked(1 * monitorScale);                          // #1

                TestSession testSession = activeTSC.createTestSession(
                        CoverageLogImportWizardPage.this.testSessionName,
                        CoverageLogImportWizardPage.this.testSessionComment,
                        new Date());
                /*
                 * If the name of the test session already existed, the user
                 * should know, that the session is called something like
                 * "testSessionName (1)"
                 */
                if(!CoverageLogImportWizardPage.this
                        .testSessionName.equals(testSession.getName())) {
                    MessageDialog.openWarning(
                            CodeCoverPlugin.getDefault().getWorkbench()
                                    .getDisplay().getActiveShell(),
                            DIALOG_WARNING_TEST_SESSION_RENAMED_TITLE,
                            String.format(
                                    DIALOG_WARNING_TEST_SESSION_RENAMED_MSG,
                                    CoverageLogImportWizardPage.this
                                            .testSessionName,
                                    testSession.getName()));
                }

                monitor.worked(1 * monitorScale);                          // #2

                try {
                    logParser = new CoverageLogParser(new File(
                            CoverageLogImportWizardPage.this.filepath),
                            coverageLogCharset);
                    builder = new MASTBuilder(
                            CodeCoverPlugin.getDefault().getLogger());
                    coverageResultLogReader = new CoverageResultLogReader(
                            testSession, builder);

                    logParser.CompilationUnit(coverageResultLogReader,
                            activeTSC.getId());
                } catch(IOException e) {
                    ErrorDialog.openError(
                            CodeCoverPlugin.getDefault()
                                    .getWorkbench().getDisplay()
                                    .getActiveShell(),
                            DIALOG_ERROR_COVERAGE_LOG_READ_TITLE,
                            null,
                            new Status( IStatus.ERROR,
                                        CodeCoverPlugin.PLUGIN_ID,
                                        IStatus.OK,
                                        DIALOG_ERROR_COVERAGE_LOG_READ_MSG,
                                        e));
                    CoverageLogImportWizardPage.this.importSuccessfull = false;
                    testSession.delete();
                    return;
                } catch(WrongUIDException e) {
                    ErrorDialog.openError(
                            CodeCoverPlugin.getDefault()
                                    .getWorkbench().getDisplay()
                                    .getActiveShell(),
                            DIALOG_ERROR_COVERAGE_LOG_ID_MISMATCH_TITLE,
                            null,
                            new Status( IStatus.ERROR,
                                        CodeCoverPlugin.PLUGIN_ID,
                                        IStatus.OK,
                                      DIALOG_ERROR_COVERAGE_LOG_ID_MISMATCH_MSG,
                                        e));
                    CoverageLogImportWizardPage.this.importSuccessfull = false;
                    testSession.delete();
                    return;
                } catch(ParseException e) {
                    ErrorDialog.openError(
                            CodeCoverPlugin.getDefault()
                                    .getWorkbench().getDisplay()
                                    .getActiveShell(),
                            DIALOG_ERROR_COVERAGE_LOG_PARSE_TITLE,
                            null,
                            new Status( IStatus.ERROR,
                                        CodeCoverPlugin.PLUGIN_ID,
                                        IStatus.OK,
                                        DIALOG_ERROR_COVERAGE_LOG_PARSE_MSG,
                                        e));
                    CoverageLogImportWizardPage.this.importSuccessfull = false;
                    testSession.delete();
                    return;
                }  catch(TokenMgrError e) {
                    ErrorDialog.openError(
                            CodeCoverPlugin.getDefault()
                                    .getWorkbench().getDisplay()
                                    .getActiveShell(),
                            DIALOG_ERROR_COVERAGE_LOG_PARSE_TITLE,
                            null,
                            new Status( IStatus.ERROR,
                                        CodeCoverPlugin.PLUGIN_ID,
                                        IStatus.OK,
                                        DIALOG_ERROR_COVERAGE_LOG_PARSE_MSG,
                                        e));
                    CoverageLogImportWizardPage.this.importSuccessfull = false;
                    testSession.delete();
                    return;
                }
                monitor.worked(1 * monitorScale);                          // #3
                } finally {
                    monitor.done();
                }
            }
        };

        try {
            CodeCoverPlugin.getDefault().getTSContainerManager()
                    .setActiveTSContainer(this.selectedTSCInfo, tscRunnable,
                            null);
        } catch(FileLoadException e) {
            ErrorDialog.openError(
                    CodeCoverPlugin.getDefault()
                            .getWorkbench().getDisplay()
                            .getActiveShell(),
                    DIALOG_ERROR_LOAD_TSC_TITLE,
                    null,
                    new Status( IStatus.ERROR,
                                CodeCoverPlugin.PLUGIN_ID,
                                IStatus.OK,
                                DIALOG_ERROR_LOAD_TSC_MSG,
                                e));
            return false;
        } catch(OutOfMemoryError e) {
            ErrorDialog.openError(
                    CodeCoverPlugin.getDefault()
                            .getWorkbench().getDisplay()
                            .getActiveShell(),
                    DIALOG_ERROR_ACTIVATE_TSC_OUT_OF_MEM_TITLE,
                    null,
                    new Status( IStatus.ERROR,
                                CodeCoverPlugin.PLUGIN_ID,
                                IStatus.OK,
                                DIALOG_ERROR_ACTIVATE_TSC_OUT_OF_MEM_MSG,
                                e));
            return false;
        } catch(InvocationTargetException e) {
            CodeCoverPlugin.getDefault().getLogger().error(
                    "Unknown error while importing coverage log",  //$NON-NLS-1$
                    e);
        } catch(CancelException e) {
            CodeCoverPlugin.getDefault().getLogger().warning(
                "Canceled import of coverage log.", e);            //$NON-NLS-1$
        }

        return CoverageLogImportWizardPage.this.importSuccessfull;
    }

    private final static class ViewerContentProvider
            implements ITreeContentProvider {

        @Override
		public Object[] getChildren(Object parent) {
            if(parent instanceof IProject) {
                return CodeCoverPlugin.getDefault().getTSContainerManager()
                        .getTestSessionContainers((IProject)parent).toArray();
            } else {
                return null;
            }
        }

        @Override
		public Object getParent(Object element) {
            if(element instanceof TSContainerInfo) {
                return ((TSContainerInfo)element).getProject();
            }
            return null;
        }

        @Override
		public boolean hasChildren(Object element) {
            if(element instanceof IProject) {
                return !CodeCoverPlugin.getDefault().getTSContainerManager()
                        .getTestSessionContainers((IProject)element).isEmpty();
            }
            return false;
        }

        @Override
		public Object[] getElements(Object inputElement) {
            List<IProject> projects = new ArrayList<IProject>();
            if(inputElement instanceof IProject[]) {
                /*
                 * show only (open) projects which contain known test session
                 * containers
                 */
                for(IProject project : (IProject[])inputElement) {
                    if(!CodeCoverPlugin.getDefault().getTSContainerManager()
                            .getTestSessionContainers(project).isEmpty()) {
                        projects.add(project);
                    }
                }
                return projects.toArray();
            } else {
                return null;
            }
        }

        @Override
		public void dispose() {}

        @Override
		public void inputChanged(Viewer viewer, Object oldInput,
                Object newInput) {}

    }

    private final static class ViewerLabelProvider extends LabelProvider {
        @Override
        public String getText(Object element) {
            if(element instanceof IProject) {
                return ((IProject)element).getName();
            } else if(element instanceof TSContainerInfo) {
                return ((TSContainerInfo)element).getName();
            } else {
                return "";                                         //$NON-NLS-1$
            }
        }

        @Override
        public Image getImage(Object element) {
            if(element instanceof IProject) {
                return PlatformUI.getWorkbench().getSharedImages().getImage(
                        IDE.SharedImages.IMG_OBJ_PROJECT);
            } else if(element instanceof TSContainerInfo) {
                return CodeCoverPlugin.getDefault().getImageRegistry().get(
                        CodeCoverPlugin.Image.SESSION_CONTAINER.getPath());
            } else {
                return null;
            }
        }
    }

    private final class FileFieldListener implements ModifyListener {
        @Override
		public void modifyText(ModifyEvent event) {
            String error = null;
            CoverageLogImportWizardPage.this.filepath
                = CoverageLogImportWizardPage.this.fldFile.getStringValue();
            File covLogFile = new File(
                CoverageLogImportWizardPage.this.filepath);
            if(CoverageLogImportWizardPage.this.fldFile.getStringValue().length() == 0) {
                error = ERROR_FILE_NONE_SPECIFIED;
            } else if(!covLogFile.exists()) {
                error = ERROR_FILE_FILE_NOT_FOUND;
            } else if(!covLogFile.isFile()) {
                error = ERROR_FILE_NOT_A_FILE;
            } else if(!covLogFile.canRead()) {
                error = ERROR_FILE_NOT_READABLE;
            } else if (CoverageLogImportWizardPage.this.txtTestSessionName.getText().length() == 0) {
                // if we do not have a test session name set, we set the filename
                String fileName = covLogFile.getName();
                String extension = FileTool.getExtension(fileName);
                if (extension != null) {
                    fileName = fileName.substring(0, fileName.length() - extension.length() - 1);
                }
                CoverageLogImportWizardPage.this.txtTestSessionName.setText(fileName);
            }
            CoverageLogImportWizardPage.this.fileError = error;

            CoverageLogImportWizardPage.this.applyErrorMessage();
        }
    }

    private final class CharsetComboListener implements ModifyListener {
        @Override
		public void modifyText(ModifyEvent event) {
            String error = null;
            boolean charsetUnsupported = false;
            CoverageLogImportWizardPage.this.charsetName
                    = CoverageLogImportWizardPage.this.cmbCharset.getText();
            try {
                Charset.forName(CoverageLogImportWizardPage.this.charsetName);
            } catch(IllegalCharsetNameException e) {
                charsetUnsupported = true;
            } catch(UnsupportedCharsetException e) {
                charsetUnsupported = true;
            }
            if(CoverageLogImportWizardPage.
                    this.charsetName.length() == 0) {
                error = ERROR_CHARSET_NONE_SPECIFIED;
            } else if(charsetUnsupported) {
                error = ERROR_CHARSET_UNSUPPORTED;
            }
            CoverageLogImportWizardPage.this.charsetError = error;

            CoverageLogImportWizardPage.this.applyErrorMessage();
        }
    }

    private final class ViewerListener implements ISelectionChangedListener {
        @Override
		public void selectionChanged(SelectionChangedEvent event) {
            IStructuredSelection sel;
            String error = null;
            if(!(event.getSelection() instanceof IStructuredSelection)) {
                return;
            }
            sel = (IStructuredSelection)event.getSelection();
            if(sel.getFirstElement() instanceof TSContainerInfo) {
                CoverageLogImportWizardPage.this.selectedTSCInfo
                        = (TSContainerInfo)sel.getFirstElement();
            } else {
                CoverageLogImportWizardPage.this.selectedTSCInfo = null;
                error = ERROR_TSC_NONE_SPECIFIED;
            }
            CoverageLogImportWizardPage.this.tscError = error;

            CoverageLogImportWizardPage.this.applyErrorMessage();
        }
    }

    private final class TestSessionFieldsListener implements ModifyListener {
        @Override
		public void modifyText(ModifyEvent event) {
            String error = null;
            if(event.widget
                    == CoverageLogImportWizardPage.this.txtTestSessionName) {
                CoverageLogImportWizardPage.this.testSessionName
                        = CoverageLogImportWizardPage.this.txtTestSessionName
                                .getText();
                if(CoverageLogImportWizardPage.
                        this.testSessionName.length() == 0) {
                    error = ERROR_TEST_SESSION_NAME_NONE_SPECIFIED;
                }
                CoverageLogImportWizardPage.this.testSessionNameError = error;
            } else if(event.widget
                    == CoverageLogImportWizardPage.this.txtTestSessionComment) {
                CoverageLogImportWizardPage.this.testSessionComment
                        = CoverageLogImportWizardPage.this.txtTestSessionComment
                                .getText();
            }

            CoverageLogImportWizardPage.this.applyErrorMessage();
        }
    }

    private void applyErrorMessage() {
        if(this.fileError != null) {
            this.setErrorMessage(this.fileError);
            this.setPageComplete(false);
        } else if(this.charsetError != null) {
            this.setErrorMessage(this.charsetError);
            this.setPageComplete(false);
        } else if(this.tscError != null) {
            this.setErrorMessage(this.tscError);
            this.setPageComplete(false);
        } else if(this.testSessionNameError != null) {
            this.setErrorMessage(this.testSessionNameError);
            this.setPageComplete(false);
        } else {
            this.setErrorMessage(null);
            this.setPageComplete(true);
        }
    }

}
