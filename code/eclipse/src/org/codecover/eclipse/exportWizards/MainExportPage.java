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
import java.util.List;
import java.util.Vector;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerManager;
import org.codecover.eclipse.views.controls.TestSessionsViewerFactory;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.report.Report;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.ContainerCheckedTreeViewer;

/**
 * @author Markus Wittlinger, Robert Hanussek
 * @version 1.0 ($Id: MainExportPage.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MainExportPage extends WizardPage {

    private static final String ERROR_NO_TEST_CASES_SELECTED = Messages
            .getString("MainExportPage.ERROR_NO_TEST_CASES_SELECTED"); //$NON-NLS-1$

    private static final String ERROR_NO_TEST_SESSION_CONTAINER_SELECTED = Messages
            .getString("MainExportPage.ERROR_NO_TEST_SESSION_CONTAINER_SELECTED"); //$NON-NLS-1$

    private static final String WARNING_INCOMPLETE_TEST_SESSION_SELECTION = Messages
            .getString("MainExportPage.WARNING_INCOMPLETE_TEST_SESSION_SELECTION"); //$NON-NLS-1$
    
    private static final String LABEL_REFRESH = Messages
            .getString("MainExportPage.LABEL_REFRESH"); //$NON-NLS-1$

    private static final String LABEL_DESELECT_ALL = Messages
            .getString("MainExportPage.LABEL_DESELECT_ALL"); //$NON-NLS-1$

    private static final String LABEL_SELECT_ALL = Messages
            .getString("MainExportPage.LABEL_SELECT_ALL"); //$NON-NLS-1$

    private static final String LABEL_AVAILABLE_TEST_SESSIONS = Messages
            .getString("MainExportPage.LABEL_AVAILABLE_TEST_SESSIONS"); //$NON-NLS-1$

    private static final String LABEL_EXPORT_TO_FILE_TITLE = Messages
            .getString("MainExportPage.LABEL_EXPORT_TO_FILE_TITLE"); //$NON-NLS-1$

    private static final String LABEL_TSC = Messages
            .getString("MainExportPage.LABEL_TSC"); //$NON-NLS-1$

    private static final String LABEL_BROWSE = Messages
            .getString("MainExportPage.LABEL_BROWSE"); //$NON-NLS-1$

    private static final String LABEL_DESTINATION = Messages
            .getString("MainExportPage.LABEL_DESTINATION"); //$NON-NLS-1$

    private static final String LABEL_TYPE_COMBO = Messages
            .getString("MainExportPage.LABEL_TYPE_COMBO"); //$NON-NLS-1$

    private static final String LABEL_DESTINATION_GROUP = Messages
            .getString("MainExportPage.LABEL_DESTINATION_GROUP"); //$NON-NLS-1$

    private static final String DESCRIPTION = Messages
            .getString("MainExportPage.DESCRIPTION"); //$NON-NLS-1$

    private static final String LABEL_ENUM_TSC = Messages
            .getString("MainExportPage.LABEL_ENUM_TSC"); //$NON-NLS-1$

    private static final String LABEL_ENUM_REPORT = Messages
            .getString("MainExportPage.LABEL_ENUM_REPORT"); //$NON-NLS-1$

    private static final String FILE_ERROR_NONE_SPECIFIED = Messages
            .getString("MainExportPage.FILE_ERROR_NONE_SPECIFIED"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_LOAD_TSC_TITLE = Messages
            .getString("MainExportPage.DIALOG_ERROR_LOAD_TSC_TITLE"); //$NON-NLS-1$
    
    private static final String DIALOG_ERROR_LOAD_TSC_MSG = Messages
            .getString("MainExportPage.DIALOG_ERROR_LOAD_TSC_MSG"); //$NON-NLS-1$
    
    private TSContainerInfo selectedTSCInfo;

    private TestSessionContainer selectedTSC;

    private ContainerCheckedTreeViewer tsViewer;

    private Combo containerCombo;

    private List<TSContainerInfo> containerList;

    private Combo typeCombo;

    private Text destinationLocation;

    /**
     * @param pageName
     */
    protected MainExportPage(String pageName) {
        super(pageName);
        setTitle(pageName);
        setDescription(DESCRIPTION);

        this.selectedTSCInfo = null;
        this.selectedTSC = null;
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

        // Test Session Container Label
        createTestSessionContainerSelection(mainComposite);
        fillContainerCombo();

        // TestSession List
        createTestSessionViewer(mainComposite);

        createDestinationGroup(mainComposite);

        fillTypeCombo();

        handleErrors();
    }

    /**
     * 
     */
    private void fillTypeCombo() {
        for (Types types : Types.values()) {
            this.typeCombo.add(types.getName());
        }

        this.typeCombo.select(0);
    }

    /**
     * 
     */
    private void fillContainerCombo() {
        TSContainerManager containerManager = CodeCoverPlugin.getDefault()
                .getTSContainerManager();
        ActiveTSContainerInfo activeTSCInfo = containerManager
                .getActiveTSContainer();
        this.containerList = containerManager.getTestSessionContainers();
        int selectionIndex = 0;

        for (TSContainerInfo tscInfo : this.containerList) {
            this.containerCombo.add(tscInfo.getName());
            if (activeTSCInfo != null && activeTSCInfo.equals(tscInfo)) {
                selectionIndex = this.containerList.indexOf(tscInfo);
            }
        }

        if (!this.containerList.isEmpty()) {
            this.containerCombo.select(selectionIndex);
        }
    }

    private TSContainerInfo getSelectedTestSessionContainer() {
        int index = this.containerCombo.getSelectionIndex();

        if (index < 0 || index >= this.containerList.size()) {
            return null;
        }

        return this.containerList.get(index);
    }

    /**
     * Gets the selected type
     * 
     * @return the {@link Types}
     */
    Types getSelectedType() {
        String itemText = this.typeCombo.getItem(this.typeCombo
                .getSelectionIndex());

        return Types.getTypeFromName(itemText);
    }

    /**
     * Gets all {@link TestSession}s which contain at least one
     * {@link TestCase} which is selected.
     * 
     * @return  all {@link TestSession}s which contain at least one
     *          {@link TestCase} which is selected
     */
    public List<TestSession> getSelectedTestSessions() {
        List<TestSession> sessions = new Vector<TestSession>();
        TestSession curTestSession;
        
        for (TestCase testCase : this.getSelectedTestCases()) {
            curTestSession = testCase.getTestSession();
            if(!sessions.contains(curTestSession)) {
                sessions.add(curTestSession);
            }
        }

        return sessions;
    }
    
    /**
     * Gets the {@link List} of selected {@link TestCase}s
     * 
     * @return the {@link List}
     */
    public List<TestCase> getSelectedTestCases() {
        List<TestCase> testCases = new Vector<TestCase>();

        for (Object object : this.tsViewer.getCheckedElements()) {
            if (object instanceof TestCase) {
                testCases.add((TestCase) object);
            }
        }

        return testCases;
    }

    /**
     * Gets the selected {@link File}, to which is to be saved
     * 
     * @return the {@link File}
     */
    public File getDestinationLocation() {
        return new File(this.destinationLocation.getText());
    }

    /**
     * @param mainComposite
     */
    private void createDestinationGroup(Composite mainComposite) {
        Group group = new Group(mainComposite, SWT.NONE);
        group.setText(LABEL_DESTINATION_GROUP);

        GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = 3;
        gridLayout.makeColumnsEqualWidth = false;
        group.setLayout(gridLayout);
        group.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        Label typeLabel = new Label(group, SWT.NONE);
        typeLabel.setText(LABEL_TYPE_COMBO);

        this.typeCombo = new Combo(group, SWT.DROP_DOWN | SWT.SINGLE
                | SWT.READ_ONLY);
        this.typeCombo.addSelectionListener(new SelectionAdapter() {
            /**
             * (non-Javadoc)
             * 
             * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                onTypeComboSelectionChanged(e);
            }
        });
        GridData data = new GridData(GridData.FILL_HORIZONTAL
                | GridData.GRAB_HORIZONTAL);
        data.horizontalSpan = 2;
        this.typeCombo.setLayoutData(data);

        Label destinationLabel = new Label(group, SWT.NONE);
        destinationLabel.setText(LABEL_DESTINATION);

        this.destinationLocation = new Text(group, SWT.BORDER);
        this.destinationLocation.addModifyListener(new ModifyListener() {
            /**
             * (non-Javadoc)
             * 
             * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
             */
            @Override
			public void modifyText(ModifyEvent e) {
                onDestinationLocationTextChanged(e);
            }
        });
        this.destinationLocation.clearSelection();
        this.destinationLocation.setLayoutData(new GridData(
                GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL));

        Button button = new Button(group, SWT.PUSH);
        button.setText(LABEL_BROWSE);
        button.addSelectionListener(new SelectionAdapter() {
            /**
             * (non-Javadoc)
             * 
             * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                onBrowseButtonPushed(e);
            }
        });
    }

    /**
     * @param mainComposite
     */
    private void createTestSessionContainerSelection(Composite mainComposite) {
        Composite composite = new Composite(mainComposite, SWT.NONE);

        GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        layout.makeColumnsEqualWidth = false;
        layout.marginWidth = 0;
        composite.setLayout(layout);
        composite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        Label labelTestSessionContainer = new Label(composite, SWT.NONE);
        labelTestSessionContainer.setText(LABEL_TSC);

        this.containerCombo = new Combo(composite, SWT.DROP_DOWN | SWT.SINGLE
                | SWT.READ_ONLY);
        this.containerCombo.setLayoutData(new GridData(GridData.FILL_HORIZONTAL
                | GridData.GRAB_HORIZONTAL));
        this.containerCombo.addSelectionListener(new SelectionAdapter() {
            /**
             * (non-Javadoc)
             * 
             * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                onContainerComboSelectionChanged(e);
            }
        });
    }

    /**
     * @param e
     */
    protected void onDeselectAllSelected(SelectionEvent e) {
        this.tsViewer.setGrayedElements(new Object[0]);
        this.tsViewer.setAllChecked(false);
        handleErrors();
    }

    /**
     * @param e
     */
    protected void onSelectAllSelected(SelectionEvent e) {
        this.tsViewer.setGrayedElements(new Object[0]);
        this.tsViewer.setAllChecked(true);
        handleErrors();
    }

    /**
     * @param e
     */
    protected void onRefreshSelected(SelectionEvent e) {
        refreshTestSessionViewer();
        handleErrors();
    }

    /**
     * @param e
     */
    protected void onContainerComboSelectionChanged(SelectionEvent e) {
        refreshTestSessionViewer();
        handleErrors();
    }

    /**
     * @param e
     */
    protected void onBrowseButtonPushed(SelectionEvent e) {
        FileDialog fileDialog = new FileDialog(getContainer().getShell(),
                SWT.SAVE);
        fileDialog.setText(LABEL_EXPORT_TO_FILE_TITLE);
        fileDialog.setFilterExtensions(new String[] {"*.xml", "*.*"}); //$NON-NLS-1$//$NON-NLS-2$

        String location = fileDialog.open();

        if (location != null) {
            this.destinationLocation.setText(location);
        }

        handleErrors();
    }

    /**
     * @param e
     */
    protected void onTypeComboSelectionChanged(SelectionEvent e) {
        handleErrors();
    }

    /**
     * @param e
     */
    protected void onTestSessionViewerSelectionChanged(SelectionChangedEvent e){
        handleErrors();
    }

    /**
     * @param e
     */
    protected void onDestinationLocationTextChanged(ModifyEvent e) {
        handleErrors();
    }

    private String checkFile() {
        String error = null;
        String filePath = this.destinationLocation.getText();
        if (filePath.length() == 0) {
            error = FILE_ERROR_NONE_SPECIFIED;
        }
        return error;
    }

    /**
     * @param mainComposite
     */
    private void createTestSessionViewer(Composite mainComposite) {
        ActiveTSContainerInfo activeTSCInfo = CodeCoverPlugin.getDefault()
                .getTSContainerManager().getActiveTSContainer();
        // Test Session Label
        Label labelTestSession = new Label(mainComposite, SWT.NONE);
        labelTestSession.setText(LABEL_AVAILABLE_TEST_SESSIONS);

        Composite listComposite = new Composite(mainComposite, SWT.NONE);
        GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        layout.marginWidth = 0;
        layout.makeColumnsEqualWidth = false;
        listComposite.setLayout(layout);

        listComposite.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.GRAB_VERTICAL | GridData.FILL_BOTH));

        //this.testSessionList = new CheckboxTreeViewer(listComposite, SWT.BORDER);
        //this.testSessionList.getControl().setLayoutData(listData);
        this.tsViewer
                = TestSessionsViewerFactory.newContainerCheckedTreeViewer(
                        listComposite, false,
                        new Runnable() { @Override
						public void run() {} });
        GridData listData = new GridData(GridData.GRAB_HORIZONTAL
                | GridData.GRAB_VERTICAL | GridData.FILL_BOTH);
        this.tsViewer.getTree().getParent().setLayoutData(listData);
        this.tsViewer
                .addSelectionChangedListener(new ISelectionChangedListener() {
                    /**
                     * (non-Javadoc)
                     * 
                     * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
                     */
                    @Override
					public void selectionChanged(SelectionChangedEvent event) {
                        onTestSessionViewerSelectionChanged(event);
                    }
                });
        createSelectionButtons(listComposite);
        
        this.tsViewer.setInput(this.getTestSessionContainer());
        // apply selection of active test cases of test sessions view to viewer
        if(activeTSCInfo != null) {
            Object[] sel = activeTSCInfo.getActiveTestCases().toArray();
            // reveal preselected elements
            for(Object o : sel) {
                if(o instanceof TestCase) {
                    this.tsViewer.reveal(o);
                }
            }
            // check preselected elements
            this.tsViewer.setCheckedElements(sel);
        }
    }

    /**
     * Create the selection buttons in the listComposite.
     * 
     * @param listComposite
     */
    private void createSelectionButtons(Composite listComposite) {
        Composite buttonsComposite = new Composite(listComposite, SWT.NONE);
        GridLayout layout = new GridLayout();
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        buttonsComposite.setLayout(layout);

        buttonsComposite.setLayoutData(new GridData(
                GridData.VERTICAL_ALIGN_BEGINNING));

        Button selectAll = new Button(buttonsComposite, SWT.PUSH);
        selectAll.setText(LABEL_SELECT_ALL);
        selectAll.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onSelectAllSelected(e);
            }
        });
        setButtonLayoutData(selectAll);

        Button deselectAll = new Button(buttonsComposite, SWT.PUSH);
        deselectAll.setText(LABEL_DESELECT_ALL);
        deselectAll.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onDeselectAllSelected(e);
            }

        });
        setButtonLayoutData(deselectAll);

        Button refresh = new Button(buttonsComposite, SWT.PUSH);
        refresh.setText(LABEL_REFRESH);
        refresh.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onRefreshSelected(e);
            }
        });
        setButtonLayoutData(refresh);
    }

    private TestSessionContainer getTestSessionContainer() {
        TSContainerInfo selectedTSCInfo = getSelectedTestSessionContainer();
        ActiveTSContainerInfo activeTSCInfo;

        if (selectedTSCInfo == null) {
            this.selectedTSCInfo = null;
            this.selectedTSC = null;
        } else if (!selectedTSCInfo.equals(this.selectedTSCInfo)) {
            this.selectedTSCInfo = selectedTSCInfo;
            activeTSCInfo = CodeCoverPlugin.getDefault()
                    .getTSContainerManager().getActiveTSContainer();
            if (selectedTSCInfo.equals(activeTSCInfo)) {
                this.selectedTSC = activeTSCInfo.getTestSessionContainer();
            } else {
                try {
                    this.selectedTSC = TSContainerManager.load(selectedTSCInfo,
                            new NullProgressMonitor());
                } catch(FileLoadException e) {
                    MessageDialog.openError(this.getShell(),
                            DIALOG_ERROR_LOAD_TSC_TITLE,
                            DIALOG_ERROR_LOAD_TSC_MSG);
                }
            }
        }

        return this.selectedTSC;
    }

    private void refreshTestSessionViewer() {
        this.tsViewer.setInput(this.getTestSessionContainer());
    }

    private void handleErrors() {
        String fileError = checkFile();
        if (getSelectedTestSessionContainer() == null) {
            setErrorMessage(ERROR_NO_TEST_SESSION_CONTAINER_SELECTED);
            setPageComplete(false);
        } else if (this.getSelectedTestCases().size() == 0) {
            setErrorMessage(ERROR_NO_TEST_CASES_SELECTED);
            setPageComplete(false);
        } else if (fileError != null) {
            setErrorMessage(fileError);
            setPageComplete(false);
        } else {
            setErrorMessage(null);
            setPageComplete(true);
            if(this.getSelectedType() == Types.TEST_SESSION_CONTAINER
                    && this.containsNotFullySelectedTestSession()) {
                setMessage(WARNING_INCOMPLETE_TEST_SESSION_SELECTION,
                        IMessageProvider.WARNING);
            } else {
                this.setMessage(DESCRIPTION, IMessageProvider.NONE);
            }
        }
    }

    /**
     * Checks if there are test sessions in the viewer where some but not all
     * test cases are selected.
     * 
     * @return  <code>true</code> if there are test sessions in the viewer where
     *          some but not all test cases are selected, <code>false</code>
     *          otherwise
     */
    private boolean containsNotFullySelectedTestSession() {
        for (Object grayedObj : this.tsViewer.getGrayedElements()) {
            if (grayedObj instanceof TestSession) {
                return true;
            }
        }
        return false;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.WizardPage#canFlipToNextPage()
     */
    @Override
    public boolean canFlipToNextPage() {
        return super.canFlipToNextPage()
                && getSelectedType().equals(Types.REPORT);
    }

    /**
     * An enum holding {@link Types#REPORT} and
     * {@link Types#TEST_SESSION_CONTAINER}, which represent the different
     * types of report.
     * 
     * @author Markus Wittlinger
     * @version 1.0 ($Id: MainExportPage.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    protected enum Types {
        /**
         * Constant for the export of a {@link TestSessionContainer}
         */
        TEST_SESSION_CONTAINER(LABEL_ENUM_TSC),
        /**
         * Constant for the export of a {@link Report}
         */
        REPORT(LABEL_ENUM_REPORT);

        private final String name;

        Types(String name) {
            this.name = name;
        }

        /**
         * Gets the name of the enum constant
         * 
         * @return the name
         */
        public String getName() {
            return this.name;
        }

        /**
         * Gets the {@link Types Type} with the given name
         * 
         * @param name
         *            the given name
         * @return the type with the given name
         */
        public static Types getTypeFromName(String name) {
            for (Types types : values()) {
                if (types.getName().equals(name)) {
                    return types;
                }
            }
            return null;
        }
    }
}
