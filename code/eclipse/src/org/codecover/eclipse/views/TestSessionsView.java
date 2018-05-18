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

package org.codecover.eclipse.views;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.importWizards.CoverageLogImportWizard;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerManagerListener;
import org.codecover.eclipse.tscmanager.exceptions.CancelException;
import org.codecover.eclipse.views.controls.DeleteTSCsSelectDialog;
import org.codecover.eclipse.views.controls.DeleteTestElementsConfirmDialog;
import org.codecover.eclipse.views.controls.DeleteTestElementsRunnable;
import org.codecover.eclipse.views.controls.DeleteTestElementsSelectDialog;
import org.codecover.eclipse.views.controls.MergeWizard;
import org.codecover.eclipse.views.controls.SaveActiveTSContainerAction;
import org.codecover.eclipse.views.controls.TestElementPropertiesDialog;
import org.codecover.eclipse.views.controls.TestSessionsViewerFactory;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.utils.ChangeType;
import org.codecover.model.utils.LogLevel;
import org.codecover.model.utils.Logger;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.FileTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

/**
 * The Test Sessions view shows the content of the active test session
 * container and allows the user to select the active test session container
 * from the list of known test session containers. Moreover the following
 * actions can be executed:
 * <ul>
 * <li>Deletion of test session containers</li>
 * <li>Deletion of test elements</li>
 * <li>Merging of test elements</li>
 * <li>Changing the name and comment of test elements</li>
 * </ul>
 *
 * @author Robert Hanussek
 * @version 1.0 ($Id: TestSessionsView.java 54 2009-07-20 11:25:13Z ahija $)
 */
public class TestSessionsView extends ViewPart
        implements TSContainerManagerListener {

    private static final String LABEL_TEST_SESSION_CONTAINER = Messages.getString("TestSessionsView.LABEL_TEST_SESSION_CONTAINER"); //$NON-NLS-1$
    private static final String COMBO_ENTRY_NO_TEST_SESSION_CONTAINERS = Messages.getString("TestSessionsView.COMBO_ENTRY_NO_TEST_SESSION_CONTAINERS"); //$NON-NLS-1$
    private static final String COMBO_ENTRY_NO_TEST_SESSION_CONTAINER_ACTIVE = Messages.getString("TestSessionsView.COMBO_ENTRY_NO_TEST_SESSION_CONTAINER_ACTIVE"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_INCONSISTENCY_TITLE = Messages.getString("TestSessionsView.DIALOG_ERROR_INCONSISTENCY_TITLE"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_INCONSISTENCY_MSG = Messages.getString("TestSessionsView.DIALOG_ERROR_INCONSISTENCY_MSG"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_LOADING_TSC_TITLE = Messages.getString("TestSessionsView.DIALOG_ERROR_LOADING_TSC_TITLE"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_LOADING_TSC_MSG = Messages.getString("TestSessionsView.DIALOG_ERROR_LOADING_TSC_MSG"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_ACTIVATING_TSC_OUT_OF_MEM_TITLE = Messages.getString("TestSessionsView.DIALOG_ERROR_ACTIVATING_TSC_OUT_OF_MEM_TITLE"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_ACTIVATING_TSC_OUT_OF_MEM_MSG = Messages.getString("TestSessionsView.DIALOG_ERROR_ACTIVATING_TSC_OUT_OF_MEM_MSG"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_UNKNOWN_TSC_SELECTED_TITLE = Messages.getString("TestSessionsView.DIALOG_ERROR_UNKNOWN_TSC_SELECTED_TITLE"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_UNKNOWN_TSC_SELECTED_MSG = Messages.getString("TestSessionsView.DIALOG_ERROR_UNKNOWN_TSC_SELECTED_MSG"); //$NON-NLS-1$
    private static final String TSC_DELETE_ACTION_TEXT = Messages.getString("TestSessionsView.TSC_DELETE_ACTION_TEXT"); //$NON-NLS-1$
    private static final String TSC_DELETE_ACTION_TOOLTIP = Messages.getString("TestSessionsView.TSC_DELETE_ACTION_TOOLTIP"); //$NON-NLS-1$
    private static final String TSC_DELETE_MULTIPLE_ACTION_TEXT = Messages.getString("TestSessionsView.TSC_DELETE_MULTIPLE_ACTION_TEXT"); //$NON-NLS-1$
    private static final String TSC_DELETE_MULTIPLE_ACTION_TOOLTIP = Messages.getString("TestSessionsView.TSC_DELETE_MULTIPLE_ACTION_TOOLTIP"); //$NON-NLS-1$
    private static final String TSC_DELETE_DIALOG_TITLE = Messages.getString("TestSessionsView.TSC_DELETE_DIALOG_TITLE"); //$NON-NLS-1$
    private static final String TSC_DELETE_DIALOG_MSG = Messages.getString("TestSessionsView.TSC_DELETE_DIALOG_MSG"); //$NON-NLS-1$
    private static final String TSC_DELETE_DIALOG_ERROR_TITLE = Messages.getString("TestSessionsView.TSC_DELETE_DIALOG_ERROR_TITLE"); //$NON-NLS-1$
    private static final String TSC_DELETE_DIALOG_ERROR_MSG = Messages.getString("TestSessionsView.TSC_DELETE_DIALOG_ERROR_MSG"); //$NON-NLS-1$
    private static final String ELEMENTS_DELETE_ACTION_TEXT_CONTEXTMENU = Messages.getString("TestSessionsView.ELEMENTS_DELETE_ACTION_TEXT_CONTEXTMENU"); //$NON-NLS-1$
    private static final String ELEMENTS_DELETE_ACTION_TEXT_TOOLBAR = Messages.getString("TestSessionsView.ELEMENTS_DELETE_ACTION_TEXT_TOOLBAR"); //$NON-NLS-1$
    private static final String ELEMENTS_DELETE_ACTION_TOOLTIP = Messages.getString("TestSessionsView.ELEMENTS_DELETE_ACTION_TOOLTIP"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_ELEMENT_DELETE_TITLE = Messages.getString("TestSessionsView.DIALOG_ERROR_ELEMENT_DELETE_TITLE"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_ELEMENT_DELETE_MSG = Messages.getString("TestSessionsView.DIALOG_ERROR_ELEMENT_DELETE_MSG"); //$NON-NLS-1$
    private static final String MERGE_ACTION_TEXT = Messages.getString("TestSessionsView.MERGE_ACTION_TEXT"); //$NON-NLS-1$
    private static final String MERGE_ACTION_TOOLTIP = Messages.getString("TestSessionsView.MERGE_ACTION_TOOLTIP"); //$NON-NLS-1$
    private static final String SELECT_ALL_ACTION_TEXT = Messages.getString("TestSessionsView.SELECT_ALL_ACTION_TEXT"); //$NON-NLS-1$
    private static final String SELECT_ALL_ACTION_TOOLTIP = Messages.getString("TestSessionsView.SELECT_ALL_ACTION_TOOLTIP"); //$NON-NLS-1$
    private static final String ACTIVATE_ALL_ACTION_TEXT = Messages.getString("TestSessionsView.ACTIVATE_ALL_ACTION_TEXT"); //$NON-NLS-1$
    private static final String ACTIVATE_ALL_ACTION_TOOLTIP = Messages.getString("TestSessionsView.ACTIVATE_ALL_ACTION_TOOLTIP"); //$NON-NLS-1$
    private static final String DEACTIVATE_ALL_ACTION_TEXT = Messages.getString("TestSessionsView.DEACTIVATE_ALL_ACTION_TEXT"); //$NON-NLS-1$
    private static final String DEACTIVATE_ALL_ACTION_TOOLTIP  = Messages.getString("TestSessionsView.DEACTIVATE_ALL_ACTION_TOOLTIP"); //$NON-NLS-1$
    private static final String PROPERTIES_ACTION_TEXT = Messages.getString("TestSessionsView.PROPERTIES_ACTION_TEXT"); //$NON-NLS-1$


    /**
     * The <code>TSContainerInfo</code>-representations of the test session
     * containers which are listed in the combo box (and known to this view).
     */
    private List<TSContainerInfo> tscInfos;

    /**
     * The <code>TSContainerInfo</code>-representation of the test session
     * container which is currently visualized in the viewer.
     */
    private TSContainerInfo tscInfo;

    /**
     * The (active) <code>TestCase</code>s which are currently visualized in the
     * viewer as being active (by checked checkboxes).
     */
    private Set<TestCase> testCases;

    private CheckboxTreeViewer viewer;
    private Combo combo;

    /**
     * Saves the active test session container.
     */
    private Action saveTSC;
    /**
     * Deletes one or multiple test session containers.
     */
    private Action deleteTSC;
    /**
     * Importer for CoverageLogFiles.
     * @see CoverageLogImportWizard
     */
    private Action coverageImportToolBar;
    /**
     * Deletes test elements and is added to the toolbar.
     */
    private Action deleteToolBar;
    /**
     * Deletes test elements and is added to the context menu of the tree
     * viewer.
     */
    private Action deleteContextMenu;
    /**
     * Merges test elements.
     */
    private Action merge;
    /**
     * Context menu item of the tree viewer which selects all test elements.
     */
    private Action selectAll;
    /**
     * Context menu item of the tree viewer which activates all test cases.
     */
    private Action activateAll;
    /**
     * Context menu item of the tree viewer which deactivates all test cases.
     */
    private Action deactivateAll;
    /**
     * Context menu item of the tree viewer which allows the user to edit
     * the name and comment of a test element.
     */
    private Action propertiesAction;
    /**
     * Debug action which is added to the pulldown if the plugin's logger is in
     * debug mode.
     *
     * @see CodeCoverPlugin#getLogLevel()
     */
    private Action showActiveTestCases;

    /**
     * Saves the expanded elements of the viewer, which are only
     * <code>TestSession</code>s. Thus only the names of the expanded
     * <code>TestSession</code>s of a <code>TSContainerInfo</code> are saved.
     */
    private final Map<TSContainerInfo, Set<String>> expandedElements;

    private final Logger logger;

    /**
     * The constructor.
     */
    public TestSessionsView() {
        this.tscInfos = new LinkedList<TSContainerInfo>();
        this.testCases = new HashSet<TestCase>();
        this.expandedElements = new HashMap<TSContainerInfo, Set<String>>();
        this.logger = CodeCoverPlugin.getDefault().getLogger();
    }

    /*
     * methods to set and get the data which is visualized by this view
     */

    /**
     * Sets the input of the viewer to the given data.
     */
    private void setViewerInput(ActiveTSContainerInfo tscInfo) {
        // save expanded elements of old test session container
        saveExpandedElements(this.getVisTSCInfo());

        if(tscInfo != null) {
            this.tscInfo = tscInfo.getTSContainerInfo();
            this.saveTSC.setEnabled(!this.tscInfo.isSynchronized());
            // the given set is immutable thus we need to copy it
            this.testCases =new HashSet<TestCase>(tscInfo.getActiveTestCases());
            this.viewer.setInput(tscInfo.getTestSessionContainer());
        } else {
            this.saveTSC.setEnabled(false);
            this.tscInfo = null;
            this.testCases = new HashSet<TestCase>();
            this.viewer.setInput(null);
        }

        // check the checkboxes of the active test cases
        this.checkActiveTestCases();
        // set expanded elements of newly selected session container
        restoreExpandedElements(tscInfo);

        this.deleteTSC.setEnabled(tscInfo != null);
        this.merge.setEnabled(tscInfo != null);
        this.deleteContextMenu.setEnabled(tscInfo != null);
        this.deleteToolBar.setEnabled(tscInfo != null);
    }

    /**
     * Returns the <code>TSContainerInfo</code>-representations of the test
     * session containers which are currently listed in the combo box.
     *
     * @return  the <code>TSContainerInfo</code>-representations of the test
     *          session containers which are currently listed in the combo box.
     */
    private List<TSContainerInfo> getVisTSCInfos() {
        return this.tscInfos;
    }

    /**
     * Returns the <code>TestSessionContainer</code> which is currently
     * visualized in the viewer.
     *
     * @return  the <code>TestSessionContainer</code> which is currently
     *          visualized in the viewer.
     */
    private TestSessionContainer getVisTSC() {
        return (TestSessionContainer)this.viewer.getInput();
    }

    /**
     * Returns the <code>TSContainerInfo</code>-representation of the test
     * session container which is currently visualized in the viewer.
     *
     * @return  the <code>TSContainerInfo</code>-representation of the test
     *          session container which is currently visualized in the viewer.
     */
    private TSContainerInfo getVisTSCInfo() {
        return this.tscInfo;
    }

    /**
     * Returns the <code>TestCase</code>s which are currently visualized in the
     * viewer as being active (by checked checkboxes).
     *
     * @return  the <code>TestCase</code>s which are currently visualized in the
     *          viewer as being active (by checked checkboxes).
     */
    private Set<TestCase> getVisTestCases() {
        return this.testCases;
    }

    /*
     * methods to create and initialize the controls of the view
     */

    /**
     * This is a callback that will allow us to create the controls of the view
     * and initialize it.
     */
    @Override
    public void createPartControl(Composite parent) {
        // create label
        Label label = new Label(parent, SWT.NONE);
        label.setText(LABEL_TEST_SESSION_CONTAINER);

        // create combo box
        /*
         * create a drop-down-combo which is read only and which allows only
         * single items to be selected
         */
        this.combo = new Combo(parent, SWT.DROP_DOWN | SWT.SINGLE
                | SWT.READ_ONLY);

        // create viewer
        this.viewer = TestSessionsViewerFactory.newContainerCheckedTreeViewer(
                parent,
                false,
                new Runnable() {
                    @Override
					public void run() {
                        CodeCoverPlugin.getDefault().getTSContainerManager()
                                .removeListener(TestSessionsView.this);
                    }
                });
        // we allow the drag and drop of coverageLogFiles
        this.viewer.addDropSupport(DND.DROP_COPY | DND.DROP_DEFAULT,
                new Transfer[] {FileTransfer.getInstance()}, new CoverageLogDropListener());

        // create actions and add them to context menu and toolbars
        this.makeActions();
        this.fillActionMenus();
        this.hookDoubleClickAction();

        // layout
        parent.setLayout(new FormLayout());
        FormData formData = new FormData();

        formData = new FormData();
        formData.left = new FormAttachment(0, 5);
        formData.top = new FormAttachment(this.combo, 0, SWT.CENTER);
        label.setLayoutData(formData);

        formData = new FormData();
        formData.left = new FormAttachment(label, 5);
        formData.top = new FormAttachment(0, 5);
        this.combo.setLayoutData(formData);

        formData = new FormData();
        formData.left = new FormAttachment(0, 5);
        formData.right = new FormAttachment(100, -5);
        formData.top = new FormAttachment(this.combo, 5);
        formData.bottom = new FormAttachment(100, -5);
        this.viewer.getTree().getParent().setLayoutData(formData);

        // initialize combo box and viewer
        this.initControls();

        // register listeners on combo box and viewer
        this.combo.addSelectionListener(new ComboSelectionListener());
        this.viewer.addCheckStateListener(new ViewerCheckboxListener());
        // register listener on TSContainerManager
        CodeCoverPlugin.getDefault().getTSContainerManager().addListener(this);
    }

    /*
     * methods to react on change of the list of known test session containers
     * or on change of the active test session container
     */

    /**
     * Fills the combo box with an entry for every test session container,
     * sets the input of the viewer to the activated test session container and
     * disable the actions if there is no active test session container.
     */
    private void initControls() {
        ActiveTSContainerInfo activeTSCInfo = CodeCoverPlugin.getDefault()
                .getTSContainerManager().getActiveTSContainer();
        this.tscInfos = new LinkedList<TSContainerInfo>(
                CodeCoverPlugin.getDefault()
                .getTSContainerManager().getTestSessionContainers());

        this.combo.removeAll(); // just to be safe
        for(TSContainerInfo tscInfo : this.getVisTSCInfos()) {
            this.combo.add(tscInfo.getName());
        }
        // adjust size of combo box
        this.combo.pack(true);

        // set the active TSC and synchronize viewer with selection of combo box
        if(!this.getVisTSCInfos().isEmpty()) {
            if(activeTSCInfo != null) {
                this.combo.select(this.getVisTSCInfos().indexOf(activeTSCInfo));
            } else {
                this.noTSCactive();
            }
            this.setViewerInput(activeTSCInfo);
        } else /* (if combo box is empty) */{
            this.noTSCinCombo();
            this.setViewerInput(null);
        }
    }

    private class CoverageLogDropListener extends DropTargetAdapter
    {
        @Override
        public void dragEnter(DropTargetEvent event) {
            // we have to look at the event and modify it a litte in order to allow file drop
            if (event.detail == DND.DROP_DEFAULT) {
                if ((event.operations & DND.DROP_COPY) != 0) {
                    event.detail = DND.DROP_COPY;
                } else {
                    event.detail = DND.DROP_NONE;
                }
            }
            for (int i = 0; i < event.dataTypes.length; i++) {
                if (FileTransfer.getInstance().isSupportedType(event.dataTypes[i])) {
                    event.currentDataType = event.dataTypes[i];
                    // files should only be copied
                    if (event.detail != DND.DROP_COPY) {
                        event.detail = DND.DROP_NONE;
                    }
                    break;
                }
            }
        }

        @Override
        public void dragOperationChanged(DropTargetEvent event) {
            if (event.detail == DND.DROP_DEFAULT) {
                if ((event.operations & DND.DROP_COPY) != 0) {
                    event.detail = DND.DROP_COPY;
                } else {
                    event.detail = DND.DROP_NONE;
                }
            }
            if (FileTransfer.getInstance().isSupportedType(event.currentDataType)) {
                if (event.detail != DND.DROP_COPY) {
                    event.detail = DND.DROP_NONE;
                }
            }
        }

        @Override
        public void drop(DropTargetEvent event)
        {
            if (TestSessionsView.this.getVisTSC() == null) {
                // no active TSC
                return;
            }

            if (FileTransfer.getInstance().isSupportedType(event.currentDataType)) {
                String[] files = (String[]) event.data;
                if (files == null || files.length == 0)
                {
                    return;
                }

                // for every file, we start a Wizard
                for (String thisFile : files)
                {
                    CoverageLogImportWizard coverageLogImportWizard = new CoverageLogImportWizard();
                    coverageLogImportWizard.init(PlatformUI.getWorkbench(), null);

                    // Instantiates the wizard container with the wizard and opens it
                    WizardDialog wizardDialog = new WizardDialog(TestSessionsView.this.getSite().getShell(),
                            coverageLogImportWizard);
                    wizardDialog.create();

                    // we now select a specific file
                    coverageLogImportWizard.selectCoverageLogFile(thisFile);

                    if (wizardDialog.open() != Window.OK)
                    {
                        // this import was cancelled -> cancel all imports
                        break;
                    }
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerAdded(org.codecover.eclipse.tscmanager.TSContainerManager.TSContainerInfo,
     *      int)
     */
    @Override
	public void testSessionContainerAdded(  final TSContainerInfo tscInfo,
                                            final int index) {
        TestSessionsView.runInUI(new Runnable() {
            @Override
			public void run() {
                if(!TestSessionsView.this.combo.isDisposed()) {
                    // first remove no-TSCs-message
                    if(TestSessionsView.this.getVisTSCInfos().isEmpty()) {
                        TestSessionsView.this.combo.removeAll();
                    }
                    TestSessionsView.this.combo.add(tscInfo.getName(), index);
                    // add no-TSC-active-message to the end of the combo
                    if(TestSessionsView.this.getVisTSCInfos().isEmpty()) {
                        if(TestSessionsView.this.getVisTSCInfo() == null) {
                            noTSCactive();
                        } else {
                            TestSessionsView.this.showErrorInconsistency();
                        }
                    }
                    TestSessionsView.this.getVisTSCInfos().add(index, tscInfo);
                    TestSessionsView.this.combo.pack(true);
                }
            }
        }, false);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerRemoved(org.codecover.eclipse.tscmanager.TSContainerManager.TSContainerInfo)
     */
    @Override
	public void testSessionContainerRemoved(final TSContainerInfo tscInfo) {
        TestSessionsView.runInUI(new Runnable() {
            @Override
			public void run() {
                int index;
                if(!TestSessionsView.this.combo.isDisposed()) {
                    index = TestSessionsView.this
                                            .getVisTSCInfos().indexOf(tscInfo);
                    TestSessionsView.this.getVisTSCInfos().remove(tscInfo);
                    TestSessionsView.this.removeExpandedElements(tscInfo);
                    if(index != -1) {
                        TestSessionsView.this.combo.remove(index);
                        TestSessionsView.this.combo.pack(true);
                    } else {
                        TestSessionsView.this.showErrorInconsistency();
                    }
                    if(TestSessionsView.this.getVisTSCInfos().isEmpty()) {
                        TestSessionsView.this.noTSCinCombo();
                    }
                }
            }
        }, false);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerActivated(ActiveTSContainerInfo)
     */
    @Override
	public void testSessionContainerActivated(
            final ActiveTSContainerInfo tscInfo) {
        TestSessionsView.runInUI(new Runnable() {
            @Override
			public void run() {
                if(!TestSessionsView.this.combo.isDisposed()
                        && !TestSessionsView.this.viewer.getControl()
                                .isDisposed()) {
                    if(tscInfo != null) {
                        int index = TestSessionsView.this.getVisTSCInfos()
                                .indexOf(tscInfo.getTSContainerInfo());
                        if(index != -1) {
                            TestSessionsView.this.combo.select(index);
                            /*
                             * if previously there was no TSC active, the
                             * according message must be removed from the combo
                             */
                            if(TestSessionsView.this.getVisTSCInfo() == null) {
                                TestSessionsView.this.combo.remove(
                                        TestSessionsView.this
                                                .combo.getItemCount()-1);
                                TestSessionsView.this.combo.pack(true);
                            }
                        } else {
                            TestSessionsView.this.showErrorInconsistency();
                        }
                    } else if(!TestSessionsView.this
                            .getVisTSCInfos().isEmpty()){
                        TestSessionsView.this.noTSCactive();
                    }

                    TestSessionsView.this.setViewerInput(tscInfo);
                }
            }
        /*
         * the sync parameter must be true or else the enable-state of the save
         * action can be set wrong, because the view will ignore
         * synchronizedStateChanged-events until the TSContainerInfo returned by
         * this.getVisTSCInfo is set correctly
         */
        }, true);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testCaseChanged(ActiveTSContainerInfo, ChangeType, TestCase)
     */
    @Override
	public void testCaseChanged(    final ActiveTSContainerInfo tscInfo,
                                    final ChangeType changeType,
                                    final TestCase testCase) {
        TestSessionsView.runInUI(new Runnable() {
            @Override
			public void run() {
                final String[] updateProps = {
                        TestSessionsViewerFactory.UPDATE_PROPERTY_NAME,
                        TestSessionsViewerFactory.UPDATE_PROPERTY_COMMENT
                        /*
                         * date is omitted here, since the model doesn't allow
                         * changes of the date of test elements
                         */};
                if(!TestSessionsView.this.viewer.getControl().isDisposed()) {
                    if(tscInfo.getTestSessionContainer()
                            == TestSessionsView.this.getVisTSC()) {
                        if(changeType == ChangeType.CHANGE) {
                            TestSessionsView.this.viewer.update(testCase,
                                            updateProps);
                        } else if(changeType == ChangeType.ADD) {
                            TestSessionsView.this.viewer.refresh(
                                    testCase.getTestSession(), false);
                            TestSessionsView.this.viewer.reveal(testCase);
                        } else if(changeType == ChangeType.REMOVE) {
                            if(TestSessionsView.this.getVisTestCases()
                                    .remove(testCase)) {
                                TestSessionsView.this.viewer.setCheckedElements(
                                        TestSessionsView.this.getVisTestCases()
                                                .toArray());
                            }
                            TestSessionsView.this.viewer.refresh(
                                    testCase.getTestSession(), false);
                        }
                    }
                }
            }
        },
        /*
         * the sync parameter must be true or else the reveal method can cause
         * trouble if the test case is already deleted, when reveal is called
         */
        true);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionChanged(ActiveTSContainerInfo, ChangeType, TestSession)
     */
    @Override
	public void testSessionChanged( final ActiveTSContainerInfo tscInfo,
                                    final ChangeType changeType,
                                    final TestSession testSession) {
        TestSessionsView.runInUI(new Runnable() {
            @Override
			public void run() {
                boolean testCaseRemoved;
                boolean curTestCaseRemoved;
                final String[] updateProps = {
                        TestSessionsViewerFactory.UPDATE_PROPERTY_NAME,
                        TestSessionsViewerFactory.UPDATE_PROPERTY_COMMENT
                        /*
                         * date is omitted here, since the model doesn't allow
                         * changes of the date of test elements
                         */};
                if(!TestSessionsView.this.viewer.getControl().isDisposed()
                        && tscInfo.getTestSessionContainer()
                                == TestSessionsView.this.getVisTSC()) {
                    if(changeType == ChangeType.CHANGE) {
                        TestSessionsView.this.viewer.update(testSession,
                                updateProps);
                    } else if(changeType == ChangeType.ADD) {
                        TestSessionsView.this.viewer.refresh(false);
                    } else if(changeType == ChangeType.REMOVE) {
                        testCaseRemoved = false;
                        for(TestCase testCase : testSession.getTestCases()) {
                            curTestCaseRemoved = TestSessionsView.this
                                    .getVisTestCases().remove(testCase);
                            testCaseRemoved = testCaseRemoved
                                    || curTestCaseRemoved;
                        }
                        if(testCaseRemoved) {
                            TestSessionsView.this.viewer.setCheckedElements(
                                    TestSessionsView.this.getVisTestCases()
                                            .toArray());
                        }
                        TestSessionsView.this.viewer.refresh(false);
                    }
                }
            }
        }, false);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerChanged(ChangeType, ActiveTSContainerInfo)
     */
    @Override
	public void testSessionContainerChanged(ChangeType changeType,
            ActiveTSContainerInfo tscInfo) {}

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testCasesActivated(ActiveTSContainerInfo)
     */
    @Override
	public void testCasesActivated(final ActiveTSContainerInfo tscInfo) {
        TestSessionsView.runInUI(new Runnable() {
            @Override
			public void run() {
                if(!TestSessionsView.this.viewer.getControl().isDisposed()
                        && tscInfo.getTestSessionContainer()
                                == TestSessionsView.this.getVisTSC()) {
                    if(!TestSessionsView.this.getVisTestCases()
                            .equals(tscInfo.getActiveTestCases())) {
                        TestSessionsView.this.getVisTestCases().clear();
                        TestSessionsView.this.getVisTestCases()
                                .addAll(tscInfo.getActiveTestCases());
                        TestSessionsView.this.viewer
                                .setCheckedElements(
                                        tscInfo.getActiveTestCases().toArray());
                    }
                }
            }
        }, false);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#synchronizedStateChanged(TSContainerInfo, boolean)
     */
    @Override
	public void synchronizedStateChanged(final TSContainerInfo tscInfo,
            final boolean isSynchronized) {
        runInUI(new Runnable() {
            @Override
			public void run() {
                TSContainerInfo visTSCInfo
                        = TestSessionsView.this.getVisTSCInfo();
                if(visTSCInfo != null
                        && visTSCInfo.equals(tscInfo)) {
                    TestSessionsView.this.saveTSC.setEnabled(!isSynchronized);
                }
            }
        }, false);
    }

    private static void runInUI(Runnable runnable, boolean sync) {
        if(Display.getCurrent() != null) {
            runnable.run();
        } else {
            if(sync) {
                Display.getDefault().syncExec(runnable);
            } else {
                Display.getDefault().asyncExec(runnable);
            }
        }
    }

    private final class ViewerCheckboxListener implements ICheckStateListener {
        /**
         * Handles changes in states of checkboxes (checked, unchecked) of the
         * viewer.
         * @param event the {@link CheckStateChangedEvent}
         */
        @Override
		public void checkStateChanged(CheckStateChangedEvent event) {
            if(event.getElement() instanceof TestSession) {
                TestSession testSession = (TestSession) event.getElement();
                if(event.getChecked() == true) {
                    TestSessionsView.this.getVisTestCases()
                            .addAll(testSession.getTestCases());
                } else if(event.getChecked() == false) {
                    TestSessionsView.this.getVisTestCases()
                            .removeAll(testSession.getTestCases());
                }
            } else if(event.getElement() instanceof TestCase) {
                TestCase testCase = (TestCase) event.getElement();
                if(event.getChecked() == true) {
                    TestSessionsView.this.getVisTestCases().add(testCase);
                } else {
                    TestSessionsView.this.getVisTestCases().remove(testCase);
                }
            }
            CodeCoverPlugin.getDefault().getTSContainerManager()
                   .setActiveTestCases(TestSessionsView.this.getVisTestCases());
        }
    }

    /**
     * Sets the state of the checkboxes of the active <code>TestCase</code>s
     * of the active <code>TestSessionContainer</code> to checked.
     */
    private void checkActiveTestCases() {
        if(this.getVisTSC() == null) {
            return;
        }
        // check test cases
        for(TestCase testCase : this.getVisTestCases()) {
            this.viewer.setChecked(testCase, true);
        }
        // check test sessions
        for(TestSession testSession : this.getVisTSC().getTestSessions()) {
            if(isCheckboxStateOfSubtree(true, testSession)) {
                this.viewer.setChecked(testSession, true);
            } else if(isCheckboxStateOfSubtree(false, testSession)) {
                this.viewer.setChecked(testSession, false);
            } else {
                this.viewer.setGrayChecked(testSession, true);
            }
        }
    }

    /**
     * Checks if the states of the checkboxes of all <code>TestCase</code>s
     * of a given <code>TestSession</code> match a given state (checked or
     * unchecked).
     *
     * @param state
     *            the state to check the checkboxes for
     * @param testSession
     *            the <code>TestSession</code> which <code>TestCase</code>s
     *            are checked for the given state of their checkboxes
     * @return <code>true</code>, if the states of the checkboxes of all
     *         <code>TestCase</code>s of the given <code>TestSession</code>
     *         match the given state (checked or unchecked).
     */
    private boolean isCheckboxStateOfSubtree(boolean state,
            TestSession testSession) {
        if(state == true && testSession.getTestCases().isEmpty()) {
            return false;
        }
        for(TestCase testCase : testSession.getTestCases()) {
            if(this.viewer.getChecked(testCase) != state) {
                return false;
            }
        }
        return true;
    }

    /*
     * methods and class to handle the combo box (which lists the known test
     * session containers)
     */

    /**
     * Handles selection changes of the combo box.
     */
    private final class ComboSelectionListener extends SelectionAdapter {
        /* (non-Javadoc)
         * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
         */
        @Override
        public void widgetSelected(SelectionEvent event) {
            final TSContainerInfo selectedTSCInfo
                    = TestSessionsView.this.selectedTSContainer();
            if(selectedTSCInfo != null) {
                final IRunnableWithProgress activateRunnable
                        = new IRunnableWithProgress() {
                    @Override
					public void run(IProgressMonitor monitor)
                    throws InvocationTargetException, InterruptedException {
                        try {
                            CodeCoverPlugin.getDefault().getTSContainerManager()
                                    .setActiveTSContainer(selectedTSCInfo, null,
                                                monitor);
                        } catch(FileLoadException e) {
                            throw new InvocationTargetException(e);
                        } catch(OutOfMemoryError e) {
                            TestSessionsView.this.logger.error(
                                    "Out of memory while" +        //$NON-NLS-1$
                                    " activating test session" +   //$NON-NLS-1$
                                    " container: "                 //$NON-NLS-1$
                                    + selectedTSCInfo.getPath().toString(),
                                    new InvocationTargetException(e));
                            throw new InvocationTargetException(e);
                        } catch(InvocationTargetException e) {
                            /*
                             * ignore because it can't be thrown if no runnable
                             * was passed
                             */
                        } catch(CancelException e) {
                            TestSessionsView.this.logger.warning(
                                    "Canceled activation " +       //$NON-NLS-1$
                                    " of test session container",  //$NON-NLS-1$
                                    e);
                            throw new InvocationTargetException(e);
                        }
                    }
                };
                try {
                    CodeCoverPlugin.getDefault().getWorkbench()
                            .getProgressService()
                            .busyCursorWhile(activateRunnable);
                } catch(InvocationTargetException e) {
                    if(TestSessionsView.this.getVisTSCInfo() != null) {
                        int index = TestSessionsView.this
                                .getVisTSCInfos().indexOf(
                                        TestSessionsView.this
                                                .getVisTSCInfo());
                        if(index != -1) {
                            TestSessionsView.this.combo.select(index);
                        } else {
                            TestSessionsView.this.showErrorInconsistency();
                        }
                    }
                    /*
                     * if the visualized TSC is null, there is an entry
                     * at the end of combo box which says that no TSC is
                     * activated
                     */
                    else {
                        TestSessionsView.this.combo.select(
                                TestSessionsView.this.combo
                                        .getItemCount()-1);
                    }
                    if(e.getCause() instanceof FileLoadException) {
                        MessageDialog.openError(
                                TestSessionsView.this.getSite().getShell(),
                                DIALOG_ERROR_LOADING_TSC_TITLE,
                                DIALOG_ERROR_LOADING_TSC_MSG);
                    } else if(e.getCause() instanceof OutOfMemoryError) {
                        MessageDialog.openError(
                                TestSessionsView.this.getSite().getShell(),
                                DIALOG_ERROR_ACTIVATING_TSC_OUT_OF_MEM_TITLE,
                                DIALOG_ERROR_ACTIVATING_TSC_OUT_OF_MEM_MSG);
                    } else if(!(e.getCause() instanceof CancelException)) {
                        TestSessionsView.this.logger.error(
                                "Unknown error while activating" + //$NON-NLS-1$
                                " test session container",         //$NON-NLS-1$
                                e);
                    }
                } catch(InterruptedException e) {
                    TestSessionsView.this.logger.error(
                            "Unknown error while activating" +     //$NON-NLS-1$
                            " test session container",             //$NON-NLS-1$
                            e);
                }
            } else {
                MessageDialog.openError(
                        TestSessionsView.this.getSite().getShell(),
                        DIALOG_ERROR_UNKNOWN_TSC_SELECTED_TITLE,
                        DIALOG_ERROR_UNKNOWN_TSC_SELECTED_MSG);
                TestSessionsView.this.logger.fatal(
                        "Unknown test session container selected" +//$NON-NLS-1$
                        " in Test Sessions view");                 //$NON-NLS-1$
            }
        }
    }

    /**
     * Returns the <code>TSContainerInfo</code> which is selected in the
     * combo box.
     */
    private TSContainerInfo selectedTSContainer() {
        int selectedIndex = this.combo.getSelectionIndex();
        if(!this.getVisTSCInfos().isEmpty()) {
            if(selectedIndex == this.getVisTSCInfos().size()) {
                return null;
            } else {
                return this.getVisTSCInfos().get(selectedIndex);
            }
        } else {
            return null;
        }
    }

    /**
     * Makes the combo visualize that no test session containers are loaded.
     */
    private void noTSCinCombo() {
        this.combo.removeAll();
        this.combo.add(COMBO_ENTRY_NO_TEST_SESSION_CONTAINERS);
        this.combo.pack(true);
        this.combo.select(0);
    }

    /**
     * Makes the combo visualize that no test session container is active.
     */
    private void noTSCactive() {
        TestSessionsView.this.combo.add(
                COMBO_ENTRY_NO_TEST_SESSION_CONTAINER_ACTIVE);
        TestSessionsView.this.combo.select(
                TestSessionsView.this.combo.getItemCount()-1);
        this.combo.pack(true);
    }

    private void showErrorInconsistency() {
        MessageDialog.openError(this.getSite().getShell(),
                                DIALOG_ERROR_INCONSISTENCY_TITLE,
                                DIALOG_ERROR_INCONSISTENCY_MSG);
        TestSessionsView.this.logger.fatal(
           "User interface and data model are inconsistent!");     //$NON-NLS-1$

    }


    /*
     * methods and classes to create actions and hook them to context menus,
     * toolbars and the tree viewer
     */

    private void makeActions() {
        /*
         * save was moved to its own class since it could be used in other views
         * as well
         */
        this.saveTSC = new SaveActiveTSContainerAction();
        this.deleteTSC = this.makeTSCDeleteAction();
        this.selectAll = this.makeSelectAllAction();
        this.activateAll = this.makeActivateAllAction();
        this.deactivateAll = this.makeDeactivateAllAction();
        this.merge = this.makeMergeAction();
        this.deleteToolBar = this.makeTestElementDeleteAction(false);
        this.coverageImportToolBar = this.makeImportCoverageAction();
        this.deleteContextMenu = this.makeTestElementDeleteAction(true);
        this.propertiesAction = this.makePropertiesAction();
        this.showActiveTestCases = this.makeActiveTestCasesDialogAction();
    }

    private void fillActionMenus() {
        IActionBars bars = this.getViewSite().getActionBars();
        // fill toolbar of view
        bars.getToolBarManager().add(this.saveTSC);
        bars.getToolBarManager().add(this.deleteTSC);
        bars.getToolBarManager().add(new Separator());
        bars.getToolBarManager().add(this.merge);
        bars.getToolBarManager().add(this.deleteToolBar);
        bars.getToolBarManager().add(new Separator());
        bars.getToolBarManager().add(this.coverageImportToolBar);

        // fill pulldown menu of view
        bars.getMenuManager().add(this.saveTSC);
        bars.getMenuManager().add(this.deleteTSC);
        if(CodeCoverPlugin.getDefault().getLogLevel() == LogLevel.DEBUG) {
            bars.getMenuManager().add(this.showActiveTestCases);
        }
        bars.getMenuManager().add(new Separator());
        bars.getMenuManager().add(this.merge);
        bars.getMenuManager().add(this.deleteToolBar);
        bars.getMenuManager().add(new Separator());
        bars.getMenuManager().add(this.coverageImportToolBar);

        // fill context menu
        MenuManager menuMgr = new MenuManager("#PopupMenu");       //$NON-NLS-1$
        menuMgr.setRemoveAllWhenShown(true);
        menuMgr.addMenuListener(new IMenuListener() {
            @Override
			public void menuAboutToShow(IMenuManager manager) {
                manager.add(TestSessionsView.this.selectAll);
                manager.add(new Separator());
                manager.add(TestSessionsView.this.activateAll);
                manager.add(TestSessionsView.this.deactivateAll);
                manager.add(TestSessionsView.this.deleteContextMenu);
                manager.add(TestSessionsView.this.propertiesAction);
                // other plugins can contribute their actions here
                manager.add(new Separator(
                        IWorkbenchActionConstants.MB_ADDITIONS));
            }
        });
        Menu menu = menuMgr.createContextMenu(this.viewer.getControl());
        this.viewer.getControl().setMenu(menu);
        this.getSite().registerContextMenu(menuMgr, this.viewer);
    }

    private void hookDoubleClickAction() {
        this.viewer.addDoubleClickListener(new IDoubleClickListener() {
            @Override
			public void doubleClick(DoubleClickEvent event) {
                TestSessionsView.this.propertiesAction.run();
            }
        });
    }

    private Action makeTSCDeleteAction() {
        return new TSCDeleteAction();
    }

    private class TSCDeleteAction extends Action implements IMenuCreator {

        private Menu menu;

        TSCDeleteAction() {
            super(TSC_DELETE_ACTION_TEXT, IAction.AS_DROP_DOWN_MENU);
            this.setToolTipText(TSC_DELETE_ACTION_TOOLTIP);
            this.setImageDescriptor(CodeCoverPlugin.getDefault()
                    .getImageRegistry().getDescriptor(CodeCoverPlugin.Image
                            .SESSION_CONTAINER_DELETE.getPath()));
            this.setMenuCreator(this);
            this.menu = null;
        }

        /**
         * Deletes the active test session container.
         */
        @Override
        public void run() {
            TSContainerInfo activeTSCInfo
                    = TestSessionsView.this.getVisTSCInfo();

            if(activeTSCInfo == null) {
                return;
            }

            boolean deleteTSC = MessageDialog.openQuestion(
                    TestSessionsView.this.viewer.getControl().getShell(),
                    TSC_DELETE_DIALOG_TITLE,
                    String.format(TSC_DELETE_DIALOG_MSG,
                            activeTSCInfo.getName()));
            if(deleteTSC) {
                try {
                    CodeCoverPlugin.getDefault().getTSContainerManager()
                            .deleteTestSessionContainer(activeTSCInfo,
                                    new NullProgressMonitor());
                } catch(CoreException e) {
                    ErrorDialog.openError(
                            CodeCoverPlugin.getDefault()
                                    .getWorkbench().getDisplay()
                                    .getActiveShell(),
                            TSC_DELETE_DIALOG_ERROR_TITLE,
                            null,
                            new Status( IStatus.ERROR,
                                        CodeCoverPlugin.PLUGIN_ID,
                                        IStatus.OK,
                                        TSC_DELETE_DIALOG_ERROR_MSG,
                                        e));
                } catch(CancelException e) {
                    TestSessionsView.this.logger.warning("Canceled deleting of" +    //$NON-NLS-1$
                            " test session container.", e);    //$NON-NLS-1$
                }
            }
        }

        @Override
		public Menu getMenu(Control parent) {
            ActionContributionItem item;
            if(this.menu != null && this.menu.getParent().equals(parent)) {
                return this.menu;
            } else {
                if(this.menu != null) {
                    this.menu.dispose();
                }
                this.menu = new Menu(parent);
                item = new ActionContributionItem(
                        new MultipleTSCsDeleteAction());
                item.fill(this.menu, -1);
                return this.menu;
            }
        }

        @Override
		public Menu getMenu(Menu parent) {
            return null;
        }

        @Override
		public void dispose() {
            if(this.menu != null)  {
                this.menu.dispose();
                this.menu = null;
            }
        }
    };

    private class MultipleTSCsDeleteAction extends Action {

        MultipleTSCsDeleteAction() {
            this.setText(TSC_DELETE_MULTIPLE_ACTION_TEXT);
            this.setToolTipText(TSC_DELETE_MULTIPLE_ACTION_TOOLTIP);
        }

        @Override
        public void run() {
            (new DeleteTSCsSelectDialog(TestSessionsView.this.getSite()
                    .getShell())).open();
        }
    }

    private Action makeTestElementDeleteAction(boolean forContextMenu) {
        return new TestElementDeleteAction(this.getSite().getShell(),
                                this.viewer,
                                forContextMenu);
    }

    /**
     * An action to delete test sessions or test cases selected in the (tree)
     * viewer of the Test Sessions view.
     */
    private final class TestElementDeleteAction extends Action
            implements ISelectionChangedListener {

        private Shell shell;

        private CheckboxTreeViewer viewerOfTestSessionsView;

        private boolean inContextMenu;

        TestElementDeleteAction(Shell shell, CheckboxTreeViewer viewer,
                boolean inContextMenu) {
            this.shell = shell;
            this.viewerOfTestSessionsView = viewer;
            this.inContextMenu = inContextMenu;

            if(this.inContextMenu) {
                this.setText(ELEMENTS_DELETE_ACTION_TEXT_CONTEXTMENU);
                this.setImageDescriptor(PlatformUI.getWorkbench()
                        .getSharedImages()
                        .getImageDescriptor(ISharedImages.IMG_TOOL_DELETE));
            } else {
                this.setText(ELEMENTS_DELETE_ACTION_TEXT_TOOLBAR);
                this.setImageDescriptor(CodeCoverPlugin.getDefault()
                        .getImageRegistry().getDescriptor(
                            CodeCoverPlugin.Image.ELEMENTS_DELETE.getPath()));
            }
            this.setToolTipText(ELEMENTS_DELETE_ACTION_TOOLTIP);
        }

        @Override
        public void run() {
            IStructuredSelection sel = null;

            if(TestSessionsView.this.getVisTSC() == null) {
                return;
            }

            if(this.viewerOfTestSessionsView.getSelection()
                    instanceof IStructuredSelection) {
                sel = (IStructuredSelection)this.viewerOfTestSessionsView
                        .getSelection();
            }

            if(this.inContextMenu && sel != null && sel.size() == 1) {
                Object[] elementToDelete = sel.toArray();
                DeleteTestElementsConfirmDialog confirmDialog
                        = new DeleteTestElementsConfirmDialog(
                                this.shell, elementToDelete);
                confirmDialog.setBlockOnOpen(true);
                if(confirmDialog.open()
                        == DeleteTestElementsConfirmDialog.YES_BUTTON_INDEX) {
                    try {
                        CodeCoverPlugin.getDefault().getTSContainerManager()
                                .setActiveTSContainer(
                                        TestSessionsView.this.getVisTSCInfo(),
                                        new DeleteTestElementsRunnable(
                                                elementToDelete),
                                        null);
                    } catch(CancelException e) {
                        TestSessionsView.this.logger.warning(
                                "User canceled deletion of" +      //$NON-NLS-1$
                                " test sessions/test cases", e);   //$NON-NLS-1$
                    } catch(Exception e) {
                        ErrorDialog.openError(
                                CodeCoverPlugin.getDefault()
                                        .getWorkbench().getDisplay()
                                        .getActiveShell(),
                                DIALOG_ERROR_ELEMENT_DELETE_TITLE,
                                null,
                                new Status(IStatus.ERROR,
                                           CodeCoverPlugin.PLUGIN_ID,
                                           IStatus.OK,
                                           DIALOG_ERROR_ELEMENT_DELETE_MSG,
                                           e));
                        TestSessionsView.this.logger.error(
                                "Error while deleting" +           //$NON-NLS-1$
                                " test session/test case", e);     //$NON-NLS-1$
                    } catch(OutOfMemoryError e) {
                        ErrorDialog.openError(
                                CodeCoverPlugin.getDefault()
                                        .getWorkbench().getDisplay()
                                        .getActiveShell(),
                                DIALOG_ERROR_ELEMENT_DELETE_TITLE,
                                null,
                                new Status(IStatus.ERROR,
                                           CodeCoverPlugin.PLUGIN_ID,
                                           IStatus.OK,
                                           DIALOG_ERROR_ELEMENT_DELETE_MSG,
                                           e));
                        TestSessionsView.this.logger.error(
                                "Error while deleting" +           //$NON-NLS-1$
                                " test session/test case",         //$NON-NLS-1$
                                new InvocationTargetException(e));
                    }
                }
            } else {
                (new DeleteTestElementsSelectDialog(this.shell, sel,
                        TestSessionsView.this.getVisTSCInfo(),
                        TestSessionsView.this.getVisTSC(),
                        TestSessionsView.this.logger)).open();
            }
        }

        @Override
		public void selectionChanged(SelectionChangedEvent e) {
            IStructuredSelection sel;
            if(e.getSelection().isEmpty()) {
                this.setEnabled(false);
                return;
            }
            if(!(e.getSelection() instanceof IStructuredSelection)) {
                return;
            }
            sel = (IStructuredSelection)e.getSelection();
            if(sel.getFirstElement() instanceof TestSession
                    || sel.getFirstElement() instanceof TestCase) {
                this.setEnabled(true);
            } else {
                this.setEnabled(false);
            }
        }
    }

    private Action makeImportCoverageAction() {
        Action action = new Action() {
            @Override
            public void run() {
                if (TestSessionsView.this.getVisTSC() == null) {
                    return;
                }
                CoverageLogImportWizard coverageLogImportWizard = new CoverageLogImportWizard();
                coverageLogImportWizard.init(PlatformUI.getWorkbench(), null);

                // Instantiates the wizard container with the wizard and opens it
                WizardDialog wizardDialog = new WizardDialog(TestSessionsView.this.getSite().getShell(),
                        coverageLogImportWizard);
                wizardDialog.create();
                wizardDialog.open();
            }
        };
        action.setText(Messages.getString("CoverageLogImportWizard.0")); //$NON-NLS-1$
        action.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry().
                getDescriptor(CodeCoverPlugin.Image.COVERAGE_LOG.getPath()));

        return action;
    }

    private Action makeMergeAction() {
        Action action = new Action() {
            @Override
            public void run() {
                MergeWizard mergeWizard;
                WizardDialog wizardDialog;
                if(TestSessionsView.this.getVisTSC() == null) {
                    return;
                }
                mergeWizard = new MergeWizard(
                        TestSessionsView.this.getVisTSC(),
                        (IStructuredSelection)TestSessionsView.this.viewer
                                .getSelection());
                //Instantiates the wizard container with the wizard and opens it
                wizardDialog = new WizardDialog(
                        TestSessionsView.this.getSite().getShell(),
                        mergeWizard);
                wizardDialog.create();
                wizardDialog.open();
            }
        };
        action.setText(MERGE_ACTION_TEXT);
        action.setToolTipText(MERGE_ACTION_TOOLTIP);
        action.setImageDescriptor(CodeCoverPlugin.getDefault()
                .getImageRegistry().getDescriptor(
                        CodeCoverPlugin.Image.ELEMENTS_MERGE.getPath()));

        return action;
    }

    private Action makeSelectAllAction() {
        Action action = new Action() {
            @Override
            public void run() {
                List<Object> allTSandTC = new ArrayList<Object>();
                // add all test sessions
                allTSandTC.addAll(TestSessionsView.this.getVisTSC()
                        .getTestSessions());
                // add all test cases
                for(TestSession testSession : TestSessionsView.this
                        .getVisTSC().getTestSessions()) {
                    allTSandTC.addAll(testSession.getTestCases());
                }
                TestSessionsView.this.viewer
                        .setSelection(new StructuredSelection(allTSandTC));
            }
        };
        action.setText(SELECT_ALL_ACTION_TEXT);
        action.setToolTipText(SELECT_ALL_ACTION_TOOLTIP);

        return action;
    }

    private Action makeActivateAllAction() {
        Action action = new Action() {
            @Override
            public void run() {
                for(TestSession testSession : TestSessionsView.this
                        .getVisTSC().getTestSessions()) {
                    TestSessionsView.this.getVisTestCases()
                            .addAll(testSession.getTestCases());
                }
                TestSessionsView.this.viewer.setAllChecked(true);

                CodeCoverPlugin.getDefault().getTSContainerManager()
                        .setActiveTestCases(
                                TestSessionsView.this.getVisTestCases());
            }
        };
        action.setText(ACTIVATE_ALL_ACTION_TEXT);
        action.setToolTipText(ACTIVATE_ALL_ACTION_TOOLTIP);

        return action;
    }

    private Action makeDeactivateAllAction() {
        Action action = new Action() {
            @Override
            public void run() {
                TestSessionsView.this.getVisTestCases().clear();
                TestSessionsView.this.viewer.setAllChecked(false);

                CodeCoverPlugin.getDefault().getTSContainerManager()
                        .setActiveTestCases(
                                TestSessionsView.this.getVisTestCases());
            }
        };
        action.setText(DEACTIVATE_ALL_ACTION_TEXT);
        action.setToolTipText(DEACTIVATE_ALL_ACTION_TOOLTIP);

        return action;
    }

    private Action makePropertiesAction() {
        return new TestElementPropertiesAction(this.getSite().getShell(),
                this.viewer);
    }

    /**
     * An action which allows editing the properties of a test session or test
     * case selected in the (tree) viewer of the Test Sessions view.
     */
    private final class TestElementPropertiesAction extends Action
            implements ISelectionChangedListener {

        private Shell shell;

        private CheckboxTreeViewer viewer;

        TestElementPropertiesAction(Shell shell, CheckboxTreeViewer viewer) {
            this.shell = shell;
            this.viewer = viewer;
            this.setText(PROPERTIES_ACTION_TEXT);
        }

        /*
         * (non-Javadoc)
         *
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            ISelection sel = this.viewer.getSelection();
            if(!(sel instanceof IStructuredSelection)
                    || ((IStructuredSelection)sel).isEmpty()) {
                return;
            }

            Object selectedObject
                    = ((IStructuredSelection)sel).getFirstElement();
            (new TestElementPropertiesDialog(this.shell,
                    selectedObject,
                    TestSessionsView.this.getVisTSCInfo(),
                    TestSessionsView.this.logger)).open();
        }

        /*
         * (non-Javadoc)
         *
         * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
         */
        @Override
		public void selectionChanged(SelectionChangedEvent e) {
            IStructuredSelection sel;
            if(e.getSelection().isEmpty()) {
                this.setEnabled(false);
                return;
            }
            if(!(e.getSelection() instanceof IStructuredSelection)) {
                return;
            }
            sel = (IStructuredSelection)e.getSelection();
            if(sel.getFirstElement() instanceof TestSession
                    || sel.getFirstElement() instanceof TestCase) {
                this.setEnabled(true);
            } else {
                this.setEnabled(false);
            }
        }

    }

    /**
     * Generates an action which opens a dialog which lists all active test
     * cases as reported by the TSContainerManager. This action is used for
     * debugging purposes.
     *
     * @return  an action which opens a dialog which lists all active test
     *          cases as reported by the TSContainerManager.
     */
    private Action makeActiveTestCasesDialogAction() {
        Action action = new Action() {
            @Override
            public void run() {
                ActiveTSContainerInfo activeTSCInfo = CodeCoverPlugin
                        .getDefault()
                        .getTSContainerManager().getActiveTSContainer();
                StringBuilder msg = new StringBuilder(
                        "The following listing shows the active" + //$NON-NLS-1$
                        " test cases\n" +                          //$NON-NLS-1$
                        "as reported by the TSContainerManager." + //$NON-NLS-1$
                        "\n\n");                                   //$NON-NLS-1$
                if(activeTSCInfo != null) {
                    msg.append("Active Test Session Container:\n"  //$NON-NLS-1$
                            + activeTSCInfo.getName()
                            + "\n\n"                               //$NON-NLS-1$
                            + "Active Test Cases:\n");             //$NON-NLS-1$

                    boolean isEmpty = true;
                    for(TestCase testCase : activeTSCInfo.getActiveTestCases()){
                        msg.append(testCase.getName() + " ("       //$NON-NLS-1$
                                + testCase.getTestSession().getName()
                                + ") \n");                         //$NON-NLS-1$
                        isEmpty = false;
                    }
                    if(isEmpty) {
                        msg.append( "No active test cases." +      //$NON-NLS-1$
                                    " (Select some!)");            //$NON-NLS-1$
                    }
                } else {
                    msg.append( "No active test session" +         //$NON-NLS-1$
                                " container. (Activate one!)");    //$NON-NLS-1$
                }
                MessageDialog.openInformation(
                        TestSessionsView.this.viewer.getControl().getShell(),
                        "Active Test Cases",                       //$NON-NLS-1$
                        msg.toString());
            }
        };
        action.setText("Show active test cases");                  //$NON-NLS-1$
        action.setToolTipText(  "Show active test cases of active"+//$NON-NLS-1$
                                " test session container");        //$NON-NLS-1$
        action.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
        return action;
    }

    /*
     * methods to store and restore the expanded elements of the tree viewer
     */

    private void saveExpandedElements(TSContainerInfo tscInfo) {
        if(tscInfo != null) {
            this.expandedElements.remove(tscInfo);
            this.expandedElements.put(tscInfo,
               generateTestSessionNamesList(this.viewer.getExpandedElements()));
        }
    }

    private static Set<String> generateTestSessionNamesList(Object[] objs) {
        Set<String> testSessionNames = new HashSet<String>();
        for(Object o : objs) {
            if(o instanceof TestSession) {
                testSessionNames.add(((TestSession)o).getName());
            }
        }
        return testSessionNames;
    }

    private static Object[] fetchTestSessions(Set<String> testSessionNames,
            TestSessionContainer tsc) {
        Set<TestSession> testSessions = new HashSet<TestSession>();
        TestSession testSession;
        if(testSessionNames != null) {
            for(String testSessionName : testSessionNames) {
                testSession = tsc.getTestSessionWithName(testSessionName);
                if(testSession != null) {
                    testSessions.add(testSession);
                }
            }
        }
        return testSessions.toArray();
    }

    private void restoreExpandedElements(ActiveTSContainerInfo activeTSCInfo) {
        Object[] tscExpElems;
        if(activeTSCInfo != null) {
            tscExpElems = fetchTestSessions(
                    this.expandedElements.get(activeTSCInfo),
                    activeTSCInfo.getTestSessionContainer());
            if(tscExpElems != null) {
                this.viewer.setExpandedElements(tscExpElems);
            }
        }
    }

    private void removeExpandedElements(TSContainerInfo tscInfo){
        this.expandedElements.remove(tscInfo);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        this.viewer.getControl().setFocus();
    }

}
