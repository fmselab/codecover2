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

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.MergeException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Text;

/**
 * The (one and only) page of the wizard to merge test sessions or test cases.
 *
 * @see MergeWizard
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: MergeWizardPage.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MergeWizardPage extends WizardPage {

    private static final String PAGE_NAME = Messages
            .getString("MergeWizardPage.PAGE_NAME"); //$NON-NLS-1$
    private static final String DESCRIPTION = Messages
            .getString("MergeWizardPage.DESCRIPTION"); //$NON-NLS-1$
    private static final String GROUP_ELEMENTS = Messages
            .getString("MergeWizardPage.GROUP_ELEMENTS"); //$NON-NLS-1$
    private static final String BUTTON_TEST_SESSION = Messages
            .getString("MergeWizardPage.BUTTON_TEST_SESSION"); //$NON-NLS-1$
    private static final String BUTTON_TEST_CASE = Messages
            .getString("MergeWizardPage.BUTTON_TEST_CASE"); //$NON-NLS-1$
    private static final String LABEL_SELECT_ELEMENTS = Messages
            .getString("MergeWizardPage.LABEL_SELECT_ELEMENTS"); //$NON-NLS-1$
    private static final String GROUP_MERGED_ELEMENT_TS = Messages
            .getString("MergeWizardPage.GROUP_MERGED_ELEMENT_TS"); //$NON-NLS-1$
    private static final String GROUP_MERGED_ELEMENT_TC = Messages
            .getString("MergeWizardPage.GROUP_MERGED_ELEMENT_TC"); //$NON-NLS-1$
    private static final String LABEL_ELEMENT_NAME = Messages
            .getString("MergeWizardPage.LABEL_ELEMENT_NAME"); //$NON-NLS-1$
    private static final String LABEL_ELEMENT_COMMENT = Messages
            .getString("MergeWizardPage.LABEL_ELEMENT_COMMENT"); //$NON-NLS-1$
    private static final String ACTION_TEXT_SELECT_ALL_TS = Messages
            .getString("MergeWizardPage.ACTION_TEXT_SELECT_ALL_TS"); //$NON-NLS-1$
    private static final String ACTION_TEXT_SELECT_ALL_TC = Messages
            .getString("MergeWizardPage.ACTION_TEXT_SELECT_ALL_TC"); //$NON-NLS-1$
    private static final String ACTION_TEXT_DESELECT_ALL = Messages
            .getString("MergeWizardPage.ACTION_TEXT_DESELECT_ALL"); //$NON-NLS-1$
    private static final String ERROR_TEST_CASES_OF_DIFFERENT_TEST_SESSIONS = Messages
            .getString("MergeWizardPage.ERROR_TEST_CASES_OF_DIFFERENT_TEST_SESSIONS"); //$NON-NLS-1$
    private static final String ERROR_TEST_ELEMENTS_UNDER_2_SELECTED = Messages
            .getString("MergeWizardPage.ERROR_TEST_ELEMENTS_UNDER_2_SELECTED"); //$NON-NLS-1$
    private static final String ERROR_ELEMENT_NAME_NONE_SPECIFIED = Messages
            .getString("MergeWizardPage.ERROR_ELEMENT_NAME_NONE_SPECIFIED"); //$NON-NLS-1$
    private static final String WARNING_TEST_CASES_SELECTED = Messages
            .getString("MergeWizardPage.WARNING_TEST_CASES_SELECTED"); //$NON-NLS-1$
    private static final String WARNING_TEST_SESSIONS_SELECTED = Messages
            .getString("MergeWizardPage.WARNING_TEST_SESSIONS_SELECTED"); //$NON-NLS-1$

    private TestSessionContainer tsc;
    private List<TestSession> selectedTestSessions;
    private List<TestCase> selectedTestCases;

    private IStructuredSelection preSelection;

    private Button bttTestSession;
    private Button bttTestCase;
    private CheckboxTreeViewer viewer;
    private Group grpMergedElement;
    private Text txtElementName;
    private Text txtElementComment;

    private String elementsError;
    private String elementNameError;

    /**
     * Constructor
     * 
     * @param tsc
     *            the given {@link TestSessionContainer}
     * @param selection
     *            the given {@link IStructuredSelection}
     */
    protected MergeWizardPage(TestSessionContainer tsc,
            IStructuredSelection selection) {
        super(PAGE_NAME);
        if(tsc == null) {
            throw new NullPointerException(
                    "tsc (to merge elements of) mustn't be null"); //$NON-NLS-1$
        }
        this.setTitle(PAGE_NAME);
        this.setDescription(DESCRIPTION);
        this.tsc = tsc;
        this.selectedTestSessions = new LinkedList<TestSession>();
        this.selectedTestCases = new LinkedList<TestCase>();
        this.preSelection = selection;

        this.elementsError = ERROR_TEST_ELEMENTS_UNDER_2_SELECTED;
        this.elementNameError = ERROR_ELEMENT_NAME_NONE_SPECIFIED;
    }

    @Override
	public void createControl(Composite parent) {
        Composite mainComposite = new Composite(parent, SWT.NONE);
        GridLayout gridLayout;
        GridData gridData;
        gridLayout = new GridLayout();
        gridLayout.numColumns = 1;
        mainComposite.setLayout(gridLayout);

        /*
         * create group which will host the radio buttons to select which type
         * of test element are merged
         */
        Group grpElementType = new Group(mainComposite, SWT.NONE);
        grpElementType.setText(GROUP_ELEMENTS);
        gridLayout = new GridLayout();
        gridLayout.numColumns = 1;
        grpElementType.setLayout(gridLayout);
        // create radio button for test sessions
        this.bttTestSession = new Button(grpElementType, SWT.RADIO);
        this.bttTestSession.setText(BUTTON_TEST_SESSION);
        // layout radio button for test sessions
        this.bttTestSession.setLayoutData(new GridData());
        // create radio button for test sessions
        this.bttTestCase = new Button(grpElementType, SWT.RADIO);
        this.bttTestCase.setText(BUTTON_TEST_CASE);
        // layout radio button for test sessions
        this.bttTestCase.setLayoutData(new GridData());
        // preselect test sessions as the test elements to merge
        this.bttTestSession.setSelection(true);
        // register listeners
        this.bttTestSession.addSelectionListener(new ElementTypeListener());
        this.bttTestCase.addSelectionListener(new ElementTypeListener());

        // create label for (following) tree viewer
        Label viewerLabel = new Label(mainComposite, SWT.NONE);
        viewerLabel.setText(LABEL_SELECT_ELEMENTS);

        // create tree viewer which lists test sessions and test cases
        this.viewer = TestSessionsViewerFactory.newCheckboxTreeViewer(
                mainComposite,
                false,
                new Runnable() { @Override
				public void run() {} });
        this.createContextMenu();
        this.viewer.setInput(this.tsc);
        this.applySelection();
        this.viewer.addCheckStateListener(new ViewerListener());

        /*
         * create group which will host the fields for the name and comment of
         * the test element created by the merge
         */
        this.grpMergedElement = new Group(mainComposite, SWT.NONE);
        if(this.bttTestSession.getSelection()) {
            this.grpMergedElement.setText(GROUP_MERGED_ELEMENT_TS);
        } else {
            this.grpMergedElement.setText(GROUP_MERGED_ELEMENT_TC);
        }
        gridLayout = new GridLayout();
        gridLayout.numColumns = 2;
        this.grpMergedElement.setLayout(gridLayout);
        Label lblTEName = new Label(this.grpMergedElement, SWT.LEAD);
        lblTEName.setText(LABEL_ELEMENT_NAME);
        // create text field for test session name
        this.txtElementName = new Text(this.grpMergedElement,
                SWT.SINGLE | SWT.BORDER);
        // layout text field for test session name
        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        this.txtElementName.setLayoutData(gridData);
        // create label of text field for test session comment
        Label lblTEComment = new Label(this.grpMergedElement, SWT.LEAD);
        lblTEComment.setText(LABEL_ELEMENT_COMMENT);
        // layout label of text field for test session comment
        gridData = new GridData();
        gridData.verticalAlignment = GridData.BEGINNING;
        lblTEComment.setLayoutData(gridData);
        // create text field for test session comment
        this.txtElementComment = new Text(this.grpMergedElement,
                SWT.MULTI | SWT.WRAP | SWT.V_SCROLL | SWT.BORDER);
        // layout text field for test session comment
        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        this.txtElementComment.setLayoutData(gridData);
        // register listeners for test session name/comment
        ModifyListener nameFieldListener = new ElementNameFieldListener();
        this.txtElementName.addModifyListener(nameFieldListener);

        // layout mainComposite
        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        grpElementType.setLayoutData(gridData);

        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        viewerLabel.setLayoutData(gridData);

        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.verticalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        this.viewer.getTree().getParent().setLayoutData(gridData);

        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        this.grpMergedElement.setLayoutData(gridData);

        this.setControl(mainComposite);
        this.applyErrorMessage();
    }

    private void createContextMenu() {
        MenuManager menuMgr;
        Menu menu;
        // make select all and deselect all actions
        final Action selectAllTestSessions = new Action() {
            @Override public void run() {
                TestSessionContainer tsc = (TestSessionContainer)MergeWizardPage
                        .this.viewer.getInput();
                List<TestSession> testSessions;
                if(tsc != null) {
                    testSessions = tsc.getTestSessions();
                    MergeWizardPage.this.viewer.setCheckedElements(
                            testSessions.toArray());
                    MergeWizardPage.this.selectedTestSessions.clear();
                    MergeWizardPage.this.selectedTestSessions.addAll(
                            testSessions);
                    MergeWizardPage.this.selectedTestCases.clear();
                    MergeWizardPage.this.checkElements();
                    MergeWizardPage.this.applyErrorMessage();
                }
            }
        };
        selectAllTestSessions.setText(ACTION_TEXT_SELECT_ALL_TS);
        final Action selectAllTestCases = new Action() {
            @Override public void run() {
                TestSessionContainer tsc = (TestSessionContainer)MergeWizardPage
                        .this.viewer.getInput();
                List<TestCase> testCases = new LinkedList<TestCase>();
                if(tsc != null) {
                    for(TestSession ts : tsc.getTestSessions()) {
                        testCases.addAll(ts.getTestCases());
                    }
                    MergeWizardPage.this.viewer.setCheckedElements(
                            testCases.toArray());
                    MergeWizardPage.this.selectedTestSessions.clear();
                    MergeWizardPage.this.selectedTestCases.clear();
                    MergeWizardPage.this.selectedTestCases.addAll(testCases);
                    MergeWizardPage.this.checkElements();
                    MergeWizardPage.this.applyErrorMessage();
                }
            }
        };
        selectAllTestCases.setText(ACTION_TEXT_SELECT_ALL_TC);
        final Action deselectAll = new Action() {
            @Override public void run() { viewer.setAllChecked(false); }
        };
        deselectAll.setText(ACTION_TEXT_DESELECT_ALL);
        // create context menu
        menuMgr = new MenuManager("#PopupMenu");       //$NON-NLS-1$
        menuMgr.setRemoveAllWhenShown(true);
        menuMgr.addMenuListener(new IMenuListener() {
            @Override
			public void menuAboutToShow(IMenuManager manager) {
                manager.add(selectAllTestSessions);
                manager.add(selectAllTestCases);
                manager.add(deselectAll);
            }
        });
        menu = menuMgr.createContextMenu(viewer.getTree());
        viewer.getTree().setMenu(menu);
    }
    
    private void applySelection() {
        if(this.preSelection != null) {
            Object[] sel = this.preSelection.toArray();
            boolean selectionContainsTestSession = false;
            boolean selectionContainsTestCase = false;
            // expand preselected elements
            Set<TestSession> expandedElements
                    = new HashSet<TestSession>();
            for(Object o : sel) {
                if(o instanceof TestCase) {
                    expandedElements.add(((TestCase)o).getTestSession());
                    this.selectedTestCases.add((TestCase)o);
                    selectionContainsTestCase = true;
                } else if(o instanceof TestSession) {
                    this.selectedTestSessions.add((TestSession)o);
                    selectionContainsTestSession = true;
                }
            }
            /*
             * only change from default to test cases being the merge target, if
             * there is no test session in the selection
             */
            if(selectionContainsTestCase && !selectionContainsTestSession) {
                this.bttTestCase.setSelection(true);
                this.bttTestSession.setSelection(false);
            }
            this.viewer.setExpandedElements(expandedElements.toArray());
            // check preselected elements
            this.viewer.setCheckedElements(sel);
            this.checkElements();
        }
    }

    /**
     * Merges the selected elements.
     * 
     * @return true &rarr; if merging was successful. <br>
     *         false &rarr; if not.
     */
    boolean mergeElements() {
        if(this.bttTestSession.getSelection()) {
            try {
                this.tsc.mergeTestSessions(this.selectedTestSessions,
                        this.txtElementName.getText(),
                        this.txtElementComment.getText());
            } catch(MergeException e) {
                CodeCoverPlugin.getDefault().getLogger()
                    .error("Error while merging test sessions",    //$NON-NLS-1$
                            e);
                return false;
            }
        } else {
            try {
                this.tsc.mergeTestCases(this.selectedTestCases,
                        this.txtElementName.getText(),
                        this.txtElementComment.getText());
            } catch(MergeException e) {
                CodeCoverPlugin.getDefault().getLogger()
                    .error("Error while merging test cases",  //$NON-NLS-1$
                            e);
                return false;
            }
        }
        return true;
    }

    private final class ElementTypeListener implements SelectionListener {

        @Override
		public void widgetDefaultSelected(SelectionEvent e) {}

        @Override
		public void widgetSelected(SelectionEvent e) {
            if(MergeWizardPage.this.bttTestSession.getSelection()) {
                MergeWizardPage.this.grpMergedElement
                        .setText(GROUP_MERGED_ELEMENT_TS);
            } else {
                MergeWizardPage.this.grpMergedElement
                        .setText(GROUP_MERGED_ELEMENT_TC);
            }
            MergeWizardPage.this.checkElements();
            MergeWizardPage.this.applyErrorMessage();
        }

    }

    private final class ViewerListener implements ICheckStateListener {

        @Override
		public void checkStateChanged(CheckStateChangedEvent event) {
            TestCase testCase;
            MergeWizardPage.this.selectedTestSessions.clear();
            MergeWizardPage.this.selectedTestCases.clear();
            for(Object checkedElement
                    : MergeWizardPage.this.viewer.getCheckedElements()) {
                if(checkedElement instanceof TestSession) {
                    MergeWizardPage.this.selectedTestSessions
                            .add((TestSession)checkedElement);
                } else if(checkedElement instanceof TestCase) {
                    testCase = (TestCase)checkedElement;
                    MergeWizardPage.this.selectedTestCases
                            .add(testCase);
                }
            }
            MergeWizardPage.this.checkElements();
            MergeWizardPage.this.applyErrorMessage();
        }

    }

    /**
     * Checks the selected elements, and sets errors or warnings to be
     * displayed:
     * <ul>
     * <li>Error if there are less than two test elements of the selected type
     * (test session or test case) selected.</li>
     * <li>Error if the selected type to merge is test case and there are test
     * cases of different test sessions selected.</li>
     * <li>Warning if there are test cases selected although test sessions
     * are the selected type to be merged or vice versa.</li>
     * </ul>
     * The name field is not checked. This is done by
     * {@link ElementNameFieldListener}.
     */
    private void checkElements() {
        String error = null;
        // if test sessions are to be merged...
        if(this.bttTestSession.getSelection()) {
            if(this.selectedTestSessions.size() < 2) {
                error = ERROR_TEST_ELEMENTS_UNDER_2_SELECTED;
            }
            /*
             * check if test cases are selected although test sessions are
             * to be merged
             */
            if(!this.selectedTestCases.isEmpty()) {
                this.setMessage(WARNING_TEST_CASES_SELECTED,
                        IMessageProvider.WARNING);
            } else {
                this.setMessage(DESCRIPTION,
                        IMessageProvider.NONE);
            }
        } else /* if test cases are to be merged... */ {
            if(this.selectedTestCases.size() < 2) {
                error = ERROR_TEST_ELEMENTS_UNDER_2_SELECTED;
            } else if(this.isTestCasesWithDifferentParentsSelected()) {
                error = ERROR_TEST_CASES_OF_DIFFERENT_TEST_SESSIONS;
            }
            /*
             * check if test sessions are selected although test cases are
             * to be merged
             */
            if(!this.selectedTestSessions.isEmpty()) {
                this.setMessage(WARNING_TEST_SESSIONS_SELECTED,
                        IMessageProvider.WARNING);
            } else {
                this.setMessage(DESCRIPTION,
                        IMessageProvider.NONE);
            }
        }
        this.elementsError = error;
    }

    private boolean isTestCasesWithDifferentParentsSelected() {
        TestSession parentTestSession = null;
        for(TestCase testCase : this.selectedTestCases) {
            /*
             * check if test cases of different test sessions are
             * selected
             */
            if(parentTestSession != null) {
                if(parentTestSession != testCase.getTestSession()) {
                    return true;
                }
            } else {
                parentTestSession = testCase.getTestSession();
            }
        }
        return false;
    }

    /**
     * Checks if the user entered something into the name field.
     */
    private final class ElementNameFieldListener implements ModifyListener {
        @Override
		public void modifyText(ModifyEvent event) {
            String error = null;
            String elementName;
            if(event.widget == MergeWizardPage.this.txtElementName) {
                elementName = MergeWizardPage.this.txtElementName.getText();
                if(elementName.length() == 0) {
                    error = ERROR_ELEMENT_NAME_NONE_SPECIFIED;
                }
                MergeWizardPage.this.elementNameError = error;
            }
            MergeWizardPage.this.applyErrorMessage();
        }
    }

    private void applyErrorMessage() {
        if(this.elementsError != null) {
            this.setErrorMessage(this.elementsError);
            this.setPageComplete(false);
        } else if(this.elementNameError != null) {
            this.setErrorMessage(this.elementNameError);
            this.setPageComplete(false);
        } else {
            this.setErrorMessage(null);
            this.setPageComplete(true);
        }
    }
}
