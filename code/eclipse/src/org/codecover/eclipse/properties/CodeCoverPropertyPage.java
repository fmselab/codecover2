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

package org.codecover.eclipse.properties;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.instrumentation.InstrumenterDescriptor;
import org.codecover.model.utils.criteria.Criterion;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.dialogs.PropertyPage;

/**
 * TODO comment
 * 
 * @author Tilmann Scheller, Markus Wittlinger
 * @version 1.0 ($Id: CodeCoverPropertyPage.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CodeCoverPropertyPage extends PropertyPage {
    private static final String SELECTION_ERROR_MESSAGE = Messages
            .getString("CodeCoverPropertyPage.SELECTION_ERROR_MESSAGE"); //$NON-NLS-1$

    private static final String SELECT_THE_COVERAGE_CRITERIA_LABEL = Messages
            .getString("CodeCoverPropertyPage.SELECT_THE_COVERAGE_CRITERIA_LABEL"); //$NON-NLS-1$

    private static final String ENABLE_CODE_COVER_LABEL = Messages
            .getString("CodeCoverPropertyPage.ENABLE_CODE_COVER_LABEL"); //$NON-NLS-1$

    private Button codeCoverState;

    private CheckboxTableViewer tableViewer;

    /**
     * Constructor for SamplePropertyPage.
     */
    public CodeCoverPropertyPage() {
        super();
        noDefaultAndApplyButton();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected Control createContents(Composite parent) {
        Composite mainComposite = new Composite(parent, SWT.NONE);
        mainComposite.setLayout(new GridLayout());
        mainComposite
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        this.codeCoverState = new Button(mainComposite, SWT.CHECK);
        this.codeCoverState.setText(ENABLE_CODE_COVER_LABEL);
        this.codeCoverState.setLayoutData(new GridData(SWT.FILL, SWT.CENTER,
                true, false));
        this.codeCoverState.setFont(parent.getFont());
        this.codeCoverState.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                CodeCoverPropertyPage.this.handleCriteriaListState();
                CodeCoverPropertyPage.this.validatePage();
            }
        });

        this.codeCoverState.setSelection(CodeCoverPlugin
                .isCodeCoverActivated(getProject()));

        Label tableLabel = new Label(mainComposite, SWT.NONE);
        tableLabel.setFont(parent.getFont());
        tableLabel.setText(SELECT_THE_COVERAGE_CRITERIA_LABEL);

        this.tableViewer = CheckboxTableViewer.newCheckList(mainComposite,
                SWT.BORDER);
        this.tableViewer.getTable().setFont(parent.getFont());
        this.tableViewer.getTable().setLayoutData(
                new GridData(SWT.FILL, SWT.FILL, true, true));

        this.tableViewer.setContentProvider(new CriteriaListContentProvider());
        this.tableViewer.setLabelProvider(new CriteriaListLabelProvider());
        this.tableViewer.addCheckStateListener(new CriteriaListCheckListener());

        this.tableViewer.setInput(getAllCriteria().toArray());

        for (Criterion criterion : getAllCriteria()) {
            boolean state = CodeCoverPlugin.getCriterionSelectedState(
                    getProject(), criterion);
            this.tableViewer.setChecked(criterion, state);
        }

        handleCriteriaListState();
        validatePage();

        return mainComposite;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        /* store whether CodeCover is enabled for the respective project */
        CodeCoverPlugin.setCodeCoverActivation(getProject(),
                this.codeCoverState.getSelection());

        List<Criterion> selectionList = getSelectedCriteria();
        for (Criterion criterion : getAllCriteria()) {
            /*
             * Save the state for all criteria, according to the presence in the
             * selection list.
             */
            CodeCoverPlugin.setCriterionSelectedState(getProject(), criterion,
                    selectionList.contains(criterion));
        }

        /*
         * in case CodeCover got enabled, initiate a rebuild to trigger
         * instrumentation.
         */
        if (this.codeCoverState.getSelection()) {
            CodeCoverPlugin.build(getProject());
        }

        return true;
    }

    private final IProject getProject() {
        /*
         * since we use this implementation of a PropertyPage both for IProjects
         * and IJavaProjects (see used extension points in plugin.xml) we need
         * to do proper casting here (getElement() will return objects of
         * different types, depending on the context of the PropertyPage)
         */
        IProject project;
        IAdaptable element = getElement();

        if (element instanceof IJavaProject) {
            project = ((IJavaProject) element).getProject();
        } else if (element instanceof IProject) {
            project = (IProject) element;
        } else {
            // this should never be reached
            // TODO throw exception
            project = null;
        }

        return project;
    }

    private final Set<Criterion> getAllCriteria() {
        InstrumenterDescriptor descriptor = new org.codecover.instrumentation.java15.InstrumenterDescriptor();

        return descriptor.getSupportedCriteria();
    }

    private final List<Criterion> getSelectedCriteria() {
        List<Criterion> list = new LinkedList<Criterion>();

        for (Object element : this.tableViewer.getCheckedElements()) {
            list.add((Criterion) element);
        }

        return list;
    }

    private final void handleCriteriaListState() {
        /* Enable the criteria list only if CodeCover is enabled. */
        this.tableViewer.getTable().setEnabled(
                this.codeCoverState.getSelection());
    }

    private final void validatePage() {
        if (!this.codeCoverState.getSelection()) {
            /*
             * If CodeCover isn't enabled, it doesn't matter whether criteria
             * are selected or not.
             */
            setErrorMessage(null);
            setValid(true);
        } else if (this.tableViewer.getCheckedElements().length == 0) {
            setErrorMessage(SELECTION_ERROR_MESSAGE);
            setValid(false);
        } else {
            setErrorMessage(null);
            setValid(true);
        }
    }

    private final class CriteriaListLabelProvider extends LabelProvider
            implements ITableLabelProvider {

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object,
         *      int)
         */
        @Override
		public Image getColumnImage(Object element, int columnIndex) {
            // no images.
            return null;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object,
         *      int)
         */
        @Override
		public String getColumnText(Object element, int columnIndex) {
            if (element instanceof Criterion) {
                Criterion criterion = (Criterion) element;
                return criterion.getName();
            }
            return null;
        }
    }

    private final class CriteriaListContentProvider implements
            IStructuredContentProvider {

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
         */
        @Override
		public Object[] getElements(Object inputElement) {
            return (Object[]) inputElement;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IContentProvider#dispose()
         */
        @Override
		public void dispose() {
            // Do nothing here.
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer,
         *      java.lang.Object, java.lang.Object)
         */
        @Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
            // Do nothing here.
        }
    }

    private final class CriteriaListCheckListener implements
            ICheckStateListener {

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ICheckStateListener#checkStateChanged(org.eclipse.jface.viewers.CheckStateChangedEvent)
         */
        @Override
		public void checkStateChanged(CheckStateChangedEvent event) {
            CodeCoverPropertyPage.this.validatePage();
        }
    }
}