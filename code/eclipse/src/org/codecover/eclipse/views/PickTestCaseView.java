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

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.actions.PickCodeActionDelegate;
import org.codecover.eclipse.actions.PickTestCaseActionDelegate;
import org.codecover.eclipse.actions.PickTestCaseActionDelegate.PickedTestCase;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.utils.EclipseMASTLinkage;
import org.codecover.model.TestCase;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.MetaDataObject;
import org.codecover.model.mast.SourceFile;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.texteditor.ITextEditor;

/**
 * Display the subset of test cases that cover the currently selected
 * MAST-Element.
 * <p>
 * The logic of detecting which test case covers a selection is implemented in
 * {@link PickTestCaseActionDelegate}.
 * 
 * @author Markus Wittlinger, Johannes Langauf
 * @version 1.0 ($Id: PickTestCaseView.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class PickTestCaseView extends ViewPart {

    private static final String LABEL_PICKED_TEST_CASE = Messages
            .getString("PickTestCaseView.LABEL_PICKED_TEST_CASE"); //$NON-NLS-1$

    private static final String LABEL_TEST_SESSION = Messages
            .getString("PickTestCaseView.LABEL_TEST_SESSION"); //$NON-NLS-1$

    private TableViewer tableViewer;

    private ISelectionListener selectionListener;

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public void createPartControl(Composite parent) {
        parent.setLayout(new GridLayout());

        this.tableViewer = new TableViewer(parent, SWT.HIDE_SELECTION
                | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER | SWT.FULL_SELECTION
                | SWT.MULTI);
        this.tableViewer.getTable().setLayoutData(
                new GridData(SWT.FILL, SWT.FILL, true, true));
        this.tableViewer.getTable().setHeaderVisible(true);

        this.tableViewer.setContentProvider(new PickTestCaseContentProvider());
        this.tableViewer.setLabelProvider(new PickTestCaseLabelProvider());

        createTableColumns();

        /* register Listener to track current (text) selection */
        selectionListener = new PickSelectionListener(this);
        ISelectionService selectionServ;
        selectionServ = getSite().getWorkbenchWindow().getSelectionService();
        selectionServ.addPostSelectionListener(selectionListener);

    }

    @Override
    public void dispose() {
        /* unregister Listener to track current (text) selection */
        ISelectionService selectionServ;
        selectionServ = getSite().getWorkbenchWindow().getSelectionService();
        selectionServ.removePostSelectionListener(selectionListener);
        super.dispose();

        /* clean up pointers */
        selectionListener = null;
        tableViewer = null;
    }

    private void createTableColumns() {
        TableColumn testCaseColumn = new TableColumn(this.tableViewer
                .getTable(), SWT.NONE);
        testCaseColumn.setText(LABEL_PICKED_TEST_CASE);
        testCaseColumn.pack();

        TableColumn testSessionColumn = new TableColumn(this.tableViewer
                .getTable(), SWT.NONE);
        testSessionColumn.setText(LABEL_TEST_SESSION);
        testSessionColumn.pack();

    }

    /**
     * Refreshes the {@link TableViewer} with the given input and packs the
     * columns.
     * 
     * @param input
     *            the new input for the viewer.
     */
    private final void refreshAndPackTable(Object input) {
        this.tableViewer.setInput(input);
        this.tableViewer.refresh();

        for (TableColumn column : this.tableViewer.getTable().getColumns()) {
            column.pack();
        }
    }

    /**
     * Displays the given {@link List} of {@link PickedTestCase}s in this view.
     * 
     * @param pickedTestCases
     *            the {@link List} of {@link PickedTestCase}s
     * @return true, if displayed; false on error
     */
    public boolean displayPickedTestCases(List<PickedTestCase> pickedTestCases) {
        if (this.tableViewer.getTable().isDisposed()) {
            return false;
        }

        if (pickedTestCases == null) {
            throw new NullPointerException("pickedTestCases == null"); //$NON-NLS-1$
        }

        refreshAndPackTable(pickedTestCases.toArray());
        return true;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        this.tableViewer.getTable().forceFocus();
    }

    private final class PickTestCaseContentProvider implements
            IStructuredContentProvider {

        /**
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
         */
        @Override
		public Object[] getElements(Object inputElement) {
            return (Object[]) inputElement;
        }

        /**
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IContentProvider#dispose()
         */
        @Override
		public void dispose() {
            // Do nothing here
        }

        /**
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer,
         *      java.lang.Object, java.lang.Object)
         */
        @Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
            // Do nothing here
        }
    }

    private class PickSelectionListener implements ISelectionListener {

        PickTestCaseView view;

        /**
         * Constructor
         * 
         * @param resultView
         *            the view in which to display the results.
         */
        public PickSelectionListener(PickTestCaseView resultView) {
            view = resultView;
        }

        @Override
		public void selectionChanged(IWorkbenchPart part, ISelection selection) {
            if (part instanceof ITextEditor) {
                ITextEditor editor = (ITextEditor) part;
                CodeCoverPlugin.getDefault().getLogger().debug(
                        "selection: " + selection.toString()); //$NON-NLS-1$
                Set<PickedTestCase> pickResult = pickTestCase(editor);
                List<PickedTestCase> pickResultList = new LinkedList<PickedTestCase>(pickResult);
                view.displayPickedTestCases(pickResultList);
            }
        }

        /**
         * Pick the test cases that fully cover the current selection of
         * <code>editor</code>. Uses the currently active test cases.
         * 
         * @param editor
         * @return The covering test cases.
         *         <p>
         *         null if no test session container is active. An empty set if
         *         nothing can be found at current selection.
         */
        private Set<PickedTestCase> pickTestCase(ITextEditor editor) {
            CodeCoverPlugin plugin = CodeCoverPlugin.getDefault();
            ActiveTSContainerInfo activeTSCI = plugin.getTSContainerManager()
                    .getActiveTSContainer();

            if (activeTSCI == null) {
                /* no tsc active */
            } else {
                Collection<TestCase> activeTestCases = activeTSCI
                        .getActiveTestCases();
                TestSessionContainer tsc = activeTSCI.getTestSessionContainer();

                Set<PickedTestCase> pickedTestCases;
                if (activeTestCases.isEmpty()) {
                    /* no test cases selected, therefore none can be picked */
                    pickedTestCases = Collections.emptySet();
                } else {
                    // XXX: improve performance by caching sourceFile until
                    // editor changes
                    SourceFile file = PickCodeActionDelegate.getSourceFile(
                            editor, tsc);
                    if (file == null) {
                        /* found no matching source file */
                        pickedTestCases = Collections.emptySet();
                    } else {
                        Location selection = EclipseMASTLinkage.getSelection(
                                editor, file);
                        MetaDataObject selectedElement;
                        // XXX: improve performance by limiting search scope for
                        // pickedElement
                        selectedElement = new PickTestCaseActionDelegate()
                                .findSurroundingElement(selection, tsc);

                        if (selectedElement == null) {
                            /* no element at selection */
                            pickedTestCases = Collections.emptySet();
                        } else {
                            pickedTestCases = PickTestCaseActionDelegate
                                    .pickTestCases(activeTestCases, tsc,
                                            selectedElement);
                        }
                    }
                }
                return pickedTestCases;
            }
            return null;
        }

    }

    private final class PickTestCaseLabelProvider extends LabelProvider
            implements ITableLabelProvider {

        /**
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object,
         *      int)
         */
        @Override
		public Image getColumnImage(Object element, int columnIndex) {
            switch (columnIndex) {
                case 0:
                    return CodeCoverPlugin.getDefault().getImageRegistry().get(
                            CodeCoverPlugin.Image.TEST_CASE.getPath());
                default:
                    return null;
            }
        }

        /**
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object,
         *      int)
         */
        @Override
		public String getColumnText(Object element, int columnIndex) {
            if (element instanceof PickedTestCase) {
                PickedTestCase pickedTestCase = (PickedTestCase) element;
                switch (columnIndex) {
                    case 0:
                        return pickedTestCase.toString();
                    case 1:
                        return pickedTestCase.getTestCase().getTestSession()
                                .getName();
                    default:
                        return null;
                }
            }
            return null;
        }
    }
}
