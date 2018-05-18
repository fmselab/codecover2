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

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.ConcurrentModificationException;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.TSContainerInfo;
import org.codecover.eclipse.tscmanager.exceptions.CancelException;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.utils.Logger;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;

/**
 * Allows the user to select test elements for deletion and deletes the selected
 * test elements if the user confirmed the deletion.
 * 
 * @see DeleteTestElementsConfirmDialog
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: DeleteTestElementsSelectDialog.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DeleteTestElementsSelectDialog extends Dialog {

    private static final String TITLE = Messages.getString("TestSessionsView.ELEMENTS_DELETE_DIALOG_TITLE"); //$NON-NLS-1$
    private static final String TEXT = Messages.getString("TestSessionsView.ELEMENTS_DELETE_DIALOG_TEXT"); //$NON-NLS-1$

    private static final String ACTION_TEXT_SELECT_ALL = Messages.getString("DeleteTestElementsSelectDialog.ACTION_TEXT_SELECT_ALL"); //$NON-NLS-1$
    private static final String ACTION_TEXT_DESELECT_ALL = Messages.getString("DeleteTestElementsSelectDialog.ACTION_TEXT_DESELECT_ALL"); //$NON-NLS-1$
    
    private static final String DIALOG_ERROR_RACE_COND_TITLE = Messages.getString("DeleteTestElementsSelectDialog.DIALOG_ERROR_RACE_COND_TITLE"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_RACE_COND_MSG = Messages.getString("DeleteTestElementsSelectDialog.DIALOG_ERROR_RACE_COND_MSG"); //$NON-NLS-1$
    
    private final IStructuredSelection selection;
    
    private final TSContainerInfo tscInfo;

    private final TestSessionContainer tsc;
    
    private CheckboxTreeViewer viewer;
    
    private final Logger logger;  

    /**
     * Constructs a dialog which allows the user to delete test elements of the
     * given test session container.
     * 
     * @param shell     the parent shell
     * @param selection a pre-selection
     * @param tscInfo   the <code>TSContainerInfo</code>-representation of the
     *                  test session container to delete test elements from
     * @param tsc       the test session container to delete test elements from
     * @param logger    a logger
     * 
     * @throws NullPointerException if the given
     *                              <code>TSContainerInfo</code>-representation
     *                              or the given test session container itself
     *                              is <code>null</code>
     */
    public DeleteTestElementsSelectDialog(  Shell shell,
                                            IStructuredSelection selection,
                                            TSContainerInfo tscInfo,
                                            TestSessionContainer tsc,
                                            Logger logger)
            throws NullPointerException {
        super(shell);
        if(tscInfo == null) {
            throw new NullPointerException(
                    "tscInfo mustn't be null");                    //$NON-NLS-1$
        }
        if(tsc == null) {
            throw new NullPointerException(
                    "tsc mustn't be null");                        //$NON-NLS-1$
        }
        this.selection = selection;
        this.tscInfo = tscInfo;
        this.tsc = tsc;
        this.logger = logger;
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
        lblViewer.setText(TEXT);

        // create tree viewer which lists test sessions and test cases
        this.viewer = TestSessionsViewerFactory.newCheckboxTreeViewer(
                composite,
                false,
                new Runnable() { @Override
				public void run() {} });
        createContextMenu(this.viewer);
        // set input of viewer
        this.viewer.setInput(this.tsc);
        if(this.selection != null) {
            Object[] sel = this.selection.toArray();
            // reveal preselected elements
            for(Object o : sel) {
                if(o instanceof TestCase) {
                    this.viewer.reveal(o);
                }
            }
            // check preselected elements
            this.viewer.setCheckedElements(sel);
        }
        
        // layout top label
        lblViewer.setLayoutData(new GridData());
        // layout viewer
        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.verticalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        this.viewer.getTree().getParent().setLayoutData(gridData);

        return composite;
    }

    private static void createContextMenu(final CheckboxTreeViewer viewer) {
        MenuManager menuMgr;
        Menu menu;
        // make select all and deselect all actions
        final Action selectAll = new Action() {
            @Override public void run() { viewer.setAllChecked(true); }
        };
        selectAll.setText(ACTION_TEXT_SELECT_ALL);
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
                manager.add(selectAll);
                manager.add(deselectAll);
            }
        });
        menu = menuMgr.createContextMenu(viewer.getTree());
        viewer.getTree().setMenu(menu);
    }
    
    @Override
    protected void okPressed() {
        Object[] elementsToDelete;
        Dialog confirmDialog;
        elementsToDelete = removeTestCasesOfContainedTestSessions(
                this.viewer.getCheckedElements());
        this.close();
        if(elementsToDelete.length > 0) {
            confirmDialog = new DeleteTestElementsConfirmDialog(
                    this.getParentShell(), elementsToDelete);
            confirmDialog.setBlockOnOpen(true);
            if(confirmDialog.open()
                    == DeleteTestElementsConfirmDialog.YES_BUTTON_INDEX) {
                try {
                    CodeCoverPlugin.getDefault().getTSContainerManager()
                           .setActiveTSContainer(this.tscInfo,
                               new DeleteTestElementsRunnable(elementsToDelete),
                               null);
                } catch(FileLoadException e) {
                    showRaceConditionErrorDialog(e);
                } catch(OutOfMemoryError e) {
                    showRaceConditionErrorDialog(e);
                    logger.error("Out of memory while loading" +   //$NON-NLS-1$
                            " test session container: "            //$NON-NLS-1$
                            + this.tscInfo.getPath().toString(),
                            new InvocationTargetException(e));
                } catch(InvocationTargetException e) {
                    if(e.getCause() instanceof ConcurrentModificationException){
                        showRaceConditionErrorDialog(e.getCause());
                    } else {
                        logger.error("Unknown error during" +      //$NON-NLS-1$
                                " deletion of" +                   //$NON-NLS-1$
                                " test sessions/test cases", e);   //$NON-NLS-1$
                    }
                } catch(CancelException e) {
                    logger.warning(
                            "User canceled deletion of" +          //$NON-NLS-1$
                            " test sessions/test cases during" +   //$NON-NLS-1$
                            " execution", e);                      //$NON-NLS-1$
                } 
            }
        }
    }

    private static void showRaceConditionErrorDialog(Throwable t) {
        ErrorDialog.openError(
                CodeCoverPlugin.getDefault()
                        .getWorkbench().getDisplay()
                        .getActiveShell(),
                DIALOG_ERROR_RACE_COND_TITLE,
                null,
                new Status( IStatus.ERROR,
                            CodeCoverPlugin.PLUGIN_ID,
                            IStatus.OK,
                            DIALOG_ERROR_RACE_COND_MSG,
                            t));
    }
    
    private static Object[] removeTestCasesOfContainedTestSessions(
            Object[] elements) {
        List<Object> stripped = new LinkedList<Object>(
                Arrays.asList(elements));
        for(Object element : elements) {
            if(element instanceof TestSession) {
                stripped.removeAll(
                        fetchContainedTestCasesOfTestSession(
                                elements, (TestSession)element));
            }
        }
        return stripped.toArray();
    }
    
    private static Set<TestCase> fetchContainedTestCasesOfTestSession(
            Object[] elements, TestSession testSession) {
        Set<TestCase> testCases = new HashSet<TestCase>();
        for(Object element : elements) {
            if(element instanceof TestCase
                && ((TestCase)element).getTestSession() == testSession){
                testCases.add((TestCase)element);
            }
        }
        return testCases;
    }
    
}
