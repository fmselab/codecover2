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
import java.text.DateFormat;
import java.util.ConcurrentModificationException;
import java.util.Date;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.AbstractActiveTSContainerRunnable;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.tscmanager.ActiveTSContainerRunnable;
import org.codecover.eclipse.tscmanager.TSContainerInfo;
import org.codecover.eclipse.tscmanager.exceptions.CancelException;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.exceptions.NameAlreadyUsedException;
import org.codecover.model.utils.Logger;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * This dialog lets the user change the properties (name and comment) of a test
 * element (test case or test session). It is used in the Test Sessions view.
 * 
 * @see org.codecover.eclipse.views.TestSessionsView
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: TestElementPropertiesDialog.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class TestElementPropertiesDialog extends Dialog {

    private static final String RUNNABLE_DESCRIPTION = Messages.getString("TestElementPropertiesDialog.PROPERTIES_RUNNABLE_DESCRIPTION"); //$NON-NLS-1$
    private static final String TITLE_TEST_SESSION = Messages.getString("TestSessionsView.PROPERTIES_DIALOG_TITLE_TEST_SESSION"); //$NON-NLS-1$
    private static final String TITLE_TEST_CASE = Messages.getString("TestSessionsView.PROPERTIES_DIALOG_TITLE_TEST_CASE"); //$NON-NLS-1$
    private static final String LABEL_NAME_TEST_SESSION = Messages.getString("TestSessionsView.PROPERTIES_DIALOG_LABEL_NAME_TEST_SESSIONPROPERTIES_DIALOG_LABEL_NAME_TEST_SESSION"); //$NON-NLS-1$
    private static final String LABEL_COMMENT_TEST_SESSION = Messages.getString("TestSessionsView.PROPERTIES_DIALOG_LABEL_COMMENT_TEST_SESSION"); //$NON-NLS-1$
    private static final String LABEL_NAME_TEST_CASE = Messages.getString("TestSessionsView.PROPERTIES_DIALOG_LABEL_NAME_TEST_CASE"); //$NON-NLS-1$
    private static final String LABEL_COMMENT_TEST_CASE = Messages.getString("TestSessionsView.PROPERTIES_DIALOG_LABEL_COMMENT_TEST_CASE"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_RACE_COND_TITLE_TEST_CASE = Messages.getString("TestSessionsView.PROPERTIES_DIALOG_ERROR_RACE_COND_TITLE_TEST_CASE"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_RACE_COND_MSG_TEST_CASE = Messages.getString("TestSessionsView.PROPERTIES_DIALOG_ERROR_RACE_COND_MSG_TEST_CASE"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_NAME_IN_USE_TITLE_TEST_CASE = Messages.getString("TestSessionsView.PROPERTIES_DIALOG_ERROR_NAME_IN_USE_TITLE_TEST_CASE"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_NAME_IN_USE_MSG_TEST_CASE = Messages.getString("TestSessionsView.PROPERTIES_DIALOG_ERROR_NAME_IN_USE_MSG_TEST_CASE"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_RACE_COND_TITLE_TEST_SESSION = Messages.getString("TestSessionsView.PROPERTIES_DIALOG_ERROR_RACE_COND_TITLE_TEST_SESSION"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_RACE_COND_MSG_TEST_SESSION = Messages.getString("TestSessionsView.PROPERTIES_DIALOG_ERROR_RACE_COND_MSG_TEST_SESSION"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_NAME_IN_USE_TITLE_TEST_SESSION = Messages.getString("TestSessionsView.PROPERTIES_DIALOG_ERROR_NAME_IN_USE_TITLE_TEST_SESSION"); //$NON-NLS-1$
    private static final String DIALOG_ERROR_NAME_IN_USE_MSG_TEST_SESSION = Messages.getString("TestSessionsView.PROPERTIES_DIALOG_ERROR_NAME_IN_USE_MSG_TEST_SESSION"); //$NON-NLS-1$

    private final Object testElement;
    private final TSContainerInfo tscInfo;
    private Text txtName;
    private Text txtComment;
    private final Logger logger;

    /**
     * Constructs the dialog.
     * 
     * @param shell         the parent shell
     * @param testElement   the test element to change (a <code>TestCase</code>
     *                      or a <code>TestSession</code>)
     * @param tscInfo       the <code>TSContainerInfo</code>-representation of
     *                      the test session container the test element belongs
     *                      to
     * @param logger        the logger
     * 
     * @throws IllegalArgumentException if the given test element is neither a
     *                                  <code>TestCase</code> nor a
     *                                  <code>TestSession</code>
     */
    public TestElementPropertiesDialog( Shell shell,
                                        Object testElement,
                                        TSContainerInfo tscInfo,
                                        Logger logger)
            throws IllegalArgumentException
    {
        super(shell);
        if(testElement == null) {
            throw new NullPointerException(
                    "testElement mustn't be null");                //$NON-NLS-1$
        }
        if(!(testElement instanceof TestCase
                || testElement instanceof TestSession)) {
            throw new IllegalArgumentException(
                    "the given testElement must either be a" +     //$NON-NLS-1$
                    " TestCase or a TestSession");                 //$NON-NLS-1$
        }
        this.testElement = testElement;
        this.tscInfo = tscInfo;
        this.logger = logger;
        this.setShellStyle(getShellStyle() | SWT.RESIZE);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        if(this.testElement instanceof TestCase) {
            newShell.setText(TITLE_TEST_CASE);
        } else {
            newShell.setText(TITLE_TEST_SESSION);
        }
    }

    @Override
    protected Point getInitialSize() {
        return new Point(300,200);
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite)super.createDialogArea(parent);
        GridData gridData;
        GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        composite.setLayout(layout);

        // create top label with session name and date
        String testSessionDate
                = DateFormat.getDateTimeInstance().format(
                        this.getTestElementDate());
        Label lblSession = new Label(composite, SWT.NONE);
        lblSession.setText(this.getTestElementName()
                + " (" + testSessionDate + ")");     //$NON-NLS-1$ //$NON-NLS-2$
        // layout top label
        gridData = new GridData();
        gridData.horizontalSpan = 2;
        lblSession.setLayoutData(gridData);

        // create label of text field for test element name
        Label lblName = new Label(composite, SWT.LEAD);
        if(this.testElement instanceof TestCase) {
            lblName.setText(LABEL_NAME_TEST_CASE);
        } else {
            lblName.setText(LABEL_NAME_TEST_SESSION);
        }
        // create text field for test element name
        this.txtName = new Text(composite, SWT.SINGLE | SWT.BORDER);
        this.txtName.setText(this.getTestElementName());
        // layout text field for test element name
        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        this.txtName.setLayoutData(gridData);

        // create label of text field for test element comment
        Label lblComment = new Label(composite, SWT.LEAD);
        if(this.testElement instanceof TestCase) {
            lblComment.setText(LABEL_COMMENT_TEST_CASE);
        } else {
            lblComment.setText(LABEL_COMMENT_TEST_SESSION);
        }
        // layout label of text field for test element comment
        gridData = new GridData();
        gridData.verticalAlignment = GridData.BEGINNING;
        lblComment.setLayoutData(gridData);
        // create text field for test element comment
        this.txtComment = new Text(composite, SWT.MULTI | SWT.WRAP
                | SWT.V_SCROLL | SWT.BORDER);
        this.txtComment.setText(this.getTestElementComment());
        // layout text field for test element comment
        gridData = new GridData();
        gridData.horizontalAlignment = SWT.FILL;
        gridData.verticalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        this.txtComment.setLayoutData(gridData);

        return composite;
    }

    @Override
    protected void okPressed() {
        final ActiveTSContainerRunnable tscRunnable
                = new ChangePropertiesRunnable(this.txtName.getText(),
                        this.txtComment.getText());
        IRunnableWithProgress changeTC = new IRunnableWithProgress() {
            @Override
			public void run(IProgressMonitor monitor)
            throws InvocationTargetException, InterruptedException {
                try {
                    CodeCoverPlugin.getDefault().getTSContainerManager()
                            .setActiveTSContainer(
                                    TestElementPropertiesDialog.this.tscInfo,
                                    tscRunnable,
                                    null);
                } catch(FileLoadException e) {
                    throw new InvocationTargetException(e);
                } catch(OutOfMemoryError e) {
                    logger.error(
                            "Out of memory while loading test" +   //$NON-NLS-1$
                            " session container",                  //$NON-NLS-1$
                            new InvocationTargetException(e));
                    throw new InvocationTargetException(e);
                } catch(CancelException e) {
                    logger.warning(
                            "Canceled changing of test" +          //$NON-NLS-1$
                            " element properties. See the" +       //$NON-NLS-1$
                            " cause, for changes that" +           //$NON-NLS-1$
                            " already happened.",                  //$NON-NLS-1$
                            e);
                    throw new InvocationTargetException(e);
                }
            }
        };

        this.setReturnCode(Window.OK);
        try {
            CodeCoverPlugin.getDefault().getWorkbench()
                    .getProgressService().busyCursorWhile(changeTC);
            this.close();
        } catch(InvocationTargetException e) {
            if(e.getCause() instanceof ConcurrentModificationException
                    || e.getCause() instanceof FileLoadException
                    || e.getCause() instanceof OutOfMemoryError) {
                MessageDialog.openError(
                    this.getShell(),
                    (this.testElement instanceof TestCase) ?
                      DIALOG_ERROR_RACE_COND_TITLE_TEST_CASE
                    : DIALOG_ERROR_RACE_COND_TITLE_TEST_SESSION,
                    (this.testElement instanceof TestCase) ?
                      DIALOG_ERROR_RACE_COND_MSG_TEST_CASE
                    : DIALOG_ERROR_RACE_COND_MSG_TEST_SESSION);
                this.setReturnCode(Window.CANCEL);
                this.close();
            } else if(e.getCause() instanceof NameAlreadyUsedException) {
                MessageDialog.openError(
                    this.getShell(),
                    (this.testElement instanceof TestCase) ?
                      DIALOG_ERROR_NAME_IN_USE_TITLE_TEST_CASE
                    : DIALOG_ERROR_NAME_IN_USE_TITLE_TEST_SESSION,
                    (this.testElement instanceof TestCase) ?
                      DIALOG_ERROR_NAME_IN_USE_MSG_TEST_CASE
                    : DIALOG_ERROR_NAME_IN_USE_MSG_TEST_SESSION);
                this.setReturnCode(Window.CANCEL);
            } else if(!(e.getCause() instanceof CancelException)) {
                logger.error(
                        "Unknown error while changing" +           //$NON-NLS-1$
                        " test element properties",                //$NON-NLS-1$
                        e);
            }
        } catch (InterruptedException e) {
            /*
             * ignore because we didn't throw InterruptedExceptions
             */
        }
    }

    private final class ChangePropertiesRunnable
            extends AbstractActiveTSContainerRunnable {
        private final String newName;
        private final String newComment;
        public ChangePropertiesRunnable(String newName, String newComment) {
            super(TestElementPropertiesDialog.this
                    .getTestElementTestSessionContainer());
            this.newName = newName;
            this.newComment = newComment;
        }
        @Override
        public String getDescription() {
            return RUNNABLE_DESCRIPTION;
        }
        @Override
        public void runTask(ActiveTSContainerInfo activeTSCInfo,
                            IProgressMonitor monitor)
                throws InvocationTargetException, CancelException {
            int monitorScale = 1000;
            if(monitor.isCanceled()) {
                throw new CancelException(
                    "Canceled changing of test element" +      //$NON-NLS-1$
                    " properties. Nothing changed.");          //$NON-NLS-1$
            }

            // this try only ensures that the monitor is left done()
            try {
            monitor.beginTask(
                    RUNNABLE_DESCRIPTION,
                    2 * monitorScale);

            if(!this.newName.equals(TestElementPropertiesDialog.this
                    .getTestElementName())) {
                try {
                    TestElementPropertiesDialog.this
                            .setTestElementName(this.newName);
                } catch(NameAlreadyUsedException e) {
                    throw new InvocationTargetException(e);
                } catch(IllegalStateException e) {
                    // test element was deleted by an other thread
                    throw new InvocationTargetException(e);
                }
            }
            monitor.worked(1 * monitorScale);                          // #1

            if(!this.newComment.equals(TestElementPropertiesDialog.
                    this.getTestElementComment())) {
                try {
                    TestElementPropertiesDialog.this
                            .setTestElementComment(this.newComment);
                } catch(IllegalStateException e) {
                    // test element was deleted by an other thread
                    throw new InvocationTargetException(e);
                }
            }
            monitor.worked(1 * monitorScale);                          // #2
            } finally {
                monitor.done();
            }
        }
    }
    
    private TestSessionContainer getTestElementTestSessionContainer() {
        if(this.testElement instanceof TestCase) {
            return ((TestCase)this.testElement).getTestSession()
                    .getTestSessionContainer();
        } else /* if(this.testElement instanceof TestSession) */ {
            return ((TestSession)this.testElement).getTestSessionContainer();
        }
    }

    private Date getTestElementDate() {
        if(this.testElement instanceof TestCase) {
            return ((TestCase)this.testElement).getDate();
        } else /* if(this.testElement instanceof TestSession) */ {
            return ((TestSession)this.testElement).getDate();
        }
    }

    private String getTestElementName() {
        if(this.testElement instanceof TestCase) {
            return ((TestCase)this.testElement).getName();
        } else /* if(this.testElement instanceof TestSession) */ {
            return ((TestSession)this.testElement).getName();
        }
    }

    private String getTestElementComment() {
        if(this.testElement instanceof TestCase) {
            return ((TestCase)this.testElement).getComment();
        } else /* if(this.testElement instanceof TestSession) */ {
            return ((TestSession)this.testElement).getComment();
        }
    }

    private void setTestElementName(String name)
            throws NameAlreadyUsedException {
        if(this.testElement instanceof TestCase) {
            ((TestCase)this.testElement).setName(name);
        } else /* if(this.testElement instanceof TestSession) */ {
            ((TestSession)this.testElement).setName(name);
        }
    }

    private void setTestElementComment(String comment) {
        if(this.testElement instanceof TestCase) {
            ((TestCase)this.testElement).setComment(comment);
        } else /* if(this.testElement instanceof TestSession) */ {
            ((TestSession)this.testElement).setComment(comment);
        }
    }

}
