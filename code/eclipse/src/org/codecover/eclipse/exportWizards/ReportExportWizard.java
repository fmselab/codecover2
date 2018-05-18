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
import java.util.Date;
import java.util.List;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.exportWizards.MainExportPage.Types;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.metrics.MetricProvider;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileSaveException;
import org.codecover.model.extensions.PluginManager;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.ProgressHandler;
import org.codecover.report.Report;
import org.codecover.report.Template;
import org.codecover.report.exceptions.FileCreationException;
import org.codecover.report.exceptions.LoadReportGeneratorException;
import org.codecover.report.exceptions.ReportException;
import org.codecover.report.exceptions.ReportIOException;
import org.codecover.report.exceptions.ReportTemplateApplyException;
import org.codecover.report.exceptions.TemplateException;
import org.codecover.report.exceptions.TemplateIncompatibleException;
import org.codecover.report.exceptions.TemplateParseException;
import org.codecover.report.exceptions.TemplateReadException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IExportWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.statushandlers.StatusManager;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: ReportExportWizard.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ReportExportWizard extends Wizard implements IExportWizard {

    private static final String REPORT_PAGE_TITLE = Messages
            .getString("ReportExportWizard.REPORT_PAGE_TITLE"); //$NON-NLS-1$

    private static final String EXPORT_TSC_PAGE_TITLE = Messages
            .getString("ReportExportWizard.EXPORT_TSC_PAGE_TITLE"); //$NON-NLS-1$

    private static final String EXPORT_WIZARD_TITLE = Messages
            .getString("ReportExportWizard.EXPORT_WIZARD_TITLE"); //$NON-NLS-1$

    private static final String EXPORT_SESSIONS_TITLE = Messages
            .getString("ReportExportWizard.EXPORT_SESSIONS_TITLE"); //$NON-NLS-1$

    private static final String GENERATE_REPORT_TITLE = Messages
            .getString("ReportExportWizard.GENERATE_REPORT_TITLE"); //$NON-NLS-1$

    private static final String ERROR_INTERNAL = Messages
            .getString("ReportExportWizard.ERROR_INTERNAL"); //$NON-NLS-1$

    private static final String ERROR_TEMPLATE_APPLY = Messages
            .getString("ReportExportWizard.ERROR_TEMPLATE_APPLY"); //$NON-NLS-1$

    private static final String ERROR_WRITE = Messages
            .getString("ReportExportWizard.ERROR_WRITE"); //$NON-NLS-1$

    private static final String ERROR_OPEN_WRITE = Messages
            .getString("ReportExportWizard.ERROR_OPEN_WRITE"); //$NON-NLS-1$

    private static final String ERROR_GENERATOR_LOAD = Messages
            .getString("ReportExportWizard.ERROR_GENERATOR_LOAD"); //$NON-NLS-1$

    private static final String ERROR_TEMPLATE_IO = Messages
            .getString("ReportExportWizard.ERROR_TEMPLATE_IO"); //$NON-NLS-1$

    private static final String ERROR_TEMPLATE_VERSION_TEMPLATE = Messages
            .getString("ReportExportWizard.ERROR_TEMPLATE_VERSION_TEMPLATE"); //$NON-NLS-1$

    private static final String ERROR_TEMPLATE_VERSION_REPORT = Messages
            .getString("ReportExportWizard.ERROR_TEMPLATE_VERSION_REPORT"); //$NON-NLS-1$

    private static final String ERROR_TEMPLATE_SYNTAX = Messages
            .getString("ReportExportWizard.ERROR_TEMPLATE_SYNTAX"); //$NON-NLS-1$

    private static final String ERROR_TEMPLATE = Messages
            .getString("ReportExportWizard.ERROR_TEMPLATE"); //$NON-NLS-1$

    private static final String DIALOG_NO_MODIFICATION_DURING_REPORT_GENERATION = Messages
            .getString("ReportExportWizard.DIALOG_NO_MODIFICATION_DURING_REPORT_GENERATION"); //$NON-NLS-1$

    static final Logger logger = CodeCoverPlugin.getDefault().getLogger();

    private MainExportPage exportPage;

    private ReportExportPage reportPage;

    /**
     * The sessions which are exported. This field is used by the jobs which
     * execute the export.
     * 
     * @see GenerateReportJob
     * @see ExportSessionsJob
     */
    List<TestSession> sessionList;
    
    List<TestCase> testCaseList;

    /**
     * The file to export to. This field is used by the jobs which execute the
     * export.
     * 
     * @see GenerateReportJob
     * @see ExportSessionsJob
     */
    File destinationFile;

    /**
     * The file with the template, only used for report generation. This field
     * is used by the job which executes the report generation.
     * 
     * @see GenerateReportJob
     */
    File templateFile;

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.Wizard#performFinish()
     */
    @Override
    public boolean performFinish() {
        Types type = this.exportPage.getSelectedType();
        ActiveTSContainerInfo activeTSCInfo = CodeCoverPlugin.getDefault()
                .getTSContainerManager().getActiveTSContainer();;
        this.destinationFile = this.exportPage.getDestinationLocation();
        this.sessionList = this.exportPage.getSelectedTestSessions();
        this.testCaseList = this.exportPage.getSelectedTestCases();

        ensureFileIsValid(this.destinationFile);

        /*
         * if the export is about the currently active test session container,
         * the user is advised not to delete or modify test elements during
         * report generation
         */ 
        if(activeTSCInfo != null
                && !this.testCaseList.isEmpty() 
                && activeTSCInfo.getTestSessionContainer()
                        == this.testCaseList.get(0).getTestSession()
                                .getTestSessionContainer()) {
            MessageDialog.openInformation(this.getShell(), EXPORT_WIZARD_TITLE,
                    DIALOG_NO_MODIFICATION_DURING_REPORT_GENERATION);
        }
        switch (type) {
            case TEST_SESSION_CONTAINER:
                (new ExportSessionsJob()).schedule();
                break;
            case REPORT:
                this.templateFile = this.reportPage.getSelectedTemplate();
                (new GenerateReportJob()).schedule();
                break;
        }

        return true; // because scheduling will always work
    }

    private boolean ensureFileIsValid(File file) {
        if (file.exists()) {
            file.delete();
        } else if (!file.isDirectory()) {
            File parent = file.getParentFile();
            if (parent != null) {
                file.getParentFile().mkdirs();
            }
        }
        return true;
    }

    /**
     * Handles the {@link Report} generation.
     * <p>
     * The following fields of {@link ReportExportWizard} are used as parameters
     * of the export operation:
     * <dl>
     * <dt>destinationFile</dt>
     * <dd>the file, the {@link Report} is created in</dd>
     * <dt>sessionList</dt>
     * <dd>the list of {@link TestSession}s to be used in creating the
     * {@link Report}</dd>
     * <dt>templateFile</dt>
     * <dd>the {@link Template} used in creation of the {@link Report}</dd>
     * </dl>
     * </p>
     */
    private class GenerateReportJob extends Job {

        /**
         * Constructor.
         */
        public GenerateReportJob() {
            super(GENERATE_REPORT_TITLE);
            this.setUser(true);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            // the parameters of the report generation
            File destinationFile = ReportExportWizard.this.destinationFile;
            List<TestCase> testCaseList = ReportExportWizard.this.testCaseList;
            File templateFile = ReportExportWizard.this.templateFile;
            PluginManager pluginManager = CodeCoverPlugin.getDefault()
                    .getEclipsePluginManager().getPluginManager();
            Logger logger = ReportExportWizard.logger;

            ProgressHandlerToProgressMonitor progressAdapter;

            if (testCaseList.isEmpty()) {
                return Status.CANCEL_STATUS;
            }

            Report report = new Report(pluginManager, MetricProvider
                    .getAvailabeMetrics(pluginManager, logger), logger);
            try {
                report.setTemplate(templateFile.getAbsolutePath());
                report.setFileSystemPath(destinationFile.getAbsolutePath());
                report.setTestCases(testCaseList);

                progressAdapter = new ProgressHandlerToProgressMonitor(
                        (monitor != null) ? monitor : new NullProgressMonitor());
                try {
                    report.setProgressHandler(progressAdapter);
                    report.generateReport();
                } finally {
                    progressAdapter.done();
                }
            } catch (FileCreationException e) {
                showReportError(ERROR_OPEN_WRITE, e);
                return Status.CANCEL_STATUS;
            } catch (TemplateException e) {
                String mesg = ERROR_TEMPLATE;
                if (e instanceof TemplateParseException) {
                    mesg = ERROR_TEMPLATE_SYNTAX;
                } else if (e instanceof TemplateIncompatibleException) {
                    if (((TemplateIncompatibleException) e).isInner()) {
                        mesg += ERROR_TEMPLATE_VERSION_REPORT;
                    } else if (((TemplateIncompatibleException) e).isOuter()) {
                        mesg += ERROR_TEMPLATE_VERSION_TEMPLATE;
                    } else {
                        // not possible
                        logger.error("fix me: someone changed the exceptions"); //$NON-NLS-1$
                    }
                } else if (e instanceof TemplateReadException) {
                    mesg = String.format(ERROR_TEMPLATE_IO, templateFile
                            .getAbsolutePath());
                }
                showReportError(mesg, e);
                return Status.CANCEL_STATUS;
            } catch (LoadReportGeneratorException e) {
                String generator;
                Template template = report.getTemplate();
                generator = template.getPluginName() + "/" //$NON-NLS-1$
                        + template.getReportGeneratorName();
                String mesg = String.format(ERROR_GENERATOR_LOAD, generator);
                showReportError(mesg, e);
                return Status.CANCEL_STATUS;
            } catch (ReportIOException e) {
                String mesg = ERROR_WRITE;
                showReportError(mesg, e);
            } catch (ReportTemplateApplyException e) {
                String mesg = ERROR_TEMPLATE_APPLY;
                showReportError(mesg, e);

                /*
                 * unexpected internal errors: inform user of internal error and
                 * write to log
                 */
            } catch (ReportException e) {
                // unexpected programming error: subclasses must cover all cases
                showReportError(ERROR_INTERNAL, e);
                return Status.CANCEL_STATUS;
            } catch (RuntimeException e) {
                // unexpected programming error
                showReportError(ERROR_INTERNAL, e);
                return Status.CANCEL_STATUS;
            }

            return Status.OK_STATUS;
        }

        /**
         * popup an error dialog
         * 
         * @param mesg
         *            localized message
         * @param details
         *            exception to log for details
         */
        private IStatus showReportError(String mesg, Exception details) {
            // prepare content of dialog
            IStatus status = new Status(IStatus.ERROR,
                    CodeCoverPlugin.PLUGIN_ID, IStatus.ERROR, mesg, details);

            // show in dialog (and log)
            StatusManager.getManager().handle(status, StatusManager.SHOW);

            return status;
        }

        /**
         * Adapter for using {@link IProgressMonitor} where actually a
         * {@link ProgressHandler} is needed. The methods <code>beginTask</code>
         * and <code>done</code> are called by this adapter, if you follow the
         * following coding advice.
         * <p>
         * Use this class like this:
         * 
         * <pre>
         * ProgressHandlerToProgressMonitor adapter = new ProgressHandlerToProgressMonitor(
         *         theIProgressMonitor);
         * try {
         *     methodWhichWantsAProgressHandler(adapter);
         * } finally {
         *     adapter.done();
         * }
         * </pre>
         * 
         * </p>
         */
        private class ProgressHandlerToProgressMonitor implements
                ProgressHandler {

            private static final int COMPLETED_STATUS = 1000;

            private IProgressMonitor monitor;

            private int status;

            /**
             * Constructor.
             * 
             * @param monitor
             *            the {@link IProgressMonitor}
             */
            public ProgressHandlerToProgressMonitor(IProgressMonitor monitor) {
                this.monitor = monitor;
                this.status = 0;
                monitor.beginTask(GENERATE_REPORT_TITLE, COMPLETED_STATUS);
            }

            /*
             * (non-Javadoc)
             * 
             * @see org.codecover.model.utils.ProgressHandler#setProgress(float)
             */
            @Override
			public void setProgress(float progress) {
                int updatedStatus = (int) Math
                        .ceil(COMPLETED_STATUS * progress);
                // only update progress monitor if progress advances
                if (updatedStatus > this.status) {
                    this.monitor.worked(updatedStatus - this.status);
                    this.status = updatedStatus;
                }
            }

            /**
             * Tells the {@link IProgressMonitor}, that the job is done.
             */
            public void done() {
                this.monitor.done();
            }

        }

    }

    /**
     * Handles the export of {@link TestSession}s.
     * <p>
     * The following fields of {@link ReportExportWizard} are used as parameters
     * of the export operation:
     * <dl>
     * <dt>destinationFile</dt>
     * <dd>the file, the {@link TestSession}s are exported to</dd>
     * <dt>sessionList</dt>
     * <dd>the list of {@link TestSession}s to be exported</dd>
     * </dl>
     * </p>
     */
    private class ExportSessionsJob extends Job {

        /**
         * Constructor.
         */
        public ExportSessionsJob() {
            super(EXPORT_SESSIONS_TITLE);
            this.setUser(true);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            // the parameters of this export operation
            List<TestSession> sessionList = ReportExportWizard.this.sessionList;
            File destinationFile = ReportExportWizard.this.destinationFile;

            if (sessionList.isEmpty()) {
                return Status.CANCEL_STATUS;
            }

            TestSessionContainer tsc = sessionList.get(0)
                    .getTestSessionContainer();

            TestSessionContainer destinationContainer = new TestSessionContainer(
                    tsc.getCode(), tsc.getLogger(), tsc.getFiles(), tsc
                            .getCriteria(), tsc.getId(), tsc.getDate());

            // copy them into the destinationTestSessionContainer.
            for (TestSession sourceTestSession : sessionList) {
                String testSessionName = sourceTestSession.getName();
                String testSessionComment = sourceTestSession.getComment();
                Date testSessionDate = sourceTestSession.getDate();

                // Create the destination test session with the data from the
                // old
                // test session
                TestSession destinationTestSession = destinationContainer
                        .createTestSession(testSessionName, testSessionComment,
                                testSessionDate);

                // Copy each test case from the source test session into the
                // destination test session
                for (TestCase sourceTestCase : sourceTestSession.getTestCases()) {
                    destinationTestSession
                            .copyTestCaseIntoTestSession(sourceTestCase);
                }
            }

            try {
                destinationContainer.save(destinationFile);
            } catch (FileSaveException e) {
                logger.error("A FileSaveException occured", e); //$NON-NLS-1$
                return Status.CANCEL_STATUS;
            }

            return Status.OK_STATUS;
        }

    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.Wizard#canFinish()
     */
    @Override
    public boolean canFinish() {
        Types type = this.exportPage.getSelectedType();
        switch (type) {
            case TEST_SESSION_CONTAINER:
                return this.exportPage.isPageComplete();
            case REPORT:
                return this.exportPage.isPageComplete()
                        && this.reportPage.isPageComplete();
        }
        return super.canFinish();
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
     *      org.eclipse.jface.viewers.IStructuredSelection)
     */
    @Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
        setWindowTitle(EXPORT_WIZARD_TITLE);
        setNeedsProgressMonitor(true);
        this.exportPage = new MainExportPage(EXPORT_TSC_PAGE_TITLE);
        this.reportPage = new ReportExportPage(REPORT_PAGE_TITLE);
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.Wizard#addPages()
     */
    @Override
    public void addPages() {
        super.addPages();
        addPage(this.exportPage);
        addPage(this.reportPage);
    }

}
