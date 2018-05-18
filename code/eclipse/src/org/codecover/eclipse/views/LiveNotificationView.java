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

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.util.Date;
import java.util.Observable;
import java.util.Observer;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.livenotification.LiveNotificationClient;
import org.codecover.eclipse.livenotification.LiveNotificationException;
import org.codecover.eclipse.livenotification.LiveNotificationClient.ConnectionState;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.tscmanager.ActiveTSContainerRunnable;
import org.codecover.eclipse.tscmanager.TSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerManager;
import org.codecover.eclipse.tscmanager.exceptions.CancelException;
import org.codecover.instrumentation.measurement.CoverageResultLogReader;
import org.codecover.instrumentation.measurement.MeasurementConstants;
import org.codecover.instrumentation.measurement.parser.CoverageLogParser;
import org.codecover.instrumentation.measurement.parser.ParseException;
import org.codecover.instrumentation.measurement.parser.WrongUIDException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.utils.Logger;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;

/**
 * This class provides a GUI to the {@link LiveNotificationClient}.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: LiveNotificationView.java 1673 2007-07-21 10:29:47Z
 *          wittlims $)
 */
public class LiveNotificationView extends ViewPart implements Observer {

    /**
     * The id of the view <br>
     * org.codecover.eclipse.views.LiveNotificationView
     */
    public static final String ID = "org.codecover.eclipse.views.LiveNotificationView"; //$NON-NLS-1$

    private static final String TAG_PORT = "Port"; //$NON-NLS-1$

    private static final String TAG_HOST = "Host"; //$NON-NLS-1$

    private static final String TAG_HOST_AND_PORT = "HostPortInfo"; //$NON-NLS-1$

    private static final String ERROR_ENTERING_THE_COVERAGE_LOG_READ_ERROR = Messages
            .getString("LiveNotificationView.ERROR_ENTERING_THE_COVERAGE_LOG_READ_ERROR"); //$NON-NLS-1$

    private static final String ERROR_ENTERING_COVERAGE_LOG_INCOMPATIBLE = Messages
            .getString("LiveNotificationView.ERROR_ENTERING_COVERAGE_LOG_INCOMPATIBLE"); //$NON-NLS-1$

    private static final String ERROR_LOADING_TSC = Messages
            .getString("LiveNotificationView.ERROR_LOADING_TSC"); //$NON-NLS-1$

    private static final String FORMAT_TEST_SESSION_ALREADY_EXSISTED = Messages
            .getString("LiveNotificationView.FORMAT_TEST_SESSION_ALREADY_EXSISTED"); //$NON-NLS-1$

    private static final String ERROR_WRITING_COVERAGE_LOG = Messages
            .getString("LiveNotificationView.ERROR_WRITING_COVERAGE_LOG"); //$NON-NLS-1$

    private static final String ERROR_LABEL = Messages
            .getString("LiveNotificationView.ERROR_LABEL"); //$NON-NLS-1$

    private static final String ERROR_INVALID_PORT = Messages
            .getString("LiveNotificationView.ERROR_INVALID_PORT"); //$NON-NLS-1$

    private static final String ERROR_PORT_CANNOT_BE_EMPTY = Messages
            .getString("LiveNotificationView.ERROR_PORT_CANNOT_BE_EMPTY"); //$NON-NLS-1$

    private static final String ERROR_HOST_CANNOT_BE_EMPTY = Messages
            .getString("LiveNotificationView.ERROR_HOST_CANNOT_BE_EMPTY"); //$NON-NLS-1$

    private static final String DISCONNECT_BUTTON_LABEL = Messages
            .getString("LiveNotificationView.DISCONNECT_BUTTON_LABEL"); //$NON-NLS-1$

    private static final String CONNECT_BUTTON_LABEL = Messages
            .getString("LiveNotificationView.CONNECT_BUTTON_LABEL"); //$NON-NLS-1$

    private static final String PORT_LABEL = Messages
            .getString("LiveNotificationView.PORT_LABEL"); //$NON-NLS-1$

    private static final String HOSTNAME_LABEL = Messages
            .getString("LiveNotificationView.HOSTNAME_LABEL"); //$NON-NLS-1$

    private static final String CONNECTION_LABEL = Messages
            .getString("LiveNotificationView.CONNECTION_LABEL"); //$NON-NLS-1$

    private static final String ERROR_TEST_CASE_NAME_CANNOT_BE_EMPTY = Messages
            .getString("LiveNotificationView.ERROR_TEST_CASE_NAME_CANNOT_BE_EMPTY"); //$NON-NLS-1$

    private static final String DOWNLOAD_COVERAGE_LOG_FILE_BUTTON_LABEL = Messages
            .getString("LiveNotificationView.DOWNLOAD_COVERAGE_LOG_FILE_BUTTON_LABEL"); //$NON-NLS-1$

    private static final String FINISH_TEST_SESSION_BUTTON_LABEL = Messages
            .getString("LiveNotificationView.FINISH_TEST_SESSION_BUTTON_LABEL"); //$NON-NLS-1$

    private static final String END_TEST_CASE_BUTTON_LABEL = Messages
            .getString("LiveNotificationView.END_TEST_CASE_BUTTON_LABEL"); //$NON-NLS-1$

    private static final String START_TEST_CASE_BUTTON_LABEL = Messages
            .getString("LiveNotificationView.START_TEST_CASE_BUTTON_LABEL"); //$NON-NLS-1$

    private static final String TEST_CASE_NAME_LABEL = Messages
            .getString("LiveNotificationView.TEST_CASE_NAME_LABEL"); //$NON-NLS-1$

    private static final String LIVE_NOTIFICATION_GROUP_LABEL = Messages
            .getString("LiveNotificationView.LIVE_NOTIFICATION_GROUP_LABEL"); //$NON-NLS-1$

    private static final String NOT_CONNECTED = Messages
            .getString("LiveNotificationView.NOT_CONNECTED"); //$NON-NLS-1$

    private static final String CONNECTED = Messages
            .getString("LiveNotificationView.CONNECTED"); //$NON-NLS-1$

    private static final String CONNECTED_WITHOUT_MBEAN = Messages
            .getString("LiveNotificationView.CONNECTED_WITHOUT_MBEAN"); //$NON-NLS-1$

    private static final String JOB_CONNECT_TITLE = Messages
            .getString("LiveNotificationView.JOB_CONNECT_TITLE"); //$NON-NLS-1$

    private static final String LAST_ACTION_DOWNLOADED_COVERAGE_LOG_FILE = Messages
            .getString("LiveNotificationView.LAST_ACTION_DOWNLOADED_COVERAGE_LOG_FILE"); //$NON-NLS-1$

    private static final String LAST_ACTION_FINISHED_TEST_SESSION = Messages
            .getString("LiveNotificationView.LAST_ACTION_FINISHED_TEST_SESSION"); //$NON-NLS-1$

    private static final String LAST_ACTION_ENDED_TEST_CASE = Messages
            .getString("LiveNotificationView.LAST_ACTION_ENDED_TEST_CASE"); //$NON-NLS-1$

    private static final String LAST_ACTION_STARTED_TEST_CASE = Messages
            .getString("LiveNotificationView.LAST_ACTION_STARTED_TEST_CASE"); //$NON-NLS-1$

    private static final String LAST_ACTION_DEFAULT = Messages
            .getString("LiveNotificationView.LAST_ACTION_DEFAULT"); //$NON-NLS-1$

    private final LiveNotificationClient liveNotificationClient;

    private Text testCaseNameText;

    private Text hostText;

    private Text portText;

    private Label stateLabel;

    private Label lastActionLabel;

    private Button disconnectButton;

    private Button connectButton;

    private Button startTestCaseButton;

    private Button endTestCaseButton;

    private Button endTestSessionButton;

    private Button downloadButton;

    private IMemento memento;

    /**
     * Constructor
     */
    public LiveNotificationView() {
        this.liveNotificationClient = new LiveNotificationClient();
        this.liveNotificationClient.addObserver(this);
    }

    /**
     * Create contents of the view part
     * 
     * @param parent
     */
    @Override
    public void createPartControl(Composite parent) {
        Composite mainComposite = new Composite(parent, SWT.NONE);
        mainComposite.setLayout(new GridLayout());

        createConnectionPane(mainComposite);

        createLiveNotificationPane(mainComposite);
    }

    /**
     * @param mainComposite
     */
    private void createLiveNotificationPane(Composite mainComposite) {
        final Group group = new Group(mainComposite, SWT.NONE);
        group.setText(LIVE_NOTIFICATION_GROUP_LABEL);

        GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = 2;
        gridLayout.makeColumnsEqualWidth = false;
        group.setLayout(gridLayout);
        group.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        Label testCaseNameLabel = new Label(group, SWT.NONE);
        testCaseNameLabel.setText(TEST_CASE_NAME_LABEL);

        this.testCaseNameText = new Text(group, SWT.BORDER);
        GridData hostTextData = new GridData(GridData.FILL_HORIZONTAL
                | GridData.GRAB_HORIZONTAL);
        this.testCaseNameText.setEnabled(false);
        hostTextData.grabExcessHorizontalSpace = true;
        // hostTextData.horizontalSpan = 2;
        this.testCaseNameText.setLayoutData(hostTextData);

        final Composite buttonComposite = new Composite(group, SWT.NONE);
        final GridLayout buttonLayout = new GridLayout();
        buttonLayout.makeColumnsEqualWidth = false;

        buttonComposite.setLayout(buttonLayout);
        GridData buttonData = new GridData(GridData.VERTICAL_ALIGN_BEGINNING);
        buttonData.horizontalSpan = 2;
        buttonComposite.setLayoutData(buttonData);

        this.startTestCaseButton = new Button(buttonComposite, SWT.PUSH);
        this.startTestCaseButton.setEnabled(false);
        this.startTestCaseButton.setText(START_TEST_CASE_BUTTON_LABEL);
        this.startTestCaseButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                startTestCase();
            }
        });
        this.startTestCaseButton.setLayoutData(new GridData(
                GridData.HORIZONTAL_ALIGN_FILL));

        this.endTestCaseButton = new Button(buttonComposite, SWT.PUSH);
        this.endTestCaseButton.setEnabled(false);
        this.endTestCaseButton.setText(END_TEST_CASE_BUTTON_LABEL);
        this.endTestCaseButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                endTestCase();
            }
        });
        this.endTestCaseButton.setLayoutData(new GridData(
                GridData.HORIZONTAL_ALIGN_FILL));

        this.endTestSessionButton = new Button(buttonComposite, SWT.PUSH);
        this.endTestSessionButton.setEnabled(false);
        this.endTestSessionButton.setText(FINISH_TEST_SESSION_BUTTON_LABEL);
        this.endTestSessionButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                finishTestSession();
            }
        });
        this.endTestSessionButton.setLayoutData(new GridData(
                GridData.HORIZONTAL_ALIGN_FILL));

        this.downloadButton = new Button(buttonComposite, SWT.PUSH);
        this.downloadButton.setEnabled(false);
        this.downloadButton.setText(DOWNLOAD_COVERAGE_LOG_FILE_BUTTON_LABEL);
        this.downloadButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                downloadCoverageLogFile();
            }
        });
        this.downloadButton.setLayoutData(new GridData(
                GridData.HORIZONTAL_ALIGN_FILL));

        this.lastActionLabel = new Label(buttonComposite, SWT.NONE);
        this.lastActionLabel.setLayoutData(new GridData(SWT.FILL, SWT.CENTER,
                true, false));
        this.lastActionLabel.setText(LAST_ACTION_DEFAULT);
    }

    private void startTestCase() {
        String name = this.testCaseNameText.getText();

        if (name.equals("")) { //$NON-NLS-1$
            StringBuilder sb = new StringBuilder();

            sb.append(ERROR_TEST_CASE_NAME_CANNOT_BE_EMPTY);

            showErrorMessage(sb.toString());
            return;
        }

        try {
            this.liveNotificationClient.startTestCase(name);

            this.lastActionLabel.setText(LAST_ACTION_STARTED_TEST_CASE);
        } catch (LiveNotificationException e) {
            showErrorMessage(e.getMessage());
            return;
        }
    }

    private void endTestCase() {
        try {
            this.liveNotificationClient.endTestCase();

            this.lastActionLabel.setText(LAST_ACTION_ENDED_TEST_CASE);

            this.testCaseNameText.forceFocus();
            this.testCaseNameText.selectAll();
        } catch (LiveNotificationException e) {
            showErrorMessage(e.getMessage());
        }
    }

    private void finishTestSession() {
        try {
            this.liveNotificationClient.finish();

            this.lastActionLabel.setText(LAST_ACTION_FINISHED_TEST_SESSION);
        } catch (LiveNotificationException e) {
            showErrorMessage(e.getMessage());
        }
    }

    private void downloadCoverageLogFile() {
        final Logger logger = CodeCoverPlugin.getDefault().getLogger();

        File tmpLogFile = null;
        try {
            // TODO somehow put this file into the codecover/temp folder
            tmpLogFile = File.createTempFile("coverageLog", null, null); //$NON-NLS-1$

            Writer writer = new FileWriter(tmpLogFile);

            this.liveNotificationClient.downloadCoverageLogFile(writer);

            this.lastActionLabel
                    .setText(LAST_ACTION_DOWNLOADED_COVERAGE_LOG_FILE);
        } catch (LiveNotificationException e) {
            logger.error(e.getMessage(), e);
        } catch (IOException e) {
            logger.error(ERROR_WRITING_COVERAGE_LOG, e);
        }

        if (tmpLogFile == null) {
            return;
        }

        final File logFile = tmpLogFile;
        String id;
        try {
            CoverageLogParser idParser = new CoverageLogParser(logFile,
                    MeasurementConstants.CHARSET);

            id = idParser.getUID();
        } catch (IOException e) {
            logger.fatal("Error accessing the coverage log file", //$NON-NLS-1$
                    e);
            return;
        } catch (ParseException e) {
            logger.fatal("Error parsing the coverage log", //$NON-NLS-1$
                    e);
            return;
        }

        TSContainerManager tscManager = CodeCoverPlugin.getDefault()
                .getTSContainerManager();

        // search for most recent TSC with the same ID
        TSContainerInfo match = null;
        for (TSContainerInfo info : tscManager.getTestSessionContainers(id)) {
            if (info.getId().equals(id)) {
                if (match == null) {
                    match = info;
                } else {
                    if (info.getDate().after(match.getDate())) {
                        match = info;
                    }
                }
            }
        }

        // in case we can't find a matching TSC, we simply do nothing
        if (match == null) {
            return;
        }

        ActiveTSContainerRunnable tscRunnable = new ActiveTSContainerRunnable() {
            @Override
			public String getDescription() {
                return "Creating new test session from last run"; //$NON-NLS-1$
            }

            @Override
			public void run(ActiveTSContainerInfo activeTSCInfo,
                    IProgressMonitor monitor) throws CancelException {

                TestSessionContainer activeTSC = activeTSCInfo
                        .getTestSessionContainer();
                MASTBuilder builder = new MASTBuilder(logger);
                String testSessionName = "livenotificationrun"; //$NON-NLS-1$
                String testSessionComment = ""; //$NON-NLS-1$

                TestSession testSession = activeTSC.createTestSession(
                        testSessionName, testSessionComment, new Date());

                try {
                    /*
                     * this try only ensure that the monitor is left done()
                     */
                    monitor.beginTask(this.getDescription(), 3);

                    /*
                     * If the name of the test session already existed, the user
                     * should know, that the session is called something like
                     * %test session name (1)%
                     */
                    if (!testSessionName.equals(testSession.getName())) {
                        String format = FORMAT_TEST_SESSION_ALREADY_EXSISTED;
                        logger.warning(String.format(format, testSessionName,
                                testSession.getName()));
                    }

                    try {
                        CoverageLogParser logParser = new CoverageLogParser(
                                logFile, MeasurementConstants.CHARSET);

                        monitor.worked(1); // #1
                        CoverageResultLogReader coverageResultLogReader = new CoverageResultLogReader(
                                testSession, builder);
                        monitor.worked(1); // #2
                        logParser.CompilationUnit(coverageResultLogReader,
                                activeTSC.getId());
                        monitor.worked(1); // #3
                    } catch (IOException e) {
                        logger.fatal("Error accessing the coverage" + //$NON-NLS-1$
                                " log file", //$NON-NLS-1$
                                e);
                    } catch (WrongUIDException e) {
                        logger.error(ERROR_ENTERING_COVERAGE_LOG_INCOMPATIBLE,
                                e);
                    } catch (ParseException e) {
                        logger.error(
                                ERROR_ENTERING_THE_COVERAGE_LOG_READ_ERROR, e);
                    }
                } finally {
                    monitor.done();
                }
            }
        };

        try {
            CodeCoverPlugin.getDefault().getTSContainerManager()
                    .setActiveTSContainer(match, tscRunnable, null);
        } catch (FileLoadException e) {
            // TODO: remove this? (this exception is already logged by setActiveTSContainer
            logger.error(ERROR_LOADING_TSC, e);
        } catch (OutOfMemoryError e) {
            // TODO: please review, i18n this like above?
            logger.error("Out of memory while loading test" + //$NON-NLS-1$
                    " session container", //$NON-NLS-1$
                    new InvocationTargetException(e));
        } catch (InvocationTargetException e) {
            logger.error("Unknown error while merging" + //$NON-NLS-1$
                    " coverage log", //$NON-NLS-1$
                    e);
        } catch (CancelException e) {
            // won't be thrown if no progress monitor was passed
            logger.warning("Canceled activating test session" + //$NON-NLS-1$
                    " container", e); //$NON-NLS-1$
        }
    }

    /**
     * @param mainComposite
     */
    private void createConnectionPane(Composite mainComposite) {
        final Group group = new Group(mainComposite, SWT.NONE);
        group.setText(CONNECTION_LABEL);

        GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = 2;
        gridLayout.makeColumnsEqualWidth = false;
        group.setLayout(gridLayout);
        group.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        Label hostLabel = new Label(group, SWT.NONE);
        hostLabel.setText(HOSTNAME_LABEL);

        String hostValue = ""; //$NON-NLS-1$
        String portValue = ""; //$NON-NLS-1$

        if (this.memento != null) {
            IMemento child = this.memento.getChild(TAG_HOST_AND_PORT);
            if (child != null) {
                hostValue = child.getString(TAG_HOST);
                portValue = child.getString(TAG_PORT);
            }
        }

        this.hostText = new Text(group, SWT.BORDER);
        GridData hostTextData = new GridData(GridData.FILL_HORIZONTAL
                | GridData.GRAB_HORIZONTAL);
        hostTextData.grabExcessHorizontalSpace = true;
        this.hostText.setLayoutData(hostTextData);
        this.hostText.setText(hostValue);

        Label portLabel = new Label(group, SWT.NONE);
        portLabel.setText(PORT_LABEL);

        this.portText = new Text(group, SWT.BORDER);
        GridData portData = new GridData(GridData.FILL_HORIZONTAL
                | GridData.GRAB_HORIZONTAL);
        portData.grabExcessHorizontalSpace = true;
        this.portText.setLayoutData(portData);
        this.portText.setText(portValue);

        final Composite buttonComposite = new Composite(group, SWT.NONE);
        final GridLayout buttonLayout = new GridLayout();
        buttonLayout.makeColumnsEqualWidth = false;
        buttonLayout.numColumns = 2;

        buttonComposite.setLayout(buttonLayout);
        GridData buttonData = new GridData(GridData.VERTICAL_ALIGN_BEGINNING);
        buttonData.horizontalSpan = 2;
        buttonComposite.setLayoutData(buttonData);

        this.connectButton = new Button(buttonComposite, SWT.PUSH);
        this.connectButton.setEnabled(true);
        this.connectButton.setText(CONNECT_BUTTON_LABEL);
        this.connectButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                connect();
            }
        });
        this.connectButton.setLayoutData(new GridData(
                GridData.HORIZONTAL_ALIGN_FILL));

        this.disconnectButton = new Button(buttonComposite, SWT.PUSH);
        this.disconnectButton.setEnabled(false);
        this.disconnectButton.setText(DISCONNECT_BUTTON_LABEL);
        this.disconnectButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                disconnect();
            }
        });
        this.disconnectButton.setLayoutData(new GridData(
                GridData.HORIZONTAL_ALIGN_FILL));

        this.stateLabel = new Label(group, SWT.NONE);
        this.stateLabel.setText(NOT_CONNECTED);
    }

    private void disconnect() {

        try {
            this.liveNotificationClient.disconnect();
        } catch (LiveNotificationException e) {
            // It should not be necessary to inform the user of this error,
            // since the connection was killed anyways.

            // showErrorMessage(e.getMessage());
        }
    }

    private void connect() {
        String hostName = this.hostText.getText();
        String portString = this.portText.getText();

        if (hostName.equals("")) { //$NON-NLS-1$
            showErrorMessage(ERROR_HOST_CANNOT_BE_EMPTY);
            return;
        }

        if (portString.equals("")) { //$NON-NLS-1$
            showErrorMessage(ERROR_PORT_CANNOT_BE_EMPTY);
            return;
        }

        Integer port = 0;
        try {
            port = Integer.parseInt(portString);
        } catch (NumberFormatException e) {
            port = -1;
        }

        if (port < 0) {
            showErrorMessage(ERROR_INVALID_PORT);
            return;
        }

        (new ConnectJob(this.liveNotificationClient, hostName, port))
                .schedule();
    }

    private class ConnectJob extends Job {
        private final LiveNotificationClient client;

        private final String hostname;

        private final Integer port;

        /**
         * Constructor.
         * 
         * @param client
         *            the {@link LiveNotificationClient}
         * @param hostname
         *            the name of the host to connect to.
         * @param port
         *            the name of the port to use.
         */
        public ConnectJob(LiveNotificationClient client, String hostname,
                Integer port) {
            super(LiveNotificationView.JOB_CONNECT_TITLE);
            this.setUser(true);
            this.client = client;
            this.hostname = hostname;
            this.port = port;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            try {
                this.client.connect(this.hostname, this.port);
            } catch (final LiveNotificationException e) {
                Display.getDefault().asyncExec(new Runnable() {
                    @Override
					public void run() {
                        showErrorMessage(e.getMessage());
                    }
                });
                return Status.CANCEL_STATUS;
            }
            return Status.OK_STATUS;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        if (this.testCaseNameText.isEnabled()) {
            this.testCaseNameText.forceFocus();
        } else {
            this.hostText.forceFocus();
        }
    }

    private void handleConnectionState(ConnectionState connectionState) {
        switch (connectionState) {
            case NOT_CONNECTED:
                this.connectButton.setEnabled(true);
                this.disconnectButton.setEnabled(false);
                this.startTestCaseButton.setEnabled(false);
                this.endTestCaseButton.setEnabled(false);
                this.endTestSessionButton.setEnabled(false);
                this.downloadButton.setEnabled(false);
                this.testCaseNameText.setEnabled(false);
                this.stateLabel.setText(NOT_CONNECTED);
                break;
            case CONNECTED_NO_MBEAN:
                this.connectButton.setEnabled(false);
                this.disconnectButton.setEnabled(true);
                this.startTestCaseButton.setEnabled(false);
                this.endTestCaseButton.setEnabled(false);
                this.endTestSessionButton.setEnabled(false);
                this.downloadButton.setEnabled(false);
                this.testCaseNameText.setEnabled(false);
                this.stateLabel.setText(CONNECTED_WITHOUT_MBEAN);
                break;
            case CONNECTED_WITH_MBEAN:
                this.connectButton.setEnabled(false);
                this.disconnectButton.setEnabled(true);
                this.startTestCaseButton.setEnabled(true);
                this.endTestCaseButton.setEnabled(true);
                this.endTestSessionButton.setEnabled(true);
                this.downloadButton.setEnabled(true);
                this.testCaseNameText.setEnabled(true);
                this.stateLabel.setText(CONNECTED);
                break;
        }
    }

    private void showErrorMessage(String text) {
        MessageDialog.openError(this.getViewSite().getShell(), ERROR_LABEL,
                text);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
     */
    @Override
	public void update(Observable o, Object arg) {
        final ConnectionState connectionState = (ConnectionState) arg;
        Display.getDefault().asyncExec(new Runnable() {
            @Override
			public void run() {
                handleConnectionState(connectionState);
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
     */
    @Override
    public void saveState(IMemento memento) {
        super.saveState(memento);

        IMemento mem = memento.createChild(TAG_HOST_AND_PORT);
        mem.putString(TAG_HOST, this.hostText.getText());
        mem.putString(TAG_PORT, this.portText.getText());
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite,
     *      org.eclipse.ui.IMemento)
     */
    @Override
    public void init(IViewSite site, IMemento memento) throws PartInitException {
        super.init(site, memento);
        this.memento = memento;
    }
}
