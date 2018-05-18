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

package org.codecover.eclipse.builder;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.Charset;
import java.text.DateFormat;
import java.util.Date;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
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
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.RuntimeProcess;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;

/**
 * This class is responsible for tasks which need to be done after
 * the termination of a SUT, e.g. merging the coverage log of the run
 * into the Test Session Container.
 * 
 * @author Tilmann Scheller
 * @version 1.0 ($Id: CodeCoverDebugListener.java 63 2009-09-27 12:13:06Z ahija $)
 */

public class CodeCoverDebugListener implements IDebugEventSetListener {

    private static final String RUNNABLE_TITLE = Messages
            .getString("CodeCoverDebugListener.RUNNABLE_TITLE"); //$NON-NLS-1$
    private static final String NO_COVERAGE_LOG_FOUND_TITLE = Messages.getString("CodeCoverDebugListener.NO_COVERAGE_LOG_FOUND_TITLE"); //$NON-NLS-1$
    private static final String NO_COVERAGE_LOG_FOUND_MSG = Messages.getString("CodeCoverDebugListener.NO_COVERAGE_LOG_FOUND_MSG"); //$NON-NLS-1$

    final Logger logger = CodeCoverPlugin.getDefault().getLogger();
    
    @Override
	public void handleDebugEvents(DebugEvent[] events) {
        for (DebugEvent e : events) {
            // we are only interested in events which indicate termination of a
            // process
            if (e.getKind() == DebugEvent.TERMINATE) {
                Object source = e.getSource();

                if (source instanceof RuntimeProcess) {
                    handleTerminatedProcess((RuntimeProcess) source);
                }
            }
        }
    }

    private void handleTerminatedProcess(RuntimeProcess process) {
        IProject project;
        ILaunch launch = process.getLaunch();
        ILaunchConfiguration config = launch.getLaunchConfiguration();

        try {
            // find out to which project the process belongs to
            IJavaProject javaProject = JavaRuntime.getJavaProject(config);

            if (javaProject != null) {
                project = javaProject.getProject();
                // TODO check whether project exists
                
                // we are only interested in projects which are selected
                // for use with CodeCover and which were running with
                // CodeCover enabled
                if (   CodeCoverPlugin.isCodeCoverActivated(project)
                    && CodeCoverClasspathProvider.isRunningWithCodeCover(config)) {
                        logger.debug("Termination of project " + project.getName()); //$NON-NLS-1$
                        mergeCoverageLog(project);
                }
            } else {
                logger.debug("Detected termination of a launch, but couldn't find associated Java Project"); //$NON-NLS-1$
            }
        } catch (CoreException e) {
            // sometimes ant scripts in referenced projects throw an
            // Launch configuration <name> references non-existing project <project>.
            if (e.getMessage() != null && e.getMessage().contains("references non-existing project")) { //$NON-NLS-1$
                logger.info("CoreException in CodeCoverDebugListener", e); //$NON-NLS-1$
            } else {
                logger.fatal("CoreException in CodeCoverDebugListener", e); //$NON-NLS-1$
            }
        }
    }
    
    private void mergeCoverageLog(IProject proj) {
        File workingDir = CodeCoverPlugin.getDefault().getPathToCoverageLogs(proj).toFile();
        File[] logs;
        String id;
        // use the current date/time for the default test session name
        final String testSessionName = DateFormat.getDateTimeInstance().format(new Date());
        final String testSessionComment = ""; //$NON-NLS-1$
        final Charset coverageLogCharset = MeasurementConstants.CHARSET;
        final File coverageLogFile;
        
        // get list of available coverage logs
        logs = workingDir.listFiles(new FilenameFilter() {

            @Override
			public boolean accept(File arg0, String arg1) {
                return arg1.startsWith("coverage-log"); //$NON-NLS-1$
            }

        });
        
        if (logs != null) {
            // find newest coverage log
            File mostRecent = null;
            long lastModified = 0;
            for (File log : logs) {
                if (log.lastModified() > lastModified) {
                    mostRecent = log;
                    lastModified = log.lastModified();
                }
            }

            coverageLogFile = mostRecent;
        } else {
            coverageLogFile = null;
        }
        
        // display a warning in case no coverage log is found
        if (coverageLogFile == null) {
            class NoCoverageLogFoundWarning implements Runnable {
                
                private String projName;
                
                NoCoverageLogFoundWarning(String projName) {
                    this.projName = projName;
                }
                
                @Override
				public void run() {
                    MessageDialog.openWarning(
                            CodeCoverPlugin.getDefault().getWorkbench()
                                    .getDisplay().getActiveShell(),
                            NO_COVERAGE_LOG_FOUND_TITLE,
                            String.format(NO_COVERAGE_LOG_FOUND_MSG, projName));
                }
                
            }
            Display.getDefault().asyncExec(new NoCoverageLogFoundWarning(proj.getName()));
            return;
        }

        try {
            CoverageLogParser idParser = new CoverageLogParser(coverageLogFile,
                    coverageLogCharset);

            id = idParser.getUID();
            logger.debug("Parsed \"" + coverageLogFile.getAbsolutePath() + //$NON-NLS-1$
                "\". Got its UID: " + id);  //$NON-NLS-1$
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
                return RUNNABLE_TITLE;
            }

            @Override
			public void run(ActiveTSContainerInfo activeTSCInfo,
                    IProgressMonitor monitor) throws CancelException {
                TestSessionContainer activeTSC = activeTSCInfo
                        .getTestSessionContainer();
                MASTBuilder builder = new MASTBuilder(logger);
                TestSession testSession = activeTSC.createTestSession(
                        testSessionName, testSessionComment, new Date());

                try { // this try only ensure that the monitor is left done()
                    monitor.beginTask(this.getDescription(), 3);

                    try {
                        CoverageLogParser logParser = new CoverageLogParser(
                                coverageLogFile, coverageLogCharset);
                        monitor.worked(1); // #1
                        CoverageResultLogReader coverageResultLogReader = new CoverageResultLogReader(
                                testSession, builder);
                        monitor.worked(1); // #2
                        logParser.CompilationUnit(coverageResultLogReader,
                                activeTSC.getId());
                        monitor.worked(1); // #3
                        coverageLogFile.delete();
                    } catch (IOException e) {
                        logger.fatal("Error accessing the coverage" + //$NON-NLS-1$
                                " log file", //$NON-NLS-1$
                                e);
                    } catch (WrongUIDException e) {
                        logger.error("The coverage log file does" + //$NON-NLS-1$
                                " not fit to the session" + //$NON-NLS-1$
                                " container! Process aborted.", //$NON-NLS-1$
                                e);
                    } catch (ParseException e) {
                        logger.fatal("Error parsing the coverage log", //$NON-NLS-1$
                                e);
                    }
                } finally {
                    monitor.done();
                }
            }
        };

        try {
            tscManager.setActiveTSContainer(match, tscRunnable, null);
        } catch (FileLoadException e) {
            logger.fatal("Error loading the test session" + //$NON-NLS-1$
                    " container", //$NON-NLS-1$
                    e);
        } catch (OutOfMemoryError e) {
            logger.fatal("Out of memory while loading the test" + //$NON-NLS-1$
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

}
