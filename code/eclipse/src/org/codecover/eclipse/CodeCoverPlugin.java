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

package org.codecover.eclipse;

import java.net.URL;

import org.codecover.eclipse.annotation.EditorTracker;
import org.codecover.eclipse.builder.CodeCoverDebugListener;
import org.codecover.eclipse.junit.JUnitLaunchConfigurationDelegate;
import org.codecover.eclipse.tscmanager.TSContainerManager;
import org.codecover.eclipse.utils.ImageProvider;
import org.codecover.eclipse.views.BooleanAnalyzer;
import org.codecover.model.utils.LogLevel;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.criteria.Criterion;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 *
 * @author Robert Hanussek, Tilmann Scheller, Markus Wittlinger
 * @version 1.0 ($Id: CodeCoverPlugin.java 75 2011-06-28 13:28:30Z dobrowolsk $)
 */
public class CodeCoverPlugin extends AbstractUIPlugin {

    /**
     * A class holding references to the images used in the plugin.
     *
     * @author Robert Hanussek, Markus Wittlinger
     * @version 1.0 ($Id: CodeCoverPlugin.java 75 2011-06-28 13:28:30Z dobrowolsk $)
     */
    public static enum Image {
        /**
         * Image for a method.
         */
        METHOD("icons/method.gif"), //$NON-NLS-1$
        /**
         * Image for a test session container
         */
        SESSION_CONTAINER("icons/test_session_container.gif"), //$NON-NLS-1$
        /**
         * Image for a test session
         */
        TEST_SESSION("icons/test_session.gif"), //$NON-NLS-1$
        /**
         * Image for a test case
         */
        TEST_CASE("icons/test_case.gif"), //$NON-NLS-1$
        /**
         * Image for the delete test session container action
         */
        SESSION_CONTAINER_DELETE("icons/test_session_container_delete.gif"), //$NON-NLS-1$
        /**
         * Image for the save active test session container action
         */
        SESSION_CONTAINER_SAVE("icons/test_session_container_save.gif"), //$NON-NLS-1$
        /**
         * Image for the merge elements action
         */
        ELEMENTS_MERGE("icons/elements_merge.gif"), //$NON-NLS-1$
        /**
         * Image for a delete action
         */
        ELEMENTS_DELETE("icons/elements_delete.gif"), //$NON-NLS-1$
        /**
         * Image for the hide tree items action
         */
        HIDE_TREE_ITEMS("icons/hide_top_level.gif"), //$NON-NLS-1$
        /**
         * Image for the autocalculate action
         */
        AUTO_CALCULATE("icons/auto_calculate.gif"), //$NON-NLS-1$
        /**
         * Image for the calculate correlation action
         */
        CALCULATE_CORRELATION("icons/calculate_correlation.gif"), //$NON-NLS-1$
        /**
         * Image for the show legend action
         */
        SHOW_LEGEND("icons/show_legend.gif"), //$NON-NLS-1$
        /**
         * Image for the csv export action.
         */
        CSV_EXPORT("icons/csv_export.gif"), //$NON-NLS-1$
        /**
         * Image for the coverage log import wizard.
         */
        COVERAGE_LOG("icons/coverage_log.gif"), //$NON-NLS-1$

        //Added by Negar:
        OPTIONS("icons/options.ico"), //$NON-NLS-1$
        MOUSE_STYLE("icons/mousestyle.ico"), //$NON-NLS-1$
        SUT_LEVEL("icons/sutlevel.ico"), //$NON-NLS-1$
        TEST_LEVEL("icons/testlevel.ico"), //$NON-NLS-1$
        COVERAGE_CRITERIA("icons/coveragecriteria.ico"), //$NON-NLS-1$
        DRAW_GRAPH("icons/draw_graph.gif"), //$NON-NLS-1$
        CSV_EXPORT2("icons/csv_export2.gif"), //$NON-NLS-1$
        
        //Added by Ralf Ebert:
        REC_WIZ_WEIGHT("icons/rec_wiz_weight.gif"),
        REC_WIZ_CODE("icons/rec_wiz_code.gif"),
        REC_WIZ_VERSION("icons/rec_wiz_version.gif"),
        REC_WIZ_CC("icons/rec_wiz_cc.gif"),
        REC_WIZ_PROCESS("icons/rec_wiz_process.gif"),
        REC_WIZ_QS("icons/rec_wiz_qs.gif"),
        REC_WIZ_EXPERT("icons/rec_wiz_expert.gif"),
        REC_FILTER("icons/filter.gif"), //$NON-NLS-1$
        REC_DEL_CLASS_FILTER("icons/rec_del_class_filter.gif"), //$NON-NLS-1$
        REC_SORT("icons/sort.gif"); //$NON-NLS-1$

        private final String path;

        private Image(String path) {
            this.path = path;
        }

        /**
         * Gets the path of the {@link Image}
         *
         * @return the path.
         */
        public String getPath() {
            return this.path;
        }
    }

    /**
     * The shared logger of the plugin.
     */
    private final Logger logger;

    /**
     * The log level of the logger of the plugin.
     */
    private final LogLevel loglevel;

    /**
     * The shared test session container manager of the plugin.
     */
    private TSContainerManager tsContainerManager;

    private Object eclipsePluginManagerLock = new Object();
    private volatile EclipsePluginManager eclipsePluginManager;

    private Object initTSCManagerLock;
    private boolean creatingTSCManager;
    private boolean doneCreatingTSCManager;

    private InstrumentableItemsManager instrumentableItemsManager;

    private EditorTracker editorTracker;

    private Object booleanAnalyzerLock = new Object();
    private BooleanAnalyzer booleanAnalyzer = null;

    private BundleContext bundleContext = null;

    /**
     * The shared instance of the plugin.
     */
    private static CodeCoverPlugin plugin;

    /**
     * The name of the plugin
     */
    public static final String NAME = "CodeCover";                 //$NON-NLS-1$

    /**
     * The ID of the plugin.
     */
    public static final String PLUGIN_ID = "org.codecover.eclipse";//$NON-NLS-1$
    
    /**
     * The version of the plugin.
     */
    public static final String PLUGIN_VERSION = "2.0.2";//$NON-NLS-1$

    /**
     * The name of the folder which contains the test session containers of a
     * project. This folder is contained directly in the root level of a
     * project.
     * <p>
     * All files in this folder should be test session containers. However there
     * may be additional sub folders (like the {@link #TEMP_FOLDER}).
     */
    public static final String CODECOVER_FOLDER = "codecover";     //$NON-NLS-1$

    /**
     * The name of the folder which contains temporary files of a project, e.g.
     * the temporary copy of test session container while saving it. The
     * temporary folder resides inside the {@link #CODECOVER_FOLDER} of a
     * project.
     */
    public static final String TEMP_FOLDER = "temp";               //$NON-NLS-1$

    /**
     * The name of the folder in which the instrumented source files are stored
     */
    public static final String SRC_FOLDER = "src"; //$NON-NLS-1$

    /**
     * The name of the folder in whom coverage log files are stored
     */
    public static final String COVERAGE_LOG_FOLDER = "coverage-logs"; //$NON-NLS-1$

    /**
     * The {@link QualifiedName} for the
     * {@link IResource#getPersistentProperty(QualifiedName)}.
     */
    private static final QualifiedName ENABLE_CODECOVER  = new QualifiedName("", "ENABLE_CODECOVER"); //$NON-NLS-1$ //$NON-NLS-2$

    /**
     * The ID of the LaunchConfiguration extension used for running JUnit test
     * in measurement mode.<br>
     * Value: {@value}
     * @see JUnitLaunchConfigurationDelegate
     */
    public static final String JUNIT_LAUNCH_CONFIGURATION_ID = "org.codecover.eclipse.junit.launchconfig"; //$NON-NLS-1$

    /**
     * The constructor
     */
    public CodeCoverPlugin() {
        plugin = this;
        this.loglevel = LogLevel.INFO;
        this.logger = new EclipseLogger(this.getLog(), this.getLogLevel(), PLUGIN_ID);
        this.initTSCManagerLock = new Object();
        this.creatingTSCManager = false;
        this.doneCreatingTSCManager = false;
    }

    //TODO: this seems to have a race condition (as no one knows when the
    //      constructor is executed)
    /* TODO: FIXME: XXX:
     * This still has race conditon(s):
     * - This might deadlock if Eclipse messes things up.
     * - When plugin is != null we don't know it is initialized.
     * - Even if the constructor was executed we don't know our thread will
     *   see the changes.
     * It is also bad for other reasons:
     * - It causes a delay of about 50 ms
     * - It is (more or less) busy waiting, causing unnecessary load.
    /**
     * Returns the shared instance of the plug-in.
     *
     * @return the shared instance of the plug-in
     */
    public static CodeCoverPlugin getDefault() {
        // block until plug-in has been initialized
        while (plugin == null) {
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
            }
        }
        return plugin;
    }

    /**
     * Gets the {@link EclipsePluginManager}
     * @return the {@link EclipsePluginManager}
     */
    public EclipsePluginManager getEclipsePluginManager() {
        EclipsePluginManager result = this.eclipsePluginManager;
        if (result != null) {
            return result;
        }

        synchronized (this.eclipsePluginManagerLock) {
            result = this.eclipsePluginManager;
            if (result != null) {
                return result;
            }
            final BundleContext context = this.bundleContext;
            if (context == null) {
                // should never happen
                throw new RuntimeException(
                        "start() has not been called");            //$NON-NLS-1$
            }
            result = new EclipsePluginManager(this, context);
            this.eclipsePluginManager = result;
            return result;
        }
    }

    /**
     * Returns the currently opened boolean analyzer.
     *
     * @return the instance of the boolean analyzer, null if it is not opened
     */
    public BooleanAnalyzer getBooleanAnalyzer() {
        synchronized (this.booleanAnalyzerLock) {
            return this.booleanAnalyzer;
        }
    }

    /**
     * Set the currently opened boolean analyzer.
     *
     * @param booleanAnalyzer
     *            the {@link BooleanAnalyzer} to set.
     */
    public void setBooleanAnalyzer(BooleanAnalyzer booleanAnalyzer) {
        synchronized (this.booleanAnalyzerLock) {
            this.booleanAnalyzer = booleanAnalyzer;
        }
    }

    /**
     * Returns the shared <code>TSContainerManager</code> of the plugin.
     *
     * @return the shared <code>TSContainerManager</code> of the plugin
     */
    public TSContainerManager getTSContainerManager() {
        /*
         * Initialization can't be done in the start method because it can be
         * be too slow if there are many test session containers in the
         * workspace. Thus other components could try to access the
         * TSContainerManager before it is created (and would get a
         * NullPointerException).
         * Moreover the javadoc to Plugin.start(BundleContext) states:
         * This method [start(BundleContext)] is intended to perform simple
         * initialization of the plug-in environment. The platform may terminate
         * initializers that do not complete in a timely fashion.
         */
        synchronized(this.initTSCManagerLock) {
            if(this.tsContainerManager == null) {
                if(this.creatingTSCManager) {
                    if(this.doneCreatingTSCManager) {
                        throw new IllegalStateException(
                            "Last instantion of " +                //$NON-NLS-1$
                            " TSContainerManager failed, thus" +   //$NON-NLS-1$
                            " aborting now, too.");                //$NON-NLS-1$
                    }
                    throw new IllegalStateException(
                        "already creating the TSContainerManager");//$NON-NLS-1$
                }
                try {
                    this.creatingTSCManager = true;
                    this.tsContainerManager
                            = new TSContainerManager(this.logger);
                    this.creatingTSCManager = false;
                } catch(CoreException e) {
                    Display.getDefault().asyncExec(new Runnable() {
                        @Override
						public void run() {
                            MessageDialog.openError(null,
                                "CodeCover: Fatal Error",          //$NON-NLS-1$
                                "Saving is partly deactivated!" +  //$NON-NLS-1$
                                "\n\n" +                           //$NON-NLS-1$
                                "Changes you make could get" +     //$NON-NLS-1$
                                " lost!");                         //$NON-NLS-1$
                        }
                    });
                    this.logger.fatal("Saving is partly deactivated!",  //$NON-NLS-1$
                            e);
                    /*
                     * throw an exception to be safe (if we have a buggy logger
                     * which doesn't throw a FatalException on fatal messages)
                     */
                    throw new RuntimeException(
                        "Initialization of TSContainerManager" +   //$NON-NLS-1$
                        " failed: Saving is partly deactivated!"); //$NON-NLS-1$
                } finally {
                    this.doneCreatingTSCManager = true;
                }
            }
            return this.tsContainerManager;
        }
    }

    /**
     * Returns the shared {@link InstrumentableItemsManager} of the plugin
     *
     * @return the shared {@link InstrumentableItemsManager} of the plugin
     */
    public InstrumentableItemsManager getInstrumentableItemsManager() {
        if (this.instrumentableItemsManager == null) {
            this.instrumentableItemsManager = new InstrumentableItemsManager(
                    this.logger);
        }
        return this.instrumentableItemsManager;
    }

    /**
     * Returns the shared logger of the plugin.
     *
     * @return the shared logger of the plugin
     */
    public Logger getLogger() {
        return this.logger;
    }

    /**
     * Returns the log level of the logger of the plugin.
     *
     * @return the log level of the logger of the plugin
     */
    public LogLevel getLogLevel() {
        return this.loglevel;
    }

    /**
     * Returns an image descriptor for the image file at the given plug-in
     * relative path.
     *
     * @param path
     *            the path
     * @return the image descriptor
     */
    public static ImageDescriptor getImageDescriptor(String path) {
        return imageDescriptorFromPlugin(PLUGIN_ID, path);
    }

    @Override
    protected void initializeImageRegistry(ImageRegistry registry) {
        Bundle bundle = Platform.getBundle(PLUGIN_ID);
        URL url;
        ImageDescriptor imageDesc;
        for(CodeCoverPlugin.Image image : CodeCoverPlugin.Image.values()) {
            url = FileLocator.find(bundle, new Path(image.getPath()), null);
            imageDesc = ImageDescriptor.createFromURL(url);
            registry.put(image.getPath(), imageDesc);
        }
    }

    /**
     * (non-Javadoc)
     *
     * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
     */
    @Override
    public void start(BundleContext context) throws Exception {
        super.start(context);

        this.bundleContext = context;

        /* Watch for editors with source code to annotate */
        this.editorTracker = new EditorTracker(getWorkbench());

        // register launch tracker
        DebugPlugin.getDefault().addDebugEventListener(
                new CodeCoverDebugListener());
    }

    /**
     * (non-Javadoc)
     *
     * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
     */
    @Override
    public void stop(BundleContext context) throws Exception {
        // TODO: javadoc of stop says that super must be sent last
        super.stop(context);

        ResourcesPlugin.getWorkspace().removeSaveParticipant(this);

        /* Stop Tracking editors. */
        this.editorTracker.dispose();

        ImageProvider.dispose();

        // Save the data of the managers
        getInstrumentableItemsManager().saveInstrumentableItems();
        plugin = null;
    }

    /**
     * Rebuilds a CodeCover project taking into account whether
     * auto-build is activated for the workspace of the project.
     *
     * @param project the project to build
     */
    public static void build(IProject project) {
        if (isCodeCoverActivated(project)) {
            try {
                // trigger refresh
                project.touch(null);
            } catch (CoreException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    /**
     * Checks whether CodeCover is activated for a project.
     *
     * @param project
     *            the project which you want to check
     * @return the enabled state.
     */
    public static boolean isCodeCoverActivated(IProject project) {
        String state;

        try {
            state = project.getPersistentProperty(ENABLE_CODECOVER);
            return Boolean.parseBoolean(state);
        } catch (CoreException e) {
             return false;
        }
    }

    /**
     * Sets the activation for a project.
     *
     * @param project
     *            The project.
     * @param enabled
     *            true &rarr; CodeCover is activated; <br>
     *            false &rarr; CodeCover is deactivated for this project
     */
    public static void setCodeCoverActivation(IProject project, boolean enabled) {
        try {
            project.setPersistentProperty(
                    ENABLE_CODECOVER,
                    Boolean.toString(enabled));
        } catch (CoreException e) {
            // TODO error handling
        }
    }

    /**
     * Gets a unique and constant {@link QualifiedName} for a given
     * {@link Criterion}.
     *
     * @param criterion
     *            the given {@link Criterion}.
     * @return the {@link QualifiedName}
     */
    private static final QualifiedName getCriterionIdentifier(
            Criterion criterion) {
        String identifer = criterion.getPluginName()
                + "//" + criterion.getExtensionName(); //$NON-NLS-1$
        return new QualifiedName("", identifer); //$NON-NLS-1$
    }

    /**
     * Sets the activation state for the given {@link Criterion} in the given
     * {@link IProject}.
     *
     * @param project
     *            the given {@link IProject}.
     * @param criterion
     *            the given {@link Criterion}.
     * @param selected
     *            the selection state of the {@link Criterion}.
     */
    public static void setCriterionSelectedState(IProject project,
            Criterion criterion, boolean selected) {
        try {
            project.setPersistentProperty(getCriterionIdentifier(criterion),
                    Boolean.toString(selected));
        } catch (CoreException e) {
            // TODO error handling
        }
    }

    /**
     * Gets the activation state for the given {@link Criterion} in the given
     * {@link IProject}.
     *
     * @param project
     *            the given {@link IProject}.
     * @param criterion
     *            the given {@link Criterion}.
     * @return true &rarr; the {@link Criterion} is selected;<br>
     *         false &rarr; the criterion is not selected for this project
     */
    public static boolean getCriterionSelectedState(IProject project,
            Criterion criterion) {
        String state;

        try {
            state = project.getPersistentProperty(getCriterionIdentifier(criterion));
            if (state == null) {
                /* The criterion was not contained in any property */
                return false;
            } else {
                return Boolean.parseBoolean(state);
            }
        } catch (CoreException e) {
            return false;
        }
    }

    /**
     * Get path to a place where project specific files can be stored.
     *
     * @param project
     *            the {@link IProject} to use.
     * @return the {@link IPath} to the place where project specific files can
     *         be stored.
     */
    public IPath getWorkingLocation(IProject project) {
        return project.getWorkingLocation(getBundle().getSymbolicName()).makeAbsolute();
    }

    /**
     * Get path to the folder where the instrumented files of a project are stored.
     *
     * @param project
     *            the {@link IProject} to use.
     * @return the {@link IPath} to the place where the instrumented files of a
     *         project are stored.
     */
    public IPath getPathToInstrumentedSources(IProject project) {
        return getWorkingLocation(project).append(SRC_FOLDER);
    }

    /**
     * Get path to the folder where the coverage logs of a project are stored.
     *
     * @param project
     *            the {@link IProject} to use.
     * @return the {@link IPath} to the place where the coverage logs of a
     *         project are stored.
     */
    public IPath getPathToCoverageLogs(IProject project) {
        return getWorkingLocation(project).append(COVERAGE_LOG_FOLDER);
    }
}
