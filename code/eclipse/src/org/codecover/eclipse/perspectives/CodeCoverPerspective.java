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

package org.codecover.eclipse.perspectives;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.model.utils.LogLevel;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.progress.IProgressConstants;

/**
 * The factory for the CodeCover perspective which opens all views of CodeCover
 * and adds some Java related views, actions and wizards (see
 * <code>org.eclipse.jdt.internal.ui.JavaPerspectiveFactory</code>).
 * <p>
 * The perspective is designed to be usable with a resolution of at least
 * 1024x768.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: CodeCoverPerspective.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CodeCoverPerspective implements IPerspectiveFactory {

    private IPageLayout layout;

    /**
     * Creates the factory for the perspective of CodeCover.
     */
    public CodeCoverPerspective() {
        super();
    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.IPerspectiveFactory#createInitialLayout(org.eclipse.ui.IPageLayout)
     */
    @Override
	public void createInitialLayout(IPageLayout layout) {
        this.layout = layout;
        this.addViews();
        this.addViewShortcuts();
        this.addPerspectiveShortcuts();
        this.addActionsWizardsShortcuts();
    }

    private void addViews() {
        IFolderLayout left = this.layout.createFolder(
                "left",                                            //$NON-NLS-1$
                IPageLayout.LEFT,
                0.25f,
                IPageLayout.ID_EDITOR_AREA);
        left.addView("org.eclipse.jdt.junit.ResultView");          //$NON-NLS-1$
        left.addView(JavaUI.ID_PACKAGES);
        left.addPlaceholder(IPageLayout.ID_RES_NAV);

        IFolderLayout bottom = this.layout.createFolder(
                "bottomRight",                                     //$NON-NLS-1$
                IPageLayout.BOTTOM,
                0.66f,
                IPageLayout.ID_EDITOR_AREA);
        bottom.addView(
                "org.codecover.eclipse.views.TestSessionsView");   //$NON-NLS-1$
        bottom.addView(
                "org.codecover.eclipse.views.CoverageView");       //$NON-NLS-1$
        bottom.addView(
                "org.codecover.eclipse.views.BooleanAnalyzer");    //$NON-NLS-1$
        bottom.addView(
                "org.codecover.eclipse.views.PickTestCaseView");   //$NON-NLS-1$
        bottom.addView(
                "org.codecover.eclipse.views.CorrelationView");    //$NON-NLS-1$
        bottom.addView(IPageLayout.ID_PROBLEM_VIEW);
        bottom.addPlaceholder(NewSearchUI.SEARCH_VIEW_ID);
        bottom.addPlaceholder(IConsoleConstants.ID_CONSOLE_VIEW);
        bottom.addPlaceholder(IPageLayout.ID_BOOKMARKS);
        bottom.addPlaceholder(IProgressConstants.PROGRESS_VIEW_ID);

        this.layout.addView(
                "org.codecover.eclipse.views.LiveNotificationView",//$NON-NLS-1$
                IPageLayout.RIGHT,
                0.733f, // width of the view ~20%
                IPageLayout.ID_EDITOR_AREA);

        /*
         * only add Error Log view, if the log level is high enough, else the
         * user is constantly disturbed with popups of the Error Log view (if
         * he doesn't know how to deactivate this behavior)
         */
        if(CodeCoverPlugin.getDefault().getLogLevel() != LogLevel.DEBUG
                && CodeCoverPlugin.getDefault().getLogLevel() != LogLevel.INFO){
            this.layout.addFastView(
                    "org.eclipse.pde.runtime.LogView",             //$NON-NLS-1$
                    0.50f);
        }
    }

    private void addViewShortcuts() {
        // CodeCover
        this.layout.addShowViewShortcut(
                "org.codecover.eclipse.views.TestSessionsView");   //$NON-NLS-1$
        this.layout.addShowViewShortcut(
                "org.codecover.eclipse.views.CoverageView");       //$NON-NLS-1$
        this.layout.addShowViewShortcut(
                "org.codecover.eclipse.views.BooleanAnalyzer");    //$NON-NLS-1$
        this.layout.addShowViewShortcut(
                "org.codecover.eclipse.views.PickTestCaseView");   //$NON-NLS-1$
        this.layout.addShowViewShortcut(
                "org.codecover.eclipse.views.CorrelationView");    //$NON-NLS-1$
        this.layout.addShowViewShortcut(
               "org.codecover.eclipse.views.LiveNotificationView");//$NON-NLS-1$
        // Java
        this.layout.addShowViewShortcut(JavaUI.ID_PACKAGES);
        this.layout.addShowViewShortcut(JavaUI.ID_TYPE_HIERARCHY);
        this.layout.addShowViewShortcut(JavaUI.ID_SOURCE_VIEW);
        this.layout.addShowViewShortcut(JavaUI.ID_JAVADOC_VIEW);
        // search
        this.layout.addShowViewShortcut(NewSearchUI.SEARCH_VIEW_ID);
        // debugging
        this.layout.addShowViewShortcut(IConsoleConstants.ID_CONSOLE_VIEW);
        // standard workbench
        this.layout.addShowViewShortcut(IPageLayout.ID_OUTLINE);
        this.layout.addShowViewShortcut(IPageLayout.ID_PROBLEM_VIEW);
        this.layout.addShowViewShortcut(IPageLayout.ID_RES_NAV);
        this.layout.addShowViewShortcut(IPageLayout.ID_TASK_LIST);
        this.layout.addShowViewShortcut(IProgressConstants.PROGRESS_VIEW_ID);
        this.layout.addShowViewShortcut(
                "org.eclipse.pde.runtime.LogView");                //$NON-NLS-1$
    }

    private void addPerspectiveShortcuts() {
        this.layout.addPerspectiveShortcut(JavaUI.ID_PERSPECTIVE);
        this.layout.addPerspectiveShortcut(
                IDebugUIConstants.ID_DEBUG_PERSPECTIVE);
    }

    private void addActionsWizardsShortcuts() {
        // Java actions
        this.layout.addActionSet(IDebugUIConstants.LAUNCH_ACTION_SET);
        this.layout.addActionSet(JavaUI.ID_ACTION_SET);
        this.layout.addActionSet(JavaUI.ID_ELEMENT_CREATION_ACTION_SET);
        this.layout.addActionSet(IPageLayout.ID_NAVIGATE_ACTION_SET);
        // Java wizards
        this.layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.JavaProjectWizard"); //$NON-NLS-1$
        this.layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.NewPackageCreationWizard"); //$NON-NLS-1$
        this.layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.NewClassCreationWizard"); //$NON-NLS-1$
        this.layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.NewInterfaceCreationWizard"); //$NON-NLS-1$
        this.layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.NewEnumCreationWizard"); //$NON-NLS-1$
        this.layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.NewAnnotationCreationWizard"); //$NON-NLS-1$
        this.layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.NewSourceFolderCreationWizard"); //$NON-NLS-1$
        this.layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.NewSnippetFileCreationWizard"); //$NON-NLS-1$
        this.layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.folder"); //$NON-NLS-1$
        this.layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.file"); //$NON-NLS-1$
        this.layout.addNewWizardShortcut("org.eclipse.ui.editors.wizards.UntitledTextFileWizard"); //$NON-NLS-1$
    }

}
