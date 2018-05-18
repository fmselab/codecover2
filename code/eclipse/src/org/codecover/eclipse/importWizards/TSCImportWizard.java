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

package org.codecover.eclipse.importWizards;

import org.codecover.eclipse.Messages;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IImportWizard;
import org.eclipse.ui.IWorkbench;

/**
 * A wizard to import a test session container into a project.
 * 
 * @see TSCImportWizardPage
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: TSCImportWizard.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class TSCImportWizard extends Wizard implements IImportWizard {

    private static final String LABEL_MAIN_PAGE_TITLE = Messages
            .getString("TSCImportWizard.LABEL_MAIN_PAGE_TITLE");   //$NON-NLS-1$

    private static final String LABEL_WINDOW_TITLE = Messages
            .getString("TSCImportWizard.LABEL_WINDOW_TITLE");      //$NON-NLS-1$

    private TSCImportWizardPage mainPage;

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.Wizard#performFinish()
     */
    @Override
    public boolean performFinish() {
        return this.mainPage.importTestSessionContainer();
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
     *      org.eclipse.jface.viewers.IStructuredSelection)
     */
    @Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
        this.setWindowTitle(LABEL_WINDOW_TITLE);
        this.setNeedsProgressMonitor(true);
        this.mainPage = new TSCImportWizardPage(LABEL_MAIN_PAGE_TITLE,
                selection);
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.Wizard#addPages()
     */
    @Override
    public void addPages() {
        super.addPages();
        this.addPage(this.mainPage);
    }
}
