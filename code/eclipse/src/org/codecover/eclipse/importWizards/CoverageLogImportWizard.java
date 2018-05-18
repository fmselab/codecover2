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
 * A wizard to import a coverage log.
 *
 * @see CoverageLogImportWizardPage
 * @author Robert Hanussek
 * @version 1.0 ($Id: CoverageLogImportWizard.java 54 2009-07-20 11:25:13Z ahija $)
 */
public class CoverageLogImportWizard extends Wizard implements IImportWizard {

    private static final String LABEL_WINDOW_TITLE = Messages
            .getString("CoverageLogImportWizard.0"); //$NON-NLS-1$

    private CoverageLogImportWizardPage mainPage;

    @Override
    public boolean performFinish() {
        return this.mainPage.importCoverageLog();
    }

    @Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
        this.setWindowTitle(LABEL_WINDOW_TITLE);
        this.setNeedsProgressMonitor(true);
        this.mainPage = new CoverageLogImportWizardPage();
    }

    @Override
    public void addPages() {
        super.addPages();
        this.addPage(this.mainPage);
    }

    /** Overwrites the currently selected file and may be used to select a file intialy. */
    public void selectCoverageLogFile(String thisFile)
    {
        this.mainPage.selectCoverageLogFile(thisFile);
    }
}
