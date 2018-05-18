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

import org.codecover.eclipse.Messages;
import org.codecover.model.TestSessionContainer;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;

/**
 * A wizard which assists the user with merging of test sessions or test cases.
 *
 * @see MergeWizardPage
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: MergeWizard.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MergeWizard extends Wizard {

    private static final String LABEL_WINDOW_TITLE = Messages
            .getString("MergeWizard.LABEL_WINDOW_TITLE"); //$NON-NLS-1$

    private MergeWizardPage mainPage;

    private TestSessionContainer tsc;

    private IStructuredSelection selection;

    /**
     * Constructor.
     * 
     * @param tsc
     *            the given {@link TestSessionContainer}.
     * @param selection
     *            the given {@link IStructuredSelection}.
     */
    public MergeWizard(TestSessionContainer tsc,
            IStructuredSelection selection) {
        super();
        this.tsc = tsc;
        this.selection = selection;
        this.mainPage = new MergeWizardPage(this.tsc, this.selection);
        this.setWindowTitle(LABEL_WINDOW_TITLE);
        // this.setNeedsProgressMonitor(true);
    }

    @Override
    public boolean performFinish() {
        return this.mainPage.mergeElements();
    }

    @Override
    public void addPages() {
        super.addPages();
        this.addPage(this.mainPage);
    }

}
