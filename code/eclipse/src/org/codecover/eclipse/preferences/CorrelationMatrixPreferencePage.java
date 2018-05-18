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

package org.codecover.eclipse.preferences;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.preferences.RGBWithBoundariesEditor.RefreshListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * This class represents the preference page in eclipse, that displays the
 * preferences for the correlation matrix.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: CorrelationMatrixPreferencePage.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CorrelationMatrixPreferencePage extends
        org.eclipse.jface.preference.PreferencePage implements
        IWorkbenchPreferencePage {

    static final String PREFERENCE_CORRELATIONMATRIX = "org.codecover.correlationmatrix"; //$NON-NLS-1$

    private RGBWithBoundariesEditor boundariesEditor;

    /**
     * Constructor
     */
    public CorrelationMatrixPreferencePage() {
        setPreferenceStore(CodeCoverPlugin.getDefault().getPreferenceStore());
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected Control createContents(Composite parent) {
        final Group group = new Group(parent, SWT.NONE);
        group.setLayout(new GridLayout());
        group.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        final ScrolledComposite scrolledComposite = new ScrolledComposite(
                group, SWT.H_SCROLL | SWT.V_SCROLL);

        scrolledComposite.setExpandVertical(true);
        scrolledComposite.setExpandHorizontal(true);
        scrolledComposite.setAlwaysShowScrollBars(false);
        scrolledComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true));

        this.boundariesEditor = new RGBWithBoundariesEditor(
                PREFERENCE_CORRELATIONMATRIX, getPreferenceStore(),
                scrolledComposite);
        scrolledComposite.setContent(this.boundariesEditor);

        this.boundariesEditor.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                scrolledComposite
                        .setMinSize(CorrelationMatrixPreferencePage.this.boundariesEditor
                                .computeSize(SWT.DEFAULT, SWT.DEFAULT));
            }
        });

        this.boundariesEditor.addRefreshListener(new RefreshListener() {
            @Override
			public void contentChanged(RGBWithBoundariesEditor boundariesEditor) {
                scrolledComposite.setMinSize(boundariesEditor.computeSize(
                        SWT.DEFAULT, SWT.DEFAULT));
            }
        });

        return scrolledComposite;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
	public void init(IWorkbench workbench) {
        // Do nothing.
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
     */
    @Override
    protected void performDefaults() {
        this.boundariesEditor.loadDefault();
        super.performDefaults();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        this.boundariesEditor.store();
        return super.performOk();
    }
}
