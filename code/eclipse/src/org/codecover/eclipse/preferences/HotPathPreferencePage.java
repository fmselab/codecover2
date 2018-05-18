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
import org.codecover.eclipse.Messages;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Link;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PreferencesUtil;

/**
 * This class represents a preference page that is contributed to the
 * Preferences dialog. By subclassing <samp>FieldEditorPreferencePage</samp>,
 * we can use the field support built into JFace that allows us to create a page
 * that is small and knows how to save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They are stored in the
 * preference store that belongs to the main plug-in class. That way,
 * preferences can be accessed directly via the preference store.
 */

public class HotPathPreferencePage extends
        org.eclipse.jface.preference.PreferencePage implements
        IWorkbenchPreferencePage {

    static final String PREFERENCE_HOTPATH_HOT = "org.codecover.hotpath.hot"; //$NON-NLS-1$

    static final String PREFERENCE_HOTPATH_COLD = "org.codecover.hotpath.cold"; //$NON-NLS-1$

    private ColorFieldEditor hotPathHot;

    private ColorFieldEditor hotPathCold;

    /**
     * Constructor
     */
    public HotPathPreferencePage() {
        setPreferenceStore(CodeCoverPlugin.getDefault().getPreferenceStore());
        setDescription(Messages.getString("PreferencePage.PAGE_DESCRIPTION")); //$NON-NLS-1$
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
	public void init(IWorkbench workbench) {
        // Do nothing.
    }

    private Group createHotPathGroup(Composite parent) {
        Group hotpath = new Group(parent, SWT.FILL);
        GridLayout layout = new GridLayout();
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        hotpath.setLayout(layout);
        hotpath.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        hotpath.setFont(parent.getFont());
        hotpath.setText(Messages.getString("PreferencePage.HOTPATH_GROUP")); //$NON-NLS-1$
        this.hotPathHot = new ColorFieldEditor(PREFERENCE_HOTPATH_HOT, Messages
                .getString("PreferencePage.HOTPATH_HOT"), hotpath); //$NON-NLS-1$
        this.hotPathHot.setPage(this);
        this.hotPathHot.setPreferenceStore(getPreferenceStore());
        this.hotPathHot.load();
        this.hotPathCold = new ColorFieldEditor(PREFERENCE_HOTPATH_COLD,
                Messages.getString("PreferencePage.HOTPATH_COLD"), hotpath); //$NON-NLS-1$
        this.hotPathCold.setPage(this);
        this.hotPathCold.setPreferenceStore(getPreferenceStore());
        this.hotPathCold.load();
        return hotpath;
    }

    @Override
    protected Control createContents(Composite parent) {
        final Composite composite = new Composite(parent, SWT.NULL);
        GridLayout layout = new GridLayout();
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        composite.setLayout(layout);
        composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        composite.setFont(parent.getFont());
        GridData gd;
        final Link link = new Link(composite, SWT.NONE);
        link.setText(Messages.getString("PreferencePage.LINK")); //$NON-NLS-1$
        gd = new GridData(SWT.FILL, SWT.BEGINNING, true, false);
        gd.widthHint = 300; // don't get wider initially
        gd.horizontalSpan = 2;
        // gd.horizontalIndent= indent;
        link.setLayoutData(gd);
        link.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                PreferencesUtil.createPreferenceDialogOn(link.getShell(),
                        e.text, null, null);
            }
        });
        createHotPathGroup(composite);
        return composite;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        this.hotPathHot.store();
        this.hotPathCold.store();
        return super.performOk();
    }

    @Override
    protected void performDefaults() {
        this.hotPathHot.loadDefault();
        this.hotPathCold.loadDefault();
        super.performDefaults();
    }
}