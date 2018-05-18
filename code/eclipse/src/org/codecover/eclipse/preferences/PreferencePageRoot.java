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
import org.codecover.eclipse.views.CorrelationView;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: PreferencePageRoot.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class PreferencePageRoot extends
        org.eclipse.jface.preference.PreferencePage implements
        IWorkbenchPreferencePage {

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected Control createContents(Composite parent) {
        ScrolledComposite scrolledComposite = new ScrolledComposite(parent,
                SWT.H_SCROLL | SWT.V_SCROLL);
        scrolledComposite.setExpandHorizontal(true);
        scrolledComposite.setExpandVertical(true);

        Group group = new Group(scrolledComposite, SWT.NONE);
        scrolledComposite.setContent(group);

        group.setLayout(new FillLayout());

        Label todoLabel = new Label(group, SWT.NONE);
        todoLabel
                .setText("TODO: Maybe put some introductionary stuff here, like an image"); //$NON-NLS-1$

        return scrolledComposite;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
	public void init(IWorkbench arg0) {
        // Do noting.
    }

    /**
     * Gets the {@link RGB} for the hot portion of the hot path
     * 
     * @return the {@link RGB}
     */
    public static RGB getHotPathHot() {
        return PreferenceConverter.getColor(CodeCoverPlugin.getDefault()
                .getPreferenceStore(),
                HotPathPreferencePage.PREFERENCE_HOTPATH_HOT);
    }

    /**
     * Gets the {@link RGB} for the cold portion of the hot path
     * 
     * @return the {@link RGB}
     */
    public static RGB getHotPathCold() {
        return PreferenceConverter.getColor(CodeCoverPlugin.getDefault()
                .getPreferenceStore(),
                HotPathPreferencePage.PREFERENCE_HOTPATH_COLD);
    }

    /**
     * Gets the preferences for the {@link CorrelationView}
     * 
     * @return the {@link SetOfRGBWithBoundaries} representing the preferences.
     */
    public static SetOfRGBWithBoundaries getCorrelationMatrixColors() {
        return new SetOfRGBWithBoundaries(
                CodeCoverPlugin
                        .getDefault()
                        .getPreferenceStore()
                        .getString(
                                CorrelationMatrixPreferencePage.PREFERENCE_CORRELATIONMATRIX));
    }
}
