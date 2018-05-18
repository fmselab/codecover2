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
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.graphics.RGB;

/**
 * Class used to initialize default preference values.
 * 
 * @author Michael Starzmann
 * 
 * @version 1.0 ($Id: PreferenceInitializer.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
     */
    @Override
    public void initializeDefaultPreferences() {
        IPreferenceStore store = CodeCoverPlugin.getDefault().getPreferenceStore();
        PreferenceConverter.setDefault(store, HotPathPreferencePage.PREFERENCE_HOTPATH_HOT, new RGB(255, 0, 0));
        PreferenceConverter.setDefault(store, HotPathPreferencePage.PREFERENCE_HOTPATH_COLD, new RGB(0, 0, 255));
        SetOfRGBWithBoundaries rgbb = new SetOfRGBWithBoundaries();
        rgbb.add(new RGBWithBoundaries(new RGB(255,0,0),0,0));
        rgbb.add(new RGBWithBoundaries(new RGB(0,255,0),0,50));
        rgbb.add(new RGBWithBoundaries(new RGB(255,255,0),50,100));
        rgbb.add(new RGBWithBoundaries(new RGB(0,0,255),100,100));
        store.setDefault(CorrelationMatrixPreferencePage.PREFERENCE_CORRELATIONMATRIX,
                rgbb.toString());
    }
}
