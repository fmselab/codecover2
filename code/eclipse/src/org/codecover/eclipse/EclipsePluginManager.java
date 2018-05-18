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

import java.io.File;

import org.codecover.eclipse.preferences.PluginDirectoryPreferencePage;
import org.codecover.model.extensions.*;
import org.eclipse.jface.preference.IPreferenceStore;
import org.osgi.framework.BundleContext;

/**
 * Class containing functions related to loading codecover plugins
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: EclipsePluginManager.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class EclipsePluginManager {
    private final PluginManager pluginManager;

    private final BundleContext bundleContext;

    /**
     * Constructor.
     * 
     * @param codeCoverPlugin
     *                the {@link CodeCoverPlugin} instance.
     */
    public EclipsePluginManager(CodeCoverPlugin codeCoverPlugin,
            BundleContext bundleContext) {
        if (codeCoverPlugin == null) {
            throw new NullPointerException("codeCoverPlugin == null"); //$NON-NLS-1$
        }
        if (bundleContext == null) {
            throw new NullPointerException("bundleContext == null"); //$NON-NLS-1$
        }
        
        this.pluginManager = PluginManager.create();
        this.bundleContext = bundleContext;
        
        // This is not very nice. We statically reference the plugin.
        // But it works.
        pluginManager.addPlugin(PluginHandle.createPluginHandle("org.codecover.report.html", 1, 0, new org.codecover.report.html.HtmlReportPlugin())); //$NON-NLS-1$
        pluginManager.addPlugin(PluginHandle.createPluginHandle("org.codecover.report.csv", 1, 0, new org.codecover.report.csv.CSVReportPlugin())); //$NON-NLS-1$

        IPreferenceStore store = codeCoverPlugin.getPreferenceStore();
        String path = store.getString(
                          PluginDirectoryPreferencePage.PREFERENCE_PLUGINS_DIR);
        if (path != null && path.length() > 0) {
            PluginUtils.loadPluginsFromDirectory(pluginManager, codeCoverPlugin
                    .getLogger(), new File(path));
        }
    }

    /**
     * Gets the {@link PluginManager}.
     * 
     * @return the {@link PluginManager}
     */
    public PluginManager getPluginManager() {
        return pluginManager;
    }
}
