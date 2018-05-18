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

package org.codecover.ant;

import java.io.File;

import org.apache.tools.ant.BuildException;
import org.codecover.model.extensions.PluginUtils;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: AddPluginDirCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class AddPluginDirCommand extends Command {
    File dir;

    /**
     * Sets the plugin directory.
     * 
     * @param dir
     *                the {@link File} to set.
     */
    public void setDir(File dir) {
        this.dir = dir;
    }

    @Override
    public void run(Context context) {
        if (this.dir == null) {
            throw new BuildException("The attribute 'dir' is missing.");
        }

        PluginUtils.loadPluginsFromDirectory(context.getPluginManager(),
                context.getLogger(), this.dir);
    }
}
