/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This file may be used, modifies and redistributed     *
 * under the terms of either the Eclipse Public License v1.0 which            *
 * accompanies this distribution and is available at                          *
 * http://www.eclipse.org/legal/epl-v10.html or the MIT license, available at *
 * http://www.opensource.org/licenses/mit-license.php                         *
 ******************************************************************************/

package org.codecover.model.extensions;

import java.io.*;
import java.net.*;
import java.util.*;

import org.codecover.model.utils.*;

/**
 * An exception thrown when an error occurs while loading a plugin.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: NoCodecoverPluginException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class NoCodecoverPluginException extends PluginLoadException {
    private final File path;
    
    public NoCodecoverPluginException(File path) {
        super("The plugin in '" + path.getAbsolutePath() + "' does not contain a 'codecover-plugin.xml'.");
        this.path = path;
    }
    
    public File getPath() {
        return path;
    }
}
