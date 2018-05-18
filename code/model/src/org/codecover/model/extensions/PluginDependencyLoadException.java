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
 * @version 1.0 ($Id: PluginDependencyLoadException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class PluginDependencyLoadException extends PluginLoadException {
    private final String name;
    private final int majorVersion;
    private final PluginLoadException e;
    
    public PluginDependencyLoadException(String name, int majorVersion, PluginLoadException e) {
        super("An error occured when loading the dependency '" + name + "' major version " + majorVersion + ": " + e.getMessage(), e);
        this.name = name;
        this.majorVersion = majorVersion;
        this.e = e;
    }
    
    public String getName() {
        return name;
    }
    
    public int getMajorVersion() {
        return majorVersion;
    }
    
    public PluginLoadException getException() {
        return e;
    }
}
