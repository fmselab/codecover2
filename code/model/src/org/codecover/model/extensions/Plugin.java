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

import java.util.Set;

/**
 * An interface to be implemented by every plugin.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: Plugin.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface Plugin {
    /**
     * Gets the name of the plugin.
     * 
     * @return the name.
     */
    public String getName();

    /**
     * Gets the description of the plugin.
     * 
     * @return the description.
     */
    public String getDescription();

    public <T> Set<Extension<T>> getExtensions(Class<T> interfaceType);

    public <T> Extension<T> getExtensionByName(Class<T> interfaceType,
            String name);
}
