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

import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: Messages.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class Messages {
    private static final String BUNDLE_NAME = "org.codecover.eclipse.messages"; //$NON-NLS-1$

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
            .getBundle(BUNDLE_NAME);

    private Messages() {
        //
    }

    /**
     * Gets the localized {@link String} for the given key.
     * 
     * @param key
     *            the given key
     * @return the localized {@link String}
     */
    public static String getString(String key) {
        try {
            return RESOURCE_BUNDLE.getString(key);
        } catch (MissingResourceException e) {
            return '!' + key + '!';
        }
    }
}
