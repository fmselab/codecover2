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

package org.codecover.model.utils;

import java.util.List;

/**
 * Helper class, that checks the given name against a list of names and appends
 * a %(count)% to the name, with the count being the number of times, said name
 * reoccurs in the list
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: NameReoccurenceHelper.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class NameReoccurenceHelper {

    /**
     * Escapes the given string with an appended "(count)", with count being the
     * number of times said string exists in the given list of names
     * <p>
     * If the given name did not previously exists in the list of names, the
     * name is returned
     * 
     * @param names
     *            the list of names to be checked
     * @param name
     *            the name to be escaped
     * @return the escaped string
     */
    public static String escapeName(List<String> names, String name) {
        String appendedName = name;
        int count = 0;
        while (names.contains(appendedName)) {
            count++;
            String suffix = " (" + count + ")";
            appendedName = name + suffix;
        }

        return appendedName;
    }

}
