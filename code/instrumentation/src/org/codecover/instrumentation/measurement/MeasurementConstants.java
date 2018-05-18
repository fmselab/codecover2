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

package org.codecover.instrumentation.measurement;

import java.nio.charset.Charset;

/**
 * This class contains some constants related to the coverage log file and
 * methods for escaping characters.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: MeasurementConstants.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MeasurementConstants {

    /** The default {@link Charset} used.*/
    public static final Charset CHARSET = Charset.forName("UTF-8");

    /**
     * The Separator, that is used to concatenate the name of a
     * {@link CoverageCounterLog#startNamedSection(String)} with a
     * ID, when creating the full ID for a CoverableItem.
     */
    public final static char PREFIX_ID_SEPERATOR = '.';

    /**
     * The String to separate the Primary Id and a subID&mdash;e.g. for 
     * a loop sub id or a conditional id.
     */
    public final static char ID_ASSIGNMENT_SEPERATOR = '-';

    /**
     * Replaces some characters:
     * <ul>
     * <li>"<code>\r\n</code>" by "<code> </code>"</li>
     * <li>"<code>\n</code>" by "<code> </code>"</li>
     * <li>"<code>\r</code>" by "<code> </code>"</li>
     * <li>"<code>\t</code>" by "<code> </code>"</li>
     * <li>"<code>\f</code>" by "<code> </code>"</li>
     * <li>"<code>\</code>" by "<code>\\</code>"</li>
     * <li>"<code>"</code>" by "<code>\"</code>"</li>
     * </ul>
     * This is needed, cause a name is not allowed to contain the literal
     * <code>"</code>.
     * 
     * @param strToEscape
     *            The String whose characters should be escaped.
     * 
     * @return The result of the replacements.
     */
    public static String escapeName(String strToEscape) {
        strToEscape = strToEscape.replaceAll("\\r\\n", " ");
        strToEscape = strToEscape.replaceAll("\\n", " ");
        strToEscape = strToEscape.replaceAll("\\r", " ");
        strToEscape = strToEscape.replaceAll("\\t", " ");
        strToEscape = strToEscape.replaceAll("\\f", " ");
        strToEscape = strToEscape.replaceAll("\\\\", "\\\\\\\\");
        strToEscape = strToEscape.replaceAll("\"", "\\\\\"");
        return strToEscape;
    }

    /**
     * Replaces some characters:
     * <ul>
     * <li>"<code>\</code>" by "<code>\\</code>"</li>
     * <li>"<code>\n</code>" by "<code>\\n</code>"</li>
     * <li>"<code>\r</code>" by "<code>\\r</code>"</li>
     * <li>"<code>\t</code>" by "<code>\\t</code>"</li>
     * <li>"<code>\f</code>" by "<code>\\f</code>"</li>
     * <li>"<code>"</code>" by "<code>\"</code>"</li>
     * </ul>
     * This is needed, cause a comment is not allowed to contain the literal
     * <code>"</code>.
     * 
     * @param strToEscape
     *            The String whose characters should be escaped.
     * 
     * @return The result of the replacements.
     */
    public static String escapeComment(String strToEscape) {
        strToEscape = strToEscape.replaceAll("\\\\", "\\\\\\\\");
        strToEscape = strToEscape.replaceAll("\\n", "\\\\n");
        strToEscape = strToEscape.replaceAll("\\r", "\\\\r");
        strToEscape = strToEscape.replaceAll("\\t", "\\\\t");
        strToEscape = strToEscape.replaceAll("\\f", "\\\\f");
        strToEscape = strToEscape.replaceAll("\"", "\\\\\"");
        return strToEscape;
    }
}
