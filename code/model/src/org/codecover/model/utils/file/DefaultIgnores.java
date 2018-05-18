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

package org.codecover.model.utils.file;

/**
 * This class has the method {@link #getIgnorePatterns()}, which returns an
 * Array with patterns, that are ignored during the instrumentation.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: DefaultIgnores.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DefaultIgnores {

    private static final String [] DEFAULT_IGNORE_PATTERNS = new String[] {
        // Subversion
        "**/.svn/**",
        "**/_svn/**",

        // CVS
        "**/CVS/**",
        "**/.cvsignore",

        // Miscellaneous typical temporary files
        "**/*~",
        "**/#*#",
        "**/.#*",
        "**/%*%",
        "**/._*",

        // SCCS
        "**/SCCS/**",

        // MS Visual SourceSafe
        "**/vssver.scc",

        // Mac
        "**/.DS_Store",

        // Microsoft Explorer Thumbs 
        "**/Thumbs.db"
    };

    /**
     * Returns an array with default ignore patterns:
     * <ul>
     * <li>temporary files</li>
     * <li>cvs</li>
     * <li>svn</li>
     * <li>Visual SourceSafe</li>
     * <li>SCCS</li>
     * </ul>
     * 
     * @return Default ignore patterns.
     */
    public static String[] getIgnorePatterns() {
        return DEFAULT_IGNORE_PATTERNS;
    }
}
