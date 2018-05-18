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

package org.codecover;

import java.nio.charset.Charset;

/**
 * This class contains some constants, that know the SVN revision, the SVN Date
 * and the version of CodeCover.<br>
 * <br>
 * These constants are updated using <code>SVNRevisionTool</code>. After this file is
 * updated and newly compiled, it substitutes this original class. The problem is,
 * that the users of these constants are compiled before and the compiler 
 * optimizes the call this way, that these constants are copied to the caller.
 * For that reason we have to use methods, that just return the constants. This won't
 * cause problems with the compiler.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: CodeCoverInfo.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CodeCoverInfo {
    /** The version of CodeCover {@value} */ 
    private static final String VERSION = "1.0.0.0";

    /** The SVN Revision of CodeCover {@value} */
    private static final String REVISION = "?";

    /** The SVN Date of CodeCover {@value} */
    private static final String DATE = "?";

    /** The {@link Charset} of the source files of CodeCover */
    public static final Charset CODE_FILE_CHARSET = Charset.forName("UTF-8");
    
    /** The major version for the plugin ABI */
    public static final int pluginVersionMajor = 1;
    /** The minor version for the plugin ABI */
    public static final int pluginVersionMinor = 0;
    
    /**
     * @return The version of CodeCover.
     * @see #VERSION
     */
    public static String getVersion() {
        return VERSION;
    }

    /**
     * @return The SVN Revision of CodeCover.
     * @see #REVISION
     */
    public static String getRevision() {
        return REVISION;
    }

    /**
     * @return The SVN Date of CodeCover.
     * @see #DATE
     */
    public static String getDate() {
        return DATE;
    }
}
