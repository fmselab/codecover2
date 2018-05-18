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

import java.io.File;

/**
 * Contains methods used by JUnit-Tests.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: UtilsForTestingModel.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class UtilsForTestingModel {
    /** point to the instrumentation folde in code */
    public static String BASEDIR;

    static {
        String baseDir = System.getProperty("basedir");

        if (baseDir == null) {
            BASEDIR = (new File("")).getAbsolutePath() + File.separatorChar;
        } else {
            BASEDIR = (new File(baseDir)).getAbsolutePath() + File.separatorChar;
        }
    }
}
