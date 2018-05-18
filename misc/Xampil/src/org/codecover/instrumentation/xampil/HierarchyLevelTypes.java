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

package org.codecover.instrumentation.xampil;

import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.HierarchyLevelType;

/**
 * @author Stefan Franke
 */
public class HierarchyLevelTypes {

    private static final String SOURCE_FILE_INTERN = "sourceFile";

    private static final String SOURCE_FILE = "Xampil source file";

    private static final String PROGRAM_INTERN = "PROGRAM";

    private static final String PROGRAM = "program";

    private static HierarchyLevelType sourceFile = null;

    private static HierarchyLevelType program = null;

    /**
     * Returns the hierarchy level type for source files.
     * 
     * @param builder the MASTBuilder
     * @return the hierarchy level type
     */
    public static HierarchyLevelType getSourceFileType(MASTBuilder builder) {
        if (sourceFile == null) {
            return sourceFile = builder.createHierarchyLevelType(SOURCE_FILE,
                    SOURCE_FILE_INTERN);
        }
        return sourceFile;
    }

    /**
     * Returns the hierarchy level type for program units.
     * 
     * @param builder the MASTBuilder
     * @return the hierarchy level type
     */
    public static HierarchyLevelType getProgramType(MASTBuilder builder) {
        if (program == null) {
            return program = builder.createHierarchyLevelType(PROGRAM,
                    PROGRAM_INTERN);
        }
        return program;
    }
}
