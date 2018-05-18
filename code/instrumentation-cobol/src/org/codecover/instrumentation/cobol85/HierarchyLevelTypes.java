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

package org.codecover.instrumentation.cobol85;

import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.HierarchyLevelType;

/**
 * @author Stefan Franke
 * @version 1.0 ($Id: HierarchyLevelTypes.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class HierarchyLevelTypes {

    private static final String SOURCE_FILE_INTERN = "sourceFile";

    private static final String SOURCE_FILE = "COBOL source file";

    private static final String PROGRAM_UNIT_INTERN = "programUnit";

    private static final String PROGRAM_UNIT = "program unit";

    private static final String SECTION_INTERN = "section";

    private static final String SECTION = "section";

    private static final String PARAGRAPH_INTERN = "method";

    private static final String PARAGRAPH = "paragraph";

    private static final String NESTED_PROGRAM_INTERN = "nestedProgram";

    private static final String NESTED_PROGRAM = "nested program";

    private static HierarchyLevelType sourceFile = null;

    private static HierarchyLevelType programUnit = null;

    private static HierarchyLevelType section = null;

    private static HierarchyLevelType paragraph = null;

    private static HierarchyLevelType nestedProgram = null;

    /**
     * Returns the hierarchy level type for nested programs.
     * 
     * @param builder the MASTBuilder
     * @return the hierarchy level type
     */
    public static HierarchyLevelType getNestedProgramType(MASTBuilder builder) {
        if (nestedProgram == null) {
            return nestedProgram = builder.createHierarchyLevelType(NESTED_PROGRAM,
                    NESTED_PROGRAM_INTERN);
        }
        return nestedProgram;
    }

    /**
     * Returns the hierarchy level type for paragraphs.
     * 
     * @param builder the MASTBuilder
     * @return the hierarchy level type
     */
    public static HierarchyLevelType getParagraphType(MASTBuilder builder) {
        if (paragraph == null) {
            return paragraph = builder.createHierarchyLevelType(PARAGRAPH,
                    PARAGRAPH_INTERN);
        }
        return paragraph;
    }

    /**
     * Returns the hierarchy level type for program units.
     * 
     * @param builder the MASTBuilder
     * @return the hierarchy level type
     */
    public static HierarchyLevelType getProgramUnitType(MASTBuilder builder) {
        if (programUnit == null) {
            return programUnit = builder.createHierarchyLevelType(PROGRAM_UNIT,
                    PROGRAM_UNIT_INTERN);
        }
        return programUnit;
    }

    /**
     * Returns the hierarchy level type for sections.
     * 
     * @param builder the MASTBuilder
     * @return the hierarchy level type
     */
    public static HierarchyLevelType getSectionType(MASTBuilder builder) {
        if (section == null) {
            return section = builder.createHierarchyLevelType(SECTION,
                    SECTION_INTERN);
        }
        return section;
    }

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

}
