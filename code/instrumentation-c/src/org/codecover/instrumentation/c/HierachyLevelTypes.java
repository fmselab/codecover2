package org.codecover.instrumentation.c;

import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.HierarchyLevelType;

public class HierachyLevelTypes {
    private static HierarchyLevelType program = null;
    private static HierarchyLevelType sourceFile = null;
    private static HierarchyLevelType function = null;

    public static HierarchyLevelType getProgramType(MASTBuilder builder) {
        if (program == null) {
            return program = builder.createHierarchyLevelType("Program",
                    "program");
        }
        return program;
    }

    public static HierarchyLevelType getSourceFileType(MASTBuilder builder) {
        if (sourceFile == null) {
            return sourceFile = builder.createHierarchyLevelType("Source File",
                    "sourceFile");
        }
        return sourceFile;
    }

    public static HierarchyLevelType getFunctionType(MASTBuilder builder) {
        if (function == null) {
            return function = builder.createHierarchyLevelType("Function",
                    "function");
        }
        return function;
    }
}
