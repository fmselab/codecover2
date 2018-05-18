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

import java.io.File;
import java.io.PrintWriter;
import java.io.StringReader;
import java.util.UUID;

import org.codecover.instrumentation.HierarchyLevelContainer;
import org.codecover.instrumentation.xampil.manipulator.DefaultBranchManipulator;
import org.codecover.instrumentation.xampil.manipulator.DefaultConditionManipulator;
import org.codecover.instrumentation.xampil.manipulator.DefaultLoopManipulator;
import org.codecover.instrumentation.xampil.manipulator.DefaultStatementManipulator;
import org.codecover.instrumentation.xampil.parser.CharStream;
import org.codecover.instrumentation.xampil.parser.InstrumentableItemCounter;
import org.codecover.instrumentation.xampil.parser.SimpleCharStream;
import org.codecover.instrumentation.xampil.parser.XampilParser;
import org.codecover.instrumentation.xampil.syntaxtree.CompilationUnit;
import org.codecover.instrumentation.xampil.visitor.InstrumentationVisitor;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.HierarchyLevelType;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.utils.SimpleLogger;
import org.codecover.model.utils.file.FileTool;

/**
 * @author Christoph Müller
 */
public class Main {
    private static final String TARGET_FILE_PATH = "../../website/development/programming-language-files/";

    private static final String TARGET_FILE_NAME = "example.xpl";

    private static final boolean INSTRUMENT = true;

    public static void main(String[] args) {
        try {
            File targetFile;
            if (args.length == 1 && args[0] != null && !args[0].equals("")) {
                targetFile = new File(args[0]);
            } else {
                targetFile = new File(TARGET_FILE_PATH + TARGET_FILE_NAME);
            }

            targetFile = targetFile.getCanonicalFile();

            System.out.println("Reading: " + targetFile.getCanonicalPath());
            MASTBuilder mastBuilder = new MASTBuilder(new SimpleLogger());
            SourceFile sourceFile = mastBuilder.createSourceFile(targetFile.getName(),
                    FileTool.getContentFromFile(targetFile));
            CharStream charStream = new SimpleCharStream(new StringReader(sourceFile.getContent()));
            XampilParser parser = new XampilParser(charStream);
            InstrumentableItemCounter counter = new InstrumentableItemCounter();
            CompilationUnit compilationUnit = parser.CompilationUnit(counter);

            if (INSTRUMENT) {
                instrument(compilationUnit, counter, mastBuilder, sourceFile);
            } else {
                justCounters(counter);
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            System.out.println("Finished main method");
        }
    }

    private static void instrument(CompilationUnit compilationUnit,
                                   InstrumentableItemCounter counter,
                                   MASTBuilder mastBuilder,
                                   SourceFile sourceFile) throws Exception {
        PrintWriter writer = new PrintWriter(System.out);
        HierarchyLevelType rootType = HierarchyLevelTypes.getSourceFileType(mastBuilder);
        HierarchyLevelContainer rootHierarchyLevelContainer = new HierarchyLevelContainer(
                rootType.getInternalName(), rootType, rootType);
        InstrumentationVisitor visitor = new InstrumentationVisitor(writer,
                                                     counter,
                                                     mastBuilder,
                                                     sourceFile,
                                                     rootHierarchyLevelContainer,
                                                     UUID.randomUUID().toString());
        visitor.setStatementManipulator(new DefaultStatementManipulator());
        visitor.setBranchManipulator(new DefaultBranchManipulator());
        visitor.setLoopManipulator(new DefaultLoopManipulator());
        visitor.setConditionManipulator(new DefaultConditionManipulator());
        visitor.visit(compilationUnit);
        writer.flush();

        System.out.println("\n");
        justCounters(counter);
    }

    private static void justCounters(InstrumentableItemCounter counter) {
        System.out.println("Statements:  " + counter.getStatementCount());
        System.out.println("Branches:    " + counter.getBranchCount());
        System.out.println("Loops:       " + counter.getLoopCount());
        int conditionCount = counter.getConditionCount();
        System.out.println("Conditions:  " + conditionCount);
        for (int i = 0; i < conditionCount; i++) {
            System.out.println("Condition " + i + ": " + counter.getBasicBooleanCount(i));
        }
    }
}
