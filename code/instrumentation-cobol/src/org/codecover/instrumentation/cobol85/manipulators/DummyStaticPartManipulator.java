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

package org.codecover.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;

import org.codecover.instrumentation.cobol85.syntaxtree.GobackStatement;
import org.codecover.instrumentation.cobol85.syntaxtree.NodeToken;
import org.codecover.instrumentation.cobol85.syntaxtree.Statement;
import org.codecover.instrumentation.cobol85.syntaxtree.StopStatement;
import org.codecover.instrumentation.cobol85.visitor.Visitor;

/**
 * This is a dummy implementation of the static part manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: DummyStaticPartManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class DummyStaticPartManipulator implements StaticPartManipulator {

    public void generateDataDivision(String testSessionContainerUID, PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateEnvironmentDivision(String fileName,
            PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateFileDescription(String testSessionContainerUID,
            PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateInputOutputSection(String fileName,
            PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateWriteLogic(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateWriteLogicForNestedPrograms(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateWorkingStorageSection(int programUnit,
            PrintWriter printWriter) {
        // The dummy does not write
    }

    public void replaceStopRun(Visitor visitor, Statement statement, PrintWriter printWriter) {
        // The dummy just visit stop and goback node to output without a change
        if (statement.f0.choice instanceof StopStatement) {
            StopStatement stopStatement = (StopStatement) statement.f0.choice;
            stopStatement.accept(visitor);
        } else if (statement.f0.choice instanceof GobackStatement) {
            GobackStatement gobackStatement = (GobackStatement) statement.f0.choice;
            gobackStatement.accept(visitor);
        }
    }

    public void replaceEndTestCase(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void replaceStartTestCase(NodeToken nodeToken,
            PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateEndTestCaseDeclaration(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateOpenCoverageFile(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateFileControl(String fileName, PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateSelectFile(String fileName, PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateFileSection(String testSessionContainerUID,
            PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateDataEntries(int programUnit, PrintWriter printWriter) {
        // The dummy does not write
    }

}
