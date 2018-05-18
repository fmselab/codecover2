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

import org.codecover.instrumentation.cobol85.syntaxtree.NodeChoice;
import org.codecover.instrumentation.cobol85.syntaxtree.NodeToken;
import org.codecover.instrumentation.cobol85.syntaxtree.Statement;
import org.codecover.instrumentation.cobol85.visitor.Visitor;

/**
 * This is an interface for static part manipulators. A static part manipulator
 * inserts paragraphs and sections which are needed for writing the coverage
 * log file. Also, the program logic to start and end test cases, writing
 * file to disk und stop the program are included by a static part manipulator.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: StaticPartManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public interface StaticPartManipulator {

    /**
     * Generates the end of test case declaration in the file description
     * part.
     * 
     * @param printWriter the output writer
     */
    public void generateEndTestCaseDeclaration(PrintWriter printWriter);

    /**
     * Generates environment division into the output. It is not necessary to
     * call generateInputOutputSection(...) afterwards. This method generates the
     * whole entry which is needed for accessing a file with the given file name.
     * 
     * @param fileName the file name of the coverage log file
     * @param printWriter the output writer
     */
    public void generateEnvironmentDivision(String fileName, PrintWriter printWriter);

    /**
     * Generates input-output section into the output.
     * 
     * @param fileName the file name of the coverage log file
     * @param printWriter the output writer
     */
    public void generateInputOutputSection(String fileName, PrintWriter printWriter);
    
    /**
     * Generates file control into the output.
     * 
     * @param fileName the file name of the coverage log file
     * @param printWriter the output writer
     */
    public void generateFileControl(String fileName, PrintWriter printWriter);
    
    /**
     * Generates select file into the output.
     * 
     * @param fileName the file name of the coverage log file
     * @param printWriter the output writer
     */
    public void generateSelectFile(String fileName, PrintWriter printWriter);

    /**
     * Generates data division into the output. It is not necessary to
     * call generateFileDescription(...) afterwards. This method generates the
     * whole entry which is needed for accessing a file with the given file name.
     * 
     * @param testSessionContainerUID the test session container UID of the
     *        instrumented source code
     * @param printWriter the output writer
     */
    public void generateDataDivision(String testSessionContainerUID, PrintWriter printWriter);

    /**
     * Generates file section into the output.
     * 
     * @param testSessionContainerUID the test session container UID of the
     *        instrumented source code
     * @param printWriter the output writer
     */
    public void generateFileSection(String testSessionContainerUID, PrintWriter printWriter);

    /**
     * Generates file description into the output.
     * 
     * @param testSessionContainerUID the test session container UID of the
     *        instrumented source code
     * @param printWriter the output writer
     */
    public void generateFileDescription(String testSessionContainerUID, PrintWriter printWriter);

    /**
     * Generates the program logic to start and end test cases, writing
     * file to disk and stop the program. 
     * 
     * @param printWriter the output writer
     */
    public void generateWriteLogic(PrintWriter printWriter);

    /**
     * Generates the program logic to start and end test cases, writing
     * file to disk und stop the nested program. 
     * 
     * @param printWriter the output writer
     */
    public void generateWriteLogicForNestedPrograms(PrintWriter printWriter);

    /**
     * Generates the data entries in the working storage section. These data fields are
     * auxiliary looping counter and the start test case variable.
     * 
     * @param programUnit the program unit
     * @param printWriter the output writer
     */
    public void generateWorkingStorageSection(int programUnit, PrintWriter printWriter);

    /**
     * Generates the data entries in the working storage section. These data fields are
     * auxiliary looping counter and the start test case variable.
     * 
     * @param programUnit the program unit
     * @param printWriter the output writer
     */
    public void generateDataEntries(int programUnit, PrintWriter printWriter);

    /**
     * Replaces end test case comments with call a special end test case
     * section.
     * 
     * @param printWriter the output writer
     */
    public void replaceEndTestCase(PrintWriter printWriter);

    /**
     * Replaces start test case comments with call a special start test case
     * section.
     * 
     * @param nodeToken node token
     * @param printWriter the output writer
     */
    public void replaceStartTestCase(NodeToken nodeToken, PrintWriter printWriter);

    /**
     * Replaces exit program statements with call a special stop the program
     * section.
     * 
     * @param visitor visitor
     * @param statement statement
     * @param printWriter the output writer
     */
    public void replaceStopRun(Visitor visitor, Statement statement, PrintWriter printWriter);

    /**
     * Generates the call of the open log file statement.
     * 
     * @param printWriter the output writer
     */
    public void generateOpenCoverageFile(PrintWriter printWriter);
}
