///////////////////////////////////////////////////////////////////////////////
//
// $Id: StaticPartManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 28.03.2007 18:35:40
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;

import org.gbt2.instrumentation.cobol85.syntaxtree.NodeChoice;
import org.gbt2.instrumentation.cobol85.syntaxtree.NodeToken;
import org.gbt2.instrumentation.cobol85.syntaxtree.ProgramUnit;

/**
 * This is an interface for static part manipulators. A static part manipulator
 * inserts paragraphs and sections which are needed for writing the coverage
 * log file. Also, the program logic to start and end test cases, writing
 * file to disk und stop the program are included by a static part manipulator.
 * 
 * @author Stefan Franke
 * @version 1.0 - 28.03.2007
 *
 */
public interface StaticPartManipulator {
    
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
     * Generates data division into the output. It is not necessary to
     * call generateFileDescription(...) afterwards. This method generates the
     * whole entry which is needed for accessing a file with the given file name.
     * 
     * @param programName the program name of the instrumented program unit
     * @param printWriter the output writer
     */
    public void generateDataDivision(String programName, PrintWriter printWriter);
    
    /**
     * Generates file section into the output.
     * 
     * @param programName the program name of the instrumented program unit
     * @param printWriter the output writer
     */
    public void generateFileDescription(String programName, PrintWriter printWriter);
    
    /**
     * Writes a horizontal line in the coverage log file.
     * 
     * @param printWriter the output writer
     */
    public void generateHorizontalLineInCoverageFile(PrintWriter printWriter);
    
    /**
     * Generates the program logic to start and end test cases, writing
     * file to disk und stop the program. 
     * 
     * @param printWriter the output writer
     */
    public void generateWriteLogic(PrintWriter printWriter);
    
    /**
     * Generates the data fields in the working storage section. These data fields are
     * auxiliary looping counter and the start test case variable.
     * 
     * @param programUnit the program unit
     * @param printWriter the output writer
     */
    public void generateWorkingStorageSection(ProgramUnit programUnit, PrintWriter printWriter);
    
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
     * @param nodeChoice node choice
     * @param printWriter the output writer
     */
    public void replaceStopRun(NodeChoice nodeChoice, PrintWriter printWriter);
    
}
