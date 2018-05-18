///////////////////////////////////////////////////////////////////////////////
//
// $Id: DummyStaticPartManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 28.03.2007 18:49:52
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85.manipulators;

import java.io.PrintWriter;

import org.gbt2.instrumentation.cobol85.syntaxtree.GobackStatement;
import org.gbt2.instrumentation.cobol85.syntaxtree.NodeChoice;
import org.gbt2.instrumentation.cobol85.syntaxtree.NodeToken;
import org.gbt2.instrumentation.cobol85.syntaxtree.ProgramUnit;
import org.gbt2.instrumentation.cobol85.syntaxtree.StopStatement;

/**
 * This is a dummy implementation of the static part manipulator for COBOL.
 * 
 * @author Stefan Franke
 * @version 1.0 - 28.03.2007
 * 
 */
public class DummyStaticPartManipulator implements StaticPartManipulator {

    public void generateDataDivision(String programName, PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateEnvironmentDivision(String fileName,
            PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateFileDescription(String programName,
            PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateInputOutputSection(String fileName,
            PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateHorizontalLineInCoverageFile(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateWriteLogic(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void generateWorkingStorageSection(ProgramUnit programUnit,
            PrintWriter printWriter) {
        // The dummy does not write
    }

    public void replaceStopRun(NodeChoice nodeChoice, PrintWriter printWriter) {
        // The dummy just writes stop and goback to output without a change
        if (nodeChoice.choice instanceof StopStatement) {
            printWriter
                    .printf(((StopStatement) nodeChoice.choice).f0.tokenImage);
        } else if (nodeChoice.choice instanceof GobackStatement) {
            printWriter
                    .printf(((GobackStatement) nodeChoice.choice).f0.tokenImage);
        }
    }

    public void replaceEndTestCase(PrintWriter printWriter) {
        // The dummy does not write
    }

    public void replaceStartTestCase(NodeToken nodeToken,
            PrintWriter printWriter) {
        // The dummy does not write
    }

}
