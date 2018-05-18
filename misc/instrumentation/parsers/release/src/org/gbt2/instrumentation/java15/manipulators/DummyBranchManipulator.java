///////////////////////////////////////////////////////////////////////////////
//
// $Id: DummyBranchManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 17:32:21
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.manipulators;

import org.gbt2.instrumentation.java15.syntaxtree.IfStatement;
import org.gbt2.instrumentation.java15.syntaxtree.SwitchLabel;
import org.gbt2.instrumentation.java15.syntaxtree.SwitchStatement;
import org.gbt2.instrumentation.java15.syntaxtree.TryStatement;
import org.gbt2.instrumentation.java15.syntaxtree.Type;

/**
 * @author Christoph Müller
 */
public class DummyBranchManipulator extends AbstractDummyManipulator implements
        BranchManipulator {

    public void manipulateIf(IfStatement n, String ifBranchID) {
        // do not add anything;
    }

    public void manipulateElse(IfStatement n, String elseBranchID,
            boolean implicitElse) {
        // do not add anything;
    }

    public void manipulateSwitchCase(SwitchLabel switchLabel,
            String caseBranchID) {
        // do not add anything;
    }

    public void manipulateSwitchDefault(SwitchStatement n,
            String defaultBranchID, boolean implicitDefault) {
        // do not add anything;
    }

    public void manipulateTry(TryStatement n, String branchID,
            String tryBranchID) {
        // do not add anything;
    }
    
    public void manipulateCatch(TryStatement n, Type catchType,
            String catchBranchId, String tryBranchID) {
        // do not add anything;
    }

    public void manipulateAddThrowable(TryStatement n,
            String throwableBranchId, String tryBranchID) {
        // do not add anything;
    }

    public void manipulateFinally(TryStatement n, String branchID,
            String tryBranchID, boolean implicitFinally) {
        // do not add anything;
    }
}
