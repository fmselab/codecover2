///////////////////////////////////////////////////////////////////////////////
//
// $Id: DefaultBranchManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 17:31:25
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.manipulators;

import java.io.IOException;

import org.gbt2.instrumentation.java15.syntaxtree.IfStatement;
import org.gbt2.instrumentation.java15.syntaxtree.SwitchLabel;
import org.gbt2.instrumentation.java15.syntaxtree.SwitchStatement;
import org.gbt2.instrumentation.java15.syntaxtree.TryStatement;
import org.gbt2.instrumentation.java15.syntaxtree.Type;

import static org.gbt2.instrumentation.java15.visitor.TreeDumperWithException.LINE_SEPERATOR;

/**
 * @author Christoph Müller
 */
public class DefaultBranchManipulator extends AbstractDefaultManipulator
        implements BranchManipulator {

    /**
     * Always true.
     * 
     * @return true;
     */
    public boolean requiresBlockExpansionsForBranches() {
        return true;
    }

    /**
     * Always false.
     * 
     * @return false;
     */
    public boolean requiresBlockExpansionsForLoops() {
        return false;
    }

    public void manipulateIf(IfStatement n, String ifBranchID)
            throws IOException {
        super.getCounterIDManager().writeCounterIncrementingForBranch(
                ifBranchID);
    }

    public void manipulateElse(IfStatement n, String elseBranchID,
            boolean implicitElse) throws IOException {
        if (implicitElse) {
            // if the else branch is not present, we have to create an implicit
            // branch
            super.getWriter().write(" else {");
            super.getWriter().write(LINE_SEPERATOR);
            super.getWriter().write("  ");
            super.getCounterIDManager().writeCounterIncrementingForBranch(
                    elseBranchID);
            super.getWriter().write("}");
        } else {
            super.getCounterIDManager().writeCounterIncrementingForBranch(
                    elseBranchID);
        }
    }

    public void manipulateSwitchCase(SwitchLabel switchLabel,
            String caseBranchID) throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        super.getCounterIDManager().writeCounterIncrementingForBranch(
                caseBranchID);
    }

    public void manipulateSwitchDefault(SwitchStatement n,
            String defaultBranchID, boolean implicitDefault) throws IOException {
        if (implicitDefault) {
            // if the default branch is not present, we have to create an
            // implicit branch

            super.getWriter().write(" default : ");
            super.getCounterIDManager().writeCounterIncrementingForBranch(
                    defaultBranchID);
        } else {
            super.getWriter().write(LINE_SEPERATOR);
            super.getCounterIDManager().writeCounterIncrementingForBranch(
                    defaultBranchID);
        }
    }

    public void manipulateTry(TryStatement n, String branchID,
            String tryBranchID) throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        super.getCounterIDManager().writeTryBranchSetTrue(tryBranchID);
    }

    public void manipulateCatch(TryStatement n, Type catchType,
            String catchBranchId, String tryBranchID) throws IOException {

        super.getWriter().write(LINE_SEPERATOR);
        super.getCounterIDManager().writeTryBranchSetFalse(tryBranchID);
        super.getWriter().write(LINE_SEPERATOR);
        super.getCounterIDManager().writeCounterIncrementingForBranch(
                catchBranchId);
    }

    public void manipulateAddThrowable(TryStatement n,
            String throwableBranchId, String tryBranchID) throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("catch (Throwable t) {");
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("  ");
        super.getCounterIDManager().writeTryBranchSetFalse(tryBranchID);
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("  ");
        super.getCounterIDManager().writeCounterIncrementingForBranch(
                throwableBranchId);
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("}");
    }

    public void manipulateFinally(TryStatement n, String branchID,
            String tryBranchID, boolean implicitFinally) throws IOException {
        if (implicitFinally) {
            super.getWriter().write(" finally {");
            super.getWriter().write(LINE_SEPERATOR);
            super.getWriter().write("    ");
            super.getCounterIDManager().writeTryBranchFinally(branchID,
                    tryBranchID);
            super.getWriter().write(LINE_SEPERATOR);
            super.getWriter().write("  }");

        } else {
            super.getWriter().write(LINE_SEPERATOR);
            super.getCounterIDManager().writeTryBranchFinally(branchID,
                    tryBranchID);
        }

    }
}
