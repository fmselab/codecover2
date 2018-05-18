///////////////////////////////////////////////////////////////////////////////
//
// $Id: BranchManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 17:28:50
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.manipulators;

import java.io.IOException;

import org.gbt2.instrumentation.java15.syntaxtree.IfStatement;
import org.gbt2.instrumentation.java15.syntaxtree.SwitchLabel;
import org.gbt2.instrumentation.java15.syntaxtree.SwitchStatement;
import org.gbt2.instrumentation.java15.syntaxtree.TryStatement;
import org.gbt2.instrumentation.java15.syntaxtree.Type;
import org.gbt2.instrumentation.java15.visitor.InstrumentationVisitorWithException;

/**
 * This Manipulator is used for instrumentation of branches.<br>
 * </br> A object of this interface is called by the
 * {@link InstrumentationVisitorWithException}. This can either be an
 * {@link DummyBranchManipulator} or an {@link DefaultBranchManipulator}.
 * 
 * @author Christoph Müller
 * 
 * @see DummyBranchManipulator
 * @see DefaultBranchManipulator
 */
public interface BranchManipulator extends Manipulator {

    public void manipulateIf(IfStatement n, String ifBranchID)
            throws IOException;

    public void manipulateElse(IfStatement n, String elseBranchID,
            boolean implicitElse) throws IOException;

    public void manipulateSwitchCase(SwitchLabel switchLabel,
            String caseBranchID) throws IOException;

    public void manipulateSwitchDefault(SwitchStatement n,
            String defaultBranchID, boolean implicitDefault) throws IOException;

    public void manipulateTry(TryStatement n, String branchID,
            String tryBranchID) throws IOException;

    public void manipulateCatch(TryStatement n, Type catchType,
            String catchBranchId, String tryBranchID) throws IOException;

    public void manipulateAddThrowable(TryStatement n,
            String throwableBranchId, String tryBranchID) throws IOException;

    public void manipulateFinally(TryStatement n, String branchID,
            String tryBranchID, boolean implicitFinally) throws IOException;

}
