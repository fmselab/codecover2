///////////////////////////////////////////////////////////////////////////////
//
// $Id: DummyLoopManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 17:32:21
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.manipulators;

import org.gbt2.instrumentation.java15.syntaxtree.DoStatement;
import org.gbt2.instrumentation.java15.syntaxtree.ForStatement;
import org.gbt2.instrumentation.java15.syntaxtree.WhileStatement;

/**
 * @author Christoph Müller
 */
public class DummyLoopManipulator extends AbstractDummyManipulator
        implements LoopManipulator {

    public void manipulateBefore(ForStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateInner(ForStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateAfter(ForStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateBefore(WhileStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateInner(WhileStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateAfter(WhileStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateBefore(DoStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateInner(DoStatement n, String primaryLoopID) {
        // do not add anything;
    }

    public void manipulateAfter(DoStatement n, String primaryLoopID) {
        // do not add anything;
    }
 }
