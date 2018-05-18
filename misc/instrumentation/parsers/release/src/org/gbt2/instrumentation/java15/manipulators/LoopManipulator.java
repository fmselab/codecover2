///////////////////////////////////////////////////////////////////////////////
//
// $Id: LoopManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 17:28:50
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.manipulators;

import java.io.IOException;

import org.gbt2.instrumentation.java15.syntaxtree.DoStatement;
import org.gbt2.instrumentation.java15.syntaxtree.ForStatement;
import org.gbt2.instrumentation.java15.syntaxtree.WhileStatement;
import org.gbt2.instrumentation.java15.visitor.InstrumentationVisitorWithException;

/**
 * This Manipulator is used for instrumentation of loops.<br>
 * </br> A object of this interface is called by the
 * {@link InstrumentationVisitorWithException}. This can either be an
 * {@link DummyLoopManipulator} or an {@link DefaultLoopManipulator}.
 * 
 * @author Christoph Müller
 * 
 * @see DummyLoopManipulator
 * @see DefaultLoopManipulator
 */
public interface LoopManipulator extends Manipulator {

    public void manipulateBefore(ForStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateInner(ForStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateAfter(ForStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateBefore(WhileStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateInner(WhileStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateAfter(WhileStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateBefore(DoStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateInner(DoStatement n, String primaryLoopID)
            throws IOException;

    public void manipulateAfter(DoStatement n, String primaryLoopID)
            throws IOException;
}
