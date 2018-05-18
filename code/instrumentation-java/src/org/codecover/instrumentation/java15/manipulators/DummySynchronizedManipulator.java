package org.codecover.instrumentation.java15.manipulators;

import java.io.IOException;

import org.codecover.instrumentation.java15.counter.CounterIDManager;
import org.codecover.instrumentation.java15.syntaxtree.SynchronizedStatement;
import org.codecover.instrumentation.java15.visitor.TreeDumperWithException;

/**
 * @author 
 *
 * @version 1.0 ($Id$)
 */
public class DummySynchronizedManipulator extends AbstractDummyManipulator 
        implements SynchronizedManipulator {

    public void manipulateBefore(SynchronizedStatement n, String statementID)
            throws IOException {
        // do nothing

    }

    public void manipulateInnerBefore(SynchronizedStatement n, String statementID)
            throws IOException {
        // do nothing
    }

    public void manipulateInnerAfter(SynchronizedStatement n, String statementID)
            throws IOException {
        // do nothing
    }

    public void manipulatePart1(SynchronizedStatement n, String statementID)
            throws IOException {
        // TODO Automatisch erstellter Methoden-Stub
        
    }

    public void manipulatePart2(SynchronizedStatement n, String statementID)
            throws IOException {
        // TODO Automatisch erstellter Methoden-Stub
        
    }

    public void manipulatePart3(SynchronizedStatement n, String statementID)
            throws IOException {
        // TODO Automatisch erstellter Methoden-Stub
        
    }

}
