package org.codecover.instrumentation.java15.manipulators;

import java.io.IOException;

import org.codecover.instrumentation.java15.counter.CounterIDManager;
import org.codecover.instrumentation.java15.syntaxtree.ConditionalOrExpression;
import org.codecover.instrumentation.java15.visitor.TreeDumperWithException;

/**
 * @author RS, 30.10.2009
 * The ?-Operator
 *
 * @version 1.0 ($Id$)
 */
public class DummyQMOManipulator extends AbstractDummyManipulator 
implements QMOManipulator {

	public void manipulateBefore(ConditionalOrExpression n, String statementID)
	throws IOException {
		// do nothing

	}
	public void manipulateAfter(ConditionalOrExpression n, String statementID)
	throws IOException {
		// do nothing

	}

}
