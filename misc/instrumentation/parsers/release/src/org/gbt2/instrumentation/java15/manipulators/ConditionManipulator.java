///////////////////////////////////////////////////////////////////////////////
//
// $Id: ConditionManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 17:28:50
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.manipulators;

import org.gbt2.instrumentation.java15.visitor.InstrumentationVisitorWithException;

/**
 * This Manipulator is used for instrumentation of basic boolean terms in if,
 * while, do while, for and trinariy operator.<br>
 * </br> A object of this interface is called by the
 * {@link InstrumentationVisitorWithException}. This can either be an
 * {@link DummyConditionManipulator} or an {@link DefaultConditionManipulator}.
 * 
 * @author Christoph Müller
 * 
 * @see DummyConditionManipulator
 * @see DefaultConditionManipulator
 */
public interface ConditionManipulator extends Manipulator {

}
