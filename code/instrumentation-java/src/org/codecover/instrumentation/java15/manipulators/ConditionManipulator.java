/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.instrumentation.java15.manipulators;

import java.io.IOException;

import org.codecover.instrumentation.booleanterms.InstrBooleanTerm;
import org.codecover.instrumentation.java15.visitor.InstrumentationVisitor;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.CoverableItem;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.SourceFile;

/**
 * This Manipulator is used for instrumentation of basic boolean terms in if,
 * while, do while, for and ternary operator.<br>
 * </br> A object of this interface is called by the
 * {@link InstrumentationVisitor}. This can either be an
 * {@link DummyConditionManipulator} or an {@link ArrayConditionManipulator}.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: ConditionManipulator.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @see DummyConditionManipulator
 * @see ArrayConditionManipulator
 */
@SuppressWarnings("all")
public interface ConditionManipulator extends Manipulator {

    /**
     * Instrument the booleanTerm and save the result under
     * {@link ConditionManipualtionResult#instrumentedTerm}.<br>
     * Create a {@link RootTerm} out of the original term and save it under
     * {@link ConditionManipualtionResult#rootTermForMast}. This term can be
     * <code>null</code>, representing a term, that is not used for coverage
     * measurement&mdash;e.g. because of special circumstances.<br>
     * Write all initialization to the writer, that are needed for measurement.<br>
     * {@link ConditionManipualtionResult#warningMessage} is used to indicate,
     * that something got wrong. This message is printed, if not <code>null</code>.
     */
    public ConditionManipualtionResult manipulateAndDeclare(InstrBooleanTerm booleanTerm,
                                                            String conditionID,
                                                            CoverableItem coverableItem,
                                                            MASTBuilder builder,
                                                            SourceFile sourceFile) throws IOException;

    public static class ConditionManipualtionResult {
        public InstrBooleanTerm instrumentedTerm = null;
        public RootTerm rootTermForMast = null;
        public String warningMessage = null;
    }
}
