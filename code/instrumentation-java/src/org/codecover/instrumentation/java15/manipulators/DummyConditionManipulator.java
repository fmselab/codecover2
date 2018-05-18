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
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.CoverableItem;
import org.codecover.model.mast.SourceFile;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: DummyConditionManipulator.java 1895 2007-09-17 11:16:31Z
 *          kiesssn $)
 */
public class DummyConditionManipulator extends AbstractDummyManipulator
        implements ConditionManipulator {

    public ConditionManipualtionResult manipulateAndDeclare(InstrBooleanTerm booleanTerm,
            String conditionID,
            CoverableItem coverableItem,
            MASTBuilder builder,
            SourceFile sourceFile) throws IOException {
        // do not instrument anything;
        ConditionManipualtionResult returnValue = new ConditionManipualtionResult();
        returnValue.instrumentedTerm = booleanTerm;
        returnValue.rootTermForMast = null;
        return returnValue;
    }
}
