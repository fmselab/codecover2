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

package org.codecover.instrumentation.cobol85;

import java.util.HashMap;
import java.util.Map;

import org.codecover.instrumentation.booleanterms.InstrBooleanOperator;
import org.codecover.model.mast.BooleanAssignment;
import org.codecover.model.mast.BooleanResult;

/**
 * @author Stefan Franke
 * @version 1.0 ($Id: BooleanOperators.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class BooleanOperators {

    private static Map<BooleanAssignment, Boolean> andOperatorMap;

    private static Map<BooleanAssignment, Boolean> orOperatorMap;

    private static Map<BooleanAssignment, Boolean> notOperatorMap;

    private static InstrBooleanOperator andOperator;

    private static InstrBooleanOperator orOperator;

    private static InstrBooleanOperator notOperator;

    static {
        andOperatorMap = new HashMap<BooleanAssignment, Boolean>();
        andOperatorMap.put(new BooleanAssignment(BooleanResult.FALSE, BooleanResult.FALSE), Boolean.FALSE);
        andOperatorMap.put(new BooleanAssignment(BooleanResult.FALSE, BooleanResult.TRUE), Boolean.FALSE);
        andOperatorMap.put(new BooleanAssignment(BooleanResult.TRUE, BooleanResult.FALSE), Boolean.FALSE);
        andOperatorMap.put(new BooleanAssignment(BooleanResult.TRUE, BooleanResult.TRUE), Boolean.TRUE);

        orOperatorMap = new HashMap<BooleanAssignment, Boolean>();
        orOperatorMap.put(new BooleanAssignment(BooleanResult.FALSE, BooleanResult.FALSE), Boolean.FALSE);
        orOperatorMap.put(new BooleanAssignment(BooleanResult.FALSE, BooleanResult.TRUE), Boolean.TRUE);
        orOperatorMap.put(new BooleanAssignment(BooleanResult.TRUE, BooleanResult.FALSE), Boolean.TRUE);
        orOperatorMap.put(new BooleanAssignment(BooleanResult.TRUE, BooleanResult.TRUE), Boolean.TRUE);

        notOperatorMap = new HashMap<BooleanAssignment, Boolean>();
        notOperatorMap.put(new BooleanAssignment(BooleanResult.FALSE), Boolean.FALSE);
        notOperatorMap.put(new BooleanAssignment(BooleanResult.TRUE), Boolean.TRUE);

        andOperator = InstrBooleanOperator.getTwoArgumentOperator(
                "COBOL and operator", "AND", andOperatorMap);

        orOperator = InstrBooleanOperator.getTwoArgumentOperator(
                "COBOL or operator", "OR", orOperatorMap);

        notOperator = InstrBooleanOperator.getOneArgumentOperator(
                "COBOL not operator", "NOT", true, notOperatorMap);
    }

    /**
     * Returns the and operator which is needed for codecover model.
     * 
     * @return and operator
     */
    public static final InstrBooleanOperator getAndOperator() {
        return andOperator;
    }

    /**
     * Returns the not operator which is needed for codecover model.
     * 
     * @return not operator
     */
    public static final InstrBooleanOperator getNotOperator() {
        return notOperator;
    }

    /**
     * Returns the or operator which is needed for codecover model.
     * 
     * @return or operator
     */
    public static final InstrBooleanOperator getOrOperator() {
        return orOperator;
    }

}
