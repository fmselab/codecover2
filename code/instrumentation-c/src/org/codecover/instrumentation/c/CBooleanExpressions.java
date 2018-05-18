package org.codecover.instrumentation.c;

import org.codecover.instrumentation.booleanterms.InstrBooleanOperator;
import org.codecover.model.mast.BooleanAssignment;
import static org.codecover.model.mast.BooleanResult.FALSE;
import static org.codecover.model.mast.BooleanResult.NOT_EVALUATED;
import static org.codecover.model.mast.BooleanResult.TRUE;

import java.util.HashMap;
import java.util.Map;

public class CBooleanExpressions {
    public final static InstrBooleanOperator orOperator;
    public final static InstrBooleanOperator andOperator;
    public final static InstrBooleanOperator notOperator;
    public final static InstrBooleanOperator trueOperator;
    public final static InstrBooleanOperator falseOperator;
    public final static InstrBooleanOperator conditionalOperator;

    static {
        HashMap<BooleanAssignment, Boolean> possibleAssignments =
                new HashMap<BooleanAssignment, Boolean>();

        possibleAssignments.put(new BooleanAssignment(FALSE, FALSE),
                Boolean.FALSE);

        possibleAssignments.put(new BooleanAssignment(FALSE, TRUE),
                Boolean.TRUE);

        possibleAssignments.put(new BooleanAssignment(TRUE, FALSE),
                Boolean.TRUE);

        possibleAssignments.put(new BooleanAssignment(TRUE, TRUE),
                Boolean.TRUE);

        orOperator = InstrBooleanOperator.getTwoArgumentOperator(
                "OR", "||", possibleAssignments);
    }

    static {
        HashMap<BooleanAssignment, Boolean> possibleAssignments =
                new HashMap<BooleanAssignment, Boolean>();

        possibleAssignments.put(new BooleanAssignment(FALSE, FALSE),
                Boolean.FALSE);

        possibleAssignments.put(new BooleanAssignment(FALSE, TRUE),
                Boolean.FALSE);

        possibleAssignments.put(new BooleanAssignment(TRUE, FALSE),
                Boolean.FALSE);

        possibleAssignments.put(new BooleanAssignment(TRUE, TRUE),
                Boolean.TRUE);

        andOperator = InstrBooleanOperator.getTwoArgumentOperator(
                "AND", "&&", possibleAssignments);
    }

    static {
        HashMap<BooleanAssignment, Boolean> possibleAssignments =
                new HashMap<BooleanAssignment, Boolean>();

        possibleAssignments.put(new BooleanAssignment(FALSE),
                Boolean.TRUE);

        possibleAssignments.put(new BooleanAssignment(TRUE),
                Boolean.FALSE);

        notOperator = InstrBooleanOperator.getOneArgumentOperator(
                "NOT", "!", false, possibleAssignments);
    }

    static {
        HashMap<BooleanAssignment, Boolean> possibleAssignments =
                new HashMap<BooleanAssignment, Boolean>();

        possibleAssignments.put(new BooleanAssignment(),
                Boolean.TRUE);


        trueOperator = InstrBooleanOperator.getConstantOperator(
                "TRUE", "1", possibleAssignments);
    }

    static {
        HashMap<BooleanAssignment, Boolean> possibleAssignments =
                new HashMap<BooleanAssignment, Boolean>();

        possibleAssignments.put(new BooleanAssignment(),
                Boolean.FALSE);

        falseOperator = InstrBooleanOperator.getConstantOperator(
                "FALSE", "0", possibleAssignments);
    }

    static {
        Map<BooleanAssignment, Boolean> possibleAssignments = new HashMap<BooleanAssignment, Boolean>();

        possibleAssignments.put(new BooleanAssignment(FALSE, NOT_EVALUATED, FALSE),
                Boolean.FALSE);

        possibleAssignments.put(new BooleanAssignment(FALSE, NOT_EVALUATED, TRUE),
                Boolean.TRUE);

        possibleAssignments.put(new BooleanAssignment(TRUE, FALSE, NOT_EVALUATED),
                Boolean.FALSE);

        possibleAssignments.put(new BooleanAssignment(TRUE, TRUE, NOT_EVALUATED),
                Boolean.TRUE);

        conditionalOperator = new InstrBooleanOperator(
                "conditional operator", new String[]{ "", " ? ", " : ", "" }, possibleAssignments);
    }
}
