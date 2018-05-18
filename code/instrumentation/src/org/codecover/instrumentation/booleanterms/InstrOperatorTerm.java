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

package org.codecover.instrumentation.booleanterms;

import java.io.IOException;
import java.io.Writer;
import java.util.Collections;
import java.util.List;
import java.util.Vector;

import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.LocationList;
import org.codecover.model.mast.OperatorTerm;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.utils.CollectionUtil;

/**
 * This class extends the {@link InstrBooleanTerm} and represents a boolean
 * term, that consists of an operator and zero or more operands.<br>
 * <br>
 * This class is needed to represent boolean terms like <code>(a || b)</code><br>
 * The operator is an instance of {@link InstrBooleanOperator}. The operands
 * can be other {@link InstrBooleanTerm}s. There are several constructors to
 * create various types of {@link InstrOperatorTerm}s.<br>
 * <ul>
 * <li>{@link #InstrOperatorTerm(InstrBooleanOperator)}&mdash;without operands</li>
 * <li>{@link #InstrOperatorTerm(InstrBooleanOperator, InstrBooleanTerm)}&mdash;with
 * just a single operand</li>
 * <li>{@link #InstrOperatorTerm(InstrBooleanTerm, InstrBooleanOperator, InstrBooleanTerm)}&mdash;with
 * a left and a right operand (e.g. <code>a || b</code>)</li>
 * <li>{@link #InstrOperatorTerm(InstrBooleanOperator, List)}&mdash;with a
 * Queue of operands in this sequence.</li>
 * </ul>
 * After the construction {@link #checkArity()} is used to check whether:<br>
 * <code>operands.size() == operator.getArity()<br>
 * If not a {@link RuntimeException} is thrown.
 * <br>
 * The method {@link #termToString()} uses the
 * {@link InstrBooleanOperator#termToString(List)} method of its operator to
 * create the String.<br>
 * The method {@link #getAllBasicBooleanTerms(List)} orders all operands to add
 * {@link InstrBasicBooleanTerm} to the Queue by delegating this method.<br>
 * The method {@link #termToString()} transforms this {@link InstrOperatorTerm}
 * into an {@link OperatorTerm} of the MAST. This method is also implemented
 * recursively. For the {@link LocationList} of the {@link OperatorTerm} the
 * {@link Location} of the operator is used.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: InstrOperatorTerm.java 1 2007-12-12 17:37:26Z t-scheller $)
 * @see InstrBooleanOperator
 */
public class InstrOperatorTerm extends InstrBooleanTerm {
    /** An empty int array representing an empty operator location */
    public static final int[] EMPTY_START_END_LOCATION = new int[0];

    private InstrBooleanOperator operator;

    private List<InstrBooleanTerm> operands;

    private int[] operatorStartEndLocation;

    /**
     * This is a constructor for a {@link InstrOperatorTerm} with no operands.
     * 
     * @param constantOperator
     *            The {@link InstrBooleanOperator} that is a constant.
     *            
     * @throws NullPointerException
     *             if the constantOperator is <code>null</code>
     *             
     * @deprecated use {@link #InstrOperatorTerm(InstrBooleanOperator, int, int)}
     *          instead
     */
    @Deprecated
    public InstrOperatorTerm(InstrBooleanOperator constantOperator) {
        this(constantOperator, -1, -1);
    }

    /**
     * This is a constructor for a {@link InstrOperatorTerm} with no operands.
     * 
     * @param constantOperator
     *                The {@link InstrBooleanOperator} that is a constant.
     * @param startLocation
     *                The start of the location of the operator, <code>-1</code>
     *                &rarr; ignored.
     * @param endLocation
     *                The end of the location of the operator, <code>-1</code>
     *                &rarr; ignored.
     * 
     * @throws NullPointerException
     *                 if the constantOperator is <code>null</code>
     */
    public InstrOperatorTerm(InstrBooleanOperator constantOperator,
            int startLocation, int endLocation) {
        if (constantOperator == null) {
            throw new NullPointerException("operator == null");
        }

        this.operator = constantOperator;
        this.operands = Collections.<InstrBooleanTerm>emptyList();
        if (startLocation != -1 && endLocation != -1) {
            this.operatorStartEndLocation = new int[]{startLocation, endLocation};
        } else {
            this.operatorStartEndLocation = EMPTY_START_END_LOCATION;
        }

        checkArity();
    }

    /**
     * This is a constructor for a {@link InstrOperatorTerm} with just one
     * operand.
     * 
     * @param oneArgumentOperator
     *            The given {@link InstrBooleanOperator} with arity 1.
     * @param singleTerm
     *            The {@link InstrBooleanTerm} bounded by the operator.
     * 
     * @throws NullPointerException
     *             if the oneArgumentOperator or the singleTerm is
     *             <code>null</code>
     *             
     * @deprecated use {@link #InstrOperatorTerm(InstrBooleanOperator, InstrBooleanTerm, int, int)}
     *          instead
     */
    @Deprecated
    public InstrOperatorTerm(InstrBooleanOperator oneArgumentOperator,
            InstrBooleanTerm singleTerm) {
        this(oneArgumentOperator, singleTerm, -1, -1);
    }

    /**
     * This is a constructor for a {@link InstrOperatorTerm} with just one
     * operand.
     * 
     * @param oneArgumentOperator
     *            The given {@link InstrBooleanOperator} with arity 1.
     * @param singleTerm
     *            The {@link InstrBooleanTerm} bounded by the operator.
     * @param startLocation
     *                The start of the location of the operator, <code>-1</code>
     *                &rarr; ignored.
     * @param endLocation
     *                The end of the location of the operator, <code>-1</code>
     *                &rarr; ignored.
     *
     * @throws NullPointerException
     *             if the oneArgumentOperator or the singleTerm is
     *             <code>null</code>
     */
    public InstrOperatorTerm(InstrBooleanOperator oneArgumentOperator,
            InstrBooleanTerm singleTerm, int startLocation, int endLocation) {
        if (oneArgumentOperator == null) {
            throw new NullPointerException("operator == null");
        }
        if (singleTerm == null) {
            throw new NullPointerException("singleTerm == null");
        }

        this.operator = oneArgumentOperator;
        this.operands = Collections.<InstrBooleanTerm>singletonList(singleTerm);
        if (startLocation != -1 && endLocation != -1) {
            this.operatorStartEndLocation = new int[]{startLocation, endLocation};
        } else {
            this.operatorStartEndLocation = EMPTY_START_END_LOCATION;
        }

        checkArity();
    }

    /**
     * This is a constructor for a {@link InstrOperatorTerm} with two
     * operands&mdash;like <code>(a || b)</code>.
     * 
     * @param leftTerm
     *            The left boolean term (a).
     * 
     * @param twoArgumentsOperator
     *            The given {@link InstrBooleanOperator} with arity 2.
     * 
     * @param rightTerm
     *            The right boolean term (b).
     * 
     * @throws NullPointerException
     *             if the twoArgumentsOperator, the leftTerm or the rightTerm is
     *             <code>null</code>
     *             
     * @deprecated use {@link #InstrOperatorTerm(InstrBooleanTerm, InstrBooleanOperator, InstrBooleanTerm, int, int)}
     *          instead
     */
    @Deprecated
    public InstrOperatorTerm(InstrBooleanTerm leftTerm,
            InstrBooleanOperator twoArgumentsOperator,
            InstrBooleanTerm rightTerm) {
        this(leftTerm, twoArgumentsOperator, rightTerm, -1, -1);
    }

    /**
     * This is a constructor for a {@link InstrOperatorTerm} with two
     * operands&mdash;like <code>(a || b)</code>.
     * 
     * @param leftTerm
     *            The left boolean term (a).
     * @param twoArgumentsOperator
     *            The given {@link InstrBooleanOperator} with arity 2.
     * @param rightTerm
     *            The right boolean term (b).
     * @param startLocation
     *                The start of the location of the operator, <code>-1</code>
     *                &rarr; ignored.
     * @param endLocation
     *                The end of the location of the operator, <code>-1</code>
     *                &rarr; ignored.
     * 
     * @throws NullPointerException
     *             if the twoArgumentsOperator, the leftTerm or the rightTerm is
     *             <code>null</code>
     */
    public InstrOperatorTerm(InstrBooleanTerm leftTerm,
            InstrBooleanOperator twoArgumentsOperator,
            InstrBooleanTerm rightTerm, int startLocation, int endLocation) {
        if (twoArgumentsOperator == null) {
            throw new NullPointerException("operator == null");
        }
        if (leftTerm == null) {
            throw new NullPointerException("leftTerm == null");
        }
        if (rightTerm == null) {
            throw new NullPointerException("rightTerm == null");
        }

        this.operator = twoArgumentsOperator;
        Vector<InstrBooleanTerm> operandsTmp = new Vector<InstrBooleanTerm>(2);
        operandsTmp.add(leftTerm);
        operandsTmp.add(rightTerm);
        this.operands = Collections.unmodifiableList(operandsTmp);
        if (startLocation != -1 && endLocation != -1) {
            this.operatorStartEndLocation = new int[]{startLocation, endLocation};
        } else {
            this.operatorStartEndLocation = EMPTY_START_END_LOCATION;
        }

        checkArity();
    }

    /**
     * This is a constructor for a {@link InstrOperatorTerm} with more than two
     * operands&mdash;like <code>(a ? b : c)</code>.
     * 
     * @param operator
     *            The given {@link InstrBooleanOperator} with arity
     *            operands.size().
     * @param operands
     *            The operands in the sequence they occure in the expression (a,
     *            b, c).
     *            
     * @throws NullPointerException
     *             if the operator or the operands are <code>null</code>
     *             
     * @deprecated use {@link #InstrOperatorTerm(InstrBooleanOperator, List, int[])}
     *          instead
     */
    @Deprecated
    public InstrOperatorTerm(InstrBooleanOperator operator,
            List<InstrBooleanTerm> operands) {
        this(operator, operands, EMPTY_START_END_LOCATION);
    }

    /**
     * This is a constructor for a {@link InstrOperatorTerm} with more than two
     * operands&mdash;like <code>(a ? b : c)</code>.
     * 
     * @param operator
     *            The given {@link InstrBooleanOperator} with arity
     *            operands.size().
     * @param operands
     *            The operands in the sequence they occure in the expression (a,
     *            b, c).
     * @param operatorLocations
     *            The start and end location of all operators of this
     *            {@link InstrOperatorTerm}. For every operator two integers are
     *            expected: <code>[start1, end1, start2, end2, ...]</code>.
     *
     * @throws NullPointerException
     *             if the operator or the operands are <code>null</code>
     */
    public InstrOperatorTerm(InstrBooleanOperator operator,
            List<InstrBooleanTerm> operands, int[] operatorLocations) {
        if (operator == null) {
            throw new NullPointerException("operator == null");
        }
        if (operands == null) {
            throw new NullPointerException("operands == null");
        }
        if (operatorLocations == null) {
            throw new NullPointerException("operatorLocations == null");
        }

        this.operator = operator;
        this.operands = CollectionUtil.copy(operands);
        if (operatorLocations.length > 0) {
            this.operatorStartEndLocation = new int[operatorLocations.length];
            System.arraycopy(operatorLocations, 0,
                             this.operatorStartEndLocation, 0,
                             operatorLocations.length);
        } else {
            this.operatorStartEndLocation = EMPTY_START_END_LOCATION; 
        }

        checkArity();
    }

    /**
     * Checks whether:<br>
     * <code>operands.size() == operator.getArity()<br>
     * If not a {@link RuntimeException} is thrown.
     */
    private void checkArity() {
        if (this.operands.size() != this.operator.getArity()) {
            throw new RuntimeException(
                    "this.operands.size() != this.operator.getArity()");
        }
        if (this.operatorStartEndLocation.length %2 != 0) {
            throw new RuntimeException(
                    "this.operandStartEndLocation.length must be even");
        }
        if (this.operatorStartEndLocation.length > 0) {
            int expectedLocations;
            if (this.operator.getArity() <= 1) {
                expectedLocations = 2;
            } else {
                expectedLocations = (this.operator.getArity() - 1) * 2;
            }

            if (this.operatorStartEndLocation.length != expectedLocations) {
            throw new RuntimeException(
                    "wrong number of start ends for the operator(s)");
            }
        }
    }

    /**
     * The {@link InstrBooleanOperator} of this {@link InstrOperatorTerm}.
     * 
     * @return The operator.
     */
    public InstrBooleanOperator getOperator() {
        return this.operator;
    }

    /**
     * The operands of this {@link InstrOperatorTerm}.
     * 
     * @return The operands of this {@link InstrOperatorTerm} in a Queue.
     */
    public List<InstrBooleanTerm> getOperands() {
        return this.operands;
    }

    /**
     * The start and end locations of the operators.
     * 
     * @return The start and end location of all operators of this
     *         {@link InstrOperatorTerm}. For every operator two integers are
     *         used: <code>[start1, end1, start2, end2, ...]</code>.
     */
    public int[] getOperatorLocations() {
        return this.operatorStartEndLocation;
    }

    /**
     * The arity of this boolean operator term&mdash;the number of child
     * operands.
     * 
     * @return The arity.
     */
    public int getArity() {
        return this.operands.size();
    }

    /**
     * Overwrites the inner attributes {@link #operator} and {@link #operands}.
     * 
     * @param newOperator
     *          The new {@link InstrBooleanOperator}.
     * @param newOperands
     *          The new {@link InstrBooleanTerm}s as operands.
     * @param newOperatorLocations
     *            The start and end location of all operators of this
     *            {@link InstrOperatorTerm}. For every operator two integers are
     *            expected: <code>[start1, end1, start2, end2, ...]</code>.
     */
    public void resetOperandAndOperators(InstrBooleanOperator newOperator,
            List<InstrBooleanTerm> newOperands, int[] newOperatorLocations) {
        if (newOperator == null) {
            throw new NullPointerException("operator == null");
        }
        if (newOperands == null) {
            throw new NullPointerException("operands == null");
        }
        if (newOperatorLocations == null) {
            throw new NullPointerException("newOperantLocations == null");
        }

        this.operator = newOperator;
        this.operands = CollectionUtil.copy(newOperands);
        if (newOperatorLocations.length > 0) {
            this.operatorStartEndLocation = new int[newOperatorLocations.length];
            System.arraycopy(newOperatorLocations, 0,
                             this.operatorStartEndLocation, 0,
                             newOperatorLocations.length);
        } else {
            this.operatorStartEndLocation = EMPTY_START_END_LOCATION; 
        }

        checkArity();
    }

    @Override
    public String termToString() {
        return this.operator.termToString(this.operands);
    }

    @Override
    public void writeToTarget(Writer target) throws IOException {
        this.operator.writeToTarget(target, this.operands);
    }

    @Override
    public void getAllBasicBooleanTerms(List<InstrBasicBooleanTerm> termQueue) {
        for (InstrBooleanTerm thisInstrBooleanTerm : this.operands) {
            thisInstrBooleanTerm.getAllBasicBooleanTerms(termQueue);
        }
    }

    @Override
    public BooleanTerm toBooleanTerm(MASTBuilder builder, SourceFile sourceFile) {
        // get the BooleanTerms from all children
        List<BooleanTerm> booleanTermList = new Vector<BooleanTerm>(this.operands.size());
        for (InstrBooleanTerm thisInstrBooleanTerm : this.operands) {
            BooleanTerm thisBooleanTerm = thisInstrBooleanTerm.toBooleanTerm(builder, sourceFile);
            booleanTermList.add(thisBooleanTerm);
        }

        // transform the start end of this operator into Locations
        LocationList locationList;
        if (this.operatorStartEndLocation == EMPTY_START_END_LOCATION) {
            locationList = builder.createEmptyLocationList();
        } else {
            // assert this.operatorStartEndLocation.length >= 2
            List<Location> locationsOfOperators = new Vector<Location>(this.operatorStartEndLocation.length / 2);
            for (int i = 0; i < this.operatorStartEndLocation.length; i += 2) {
                Location thisLocation = builder.createLocation(sourceFile,
                        this.operatorStartEndLocation[i], this.operatorStartEndLocation[i + 1]);
                locationsOfOperators.add(thisLocation);
            }
            locationList = builder.createLocationList(locationsOfOperators);
        }

        return builder.createOperatorTerm(locationList, 
                this.operator.toBooleanOperator(builder), booleanTermList);
    }

    @Override
    public void access(InstrBooleanVisitor visitor) {
        visitor.visit(this);
    }
}
