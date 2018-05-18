///////////////////////////////////////////////////////////////////////////////
//
// $Id: InstrOperatorTerm.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 09.04.2007 16:29:06
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.booleanterms;

import java.util.LinkedList;
import java.util.Queue;

/**
 * This class extends the {@link InstrBooleanTerm} and represents a boolean
 * term, that consists of an operator and zero or more operants.<br>
 * <br>
 * This class is needed to represent boolean terms like <code>(a || b)</code><br>
 * The operator is an instance of {@link InstrBooleanOperator}. The operants
 * can be other {@link InstrBooleanTerm}s. There are several constructors to
 * create various types of {@link InstrOperatorTerm}s.<br>
 * <ul>
 * <li>{@link #InstrOperatorTerm(InstrBooleanOperator)}&mdash;without operants</li>
 * <li>{@link #InstrOperatorTerm(InstrBooleanOperator, InstrBooleanTerm)}&mdash;with
 * just a single operant</li>
 * <li>{@link #InstrOperatorTerm(InstrBooleanTerm, InstrBooleanOperator, InstrBooleanTerm)}&mdash;with
 * a left and a right operant (e.g. <code>a || b</code>)</li>
 * <li>{@link #InstrOperatorTerm(InstrBooleanOperator, Queue)}&mdash;with a
 * Queue of operants in this sequqence.</li>
 * </ul>
 * After the construction {@link #checkArity()} is used to check whether:<br>
 * <code>operants.size() == operator.getArity()<br>
 * If not a {@link RuntimeException} is thrown.
 * <br>
 * The method {@link #termToString()} uses the
 * {@link InstrBooleanOperator#termToString(String[])} method of its operator to
 * create the String. The method {@link #getAllBasicBooleanTerms(Queue)} orders
 * all operants to add {@link InstrBasicBooleanTerm} to the Queue by delegating
 * this method.
 * 
 * @author Christoph Müller
 * @see InstrBooleanOperator
 */
public class InstrOperatorTerm extends InstrBooleanTerm {
    private InstrBooleanOperator operator;

    private Queue<InstrBooleanTerm> operants;

    private InstrOperatorTerm() {
        this.operator = null;
        this.operants = new LinkedList<InstrBooleanTerm>();
    }

    /**
     * This is a constructor for a {@link InstrOperatorTerm} with no operants.
     * 
     * @param constantOperator
     *            The {@link InstrBooleanOperator} that is a constant.
     */
    public InstrOperatorTerm(InstrBooleanOperator constantOperator) {
        this();
        this.operator = constantOperator;

        checkArity();
    }

    /**
     * This is a constructor for a {@link InstrOperatorTerm} with just one
     * operant.
     * 
     * @param oneArgumentOperator
     *            The given {@link InstrBooleanOperator} with arity 1.
     * @param singleTerm
     *            The {@link InstrBooleanTerm} bounded by the operator.
     */
    public InstrOperatorTerm(InstrBooleanOperator oneArgumentOperator,
            InstrBooleanTerm singleTerm) {
        this();
        this.operator = oneArgumentOperator;
        this.operants.add(singleTerm);

        checkArity();
    }

    /**
     * This is a constructor for a {@link InstrOperatorTerm} with two
     * operants&mdash;like <code>(a || b)</code>.
     * 
     * @param leftTerm
     *            The left boolean term (a).
     * 
     * @param twoArgumentsOperator
     *            The given {@link InstrBooleanOperator} with arity 2.
     * 
     * @param rightTerm
     *            The right boolean term (b).
     */
    public InstrOperatorTerm(InstrBooleanTerm leftTerm,
            InstrBooleanOperator twoArgumentsOperator,
            InstrBooleanTerm rightTerm) {
        this();
        this.operator = twoArgumentsOperator;
        this.operants.add(leftTerm);
        this.operants.add(rightTerm);

        checkArity();
    }

    /**
     * This is a constructor for a {@link InstrOperatorTerm} with more than two
     * operants&mdash;like <code>(a ? b : c)</code>.
     * 
     * @param operator
     *            The given {@link InstrBooleanOperator} with arity
     *            operants.size().
     * @param operants
     *            The operants in the sequence they occure in the expression (a,
     *            b, c).
     */
    public InstrOperatorTerm(InstrBooleanOperator operator,
            Queue<InstrBooleanTerm> operants) {
        this.operator = operator;
        this.operants = operants;

        checkArity();
    }

    /**
     * Checks whether:<br>
     * <code>operants.size() == operator.getArity()<br>
     * If not a {@link RuntimeException} is thrown.
     */
    private void checkArity() {
        if (this.operants.size() != this.operator.getArity()) {
            throw new RuntimeException(
                    "this.operants.size() != this.operator.getArity()");
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
     * The operants of this {@link InstrOperatorTerm}.
     * 
     * @return The operants of this {@link InstrOperatorTerm} in a Queue.
     */
    public Queue<InstrBooleanTerm> getOperants() {
        return this.operants;
    }

    @Override
    public String termToString() {
        return this.operator.termToString(this.operants);
    }

    @Override
    public void getAllBasicBooleanTerms(Queue<InstrBasicBooleanTerm> termQueue) {
        for (InstrBooleanTerm thisInstrBooleanTerm : this.operants) {
            thisInstrBooleanTerm.getAllBasicBooleanTerms(termQueue);
        }
    }
}
