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
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.BooleanAssignment;
import org.codecover.model.mast.BooleanOperator;

/**
 * This class represents a boolean operator of a given arity.<br>
 * <br>
 * Arity means the number of operands, this operator is used for. A
 * {@link InstrBooleanOperator} occurs in an {@link InstrOperatorTerm}. The
 * arity has to be the number of operands:<br>
 * <code>operatorTerm.gets.size() == operatorTerm.getOperator.getArity()</code><br>
 * Moreover an {@link InstrBooleanOperator} has a name and a complex image. This
 * image is a String array and has a length of <code>arity + 1</code>. This
 * complex image is needed to transform the {@link InstrBooleanOperator} and its
 * operand into a String ({@link #termToString(List)}). The results are for
 * example:<br>
 * <ul>
 * <li><code>["", " || ", ""]</code> &rarr; <code>a || b</code></li>
 * <li><code>["!", ""]</code> &rarr; <code>!a</code></li>
 * <li><code>["true"]</code> &rarr; <code>true</code></li>
 * <li><code>["", " ? ", " : ", ""]</code> &rarr; <code>a ? b : c</code></li>
 * </ul>
 * <br>
 * The instances of this class can be got, using the constructor 
 * {@link #InstrBooleanOperator(String, String[], Map)} or one of the the
 * various static methods.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: InstrBooleanOperator.java 1 2007-12-12 17:37:26Z t-scheller $)
 * @see InstrOperatorTerm
 */
public class InstrBooleanOperator {

    private int arity;

    private String name;

    private String[] partsOfOperator;

    private Map<BooleanAssignment, Boolean> possibleAssignments;

    private BooleanOperator mastOperator;

    /**
     * Constructs a new {@link InstrBooleanOperator} and setting the fields.
     * 
     * @param name
     *            The name.
     * @param partsOfOperator
     *            The complex image.
     * @param possibleAssignments
     *            A {@link Map} with possible assignments and results for this
     *            operator. See {@link BooleanOperator}.
     */
    public InstrBooleanOperator(String name, String[] partsOfOperator,
            Map<BooleanAssignment, Boolean> possibleAssignments) {
        if (name == null) {
            String message = "name == null";
            throw new NullPointerException(message);
        }
        if (partsOfOperator == null) {
            String message = "partsOfOperator == null";
            throw new NullPointerException(message);
        }
        if (partsOfOperator.length == 0) {
            String message = "partsOfOperator.length == 0";
            throw new RuntimeException(message);
        }
        if (possibleAssignments == null) {
            String message = "possibleAssignments == 0";
            throw new NullPointerException(message);
        }
        if (possibleAssignments.isEmpty()) {
            String message = "possibleAssignments.isEmpty()";
            throw new RuntimeException(message);
        }

        this.name = name;
        this.partsOfOperator = partsOfOperator;
        this.arity = partsOfOperator.length - 1;
        this.possibleAssignments = possibleAssignments;

        for (int i = 0; i < this.partsOfOperator.length; i++) {
            if (this.partsOfOperator[i] == null) {
                String message = "this.partsOfOperator[i] == null";
                throw new NullPointerException(message);
            }
        }

        for (Entry<BooleanAssignment, Boolean> thisEntry : possibleAssignments.entrySet()) {
            if (thisEntry.getKey().getResults().size() != this.arity) {
                String message = "\""
                        + thisEntry.getKey().getResults()
                        + "\" is not a correct assignment for this boolean operator with arity \""
                        + this.arity + "\"";
                throw new RuntimeException(message);
            }
        }
    }

    /**
     * The arity of this operator&mdash;the number of bound operands.
     * 
     * @return The arity.
     */
    public int getArity() {
        return this.arity;
    }

    /**
     * The name of the {@link InstrBooleanOperator}&mdash;a kind of a
     * description.
     * 
     * @return The name.
     */
    public String getName() {
        return this.name;
    }

    @Override
    public String toString() {
        return this.name;
    }

    /**
     * Transformes the operands of an {@link InstrOperatorTerm} to a String
     * using the {@link #partsOfOperator}.
     * 
     * @param operandQueue
     *            The Queue with all the operands bounded to this operator.<br>
     *            <code>operandQueue.size() == this.getArity()</code>
     * 
     * @return A String with the formatted operator and operands.
     */
    public String termToString(List<InstrBooleanTerm> operandQueue) {
        if (operandQueue.size() != this.getArity()) {
            throw new IllegalArgumentException("operandQueue.size() != this.getArity()");
        }

        StringBuilder sBuilder = new StringBuilder();
        int position = 0;
        for (InstrBooleanTerm term : operandQueue) {
            sBuilder.append(this.partsOfOperator[position++]);
            sBuilder.append(term.termToString());
        }
        sBuilder.append(this.partsOfOperator[position]);

        return sBuilder.toString();
    }

    /**
     * Transformes the operands of an {@link InstrOperatorTerm} to a String and
     * writes it's content to the target {@link Writer}.
     * 
     * @param target
     *            The target {@link Writer}.
     * 
     * @param operandQueue
     *            The Queue with all the operands bounded to this operator.<br>
     *            <code>operandQueue.size() == this.getArity()</code>
     * 
     * @throws IOException
     *             If there occur errors when using {@link Writer#write(String)}.
     */
    public void writeToTarget(Writer target,
            List<InstrBooleanTerm> operandQueue) throws IOException {
        int position = 0;

        // writes rotational the a part of the operator and a boolean term
        for (InstrBooleanTerm term : operandQueue) {
            target.write(this.partsOfOperator[position++]);
            term.writeToTarget(target);
        }
        target.write(this.partsOfOperator[position]);
    }

    /**
     * Transforms this representation of a boolean operator into a
     * {@link BooleanOperator} of the MAST.<br>
     * Iff {@link #mastOperator} is null, a new {@link BooleanOperator} is
     * created, otherwise the cached {@link BooleanOperator} is preferred, to
     * allow that only one representation of this {@link InstrBooleanOperator}
     * is ever created. 
     * 
     * @param builder
     *            The {@link MASTBuilder} to use for creation methods.
     * 
     * @return A {@link BooleanOperator} of the MAST.
     */
    public BooleanOperator toBooleanOperator(MASTBuilder builder) {
        if (this.mastOperator == null) {
            this.mastOperator = builder.createBooleanOperator(this.arity,
                    this.possibleAssignments,
                    this.name);
        }
        return this.mastOperator;
    }

    /**
     * Constructs an operator with no operands&mdash;a constant.
     * 
     * @param name
     *            The name of the operator.
     * @param image
     *            The image of the operator.
     * @param possibleAssignments
     *            A {@link Map} with possible assignments and results for this
     *            operator. See {@link BooleanOperator}.
     * 
     * @return The constructed {@link InstrBooleanOperator}.
     */
    public static InstrBooleanOperator getConstantOperator(String name,
            String image, Map<BooleanAssignment, Boolean> possibleAssignments) {
        String[] parts = {image};
        InstrBooleanOperator newOp = new InstrBooleanOperator(name, parts,
                possibleAssignments);
        return newOp;
    }

    /**
     * Constructs an operator with one operand.<br>
     * <br>
     * The {@link #partsOfOperator} is created using the image as a prefix:<br>
     * <code>image(produceWhitespace ? " " : "")%s</code>
     * 
     * @param name
     *            The name of the operator.
     * @param image
     *            The image of the operator.
     * @param produceWhitespace
     *            Whether there has to be put a whitespace between the operator
     *            and the operand.
     * @param possibleAssignments
     *            A {@link Map} with possible assignments and results for this
     *            operator. See {@link BooleanOperator}.
     * 
     * @return The constructed {@link InstrBooleanOperator}.
     */
    public static InstrBooleanOperator getOneArgumentOperator(String name,
            String image, boolean produceWhitespace,
            Map<BooleanAssignment, Boolean> possibleAssignments) {
        if (produceWhitespace) {
            image = image + " ";
        }
        String[] parts = {image, ""};
        InstrBooleanOperator newOp = new InstrBooleanOperator(name, parts,
                possibleAssignments);
        return newOp;
    }

    /**
     * Constructs an operator with two operands.<br>
     * <br>
     * The {@link #partsOfOperator} is created using the image as a separator:<br>
     * <code>%s image %s</code>
     * 
     * @param name
     *            The name of the operator.
     * @param image
     *            The image of the operator.
     * @param possibleAssignments
     *            A {@link Map} with possible assignments and results for this
     *            operator. See {@link BooleanOperator}.
     *
     * @return The constructed {@link InstrBooleanOperator}.
     */
    public static InstrBooleanOperator getTwoArgumentOperator(String name,
            String image, Map<BooleanAssignment, Boolean> possibleAssignments) {
        String[] parts = {"", " " + image + " ", ""};
        InstrBooleanOperator newOp = new InstrBooleanOperator(name, parts,
                possibleAssignments);
        return newOp;
    }
}
