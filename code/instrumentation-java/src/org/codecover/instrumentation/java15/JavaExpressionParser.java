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

package org.codecover.instrumentation.java15;

import static org.codecover.instrumentation.java15.JavaBooleanOperators.getAndOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getAssignmentOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getAssignmentAndOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getAssignmentExclusiveOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getAssignmentOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getConditionalAndOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getConditionalOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getConditionalOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getExclusiveOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getFalseOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getNotOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getTrueOperator;

import java.util.List;
import java.util.Vector;

import org.codecover.instrumentation.booleanterms.InstrBasicBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrBooleanOperator;
import org.codecover.instrumentation.booleanterms.InstrBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrBracketTerm;
import org.codecover.instrumentation.booleanterms.InstrOperatorTerm;
import org.codecover.instrumentation.java15.parser.JavaParser;
import org.codecover.instrumentation.java15.parser.JavaParserConstants;
import org.codecover.instrumentation.java15.syntaxtree.AdditiveExpression;
import org.codecover.instrumentation.java15.syntaxtree.AndExpression;
import org.codecover.instrumentation.java15.syntaxtree.AssignmentOperator;
import org.codecover.instrumentation.java15.syntaxtree.BooleanLiteral;
import org.codecover.instrumentation.java15.syntaxtree.ConditionalAndExpression;
import org.codecover.instrumentation.java15.syntaxtree.ConditionalExpression;
import org.codecover.instrumentation.java15.syntaxtree.ConditionalOrExpression;
import org.codecover.instrumentation.java15.syntaxtree.DoStatement;
import org.codecover.instrumentation.java15.syntaxtree.EqualityExpression;
import org.codecover.instrumentation.java15.syntaxtree.ExclusiveOrExpression;
import org.codecover.instrumentation.java15.syntaxtree.Expression;
import org.codecover.instrumentation.java15.syntaxtree.ForStatement;
import org.codecover.instrumentation.java15.syntaxtree.IfStatement;
import org.codecover.instrumentation.java15.syntaxtree.InclusiveOrExpression;
import org.codecover.instrumentation.java15.syntaxtree.InstanceOfExpression;
import org.codecover.instrumentation.java15.syntaxtree.Literal;
import org.codecover.instrumentation.java15.syntaxtree.MultiplicativeExpression;
import org.codecover.instrumentation.java15.syntaxtree.Node;
import org.codecover.instrumentation.java15.syntaxtree.NodeChoice;
import org.codecover.instrumentation.java15.syntaxtree.NodeSequence;
import org.codecover.instrumentation.java15.syntaxtree.NodeToken;
import org.codecover.instrumentation.java15.syntaxtree.PostfixExpression;
import org.codecover.instrumentation.java15.syntaxtree.PrimaryExpression;
import org.codecover.instrumentation.java15.syntaxtree.PrimaryPrefix;
import org.codecover.instrumentation.java15.syntaxtree.RelationalExpression;
import org.codecover.instrumentation.java15.syntaxtree.ShiftExpression;
import org.codecover.instrumentation.java15.syntaxtree.UnaryExpression;
import org.codecover.instrumentation.java15.syntaxtree.UnaryExpressionNotPlusMinus;
import org.codecover.instrumentation.java15.syntaxtree.WhileStatement;
import org.codecover.instrumentation.java15.visitor.GJNoArguDepthFirst;
import org.codecover.instrumentation.java15.visitor.GJNoArguVisitor;
import org.codecover.instrumentation.java15.visitor.InstrBasicBooleanVisitor;

/**
 * This class is a {@link GJNoArguVisitor} extending {@link GJNoArguDepthFirst}.<br>
 * <br>
 * This class is used to parse an {@link Expression} of the java syntaxtree,
 * which occur in {@link IfStatement}, {@link WhileStatement},
 * {@link DoStatement} or {@link ForStatement}. Therfore you have to create an
 * instance of this class and call {@link #parse(Expression)} or likewise
 * {@link #parse(ConditionalOrExpression)}. The result is a
 * {@link InstrBooleanTerm}, which can for example be used for instrumentation.<br>
 * <br>
 * One instance of {@link JavaExpressionParser} can be used for parsing of
 * several {@link Expression}&mdash;for each a call of
 * {@link #parse(Expression)}. Attention: this parsing is not thread safe.<br>
 * <br>
 * The parsing is done by traversing through the whole syntax tree generated by
 * the {@link JavaParser}. For every node it is decided, whether a
 * {@link InstrBasicBooleanTerm} is found, a new {@link InstrOperatorTerm} is
 * created out of sub {@link InstrBooleanTerm}, if the creating is delegated to
 * a child node or if the node does not return a boolean. In the last case a
 * {@link IllegalStateException} is thrown. This case should <b>NEVER</b> occur
 * and is just for reassurances, that the created boolean terms are really boolean
 * terms.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: JavaExpressionParser.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @see InstrBooleanTerm
 * @see #parse(Expression)
 * @see #parse(ConditionalOrExpression)
 */
public class JavaExpressionParser extends GJNoArguDepthFirst<InstrBooleanTerm>
        implements GJNoArguVisitor<InstrBooleanTerm> {

    /**
     * Parses the given {@link Expression} to generate a boolean tree. The root
     * node&mdash;a {@link InstrBooleanTerm}&mdash;is returned.
     * 
     * @param expression
     *            The {@link Expression} to parse.
     * @return The root of the constructed boolean tree.
     */
    public synchronized InstrBooleanTerm parse(Expression expression) {
        InstrBooleanTerm term = expression.accept(this);
        return term;
    }

    /**
     * Parses the given {@link ConditionalOrExpression} to generate a boolean
     * tree. The root node&mdash;a {@link InstrBooleanTerm}&mdash;is returned.
     * 
     * @param condOrExpression
     *            The {@link ConditionalOrExpression} to parse.
     * 
     * @return The root of the constructed boolean tree.
     */
    public InstrBooleanTerm parse(ConditionalOrExpression condOrExpression) {
        InstrBooleanTerm term = condOrExpression.accept(this);
        return term;
    }

    /**
     * <PRE>
     * 
     * f0 -> ConditionalExpression()
     * f1 -> [ AssignmentOperator() Expression() ]
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(Expression n) {
        InstrBooleanTerm returnTerm = null;

        if (n.f1.present()) {
            // this is an operator term, where only the the right Expression is
            // an InstrBooleanTerm

            ConditionalExpression first = n.f0;
            NodeSequence f1Sequence = (NodeSequence) n.f1.node;
            AssignmentOperator assignmentOperator = (AssignmentOperator) f1Sequence.nodes.get(0);
            NodeToken operatorToken = (NodeToken) assignmentOperator.f0.choice; 
            Expression second = (Expression) f1Sequence.nodes.get(1);

            InstrBooleanTerm firstTerm = first.accept(this);
            InstrBooleanTerm secondTerm = second.accept(this);
            InstrBooleanOperator operator;

            if (!(firstTerm instanceof InstrBasicBooleanTerm)) {
                throw new IllegalStateException(
                "Assignment operators (=, |=, &=, ^=) have to have a variable as the first operand.");
            }
            InstrBasicBooleanTerm firstTermBasic = (InstrBasicBooleanTerm) firstTerm;

            switch (operatorToken.kind) {
            case JavaParserConstants.ASSIGN :
                // =
                operator = getAssignmentOperator();
                returnTerm = new InstrOperatorTerm(firstTermBasic, operator, secondTerm,
                        operatorToken.startOffset, operatorToken.endOffset);
                break;
            case JavaParserConstants.ORASSIGN :
                // |=
                operator = getAssignmentOrOperator();
                returnTerm = new InstrOperatorTerm(firstTermBasic, operator, secondTerm,
                        operatorToken.startOffset, operatorToken.endOffset);
                break;
            case JavaParserConstants.ANDASSIGN :
                // &=
                operator = getAssignmentAndOperator();
                returnTerm = new InstrOperatorTerm(firstTermBasic, operator, secondTerm,
                        operatorToken.startOffset, operatorToken.endOffset);
                break;
            case JavaParserConstants.XORASSIGN :
                // ^=
                operator = getAssignmentExclusiveOrOperator();
                returnTerm = new InstrOperatorTerm(firstTermBasic, operator, secondTerm,
                        operatorToken.startOffset, operatorToken.endOffset);
                break;
            default :
                throw new IllegalStateException(
                "There are only four assignment terms - operatorToken.kind = " + operatorToken.kind + " is unknown");
            } 
        } else {
            returnTerm = n.f0.accept(this);
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> ConditionalOrExpression()
     * f1 -> [ "?" Expression() ":" Expression() ]
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(ConditionalExpression n) {
        InstrBooleanTerm returnTerm = null;

        if (n.f1.present()) {
            // this is operator with three operands
            NodeSequence f1Sequence = (NodeSequence) n.f1.node;
            NodeToken firstOperator = (NodeToken) f1Sequence.nodes.get(0);
            Expression secondTerm = (Expression) f1Sequence.nodes.get(1);
            NodeToken secondOperator = (NodeToken) f1Sequence.nodes.get(2);
            Expression thirdTerm = (Expression) f1Sequence.nodes.get(3);

            List<InstrBooleanTerm> operandsList = new Vector<InstrBooleanTerm>(3);

            // first operator
            operandsList.add(n.f0.accept(this));
            // second operator
            operandsList.add(secondTerm.accept(this));
            // third operator
            operandsList.add(thirdTerm.accept(this));

            InstrBooleanOperator operator = getConditionalOperator();

            returnTerm = new InstrOperatorTerm(operator, operandsList,
                    new int[]{firstOperator.startOffset, firstOperator.endOffset,
                              secondOperator.startOffset, secondOperator.endOffset});
        } else {
            returnTerm = n.f0.accept(this);
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> ConditionalAndExpression()
     * f1 -> ( "||" ConditionalAndExpression() )*
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(ConditionalOrExpression n) {
        InstrBooleanTerm returnTerm = null;

        // the first operand is n.f0
        returnTerm = n.f0.accept(this);

        // the other operands, we have to get
        for (Node thisNode : n.f1.nodes) {
            NodeSequence sequence = (NodeSequence) thisNode;
            NodeToken operatorToken = (NodeToken) sequence.nodes.get(0);
            InstrBooleanTerm termNow = sequence.nodes.get(1).accept(this);

            // create a || operator out of the current returnTerm and the
            // termNow
            returnTerm = new InstrOperatorTerm(returnTerm,
                    getConditionalOrOperator(), termNow,
                    operatorToken.startOffset, operatorToken.endOffset);
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> InclusiveOrExpression()
     * f1 -> ( "&&" InclusiveOrExpression() )*
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(ConditionalAndExpression n) {
        InstrBooleanTerm returnTerm = null;

        // the first operand is n.f0
        returnTerm = n.f0.accept(this);

        // the other operands, we have to get
        for (Node thisNode : n.f1.nodes) {
            NodeSequence sequence = (NodeSequence) thisNode;
            NodeToken operatorToken = (NodeToken) sequence.nodes.get(0);
            InstrBooleanTerm termNow = sequence.nodes.get(1).accept(this);

            // create a && operator out of the current returnTerm and the
            // termNow
            returnTerm = new InstrOperatorTerm(returnTerm,
                    getConditionalAndOperator(), termNow,
                    operatorToken.startOffset, operatorToken.endOffset);
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> ExclusiveOrExpression()
     * f1 -> ( "|" ExclusiveOrExpression() )*
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(InclusiveOrExpression n) {
        InstrBooleanTerm returnTerm = null;

        // the first operand is n.f0
        returnTerm = n.f0.accept(this);

        // the other operands, we have to get
        for (Node thisNode : n.f1.nodes) {
            NodeSequence sequence = (NodeSequence) thisNode;
            NodeToken operatorToken = (NodeToken) sequence.nodes.get(0);
            InstrBooleanTerm termNow = sequence.nodes.get(1).accept(this);

            // create a | operator out of the current returnTerm and the
            // termNow
            returnTerm = new InstrOperatorTerm(returnTerm,
                    getOrOperator(), termNow,
                    operatorToken.startOffset, operatorToken.endOffset);
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> AndExpression()
     * f1 -> ( "^" AndExpression() )*
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(ExclusiveOrExpression n) {
        InstrBooleanTerm returnTerm = null;

        // the first operand is n.f0
        returnTerm = n.f0.accept(this);

        // the other operands, we have to get
        for (Node thisNode : n.f1.nodes) {
            NodeSequence sequence = (NodeSequence) thisNode;
            NodeToken operatorToken = (NodeToken) sequence.nodes.get(0);
            InstrBooleanTerm termNow = sequence.nodes.get(1).accept(this);

            // create a ^ operator out of the current returnTerm and the
            // termNow
            returnTerm = new InstrOperatorTerm(returnTerm,
                    getExclusiveOrOperator(), termNow,
                    operatorToken.startOffset, operatorToken.endOffset);
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> EqualityExpression()
     * f1 -> ( "&" EqualityExpression() )*
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(AndExpression n) {
        InstrBooleanTerm returnTerm = null;

        // the first operand is n.f0
        returnTerm = n.f0.accept(this);

        // the other operands, we have to get
        for (Node thisNode : n.f1.nodes) {
            NodeSequence sequence = (NodeSequence) thisNode;
            NodeToken operatorToken = (NodeToken) sequence.nodes.get(0);
            InstrBooleanTerm termNow = sequence.nodes.get(1).accept(this);

            // create a & operator out of the current returnTerm and the
            // termNow
            returnTerm = new InstrOperatorTerm(returnTerm,
                    getAndOperator(), termNow,
                    operatorToken.startOffset, operatorToken.endOffset);
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> InstanceOfExpression()
     * f1 -> ( ( "==" | "!=" ) InstanceOfExpression() )*
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(EqualityExpression n) {
        InstrBooleanTerm returnTerm = null;

        if (n.f1.present()) {
            // this is a leaf
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n);
        } else {
            returnTerm = n.f0.accept(this);
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> RelationalExpression()
     * f1 -> [ "instanceof" Type() ]
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(InstanceOfExpression n) {
        InstrBooleanTerm returnTerm = null;

        if (n.f1.present()) {
            // this is a leaf
            // dump it to a String
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n);
        } else {
            returnTerm = n.f0.accept(this);
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> ShiftExpression()
     * f1 -> ( ( "&lt;" | "&gt;" | "&lt;=" | "&gt;=" ) ShiftExpression() )*
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(RelationalExpression n) {
        InstrBooleanTerm returnTerm = null;

        if (n.f1.present()) {
            // this is a leaf
            // dump it to a String
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n);
        } else {
            returnTerm = n.f0.accept(this);
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> AdditiveExpression()
     * f1 -> ( ( "&lt;&lt;" |
     *           RSIGNEDSHIFT() |
     *           RUNSIGNEDSHIFT()
     *         ) AdditiveExpression() )*
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(ShiftExpression n) {
        InstrBooleanTerm returnTerm = null;

        if (n.f1.present()) {
            throw new IllegalStateException(
                    "the result of this operator is no boolean - this will not return a BooleanTerm");
        }

        // a boolean was expected get it
        returnTerm = n.f0.accept(this);

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> MultiplicativeExpression()
     * f1 -> ( ( "+" | "-" ) MultiplicativeExpression() )*
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(AdditiveExpression n) {
        InstrBooleanTerm returnTerm = null;

        if (n.f1.present()) {
            throw new IllegalStateException(
                    "the result of this operator is no boolean - this will not return a BooleanTerm");
        }

        // a boolean was expected get it
        returnTerm = n.f0.accept(this);

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> UnaryExpression()
     * f1 -> ( ( "*" | "/" | "%" ) UnaryExpression() )*
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(MultiplicativeExpression n) {
        InstrBooleanTerm returnTerm = null;

        if (n.f1.present()) {
            throw new IllegalStateException(
                    "the result of this operator is no boolean - this will not return a BooleanTerm");
        }

        // a boolean was expected get it
        returnTerm = n.f0.accept(this);

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> ( "+" | "-" ) UnaryExpression() |
     *       PreIncrementExpression() |
     *       PreDecrementExpression() |
     *       UnaryExpressionNotPlusMinus()
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(UnaryExpression n) {
        InstrBooleanTerm returnTerm = null;

        if (n.f0.choice instanceof UnaryExpressionNotPlusMinus) {
            // a boolean was expected get it
            returnTerm = n.f0.accept(this);
        } else {
            throw new IllegalStateException(
                    "the result of this operator is no boolean - this will not return a BooleanTerm");
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> ( "~" | "!" ) UnaryExpression() |
     *       CastExpression() |
     *       PostfixExpression()
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(UnaryExpressionNotPlusMinus n) {
        InstrBooleanTerm returnTerm = null;

        if (n.f0.which == 0) {
            // "~" | "!" ) UnaryExpression()

            // a boolean was expected get it
            NodeSequence sequence = (NodeSequence) n.f0.choice;
            NodeChoice operatorChoice = (NodeChoice) sequence.nodes.get(0);
            if (operatorChoice.which == 0) {
                // read a ~ -> no boolean
                throw new IllegalStateException(
                        "the result of this operator is no boolean - this will not return a BooleanTerm");
            }
            NodeToken operatorToken = (NodeToken) operatorChoice.choice;
            InstrBooleanTerm singleInstrBooleanTerm = sequence.nodes.get(1).accept(this);
            // create a not operator from the given operators
            InstrBooleanOperator operator = getNotOperator();

            returnTerm = new InstrOperatorTerm(operator, singleInstrBooleanTerm,
                    operatorToken.startOffset, operatorToken.endOffset);
        } else if (n.f0.which == 1) {
            // CastExpression
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n.f0.choice);
        } else if (n.f0.which == 2) {
            // PostfixExpression()
            returnTerm = n.f0.accept(this);
        } else {
            throw new IllegalStateException("Not expected n-f0.which == " + n.f0.which);
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> PrimaryExpression()
     * f1 -> [ "++" | "--" ]
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(PostfixExpression n) {
        InstrBooleanTerm returnTerm = null;

        if (n.f1.present()) {
            throw new IllegalStateException(
                    "the result of this operator is no boolean - this will not return a BooleanTerm");
        }

        // a boolean was expected get it
        returnTerm = n.f0.accept(this);

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> PrimaryPrefix()
     * f1 -> ( PrimarySuffix() )*
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(PrimaryExpression n) {
        InstrBooleanTerm returnTerm = null;

        if (n.f1.present()) {
            // we can not parse more deeply - this mus be a boolean
            // in consequence this is a leaf
            // dump it to a String
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n);
        } else {
            // a boolean was expected get it
            returnTerm = n.f0.accept(this);
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> Literal() |
     *       ( &lt;IDENTIFIER&gt; "." )* "this" |
     *       "super" "." &lt;IDENTIFIER&gt; |
     *       "(" Expression() ")" |
     *       AllocationExpression() |
     *       ResultType() "." "class" |
     *       Name()
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(PrimaryPrefix n) {
        InstrBooleanTerm returnTerm = null;

        if (n.f0.which == 0) {
            // Literal()
            // a boolean was expected get it
            returnTerm = n.f0.accept(this);
        } else if (n.f0.which == 3) {
            // "(" Expression() ")"
            NodeSequence sequence = (NodeSequence) n.f0.choice;
            InstrBooleanTerm expressionTerm = sequence.nodes.get(1)
                    .accept(this);
            returnTerm = new InstrBracketTerm(expressionTerm);
        } else {
            // we can not parse more deeply - this mus be a boolean
            // in consequence this is a leaf
            // dump it to a String
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n);
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> &lt;INTEGER_LITERAL&gt; |+
     *       &lt;FLOATING_POINT_LITERAL&gt; |
     *       &lt;CHARACTER_LITERAL&gt; |
     *       &lt;STRING_LITERAL&gt; |
     *       BooleanLiteral() |
     *       NullLiteral()
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(Literal n) {
        InstrBooleanTerm returnTerm = null;

        if (n.f0.choice instanceof BooleanLiteral) {
            // a boolean was expected get it
            returnTerm = n.f0.accept(this);
        } else {
            throw new IllegalStateException(
                    "the result of this operator is no boolean - this will not return a BooleanTerm");
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> "true" |
     *       "false"
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(BooleanLiteral n) {
        InstrBooleanTerm returnTerm = null;

        NodeToken token = (NodeToken) n.f0.choice;
        if (token.kind == JavaParserConstants.TRUE) {
            returnTerm = new InstrOperatorTerm(getTrueOperator(),
                    token.startOffset, token.endOffset);
        } else if (token.kind == JavaParserConstants.FALSE) {
            returnTerm = new InstrOperatorTerm(getFalseOperator(),
                    token.startOffset, token.endOffset);
        } else {
            throw new RuntimeException("only true or false expected");
        }

        return returnTerm;
    }
}
