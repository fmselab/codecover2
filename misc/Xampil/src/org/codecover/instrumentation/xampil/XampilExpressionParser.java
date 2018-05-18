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

package org.codecover.instrumentation.xampil;

import org.codecover.instrumentation.booleanterms.InstrBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrBracketTerm;
import org.codecover.instrumentation.booleanterms.InstrOperatorTerm;
import org.codecover.instrumentation.xampil.syntaxtree.NodeToken;
import org.codecover.instrumentation.xampil.syntaxtree.AdditiveExpression;
import org.codecover.instrumentation.xampil.syntaxtree.AndExpression;
import org.codecover.instrumentation.xampil.syntaxtree.BasicExpression;
import org.codecover.instrumentation.xampil.syntaxtree.EqualityExpression;
import org.codecover.instrumentation.xampil.syntaxtree.Expression;
import org.codecover.instrumentation.xampil.syntaxtree.MultiplicativeExpression;
import org.codecover.instrumentation.xampil.syntaxtree.Node;
import org.codecover.instrumentation.xampil.syntaxtree.NodeSequence;
import org.codecover.instrumentation.xampil.syntaxtree.NotExpression;
import org.codecover.instrumentation.xampil.syntaxtree.OrExpression;
import org.codecover.instrumentation.xampil.syntaxtree.RelationalExpression;
import org.codecover.instrumentation.xampil.visitor.GJNoArguDepthFirst;
import org.codecover.instrumentation.xampil.visitor.GJNoArguVisitor;
import org.codecover.instrumentation.xampil.visitor.InstrBasicBooleanVisitor;

/**
 * This class is a {@link GJNoArguVisitor} extending {@link GJNoArguDepthFirst}.<br>
 * <br>
 * This class is used to parse an {@link Expression} of the xampil syntaxtree,
 * which occur in {@link IfStatement} and {@link WhileStatement}. Therefore you
 * have to create an instance of this class and call {@link #parse(Expression)}
 * or likewise {@link #parse(OrExpression)}. The result is a
 * {@link InstrBooleanTerm}, which can for example be used for instrumentation.<br>
 * <br>
 * One instance of {@link XampilExpressionParser} can be used for parsing of
 * several {@link Expression}&mdash;for each a call of
 * {@link #parse(Expression)}. Attention: this parsing is not threadsafe.<br>
 * 
 * @author Stefan Franke
 */
public class XampilExpressionParser extends GJNoArguDepthFirst<InstrBooleanTerm> 
    implements GJNoArguVisitor<InstrBooleanTerm> {

    /**
     * Parses the given {@link Expression} to generate a boolean tree. The root
     * node&mdash;a {@link InstrBooleanTerm}&mdash;is returned.
     * 
     * @param expression
     *            The {@link Expression} to parse.
     * 
     * @return The root of the constructed boolean tree.
     */
    public InstrBooleanTerm parse(Expression n) {
        return n.accept(this);
    }

    /**
     * <PRE>
     * f0 -> OrExpression(basicBooleanCounter)
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(Expression n) {
        return n.f0.accept(this);
    }

    /**
     * <PRE>
     * f0 -> AndExpression(basicBooleanCounter)
     * f1 -> ( &lt;OR&gt; AndExpression(basicBooleanCounter) )*
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(OrExpression n) {
        InstrBooleanTerm returnTerm = n.f0.accept(this);
        for (Node node : n.f1.nodes) {
            NodeSequence nodeSequence = (NodeSequence) node;
            NodeToken operatorToken = (NodeToken) nodeSequence.nodes.get(0);
            InstrBooleanTerm term = nodeSequence.nodes.get(1).accept(this);
            returnTerm = new InstrOperatorTerm(returnTerm,
                    XampilBooleanOperators.getOrOperator(), term,
                    operatorToken.startOffset, operatorToken.endOffset);
        }
        return returnTerm;
    }

    /**
     * <PRE>
     * f0 -> NotExpression(basicBooleanCounter)
     * f1 -> ( &lt;AND&gt; NotExpression(basicBooleanCounter) )*
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(AndExpression n) {
        InstrBooleanTerm returnTerm = n.f0.accept(this);
        for (Node node : n.f1.nodes) {
            NodeSequence nodeSequence = (NodeSequence) node;
            NodeToken operatorToken = (NodeToken) nodeSequence.nodes.get(0);
            InstrBooleanTerm term = nodeSequence.nodes.get(1).accept(this);
            returnTerm = new InstrOperatorTerm(returnTerm,
                    XampilBooleanOperators.getAndOperator(), term,
                    operatorToken.startOffset, operatorToken.endOffset);
        }
        return returnTerm;
    }

    /**
     * <PRE>
     * f0 -> ( &lt;NOT&gt; )?
     * f1 -> EqualityExpression(basicBooleanCounter)
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(NotExpression n) {
        InstrBooleanTerm returnTerm = n.f1.accept(this);
        if (n.f0.present()) {
            NodeToken operatorToken = (NodeToken) n.f0.node;
            returnTerm = new InstrOperatorTerm(XampilBooleanOperators.getNotOperator(),
                    returnTerm,
                    operatorToken.startOffset, operatorToken.endOffset);
        }
        return returnTerm;
    }

    /**
     * <PRE>
     * f0 -> RelationalExpression(basicBooleanCounter)
     * f1 -> ( ( &lt;EQ&gt; | &lt;NEQ&gt; ) RelationalExpression(basicBooleanCounter) )?
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(EqualityExpression n) {
        InstrBooleanTerm returnTerm = n.f0.accept(this);
        if (n.f1.present()) {
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n);
        }
        return returnTerm;
    }

    /**
     * <PRE>
     * f0 -> AdditiveExpression(basicBooleanCounter)
     * f1 -> ( ( &lt;LT&gt; | &lt;GT&gt; | &lt;LE&gt; | &lt;GE&gt; ) AdditiveExpression(basicBooleanCounter) )?
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(RelationalExpression n) {
        InstrBooleanTerm returnTerm = n.f0.accept(this);
        if (n.f1.present()) {
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n);
        }
        return returnTerm;
    }

    /**
     * <PRE>
     * f0 -> MultiplicativeExpression(basicBooleanCounter)
     * f1 -> ( ( &lt;PLUS&gt; | &lt;MINUS&gt; ) MultiplicativeExpression(basicBooleanCounter) )*
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(AdditiveExpression n) {
        InstrBooleanTerm returnTerm = n.f0.accept(this);
        if (n.f1.present()) {
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n);
        }
        return returnTerm;
    }

    /**
     * <PRE>
     * f0 -> BasicExpression(basicBooleanCounter)
     * f1 -> ( ( &lt;STAR&gt; | &lt;SLASH&gt; ) BasicExpression(basicBooleanCounter) )*
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(MultiplicativeExpression n) {
        InstrBooleanTerm returnTerm = n.f0.accept(this);
        if (n.f1.present()) {
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n);
        }
        return returnTerm;
    }

    /**
     * <PRE>
     * f0 -> &lt;IDENTIFIER&gt;
     *       | &lt;INTEGER_LITERAL&gt;
     *       | &lt;STRING_LITERAL&gt;
     *       | &lt;TRUE&gt;
     *       | &lt;FALSE&gt;
     *       | &lt;LPAREN&gt; Expression(basicBooleanCounter) &lt;RPAREN&gt;
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(BasicExpression n) {
        InstrBooleanTerm returnTerm = null;
        if (n.f0.which == 0) {
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n);
        } else if (n.f0.which == 3) {
            // <TRUE>
            NodeToken token = (NodeToken) n.f0.choice; 
            returnTerm = new InstrOperatorTerm(XampilBooleanOperators.getTrueOperator(),
                    token.startOffset, token.endOffset);
        } else if (n.f0.which == 4) {
            // <FALSE>
            NodeToken token = (NodeToken) n.f0.choice; 
            returnTerm = new InstrOperatorTerm(XampilBooleanOperators.getFalseOperator(),
                    token.startOffset, token.endOffset);
        } else if (n.f0.choice instanceof NodeSequence) {
            NodeSequence nodeSequence = (NodeSequence) n.f0.choice;
            InstrBooleanTerm expressionTerm = nodeSequence.nodes.get(1).accept(this);
            returnTerm = new InstrBracketTerm(expressionTerm);
        } else {
            throw new RuntimeException("Integer or string literal used as basic boolean term.");
        }
        return returnTerm;
    }
}
