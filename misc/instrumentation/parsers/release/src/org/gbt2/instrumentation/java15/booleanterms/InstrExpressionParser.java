///////////////////////////////////////////////////////////////////////////////
//
// $Id: InstrExpressionParser.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 09.04.2007 17:09:12
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.booleanterms;

import java.util.LinkedList;
import java.util.Queue;

import org.gbt2.instrumentation.java15.parser.JavaParser;
import org.gbt2.instrumentation.java15.syntaxtree.AdditiveExpression;
import org.gbt2.instrumentation.java15.syntaxtree.AndExpression;
import org.gbt2.instrumentation.java15.syntaxtree.AssignmentOperator;
import org.gbt2.instrumentation.java15.syntaxtree.BooleanLiteral;
import org.gbt2.instrumentation.java15.syntaxtree.ConditionalAndExpression;
import org.gbt2.instrumentation.java15.syntaxtree.ConditionalExpression;
import org.gbt2.instrumentation.java15.syntaxtree.ConditionalOrExpression;
import org.gbt2.instrumentation.java15.syntaxtree.DoStatement;
import org.gbt2.instrumentation.java15.syntaxtree.EqualityExpression;
import org.gbt2.instrumentation.java15.syntaxtree.ExclusiveOrExpression;
import org.gbt2.instrumentation.java15.syntaxtree.Expression;
import org.gbt2.instrumentation.java15.syntaxtree.ForStatement;
import org.gbt2.instrumentation.java15.syntaxtree.IfStatement;
import org.gbt2.instrumentation.java15.syntaxtree.InclusiveOrExpression;
import org.gbt2.instrumentation.java15.syntaxtree.InstanceOfExpression;
import org.gbt2.instrumentation.java15.syntaxtree.Literal;
import org.gbt2.instrumentation.java15.syntaxtree.MultiplicativeExpression;
import org.gbt2.instrumentation.java15.syntaxtree.Node;
import org.gbt2.instrumentation.java15.syntaxtree.NodeChoice;
import org.gbt2.instrumentation.java15.syntaxtree.NodeSequence;
import org.gbt2.instrumentation.java15.syntaxtree.NodeToken;
import org.gbt2.instrumentation.java15.syntaxtree.PostfixExpression;
import org.gbt2.instrumentation.java15.syntaxtree.PrimaryExpression;
import org.gbt2.instrumentation.java15.syntaxtree.PrimaryPrefix;
import org.gbt2.instrumentation.java15.syntaxtree.RelationalExpression;
import org.gbt2.instrumentation.java15.syntaxtree.ShiftExpression;
import org.gbt2.instrumentation.java15.syntaxtree.UnaryExpression;
import org.gbt2.instrumentation.java15.syntaxtree.UnaryExpressionNotPlusMinus;
import org.gbt2.instrumentation.java15.syntaxtree.WhileStatement;
import org.gbt2.instrumentation.java15.visitor.GJNoArguDepthFirst;
import org.gbt2.instrumentation.java15.visitor.GJNoArguVisitor;
import org.gbt2.instrumentation.java15.visitor.TreeStringDumper;

/**
 * This class is a {@link GJNoArguVisitor} extending {@link GJNoArguDepthFirst}.<br>
 * <br>
 * This class is used to parse an {@link Expression} of the java syntaxtree,
 * which occur in {@link IfStatement}, {@link WhileStatement},
 * {@link DoStatement} or {@link ForStatement}. Therfor you have to create an
 * instance of this class and call {@link #parse(Expression)} or likewise
 * {@link #parse(ConditionalOrExpression)}. The result is a
 * {@link InstrBooleanTerm}, which can for example be used for instrumentation.<br>
 * <br>
 * One instance of {@link InstrExpressionParser} can be used for parsing of
 * several {@link Expression}&mdash;for each a call of
 * {@link #parse(Expression)}. Attention: this parsing is not threadsafe.<br>
 * <br>
 * The parsing is done by traversing through the whole syntax tree generated by
 * the {@link JavaParser}. For every node it is decided, whether a
 * {@link InstrBasicBooleanTerm} is found, a new {@link InstrOperatorTerm} is
 * created out of sub {@link InstrBooleanTerm}, if the creating is delegated to
 * a child node or if the node does not return a boolean. In the last case a
 * {@link IllegalStateException} is thrown. This case should <b>NEVER</b> occur
 * and is just for reassurance, that the created boolean terms are realy boolean
 * terms.
 * 
 * @author Christoph Müller
 * 
 * @see InstrBooleanTerm
 * @see #parse(Expression)
 * @see #parse(ConditionalOrExpression)
 */
public class InstrExpressionParser extends GJNoArguDepthFirst<InstrBooleanTerm>
        implements GJNoArguVisitor<InstrBooleanTerm> {

    private TreeStringDumper dumper = new TreeStringDumper();

    /**
     * Parses the given {@link Expression} to generate a boolean tree. The root
     * node&mdash;a {@link InstrBooleanTerm}&mdash;is returned.
     * 
     * @param expression
     *            The {@link Expression} to parse.
     * 
     * @return The root of the constructed boolean tree.
     */
    public InstrBooleanTerm parse(Expression expression) {
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
            // this is operator term, where only the the right Expression is an
            // InstrBooleanTerm

            ConditionalExpression first = n.f0;
            NodeSequence f1Sequence = (NodeSequence) n.f1.node;
            AssignmentOperator assignmentOperator = (AssignmentOperator) f1Sequence.nodes.get(0);
            NodeToken operatorToken = (NodeToken) assignmentOperator.f0.choice; 
            Expression second = (Expression) f1Sequence.nodes.get(1);

            if (operatorToken.tokenImage.equals("=")
                    || operatorToken.tokenImage.equals("|=")
                    || operatorToken.tokenImage.equals("&=")
                    || operatorToken.tokenImage.equals("^=")) {
                InstrBooleanTerm firstTerm = first.accept(this);
                InstrBooleanOperator operator = InstrBooleanOperator
                        .getOperator(n, operatorToken.tokenImage);
                InstrBooleanTerm secondTerm = second.accept(this);

                returnTerm = new InstrOperatorTerm(firstTerm, operator,
                        secondTerm);
            } else {
                throw new IllegalStateException(
                        "the result of this operator is no boolean - state PARSING not expected");
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
            // this is operator with three operants
            NodeSequence f1Sequence = (NodeSequence) n.f1.node;
            Expression secondTerm = (Expression) f1Sequence.nodes.get(1);
            Expression thirdTerm = (Expression) f1Sequence.nodes.get(3);

            Queue<InstrBooleanTerm> operatorsQueue = new LinkedList<InstrBooleanTerm>();

            // first operator
            operatorsQueue.add(n.f0.accept(this));
            // second operator
            operatorsQueue.add(secondTerm.accept(this));
            // third operator
            operatorsQueue.add(thirdTerm.accept(this));

            InstrBooleanOperator operator = InstrBooleanOperator.getOperator(n);

            returnTerm = new InstrOperatorTerm(operator, operatorsQueue);
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

        if (n.f1.present()) {
            Queue<InstrBooleanTerm> operatorsQueue = new LinkedList<InstrBooleanTerm>();

            // the first operant is n.f0
            ConditionalAndExpression first = n.f0;
            InstrBooleanTerm firstTerm = first.accept(this);
            operatorsQueue.add(firstTerm);

            // the other operants, we have to get
            for (Node thisNode : n.f1.nodes) {
                NodeSequence sequence = (NodeSequence) thisNode;
                ConditionalAndExpression operantMiddle = (ConditionalAndExpression) sequence.nodes
                        .get(1);
                InstrBooleanTerm middleTerm = operantMiddle.accept(this);
                operatorsQueue.add(middleTerm);
            }

            // create a || operator for a given count of operants
            InstrBooleanOperator operator = InstrBooleanOperator.getOperator(n,
                    operatorsQueue.size());

            returnTerm = new InstrOperatorTerm(operator, operatorsQueue);
        } else {
            returnTerm = n.f0.accept(this);
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

        if (n.f1.present()) {
            Queue<InstrBooleanTerm> operatorsQueue = new LinkedList<InstrBooleanTerm>();

            // the first operant is n.f0
            InclusiveOrExpression first = n.f0;
            InstrBooleanTerm firstTerm = first.accept(this);
            operatorsQueue.add(firstTerm);

            // the other operants, we have to get
            for (Node thisNode : n.f1.nodes) {
                NodeSequence sequence = (NodeSequence) thisNode;
                InclusiveOrExpression operantMiddle = (InclusiveOrExpression) sequence.nodes
                        .get(1);
                InstrBooleanTerm middleTerm = operantMiddle.accept(this);
                operatorsQueue.add(middleTerm);
            }

            // create a && operator for a given count of operants
            InstrBooleanOperator operator = InstrBooleanOperator.getOperator(n,
                    operatorsQueue.size());

            returnTerm = new InstrOperatorTerm(operator, operatorsQueue);
        } else {
            returnTerm = n.f0.accept(this);
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

        if (n.f1.present()) {
            Queue<InstrBooleanTerm> operatorsQueue = new LinkedList<InstrBooleanTerm>();

            // the first operant is n.f0
            ExclusiveOrExpression first = n.f0;
            InstrBooleanTerm firstTerm = first.accept(this);
            operatorsQueue.add(firstTerm);

            // the other operants, we have to get
            for (Node thisNode : n.f1.nodes) {
                NodeSequence sequence = (NodeSequence) thisNode;
                ExclusiveOrExpression operantMiddle = (ExclusiveOrExpression) sequence.nodes
                        .get(1);
                InstrBooleanTerm middleTerm = operantMiddle.accept(this);
                operatorsQueue.add(middleTerm);
            }

            // create a | operator for a given count of operants
            InstrBooleanOperator operator = InstrBooleanOperator.getOperator(n,
                    operatorsQueue.size());

            returnTerm = new InstrOperatorTerm(operator, operatorsQueue);
        } else {
            returnTerm = n.f0.accept(this);
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

        if (n.f1.present()) {
            Queue<InstrBooleanTerm> operatorsQueue = new LinkedList<InstrBooleanTerm>();

            // the first operant is n.f0
            AndExpression first = n.f0;
            InstrBooleanTerm firstTerm = first.accept(this);
            operatorsQueue.add(firstTerm);

            // the other operants, we have to get
            for (Node thisNode : n.f1.nodes) {
                NodeSequence sequence = (NodeSequence) thisNode;
                AndExpression operantMiddle = (AndExpression) sequence.nodes
                        .get(1);
                InstrBooleanTerm middleTerm = operantMiddle.accept(this);
                operatorsQueue.add(middleTerm);
            }

            // create a ^ operator for a given count of operants
            InstrBooleanOperator operator = InstrBooleanOperator.getOperator(n,
                    operatorsQueue.size());

            returnTerm = new InstrOperatorTerm(operator, operatorsQueue);
        } else {
            returnTerm = n.f0.accept(this);
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

        if (n.f1.present()) {
            Queue<InstrBooleanTerm> operatorsQueue = new LinkedList<InstrBooleanTerm>();

            // the first operant is n.f0
            EqualityExpression first = n.f0;
            InstrBooleanTerm firstTerm = first.accept(this);
            operatorsQueue.add(firstTerm);

            // the other operants, we have to get
            for (Node thisNode : n.f1.nodes) {
                NodeSequence sequence = (NodeSequence) thisNode;
                EqualityExpression operantMiddle = (EqualityExpression) sequence.nodes
                        .get(1);
                InstrBooleanTerm middleTerm = operantMiddle.accept(this);
                operatorsQueue.add(middleTerm);
            }

            // create a & operator for a given count of operants
            InstrBooleanOperator operator = InstrBooleanOperator.getOperator(n,
                    operatorsQueue.size());

            returnTerm = new InstrOperatorTerm(operator, operatorsQueue);
        } else {
            returnTerm = n.f0.accept(this);
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
            this.dumper.reset();
            n.accept(this.dumper);
            String termImage = this.dumper.getContent();
            returnTerm = new InstrBasicBooleanTerm(termImage);
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
            this.dumper.reset();
            n.accept(this.dumper);
            String termImage = this.dumper.getContent();
            returnTerm = new InstrBasicBooleanTerm(termImage);
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
            this.dumper.reset();
            n.accept(this.dumper);
            String termImage = this.dumper.getContent();
            returnTerm = new InstrBasicBooleanTerm(termImage);
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
                    "the result of this operator is no boolean - state PARSING not expected");
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
                    "the result of this operator is no boolean - state PARSING not expected");
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
                    "the result of this operator is no boolean - state PARSING not expected");
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
                    "the result of this operator is no boolean - state PARSING not expected");
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
                        "the result of this operator is no boolean - state PARSING not expected");
            }

            UnaryExpression singleOperant = (UnaryExpression) sequence.nodes
                    .get(1);
            InstrBooleanTerm singleInstrBooleanTerm = singleOperant
                    .accept(this);
            // create a not operator from the given operators
            InstrBooleanOperator operator = InstrBooleanOperator.getOperator(n);

            returnTerm = new InstrOperatorTerm(operator, singleInstrBooleanTerm);
        } else if (n.f0.which == 2) {
            // PostfixExpression()
            returnTerm = n.f0.accept(this);
        } else {
            // CastExpression()
            throw new IllegalStateException(
                    "the result of this operator is no boolean - state PARSING not expected");
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
                    "the result of this operator is no boolean - state PARSING not expected");
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
            this.dumper.reset();
            n.accept(this.dumper);
            String termImage = this.dumper.getContent();
            returnTerm = new InstrBasicBooleanTerm(termImage);
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
            Expression expression = (Expression) sequence.nodes.get(1);
            InstrBooleanTerm expressionTerm = expression.accept(this);
            returnTerm = new InstrBracketTerm(expressionTerm);
        } else {
            // we can not parse more deeply - this mus be a boolean
            // in consequence this is a leaf
            // dump it to a String
            this.dumper.reset();
            n.accept(this.dumper);
            String termImage = this.dumper.getContent();
            returnTerm = new InstrBasicBooleanTerm(termImage);
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
                    "the result of this operator is no boolean - state PARSING not expected");
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

        InstrBooleanOperator constantOperator = InstrBooleanOperator
                .getOperator(n);
        returnTerm = new InstrOperatorTerm(constantOperator);

        return returnTerm;
    }
}
