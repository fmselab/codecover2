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

import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import org.codecover.instrumentation.booleanterms.InstrBasicBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrBooleanOperator;
import org.codecover.instrumentation.booleanterms.InstrBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrBracketTerm;
import org.codecover.instrumentation.booleanterms.InstrOperatorTerm;
import org.codecover.instrumentation.cobol85.syntaxtree.AbbreviationLeaf;
import org.codecover.instrumentation.cobol85.syntaxtree.AbbreviationRest;
import org.codecover.instrumentation.cobol85.syntaxtree.ArithmeticExpression;
import org.codecover.instrumentation.cobol85.syntaxtree.ClassCondition;
import org.codecover.instrumentation.cobol85.syntaxtree.CombinableCondition;
import org.codecover.instrumentation.cobol85.syntaxtree.Condition;
import org.codecover.instrumentation.cobol85.syntaxtree.ConditionNameCondition;
import org.codecover.instrumentation.cobol85.syntaxtree.Node;
import org.codecover.instrumentation.cobol85.syntaxtree.NodeChoice;
import org.codecover.instrumentation.cobol85.syntaxtree.NodeList;
import org.codecover.instrumentation.cobol85.syntaxtree.NodeOptional;
import org.codecover.instrumentation.cobol85.syntaxtree.NodeSequence;
import org.codecover.instrumentation.cobol85.syntaxtree.NodeToken;
import org.codecover.instrumentation.cobol85.syntaxtree.RelationCondition;
import org.codecover.instrumentation.cobol85.syntaxtree.RelationalOperator;
import org.codecover.instrumentation.cobol85.syntaxtree.SignCondition;
import org.codecover.instrumentation.cobol85.syntaxtree.SimpleCondition;
import org.codecover.instrumentation.cobol85.visitor.GJDepthFirst;
import org.codecover.instrumentation.cobol85.visitor.StartOffset;
import org.codecover.instrumentation.cobol85.visitor.TreeStringDumper;

/**
 * @author Stefan Franke
 * @version 1.0 ($Id: CobolConditionParser.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CobolConditionParser extends
        GJDepthFirst<InstrBooleanTerm, CobolConditionParser.SubjectAndOperator> {

    private TreeStringDumper dumper = new TreeStringDumper();

    /**
     * Parses the given {@link Condition} to generate a boolean tree. The root
     * node&mdash;a {@link InstrBooleanTerm}&mdash;is returned.
     * 
     * @param condition
     *            The {@link Condition} to parse.
     * 
     * @return The root of the constructed boolean tree.
     */
    public InstrBooleanTerm parse(Condition condition) {
        InstrBooleanTerm term = condition
                .accept(this, new SubjectAndOperator());
        return term;
    }

    /**
     * <PRE>
     * 
     * f0 -> CombinableCondition() 
     * f1 -> ( ( &lt;AND&gt; | &lt;OR&gt; ) (CombinableCondition() | AbbreviationRest() ) )*
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(Condition condition,
            SubjectAndOperator subjectAndOperator) {
        InstrBooleanTerm returnTerm = null;

        if (condition.f1.present()) {
            List<InstrBooleanTerm> operands = new Vector<InstrBooleanTerm>(2);
            int operatorLocations[] = new int[2];

            InstrBooleanTerm firstTerm = condition.f0.accept(this,
                    subjectAndOperator);
            operands.add(firstTerm);
            
            for (Node node : condition.f1.nodes) {
                NodeSequence nodeSequence = (NodeSequence) node;

                // the operator
                NodeChoice nodeChoiceOperator = (NodeChoice) nodeSequence.nodes
                        .get(0);
                NodeToken nodeToken = (NodeToken) nodeChoiceOperator.choice;
                subjectAndOperator.logicalOperator = nodeToken.tokenImage;
                operatorLocations[0] = nodeToken.startOffset;
                operatorLocations[1] = nodeToken.endOffset;

                // the operand
                NodeChoice nodeChoiceOperand = (NodeChoice) nodeSequence.nodes
                        .get(1);
                InstrBooleanTerm secondTerm = nodeChoiceOperand.accept(this,
                        subjectAndOperator);
                operands.add(secondTerm);
                
                InstrBooleanOperator instrBooleanOperator;
                if (nodeToken.tokenImage.equalsIgnoreCase("AND")) {
                    instrBooleanOperator = BooleanOperators.getAndOperator();
                    InstrBooleanTerm instrBooleanTerm = operands.get(0);
                    if (instrBooleanTerm instanceof InstrOperatorTerm) {
                        InstrOperatorTerm instrOperatorTerm = (InstrOperatorTerm) instrBooleanTerm;
                        if (instrOperatorTerm.getOperator() == BooleanOperators
                                .getOrOperator()) {
                            InstrOperatorTerm tempOperatorTerm = new InstrOperatorTerm(
                                    instrOperatorTerm.getOperands().get(1),
                                    instrBooleanOperator, secondTerm,
                                    nodeToken.startOffset, nodeToken.endOffset);
                            operands = new Vector<InstrBooleanTerm>(2);
                            operands.add(instrOperatorTerm.getOperands().get(0));
                            operands.add(tempOperatorTerm);
                            operatorLocations = instrOperatorTerm.getOperatorLocations();
                            instrBooleanOperator = BooleanOperators
                                    .getOrOperator();
                        }
                    }
                } else {
                    instrBooleanOperator = BooleanOperators.getOrOperator();
                }

                InstrOperatorTerm instrOperatorTerm = new InstrOperatorTerm(
                        instrBooleanOperator, operands, operatorLocations);
                operands = new Vector<InstrBooleanTerm>(2);
                operands.add(instrOperatorTerm);
            }
            returnTerm = operands.get(0);
        } else {
            returnTerm = condition.f0.accept(this, subjectAndOperator);
        }

        return returnTerm;
    }

    /**
     * <PRE>
     * f0 -> [ &lt;NOT&gt; ]
     * f1 -> SimpleCondition()
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(CombinableCondition combinableCondition,
            SubjectAndOperator subjectAndOperator) {
        InstrBooleanTerm returnTerm = null;
        InstrBooleanTerm instrBooleanTerm = combinableCondition.f1.accept(this, subjectAndOperator);
        if (combinableCondition.f0.present()) {
            NodeToken nodeToken = (NodeToken) combinableCondition.f0.node;
            InstrBooleanOperator instrBooleanOperator = BooleanOperators.getNotOperator();
            returnTerm = new InstrOperatorTerm(instrBooleanOperator, instrBooleanTerm,
                    nodeToken.startOffset, nodeToken.endOffset);
        }
        returnTerm = instrBooleanTerm;
        return returnTerm;
    }

    /**
     * <PRE>
     * f0 -> ( &lt;LPARENCHAR&gt; Condition() &lt;RPARENCHAR&gt; | RelationCondition() | ClassCondition() | ConditionNameCondition() )
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(SimpleCondition simpleCondition,
            SubjectAndOperator subjectAndOperator) {
        InstrBooleanTerm returnTerm = null;
        if (simpleCondition.f0.choice instanceof ClassCondition
                || simpleCondition.f0.choice instanceof ConditionNameCondition) {
            this.dumper.reset();
            simpleCondition.f0.choice.accept(this.dumper);
            int startOffset = StartOffset
                    .getStartOffset(simpleCondition.f0.choice);
            int endOffset = this.dumper.getPosition();
            returnTerm = new InstrBasicBooleanTerm(this.dumper.getContent(),
                    startOffset, endOffset);
        } else if (simpleCondition.f0.choice instanceof RelationCondition) {
            returnTerm = simpleCondition.f0.accept(this, subjectAndOperator);
        } else {
            NodeSequence nodeSequence = (NodeSequence) simpleCondition.f0.choice;
            Condition condition = (Condition) nodeSequence.nodes.get(1);
            returnTerm = condition.accept(this, subjectAndOperator);
            returnTerm = new InstrBracketTerm(returnTerm);
        }
        return returnTerm;
    }

    /**
     * <PRE>
     * 
     * f0 -> ArithmeticExpression()
     * f1 -> ( RelationalOperator() 
     *         ( ArithmeticExpression() 
     *         | &lt;LPARENCHAR&gt; ArithmeticExpression()
     *           ( ( &lt;AND&gt; | &lt;OR&gt; ) AbbreviationRest() )* &lt;RPARENCHAR&gt; ) 
     *       | SignCondition() )
     * 
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(RelationCondition relationCondition,
            SubjectAndOperator subjectAndOperator) {
        InstrBooleanTerm returnTerm = null;
        String basicBooleanTerm;
        int startOffset;
        int endOffset;

        // gets subject
        this.dumper.reset();
        relationCondition.f0.accept(this.dumper);
        subjectAndOperator.subject = this.dumper.getContent();

        // is it a sign condition or relational operator and object
        if (relationCondition.f1.choice instanceof NodeSequence) {
            NodeSequence nodeSequence = (NodeSequence) relationCondition.f1.choice;
            RelationalOperator relationalOperator = (RelationalOperator) nodeSequence.nodes
                    .get(0);
            
            ArithmeticExpression arithmeticExpression;
            NodeChoice nodeChoice = (NodeChoice) nodeSequence.nodes.get(1);
            if (nodeChoice.choice instanceof ArithmeticExpression) {
                arithmeticExpression = (ArithmeticExpression) nodeChoice.choice;
            } else {
                NodeSequence nSequence = (NodeSequence) nodeChoice.choice;
                arithmeticExpression = (ArithmeticExpression) nSequence.nodes.get(1);

                // gets relational operator
                this.dumper.reset();
                relationalOperator.accept(this.dumper);
                subjectAndOperator.relationalOperator = this.dumper
                        .getContent();

                // gets object
                this.dumper.reset();
                arithmeticExpression.accept(this.dumper);
                startOffset = StartOffset.getStartOffset(relationCondition.f0);
                endOffset = this.dumper.getPosition();

                // create basic boolean term
                basicBooleanTerm = subjectAndOperator.subject + " "
                        + subjectAndOperator.relationalOperator + " "
                        + this.dumper.getContent();

                InstrBooleanTerm instrBooleanTerm = new InstrBasicBooleanTerm(
                        basicBooleanTerm, startOffset, endOffset);
                
                List<InstrBooleanTerm> operands = new Vector<InstrBooleanTerm>(2);
                int operatorLocations[] = new int[2];
                
                NodeList nodeList = (NodeList) nSequence.nodes.get(2);

                operands.add(instrBooleanTerm);

                for (Node node : nodeList.nodes) {
                    NodeSequence noSequence = (NodeSequence) node;

                    // the operator
                    NodeChoice nodeChoiceOperator = (NodeChoice) noSequence.nodes
                            .get(0);
                    NodeToken nodeToken = (NodeToken) nodeChoiceOperator.choice;
                    subjectAndOperator.logicalOperator = nodeToken.tokenImage;
                    operatorLocations[0] = nodeToken.startOffset;
                    operatorLocations[1] = nodeToken.endOffset;

                    // the operand
                    AbbreviationRest abbreviationRest = (AbbreviationRest) noSequence.nodes
                            .get(1);
                    InstrBooleanTerm secondTerm = abbreviationRest.accept(this,
                            subjectAndOperator);
                    operands.add(secondTerm);

                    InstrBooleanOperator instrBooleanOperator;
                    if (nodeToken.tokenImage.equalsIgnoreCase("AND")) {
                        instrBooleanOperator = BooleanOperators
                                .getAndOperator();
                        InstrBooleanTerm iBooleanTerm = operands.get(0);
                        if (iBooleanTerm instanceof InstrOperatorTerm) {
                            InstrOperatorTerm instrOperatorTerm = (InstrOperatorTerm) iBooleanTerm;
                            if (instrOperatorTerm.getOperator() == BooleanOperators
                                    .getOrOperator()) {
                                InstrOperatorTerm tempOperatorTerm = new InstrOperatorTerm(
                                        instrOperatorTerm.getOperands().get(1),
                                        instrBooleanOperator, secondTerm,
                                        nodeToken.startOffset, nodeToken.endOffset);
                                operands = new Vector<InstrBooleanTerm>(2);
                                operands.add(instrOperatorTerm.getOperands()
                                        .get(0));
                                operands.add(tempOperatorTerm);
                                operatorLocations = instrOperatorTerm.getOperatorLocations();
                                instrBooleanOperator = BooleanOperators
                                        .getOrOperator();
                            }
                        }
                    } else {
                        instrBooleanOperator = BooleanOperators.getOrOperator();
                    }

                    InstrOperatorTerm instrOperatorTerm = new InstrOperatorTerm(
                            instrBooleanOperator, operands, operatorLocations);
                    operands = new Vector<InstrBooleanTerm>(2);
                    operands.add(instrOperatorTerm);
                }
                return operands.get(0);
            }

            // gets relational operator
            this.dumper.reset();
            relationalOperator.accept(this.dumper);
            subjectAndOperator.relationalOperator = this.dumper.getContent();

            // gets object
            this.dumper.reset();
            arithmeticExpression.accept(this.dumper);
            startOffset = StartOffset.getStartOffset(relationCondition.f0);
            endOffset = this.dumper.getPosition();

            // create basic boolean term
            basicBooleanTerm = subjectAndOperator.subject + " "
                    + subjectAndOperator.relationalOperator + " " + this.dumper.getContent();
        } else {
            SignCondition signCondition = (SignCondition) relationCondition.f1.choice;

            // gets object
            this.dumper.reset();
            signCondition.accept(this.dumper);
            startOffset = StartOffset.getStartOffset(relationCondition.f0);
            endOffset = this.dumper.getPosition();

            // create basic boolean term
            basicBooleanTerm = subjectAndOperator.subject + " "
                    + this.dumper.getContent();
        }
        returnTerm = new InstrBasicBooleanTerm(basicBooleanTerm, startOffset, endOffset);
        return returnTerm;
    }

    /**
     * <PRE>
     * f0 -> ( [ &lt;NOT&gt; ] [ RelationalOperator() ] AbbreviationLeaf() )+
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(AbbreviationRest abbreviationRest,
            SubjectAndOperator subjectAndOperator) {
        InstrBooleanTerm returnTerm = null;
        List<InstrBooleanTerm> operands = new Vector<InstrBooleanTerm>(2);
        for (Node node : abbreviationRest.f0.nodes) {
            NodeSequence nodeSequence = (NodeSequence) node;
            NodeOptional nodeOptionalNot = (NodeOptional) nodeSequence.nodes
                    .get(0);
            NodeOptional nodeOptionalOperator = (NodeOptional) nodeSequence.nodes
                    .get(1);
            AbbreviationLeaf abbreviationLeaf = (AbbreviationLeaf) nodeSequence.nodes
                    .get(2);

            String operator = "";
            if (nodeOptionalNot.present()) {
                NodeToken nodeToken = (NodeToken) nodeOptionalNot.node;
                operator = nodeToken.tokenImage;
            }
            if (nodeOptionalOperator.present()) {
                this.dumper.reset();
                nodeOptionalOperator.accept(this.dumper);
                if (operator.length() == 0) {
                    operator = this.dumper.getContent();
                } else {
                    operator = operator + " " + this.dumper.getContent();
                }
            }
            if (operator.length() != 0) {
                subjectAndOperator.relationalOperator = operator;
            }
            InstrBooleanTerm instrBooleanTerm = abbreviationLeaf.accept(this,
                    subjectAndOperator);
            operands.add(instrBooleanTerm);

            InstrBooleanOperator instrBooleanOperator;
            if (operands.size() == 2) {
                if (subjectAndOperator.logicalOperator.equalsIgnoreCase("AND")) {
                    instrBooleanOperator = BooleanOperators.getAndOperator();
                } else {
                    instrBooleanOperator = BooleanOperators.getOrOperator();
                }
                // The operator in this term is not explicitly stated in the source code.
                // For that, the position can not be given into the operator term object.
                InstrOperatorTerm instrOperatorTerm = new InstrOperatorTerm(operands.get(0), 
                        instrBooleanOperator, operands.get(1),-1,-1);
                operands = new Vector<InstrBooleanTerm>(2);
                operands.add(instrOperatorTerm);
            }
        }
        if (operands.size() == 1) {
            returnTerm = operands.get(0);
        }
        return returnTerm;
    }

    /**
     * <PRE>
     * f0 -> ( ArithmeticExpression() | &lt;LPARENCHAR&gt; ArithmeticExpression() AbbreviationRest() &lt;RPARENCHAR&gt; )
     * </PRE>
     */
    @Override
    public InstrBooleanTerm visit(AbbreviationLeaf abbreviationLeaf,
            SubjectAndOperator subjectAndOperator) {
        InstrBooleanTerm returnTerm = null;
        if (abbreviationLeaf.f0.choice instanceof ArithmeticExpression) {
            this.dumper.reset();
            abbreviationLeaf.f0.choice.accept(this.dumper);
            int startOffset = StartOffset.getStartOffset(abbreviationLeaf.f0.choice);
            int endOffset = this.dumper.getPosition();
            String basicBooleanTerm = subjectAndOperator.subject + " "
                    + subjectAndOperator.relationalOperator + " "
                    + this.dumper.getContent();
            returnTerm = new InstrBasicBooleanTerm(basicBooleanTerm, startOffset, endOffset);
        } else {
            NodeSequence nodeSequence = (NodeSequence) abbreviationLeaf.f0.choice;
            ArithmeticExpression arithmeticExpression = (ArithmeticExpression) nodeSequence.nodes
                    .get(1);
            AbbreviationRest abbreviationRest = (AbbreviationRest) nodeSequence.nodes
                    .get(2);

            this.dumper.reset();
            arithmeticExpression.accept(this.dumper);
            int startOffset = StartOffset.getStartOffset(arithmeticExpression);
            int endOffset = this.dumper.getPosition();
            String basicBooleanTerm = subjectAndOperator.subject + " "
                    + subjectAndOperator.relationalOperator + " "
                    + this.dumper.getContent();
            InstrBooleanTerm instrBooleanTerm = new InstrBasicBooleanTerm(
                    basicBooleanTerm, startOffset, endOffset);

            InstrBooleanOperator instrBooleanOperator;
            if (subjectAndOperator.logicalOperator.equalsIgnoreCase("AND")) {
                instrBooleanOperator = BooleanOperators.getAndOperator();
            } else {
                instrBooleanOperator = BooleanOperators.getOrOperator();
            }
            
            // The operator in this term is not explicitly stated in the source code.
            // For that, the position can not be given into the operator term object.
            returnTerm = new InstrOperatorTerm(instrBooleanTerm,
                    instrBooleanOperator, abbreviationRest.accept(this,
                            subjectAndOperator),-1,-1);
        }
        return returnTerm;
    }

    static class SubjectAndOperator {
        String subject = null;

        String relationalOperator = null;

        String logicalOperator = null;
    }
}
