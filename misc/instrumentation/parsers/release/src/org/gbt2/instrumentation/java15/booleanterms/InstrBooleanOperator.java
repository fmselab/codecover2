///////////////////////////////////////////////////////////////////////////////
//
// $Id: InstrBooleanOperator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 09.04.2007 16:29:42
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.booleanterms;

import java.util.Formatter;
import java.util.Queue;

import org.gbt2.instrumentation.java15.syntaxtree.AndExpression;
import org.gbt2.instrumentation.java15.syntaxtree.BooleanLiteral;
import org.gbt2.instrumentation.java15.syntaxtree.ConditionalAndExpression;
import org.gbt2.instrumentation.java15.syntaxtree.ConditionalExpression;
import org.gbt2.instrumentation.java15.syntaxtree.ConditionalOrExpression;
import org.gbt2.instrumentation.java15.syntaxtree.ExclusiveOrExpression;
import org.gbt2.instrumentation.java15.syntaxtree.Expression;
import org.gbt2.instrumentation.java15.syntaxtree.InclusiveOrExpression;
import org.gbt2.instrumentation.java15.syntaxtree.NodeToken;
import org.gbt2.instrumentation.java15.syntaxtree.UnaryExpressionNotPlusMinus;

/**
 * This class represents a boolean operator of a given arity.<br>
 * <br>
 * Arity means the number of operants, this operator is used for. A
 * {@link InstrBooleanOperator} occurs in an {@link InstrOperatorTerm}. The
 * arity has to be the number of operants:<br>
 * <code>operatorTerm.getOperants.size() == operatorTerm.getOperator.getArity()</code><br>
 * Moreover an {@link InstrBooleanOperator} has a name and a Format String (@see
 * {@link Formatter}. The Format String has placeholders for all operants. By
 * the way the {@link InstrBooleanOperator} can be transformed to a String by
 * calling {@link #termToString(String[])}. The results are for example:<br>
 * <ul>
 * <li><code>%s || %s</code> &rarr; <code>a || b</code></li>
 * <li><code>!%s</code> &rarr; <code>!a</code></li>
 * <li><code>true</code> &rarr; <code>true</code></li>
 * <li><code>%s ? %s : %s</code> &rarr; <code>a ? b : c</code></li>
 * </ul>
 * <br>
 * The instances of this class can not be got using
 * {@link #InstrBooleanOperator(int, String, String)}, but by using the varius
 * static methods.
 * 
 * 
 * @author Christoph Müller
 * @see InstrOperatorTerm
 * @see Formatter
 */
public class InstrBooleanOperator {

    private int arity;

    private String name;

    private String operantFormatter;

    /**
     * Constructs a new {@link InstrBooleanOperator} and setting the fields.
     * 
     * @param arity
     *            The arity.
     * @param name
     *            The name.
     * @param operantFormatter
     *            The Format String.
     */
    private InstrBooleanOperator(int arity, String name, String operantFormatter) {
        super();
        this.arity = arity;
        this.name = name;
        this.operantFormatter = operantFormatter;
    }

    /**
     * The arity of this operator&mdash;the number of bound operants.
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

    /**
     * The formatter used to transform an {@link InstrOperatorTerm} to a String.
     * 
     * @return The Format String.
     * @see Formatter
     * @see #termToString(String[])
     */
    public String getOperantFormatter() {
        return this.operantFormatter;
    }

    /**
     * Transformes the operants of an {@link InstrOperatorTerm} to a String
     * using the {@link #operantFormatter}.
     * 
     * @param operantQueue
     *            The Queue with all the operants bounded to this operator.<br>
     *            <code>operantQueue.size() == this.getArity()</code>
     * 
     * @return A String with the formatted operator and operants.
     */
    public String termToString(Queue<InstrBooleanTerm> operantQueue) {
        Object[] operantsAsString = new String[operantQueue.size()];

        int pos = 0;
        for (InstrBooleanTerm booleanTerm : operantQueue) {
            operantsAsString[pos++] = booleanTerm.termToString();
        }

        // The cast to Object[] is needed to call the method format correct.
        // Otherwise operantsAsString might be seen as one Object for the
        // Formatter.
        return String.format(this.operantFormatter, operantsAsString);
    }

    /**
     * Constructs an operator with no operants&mdash;a constant.
     * 
     * @param name
     *            The name;
     * 
     * @param image
     *            The image of the operator.
     * 
     * @return The constructed {@link InstrBooleanOperator}.
     */
    private static InstrBooleanOperator getConstantOperator(String name,
            String image) {
        InstrBooleanOperator newOp = new InstrBooleanOperator(0, name, image);
        return newOp;
    }

    /**
     * Constructs an operator with one operant.<br>
     * <br>
     * The {@link #operantFormatter} is created using the image as a prefix:<br>
     * <code>image%s</code>
     * 
     * @param name
     *            The name;
     * 
     * @param image
     *            The image of the operator.
     * 
     * @return The constructed {@link InstrBooleanOperator}.
     */
    private static InstrBooleanOperator getOneArgumentOperator(String name,
            String image) {
        String operantFormatter = image + "%s";
        InstrBooleanOperator newOp = new InstrBooleanOperator(1, name,
                operantFormatter);
        return newOp;
    }
    
    /**
     * Constructs an operator with two operants.<br>
     * <br>
     * The {@link #operantFormatter} is created using the image as a separator:<br>
     * <code>%s image %s</code>
     * 
     * @param name
     *            The name;
     * 
     * @param image
     *            The image of the operator.
     * 
     * @return The constructed {@link InstrBooleanOperator}.
     */
    private static InstrBooleanOperator getTwoArgumentOperator(String name,
            String image) {
        String operantFormatter = "%s " + image + " %s";
        InstrBooleanOperator newOp = new InstrBooleanOperator(2, name,
                operantFormatter);
        return newOp;
    }

    /**
     * Constructs an operator with arity operants.<br>
     * <br>
     * The {@link #operantFormatter} is created using the image as a separator
     * between the operants:<br>
     * <code>%s image %s image %s image %s</code>
     * 
     * @param name
     *            The name;
     * 
     * @param image
     *            The image of the operator.
     * 
     * @return The constructed {@link InstrBooleanOperator}.
     */
    private static InstrBooleanOperator getManyArgumentOperator(int arity,
            String name, String image) {
        StringBuffer operantFormatter = new StringBuffer("%s");
        for (int i = 2; i <= arity; i++) {
            operantFormatter.append(" " + image + " %s");
        }
        InstrBooleanOperator newOp = new InstrBooleanOperator(arity, name,
                operantFormatter.toString());
        return newOp;
    }

    /**
     * Constructs a <code>||</code> operator with arity operants.
     * 
     * @param n
     *            The {@link ConditionalOrExpression} where the new operator
     *            occurs.
     * @param arity
     *            The number of operants.
     * 
     * @return The constructed {@link InstrBooleanOperator}.
     */
    @SuppressWarnings("unused")
    public static InstrBooleanOperator getOperator(ConditionalOrExpression n,
            int arity) {
        InstrBooleanOperator op = getManyArgumentOperator(arity,
                "short circuit or (||)", "||");
        return op;
    }

    /**
     * Constructs a <code>&&</code> operator with arity operants.
     * 
     * @param n
     *            The {@link ConditionalAndExpression} where the new operator
     *            occurs.
     * @param arity
     *            The number of operants.
     * 
     * @return The constructed {@link InstrBooleanOperator}.
     */
    @SuppressWarnings("unused")
    public static InstrBooleanOperator getOperator(ConditionalAndExpression n,
            int arity) {
        InstrBooleanOperator op = getManyArgumentOperator(arity,
                "short circuit and (&&)", "&&");
        return op;
    }

    /**
     * Constructs a <code>|</code> operator with arity operants.
     * 
     * @param n
     *            The {@link InclusiveOrExpression} where the new operator
     *            occurs.
     * @param arity
     *            The number of operants.
     * 
     * @return The constructed {@link InstrBooleanOperator}.
     */
    @SuppressWarnings("unused")
    public static InstrBooleanOperator getOperator(InclusiveOrExpression n,
            int arity) {
        InstrBooleanOperator op = getManyArgumentOperator(arity, "or (|)", "|");
        return op;
    }

    /**
     * Constructs a <code>^</code> operator with arity operants.
     * 
     * @param n
     *            The {@link ExclusiveOrExpression} where the new operator
     *            occurs.
     * @param arity
     *            The number of operants.
     * 
     * @return The constructed {@link InstrBooleanOperator}.
     */
    @SuppressWarnings("unused")
    public static InstrBooleanOperator getOperator(ExclusiveOrExpression n,
            int arity) {
        InstrBooleanOperator op = getManyArgumentOperator(arity,
                "exclusive or (^)", "^");
        return op;
    }

    /**
     * Constructs a <code>&</code> operator with arity operants.
     * 
     * @param n
     *            The {@link AndExpression} where the new operator occurs.
     * @param arity
     *            The number of operants.
     * 
     * @return The constructed {@link InstrBooleanOperator}.
     */
    @SuppressWarnings("unused")
    public static InstrBooleanOperator getOperator(AndExpression n, int arity) {
        InstrBooleanOperator op = getManyArgumentOperator(arity, "and (&)", "&");
        return op;
    }

    /**
     * Constructs a <code>!</code> operator with one operant.
     * 
     * @param n
     *            The {@link UnaryExpressionNotPlusMinus} where the new operator
     *            occurs.
     * 
     * @return The constructed {@link InstrBooleanOperator}.
     */
    @SuppressWarnings("unused")
    public static InstrBooleanOperator getOperator(UnaryExpressionNotPlusMinus n) {
        InstrBooleanOperator op = getOneArgumentOperator("not (!)", "!");
        return op;
    }

    /**
     * Constructs a constant <code>true</code> or <code>false</code>
     * operator with no operant.
     * 
     * @param n
     *            The {@link BooleanLiteral} where the new operator occurs.
     * 
     * @return The constructed {@link InstrBooleanOperator}.
     */
    public static InstrBooleanOperator getOperator(BooleanLiteral n) {
        InstrBooleanOperator op;
        NodeToken constantImage = (NodeToken) n.f0.choice;
        if (constantImage.tokenImage.equals("true")) {
            op = getConstantOperator("true", "true");
        } else if (constantImage.tokenImage.equals("false")) {
            op = getConstantOperator("false", "false");
        } else {
            throw new RuntimeException("only true or false expected");
        }
        return op;
    }

    /**
     * Constructs a trinary <code>? :</code> operator with three operants.
     * 
     * @param n
     *            The {@link ConditionalExpression} where the new operator
     *            occurs.
     * 
     * @return The constructed {@link InstrBooleanOperator}.
     */
    @SuppressWarnings("unused")
    public static InstrBooleanOperator getOperator(ConditionalExpression n) {
        InstrBooleanOperator op = new InstrBooleanOperator(3,
                "conditional operator ( ? : )", "%s ? %s : %s");
        return op;
    }

    /**
     * Constructs a assignement operator with two operants.<br>
     * <br>
     * This has one of the forms: <code>a = b</code><br>
     * <code>a |= b</code><br>
     * <code>a &= b</code><br>
     * <code>a ^= b</code>
     * 
     * @param n
     *            The {@link Expression} where the new operator occurs.
     * @param operatorImage
     *            The image of the operator.
     * 
     * @return The constructed {@link InstrBooleanOperator}.
     */
    @SuppressWarnings("unused")
    public static InstrBooleanOperator getOperator(Expression n,
            String operatorImage) {
        InstrBooleanOperator op;
        if (operatorImage.equals("=")) {
            op = getTwoArgumentOperator("assignment (=)", operatorImage);
        } else if (operatorImage.equals("|=")) {
            op = getTwoArgumentOperator("assignment or (|=)", operatorImage);
        } else if (operatorImage.equals("&=")) {
            op = getTwoArgumentOperator("assignment and (&=)", operatorImage);
        } else if (operatorImage.equals("^=")) {
            op = getTwoArgumentOperator("assignment xor (^=)", operatorImage);
        } else {
            throw new RuntimeException("only true or false expected");
        }
        return op;
    }
}
