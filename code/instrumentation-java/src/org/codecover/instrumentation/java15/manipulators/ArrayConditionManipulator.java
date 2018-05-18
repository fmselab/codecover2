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

package org.codecover.instrumentation.java15.manipulators;

import static org.codecover.instrumentation.java15.JavaBooleanOperators.getAndOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getAssignmentAndReplacement;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getAssignmentExclusiveOrReplacement;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getAssignmentOrReplacement;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getConditionalAndOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getConditionalOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getExclusiveOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getOrOperator;
import static org.codecover.instrumentation.java15.JavaBooleanOperators.getTrueTerm;
import static org.codecover.instrumentation.java15.visitor.TreeDumperWithException.LINE_SEPARATOR;

import java.io.IOException;
import java.io.Writer;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.codecover.instrumentation.booleanterms.InstrBasicBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrBooleanOperator;
import org.codecover.instrumentation.booleanterms.InstrBooleanTerm;
import org.codecover.instrumentation.booleanterms.InstrBracketTerm;
import org.codecover.instrumentation.booleanterms.InstrDepthFirstVisitor;
import org.codecover.instrumentation.booleanterms.InstrOperatorTerm;
import org.codecover.instrumentation.java.measurement.ConditionCounter;
import org.codecover.instrumentation.java.measurement.CounterContainer;
import org.codecover.instrumentation.java.measurement.LargeConditionCounter;
import org.codecover.instrumentation.java.measurement.MediumConditionCounter;
import org.codecover.instrumentation.java.measurement.SmallOneConditionCounter;
import org.codecover.instrumentation.java.measurement.SmallTwoConditionCounter;
import org.codecover.instrumentation.java15.JavaBooleanOperators;
import org.codecover.instrumentation.java15.counter.CounterIDManager;
import org.codecover.instrumentation.java15.counter.CounterManager;
import org.codecover.instrumentation.java15.syntaxtree.Expression;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.CoverableItem;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.SourceFile;

/**
 * This is a {@link ConditionManipulator}.<br>
 * <br>
 * Conditions are instrumented by an encapsulation: the basic boolean term
 * <b>B</b> is manipulated to:<br>
 * <pre>
 * (((conditionLongHelper1 |= BIT1) == 0 || true) && 
 *  ((<b>B</b>) &&
 *   ((conditionLongHelper1 |= BIT0) == 0 || true)))
 * </pre>
 * Moreover the whole term T is encapsulated too:
 * <pre>
 *   (((((conditionLongHelper1 = 0) == 0) || true) &amp;&amp;
 *     (<b>T</b>)) &amp;&amp;
 *    (CodeCoverCoverageCounter.C[1].incrementCounterForBitMask(conditionLongHelper1) || true)) ||
 *   (CodeCoverCoverageCounter.C[1].incrementCounterForBitMask(conditionLongHelper1) && false))
 * </pre>
 * <br>
 * Remark: Assignments can not be instrumented the way we do it generally,
 * because the JDK compiler has problems analyzing the more complex control
 * flow. For this reason, a boolean term, that contains at least one assignment,
 * is not instrumented.
 * 
 * @see ConditionManipulator
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: ArrayConditionManipulator.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public class ArrayConditionManipulator extends AbstractDefaultManipulator
        implements ConditionManipulator, CounterManager {

    // /////////////////////////////////////////////////////////////////////////
    //
    // Format Strings and constants for statement coverage
    //
    // /////////////////////////////////////////////////////////////////////////

    private static final String INT_BIT_MASK_FORMAT = "CodeCoverConditionCoverageHelper_%1$s";

    private static final String INT_BIT_MASK_DECLARATION = "int "
        + INT_BIT_MASK_FORMAT + ";";

    private static final String BASIC_BOOLEAN_TERM_WRAPPING_NO_ARRAY =
        "%n(((%1$s |= " + "(%2$d)) == 0 || true) &&%n" +
        " ((%4$s) && %n" +
        "  ((%1$s |= " + "(%3$d)) == 0 || true)))%n";

    private static final String INT_BIT_MASK_SET_ZERO = "(%1$s = 0) == 0";

    private static final String INTARRAY_BIT_MASK_DECLARATION = "int[] "
        + INT_BIT_MASK_FORMAT + ";";

    private static final String BASIC_BOOLEAN_TERM_WRAPPING_ARRAY =
        "%n(((%1$s[%5$d] |= " + "(%2$d)) == 0 || true) &&%n" +
        " ((%4$s) && %n" +
        "  ((%1$s[%5$d] |= " + "(%3$d)) == 0 || true)))%n";

    private static final String INTARRAY_BIT_MASK_SET_ZERO = "(%1$s = new int[%2$d]) == null";

    private static final String INCREMENT_COUNTER_FOR_BIT_MASK1 = "(%1$s."
            + ConditionCounter.INCREMENT_COUNTER_FOR_BIT_MASK_METHOD_NAME
            + "(%2$s, %3$d) || true)";
    
    private static final String INCREMENT_COUNTER_FOR_BIT_MASK2 = "(%1$s."
        + ConditionCounter.INCREMENT_COUNTER_FOR_BIT_MASK_METHOD_NAME
        + "(%2$s, %3$d) && false)";

    private static final String CONDITON_COUNTER_ARRAY_NAME = "conditionCounters";

    private static final String CONDITION_COUNTER_FORMAT = "%1$s" + "."
            + CONDITON_COUNTER_ARRAY_NAME + "[%2$d]";

    private static final String CONDITION_COUNTER_DECLARATION = "public static final "
            + ConditionCounter.class.getName()
            + "[] "
            + CONDITON_COUNTER_ARRAY_NAME
            + " = new "
            + ConditionCounter.class.getName() + "[%1$d];";

    private static final String SECTION_NAME_ASSIGNMENT = "final String " +
            "SECTION_NAME = \"%1$s\";";

    private static final String TYPE_ARRAY_DECLARATION1 = "final byte[] " +
            "CONDITION_COUNTER_TYPES = {";

    private static final String TYPE_ARRAY_DECLARATION2 = "};";

    private static final String CONDITION_COUNTER_INITIALIZATION =
        "      switch (CONDITION_COUNTER_TYPES[i]) {" + LINE_SEPARATOR +
        "        case 0 : break;" + LINE_SEPARATOR +
        "        case 1 : " + CONDITON_COUNTER_ARRAY_NAME + "[i] = new " + SmallOneConditionCounter.class.getName() + "(SECTION_NAME, \"C\" + i); break;" + LINE_SEPARATOR +
        "        case 2 : " + CONDITON_COUNTER_ARRAY_NAME + "[i] = new " + SmallTwoConditionCounter.class.getName() + "(SECTION_NAME, \"C\" + i); break;" + LINE_SEPARATOR +
        "        case 3 : " + CONDITON_COUNTER_ARRAY_NAME + "[i] = new " + MediumConditionCounter.class.getName() + "(SECTION_NAME, \"C\" + i); break;" + LINE_SEPARATOR +
        "        case 4 : " + CONDITON_COUNTER_ARRAY_NAME + "[i] = new " + LargeConditionCounter.class.getName() + "(SECTION_NAME, \"C\" + i); break;" + LINE_SEPARATOR +
        "      }";

    private static final String LOOP_OVER_ARRAY = "for (int i = 1; i <= %1$d; i++)";

    private static final String IF_NOT_NULL = "if ("
            + CONDITON_COUNTER_ARRAY_NAME + "[i] != null)";

    private static final String RESET_CONDITION_COUNTER = CONDITON_COUNTER_ARRAY_NAME
            + "[i]." + CounterContainer.RESET_METHOD_NAME + "();";

    private static final String SERIALIZE_CONDITION_COUNTER = CONDITON_COUNTER_ARRAY_NAME
            + "[i]."
            + CounterContainer.SERIALIZE_AND_RESET_METHOD_NAME
            + "("
            + CounterIDManager.LOG_NAME + ");";

    private static final String ASSIGNMENT_WARNING =
            "The expression contains an assignment which is not instrumented. " +
            "See \"documentation > reference\" for details.";

    // /////////////////////////////////////////////////////////////////////////
    //
    // private members
    //
    // /////////////////////////////////////////////////////////////////////////

    private TreeMap<Integer, ConditionIDAndType> instrumentedConditions;

    private int maxConditionID;

    // /////////////////////////////////////////////////////////////////////////
    //
    // constructor, public and private methods
    //
    // /////////////////////////////////////////////////////////////////////////

    /**
     * Constructor for a new {@link ArrayConditionManipulator}.
     */
    public ArrayConditionManipulator() {
        this.instrumentedConditions = new TreeMap<Integer, ConditionIDAndType>();
        this.maxConditionID = -1;
    }

    // /////////////////////////////////////////////////////////////////////////
    //
    // methods for the interface ConditionManipulator
    //
    // /////////////////////////////////////////////////////////////////////////

    /**
     * Always true.<br>
     * <br>
     * Because we add a declaration before an if, while, for statement.
     * 
     * @return true;
     */
    public boolean requiresBlockExpansionsForBranches() {
        return true;
    }

    /**
     * Always true.<br>
     * <br>
     * Because we add a declaration before an if, while, for statement.
     * 
     * @return true;
     */
    public boolean requiresBlockExpansionsForLoops() {
        return true;
    }

    public ConditionManipualtionResult manipulateAndDeclare(InstrBooleanTerm booleanTerm,
            String conditionID,
            CoverableItem coverableItem,
            MASTBuilder builder,
            SourceFile sourceFile) throws IOException {
        ConditionManipualtionResult returnValue = new ConditionManipualtionResult();

        // first we have to search for assignments
        // if there are assignments, we skip the instrumentation
        AssignmentFinder assignmentFinder = new AssignmentFinder();
        booleanTerm.access(assignmentFinder);
        if (assignmentFinder.isAssignmentPresent) {
            returnValue.instrumentedTerm = booleanTerm;
            returnValue.rootTermForMast = null;
            returnValue.warningMessage = ASSIGNMENT_WARNING;
            return returnValue;
        }

        // AFTER we have manipulated the tree, we manipulate the basic booleans
        List<InstrBasicBooleanTerm> basicTerms = new LinkedList<InstrBasicBooleanTerm>();
        // dumps all InstrBasicBooleanTerms to this List
        booleanTerm.getAllBasicBooleanTerms(basicTerms);

        Integer conditionCounterNumber = new Integer(CounterIDManager
                .getNumberFromConditionID(conditionID));
        this.maxConditionID = Math.max(this.maxConditionID,
                conditionCounterNumber.intValue());
        byte conditionCounterType;

        if (basicTerms.size() == 0) {
            // we don't need to instrument
            returnValue.instrumentedTerm = booleanTerm;
            returnValue.rootTermForMast = null;
            return returnValue;
        } else if (basicTerms.size() == 1) {
            conditionCounterType = ConditionCounter.CONDITION_COUNTER_TYPE_SMALL_ONE;
        } else if (basicTerms.size() == 2) {
            conditionCounterType = ConditionCounter.CONDITION_COUNTER_TYPE_SMALL_TWO;
        } else if (basicTerms.size() <= MediumConditionCounter.MAX_TERM_COUNT) {
            // till 16 Bits, we use one integer as a bit mask
            conditionCounterType = ConditionCounter.CONDITION_COUNTER_TYPE_MEDIUM;
        } else {
            // for more than 16 bits we use an int array
            conditionCounterType = ConditionCounter.CONDITION_COUNTER_TYPE_LARGE;
        }

        // save a new entry in the list of instrumetedConditions
        // -> this is needed, to know later, which ID requires which
        // ConditionCounter
        ConditionIDAndType conditionIDAndType = new ConditionIDAndType(
                conditionID, conditionCounterType);
        this.instrumentedConditions.put(conditionCounterNumber, conditionIDAndType);

        String intBitMaskHelperName = String.format(INT_BIT_MASK_FORMAT, 
                conditionID);
        String conditionCounterName = String.format(CONDITION_COUNTER_FORMAT,
                super.getCounterIDManager().getInnerClassName(),
                conditionCounterNumber);

        switch (conditionCounterType) {
        case ConditionCounter.CONDITION_COUNTER_TYPE_SMALL_ONE:
            // same as case MEDIUM
        case ConditionCounter.CONDITION_COUNTER_TYPE_SMALL_TWO:
            // same as case MEDIUM
        case ConditionCounter.CONDITION_COUNTER_TYPE_MEDIUM:
            // write the declaration
            super.getWriter().write(LINE_SEPARATOR);
            super.getWriter().write(
                    String.format(INT_BIT_MASK_DECLARATION, conditionID));

            // this method adds bit masks around every basic boolean term
            addBitMasksNoArray(basicTerms, intBitMaskHelperName);

            // method adds the initializer and the increment counter
            returnValue.instrumentedTerm = addInitialisationAndIncrementationNoArray(
                    booleanTerm,
                    intBitMaskHelperName,
                    conditionCounterName,
                    basicTerms.size());

            break;
        case ConditionCounter.CONDITION_COUNTER_TYPE_LARGE:
            // write the declaration
            super.getWriter().write(LINE_SEPARATOR);
            super.getWriter().write(
                    String.format(INTARRAY_BIT_MASK_DECLARATION, conditionID));

            // this method adds bit masks around every basic boolean term
            final int arraylength = addBitMasksArray(basicTerms, intBitMaskHelperName); 

            // method adds the initializer and the increment counter
            returnValue.instrumentedTerm = addInitialisationAndIncrementationArray(
                    booleanTerm,
                    intBitMaskHelperName,
                    conditionCounterName,
                    arraylength,
                    basicTerms.size());
            break;
        }

        returnValue.rootTermForMast = toRootTerm(booleanTerm,
                coverableItem,
                builder,
                sourceFile);

        return returnValue;
    }

    /**
     * Modifies every basic boolean term <b>T</b> to:<br>
     * <pre>
     * (((conditionLongHelper1 |= BIT1) == 0 | true) & 
     *  ((<b>T</b>) &&
     *   ((conditionLongHelper1 |= BIT0) == 0 | true)))
     * </pre>
     * This method is for <code>basicTerms.size() &le; 16</code>
     * 
     * @param basicTerms
     *            The root of the instrumented {@link Expression}.
     * @param intBitMaskHelper
     *            The name of the integer used for the bit mask.
     */
    private void addBitMasksNoArray(List<InstrBasicBooleanTerm> basicTerms,
            String intBitMaskHelper) {
        final int termCount = basicTerms.size();
        // the bit number is decremented -> the first term has the highest,
        // the last 0
        int thisBitNumber = termCount * 2;

        for (InstrBasicBooleanTerm term : basicTerms) {
            // for every basic term we need two bits

            Integer usedBit = new Integer(1 << (--thisBitNumber));
            Integer trueBit = new Integer(1 << (--thisBitNumber));

            String imageBefore = term.termToString();
            String imageModified = String.format(BASIC_BOOLEAN_TERM_WRAPPING_NO_ARRAY,
                    intBitMaskHelper,
                    usedBit,
                    trueBit,
                    imageBefore);
            term.modifyImage(imageModified);
        }
    }

    /**
     * Modifies every basic boolean term <b>T</b> to:<br>
     * 
     * <pre>
     *  (((conditionLongHelper1[0] |= BIT1) == 0 || true) &amp;&amp;
     *   ((<b>T</b>) &amp;&amp;
     *    ((conditionLongHelper1[0] |= BIT0) == 0 || true)))
     * </pre>
     * 
     * This method is for <code>basicTerms.size() &gt; 16</code>
     * 
     * @param basicTerms
     *            The root of the instrumented {@link Expression}.
     * @param intBitMaskHelper
     *            The name of the integer used for the bit mask.
     *
     * @see ConditionCounter#incrementCounterOfBitMask(int[], int)
     */
    private int addBitMasksArray(List<InstrBasicBooleanTerm> basicTerms,
            String intBitMaskHelper) {
        final int termCount = basicTerms.size();
        final int bitsPerEntry = LargeConditionCounter.TERMS_PER_INT;
        // how many array entries do we need -> will be incremented for every
        // while loop
        int arrayLength = 0;
        // how many terms have to instrumented -> will be decremented for
        // every while loop
        int remainingTermCount = termCount;
        Iterator<InstrBasicBooleanTerm> termIterator = basicTerms.iterator(); 

        while (remainingTermCount > 0) {
            // how many terms can we handle now
            int handleNowTermCount = Math.min(remainingTermCount, bitsPerEntry);
            remainingTermCount -= handleNowTermCount;

            // the bit number is decremented -> the first term has the highest,
            // the last 0
            int thisBitNumber = handleNowTermCount * 2;

            while (thisBitNumber >= 1) {
                InstrBasicBooleanTerm term = termIterator.next();

                // for every basic term we need two bits
                Integer usedBit = new Integer(1 << (--thisBitNumber));
                Integer trueBit = new Integer(1 << (--thisBitNumber));

                String imageBefore = term.termToString();
                String imageModified = String.format(BASIC_BOOLEAN_TERM_WRAPPING_ARRAY,
                        intBitMaskHelper,
                        usedBit,
                        trueBit,
                        imageBefore,
                        new Integer(arrayLength));
                term.modifyImage(imageModified);
            }

            // we needed one more int of the int array
            arrayLength++;
        }

        return arrayLength;
    }

    /**
     * Embeds the root term <b>T</b> into:<br>
     * 
     * <pre>
     *   (((((conditionLongHelper1 = 0) == 0) || true) &amp;&amp;
     *     (<b>T</b>)) &amp;&amp;
     *    (CodeCoverCoverageCounter.C[1].incrementCounterForBitMask(conditionLongHelper1) || true)) ||
     *   (CodeCoverCoverageCounter.C[1].incrementCounterForBitMask(conditionLongHelper1) && false))
     * </pre>
     * 
     * @param booleanTerm
     *            The root term <b>T</b>.
     * 
     * @param intBitMaskHelper
     *            The name of the integer used for the bit mask.
     * 
     * @param conditionCounterName
     *            The name of the {@link ConditionCounter} variable, the bit
     *            mask is given to.
     * 
     * @return The modified root term.
     */
    private InstrBooleanTerm addInitialisationAndIncrementationNoArray(
            InstrBooleanTerm booleanTerm,
            String intBitMaskHelper,
            String conditionCounterName,
            int termCount) {

        String setZeroImage = String.format(INT_BIT_MASK_SET_ZERO,
                intBitMaskHelper);

        return addInitialisationAndIncrementation(booleanTerm,
                intBitMaskHelper,
                conditionCounterName,
                termCount,
                setZeroImage);
    }

    /**
     * Embeds the root term <b>T</b> into:<br>
     * 
     * <pre>
     *   (((((conditionLongHelper1 = new int[]) == null) || true) &amp;&amp;
     *     (&lt;b&gt;T&lt;b&gt;)) &amp;&amp;
     *    (CodeCoverCoverageCounter.C[1].incrementCounterForBitMask(conditionLongHelper1) || true) ||
     *   (CodeCoverCoverageCounter.C[1].incrementCounterForBitMask(conditionLongHelper1) && false)
     * </pre>
     * 
     * @param booleanTerm
     *            The root term <b>T</b>.
     * 
     * @param intBitMaskHelper
     *            The name of the integer used for the bit mask.
     * 
     * @param conditionCounterName
     *            The name of the {@link ConditionCounter} variable, the bit
     *            mask is given to.
     * 
     * @return The modified root term.
     */
    private InstrBooleanTerm addInitialisationAndIncrementationArray(
            InstrBooleanTerm booleanTerm,
            String intBitMaskHelper,
            String conditionCounterName,
            int arrayLength,
            int termCount) {

        String setZeroImage = String.format(INTARRAY_BIT_MASK_SET_ZERO,
                intBitMaskHelper, new Integer(arrayLength));

        return addInitialisationAndIncrementation(booleanTerm,
                                                  intBitMaskHelper,
                                                  conditionCounterName,
                                                  termCount,
                                                  setZeroImage);
    }

    private InstrBooleanTerm addInitialisationAndIncrementation(
            InstrBooleanTerm booleanTerm,
            String intBitMaskHelper,
            String conditionCounterName,
            int termCount,
            String setZeroImage) {

        // (conditionLongHelper1 = 0) == 0
        InstrBooleanTerm setZeroTerm = new InstrBasicBooleanTerm(
                setZeroImage, -1, -1);
        // ((conditionLongHelper1 = 0) == 0)
        InstrBooleanTerm setZeroBracket = new InstrBracketTerm(setZeroTerm);
        // ((conditionLongHelper1 = 0) == 0) || true
        InstrBooleanTerm setZeroTrue = new InstrOperatorTerm(setZeroBracket,
                getConditionalOrOperator(), getTrueTerm(), -1, -1);
        // (((conditionLongHelper1 = 0) == 0) || true)
        InstrBooleanTerm leftZeroBracket = new InstrBracketTerm(setZeroTrue);

        // (T)
        InstrBracketTerm bracketAroundTheTerm = new InstrBracketTerm(booleanTerm); 
        // (((conditionLongHelper1 = 0) == 0) || true) && (T)
        InstrBooleanTerm leftTerm = new InstrOperatorTerm(leftZeroBracket,
                getConditionalAndOperator(), bracketAroundTheTerm, -1, -1);
        // LEFT = ((((conditionLongHelper1 = 0) == 0) || true) && (T))
        InstrBooleanTerm leftTermBracket = new InstrBracketTerm(leftTerm); 

        String incrementCounterImage1 = String.format(
                INCREMENT_COUNTER_FOR_BIT_MASK1, conditionCounterName,
                intBitMaskHelper, Integer.valueOf(termCount));
        // RIGHT1 = (CodeCoverCoverageCounter.C[1].incrementCounterForBitMask(conditionLongHelper1, termCount) || true)
        InstrBooleanTerm rightTerm1 = new InstrBasicBooleanTerm(
                incrementCounterImage1, -1, -1);

        // LEFT && (RIGHT1)
        InstrBooleanTerm mainTerm1 = new InstrOperatorTerm(leftTermBracket,
                getConditionalAndOperator(), rightTerm1, -1, -1);
        // (LEFT && (RIGHT1))
        InstrBooleanTerm mainTerm1Bracket = new InstrBracketTerm(mainTerm1);

        String incrementCounterImage2 = String.format(
                INCREMENT_COUNTER_FOR_BIT_MASK2, conditionCounterName,
                intBitMaskHelper, Integer.valueOf(termCount));
        // RIGHT2 = (CodeCoverCoverageCounter.C[1].incrementCounterForBitMask(conditionLongHelper1, termCount) && false)
        InstrBooleanTerm rightTerm2 = new InstrBasicBooleanTerm(
                incrementCounterImage2, -1, -1);

        // (LEFT && (RIGHT1)) || RIGHT2
        InstrBooleanTerm mainTerm = new InstrOperatorTerm(mainTerm1Bracket,
                getConditionalOrOperator(), rightTerm2, -1, -1);
        
        return mainTerm;
    }

    private RootTerm toRootTerm(InstrBooleanTerm term,
                                CoverableItem coverableItem,
                                MASTBuilder builder,
                                SourceFile sourceFile) {
        return builder.createRootTerm(term.toBooleanTerm(builder, sourceFile),
                coverableItem);
    }

    public void writeDeclarations() throws IOException {
        Writer writer = super.getWriter();

        // the declarations for the condition counter array
        if (this.instrumentedConditions.size() > 0) {
            // this declares an Array of type ConditionCounter

            writer.write(LINE_SEPARATOR);
            writer.write("  ");
            writer.write(String.format(CONDITION_COUNTER_DECLARATION, new Integer(
                    this.maxConditionID + 1)));
            writer.write(LINE_SEPARATOR);

            // now we produce something like that
            // static {
            //   conditionCounters[1] = new SmallOneConditionCounter("className", "ID");
            //   ..
            // }
            writer.write("  static {");
            writer.write(LINE_SEPARATOR);

            String sectionName = super.getCounterIDManager().
                    getSectionName();
            if (sectionName == null) {
                throw new IllegalStateException(
                        "fullClassName is unset and by the way null");
            }

            writer.write("    ");
            writer.write(String.format(SECTION_NAME_ASSIGNMENT, sectionName));
            writer.write(LINE_SEPARATOR);
            writer.write("    ");
            writer.write(TYPE_ARRAY_DECLARATION1);

            // now we create a byte array, that contains exactly maxConditionID
            // entries:
            // 0 - not used
            // 1 - CONDITION_COUNTER_TYPE_SMALL_ONE
            // 2 - CONDITION_COUNTER_TYPE_SMALL_TWO
            // 3 - CONDITION_COUNTER_TYPE_MEDIUM
            // 4 - CONDITION_COUNTER_TYPE_LARGE
            //
            // we have to create "0" for every entry, that is not in 
            // instrumentedConditions

            Iterator<Entry<Integer, ConditionIDAndType>> entryIterator = 
                this.instrumentedConditions.entrySet().iterator();
            Entry<Integer, ConditionIDAndType> thisEntry;
            if (entryIterator.hasNext()) {
                thisEntry = entryIterator.next();
            } else {
                thisEntry = null;
            }
            for (int currentCounter = 0; currentCounter <= this.maxConditionID; 
                currentCounter++) {
                if (thisEntry == null || currentCounter < thisEntry.getKey().intValue()) {
                    writer.write('0');
                } else if (currentCounter == thisEntry.getKey().intValue()) {
                    switch (thisEntry.getValue().conditionCounterType) {
                    case ConditionCounter.CONDITION_COUNTER_TYPE_SMALL_ONE:
                        writer.write('1');
                        break;
                    case ConditionCounter.CONDITION_COUNTER_TYPE_SMALL_TWO:
                        writer.write('2');
                        break;
                    case ConditionCounter.CONDITION_COUNTER_TYPE_MEDIUM:
                        writer.write('3');
                        break;
                    case ConditionCounter.CONDITION_COUNTER_TYPE_LARGE:
                        writer.write('4');
                        break;
                    default:
                        throw new IllegalStateException(
                                "conditionCounterType \"" +
                                thisEntry.getValue().conditionCounterType +
                                "\"unknown");
                    }

                    if (entryIterator.hasNext()) {
                        thisEntry = entryIterator.next();
                    } else {
                        thisEntry = null;
                    }
                } else {
                    throw new IllegalStateException("currentCounter > thisEntry.getKey().intValue()");
                }

                if (currentCounter < this.maxConditionID) {
                    writer.write(',');
                }
            }

            writer.write(TYPE_ARRAY_DECLARATION2);
            writer.write(LINE_SEPARATOR);

            writer.write("    ");
            writer.write(String.format(LOOP_OVER_ARRAY, new Integer(
                    this.maxConditionID)));
            writer.write(" {");
            writer.write(LINE_SEPARATOR);
            
            writer.write(CONDITION_COUNTER_INITIALIZATION);

            writer.write(LINE_SEPARATOR);
            writer.write("    }");
            writer.write(LINE_SEPARATOR);
            writer.write("  }");
            writer.write(LINE_SEPARATOR);
        }
    }

    public void writeReset() throws IOException {
        Writer writer = super.getWriter();

        // the reset method for the condition counter array
        if (this.instrumentedConditions.size() > 0) {
            writer.write("    ");
            writer.write(String.format(LOOP_OVER_ARRAY, new Integer(
                    this.maxConditionID)));
            writer.write(" {");
            writer.write(LINE_SEPARATOR);
            writer.write("      ");
            writer.write(IF_NOT_NULL);
            writer.write(" {");
            writer.write(LINE_SEPARATOR);
            writer.write("        ");
            writer.write(RESET_CONDITION_COUNTER);
            writer.write(LINE_SEPARATOR);
            writer.write("      }");
            writer.write(LINE_SEPARATOR);
            writer.write("    }");
            writer.write(LINE_SEPARATOR);
        }
    }

    public void writeSerialzeAndReset() throws IOException {
        Writer writer = super.getWriter();

        // the serializeAndmethod for the condition counter array
        if (this.instrumentedConditions.size() > 0) {
            writer.write("    ");
            writer.write(String.format(LOOP_OVER_ARRAY, new Integer(
                    this.maxConditionID)));
            writer.write(" {");
            writer.write(LINE_SEPARATOR);
            writer.write("      ");
            writer.write(IF_NOT_NULL);
            writer.write(" {");
            writer.write(LINE_SEPARATOR);
            writer.write("        ");
            writer.write(SERIALIZE_CONDITION_COUNTER);
            writer.write(LINE_SEPARATOR);
            writer.write("      }");
            writer.write(LINE_SEPARATOR);
            writer.write("    }");
            writer.write(LINE_SEPARATOR);
        }
    }

    /**
     * This inner class is just a container for the conditionID and the class of
     * the used {@link ConditionCounter}.<br>
     * <br>
     * It is needed to store all instrumented conditions in
     * {@link ArrayConditionManipulator#instrumentedConditions}. It has to be
     * stored, which condition needs which {@link ConditionCounter}.
     * 
     * @author Christoph Müller
     * @version 1.0 ($Id: ArrayConditionManipulator.java 69 2010-01-27 19:31:18Z schmidberger $)
     */
    private static class ConditionIDAndType {
        final String conditionID;

        final byte conditionCounterType;

        /**
         * Creates a new {@link ConditionIDAndType}
         * 
         * @param conditionID
         *            The ID of a condition.
         * 
         * @param conditionCounterType
         *            A used CONDITION_COUNTER_TYPE. (See
         *            {@link ConditionCounter}).
         */
        ConditionIDAndType(String conditionID, byte conditionCounterType) {
            this.conditionID = conditionID;
            this.conditionCounterType = conditionCounterType;
        }
    }

    /**
     * This is {@link InstrDepthFirstVisitor}, which is used to find 
     * assignments in conditions.<br>
     * <br>
     * Assignments can not be instrumented the way we do it generally, because
     * the JDK compiler has problems analyzing the more complex control flow.
     * 
     * @author Christoph Müller
     * 
     * @version 1.0 ($Id: ArrayConditionManipulator.java 69 2010-01-27 19:31:18Z schmidberger $)
     */
    static class AssignmentFinder extends InstrDepthFirstVisitor {
        boolean isAssignmentPresent = false;

        @Override
        public void visit(InstrOperatorTerm term) {
            InstrBooleanOperator op = term.getOperator();
            if (op == JavaBooleanOperators.getAssignmentOperator()) {
                this.isAssignmentPresent = true;
            } else if (op == JavaBooleanOperators.getAssignmentOrOperator()) {
                if (op.getArity() != 2) {
                    throw new RuntimeException("JavaBooleanOperators.getAssignmentOperator().getArity() != 2");
                }
                if (!(term.getOperands().get(0) instanceof InstrBasicBooleanTerm)) {
                    throw new RuntimeException("!(term.getOperands().get(0) instanceof InstrBasicBooleanTerm)");
                }

                InstrBasicBooleanTerm left = (InstrBasicBooleanTerm) term.getOperands().get(0); 
                InstrBooleanTerm right = term.getOperands().get(1); 

                // transform "A |= B" into "(A = (A | B))"
                InstrOperatorTerm newRightTerm = new InstrOperatorTerm(left, getOrOperator(), right, -1, -1);
                InstrBooleanOperator newOp = getAssignmentOrReplacement(left);
                List<InstrBooleanTerm> newOperant = Collections.<InstrBooleanTerm>singletonList(newRightTerm);

                term.resetOperandAndOperators(newOp, newOperant, InstrOperatorTerm.EMPTY_START_END_LOCATION);

                // go on with traversing
                right.access(this);
            } else if (op == JavaBooleanOperators.getAssignmentAndOperator()) {
                if (op.getArity() != 2) {
                    throw new RuntimeException("JavaBooleanOperators.getAssignmentOperator().getArity() != 2");
                }
                if (!(term.getOperands().get(0) instanceof InstrBasicBooleanTerm)) {
                    throw new RuntimeException("!(term.getOperands().get(0) instanceof InstrBasicBooleanTerm)");
                }

                InstrBasicBooleanTerm left = (InstrBasicBooleanTerm) term.getOperands().get(0); 
                InstrBooleanTerm right = term.getOperands().get(1); 

                // transform "A &= B" into "(A = (A & B))"
                InstrOperatorTerm newRightTerm = new InstrOperatorTerm(left, getAndOperator(), right, -1, -1);
                InstrBooleanOperator newOp = getAssignmentAndReplacement(left);
                List<InstrBooleanTerm> newOperant = Collections.<InstrBooleanTerm>singletonList(newRightTerm);

                term.resetOperandAndOperators(newOp, newOperant, InstrOperatorTerm.EMPTY_START_END_LOCATION);

                // go on with traversing
                right.access(this);
            } else if (op == JavaBooleanOperators.getAssignmentExclusiveOrOperator()) {
                if (op.getArity() != 2) {
                    throw new RuntimeException("JavaBooleanOperators.getAssignmentOperator().getArity() != 2");
                }
                if (!(term.getOperands().get(0) instanceof InstrBasicBooleanTerm)) {
                    throw new RuntimeException("!(term.getOperands().get(0) instanceof InstrBasicBooleanTerm)");
                }

                InstrBasicBooleanTerm left = (InstrBasicBooleanTerm) term.getOperands().get(0); 
                InstrBooleanTerm right = term.getOperands().get(1); 

                // transform "A ^= B" into "(A = (A ^ B))"
                InstrOperatorTerm newRightTerm = new InstrOperatorTerm(left, getExclusiveOrOperator(), right, -1, -1);
                InstrBooleanOperator newOp = getAssignmentExclusiveOrReplacement(left);
                List<InstrBooleanTerm> newOperant = Collections.<InstrBooleanTerm>singletonList(newRightTerm);

                term.resetOperandAndOperators(newOp, newOperant, InstrOperatorTerm.EMPTY_START_END_LOCATION);

                // go on with traversing
                right.access(this);
            } else {
                super.visit(term);
            }
        }

        @Override
        public void visit(InstrBasicBooleanTerm term) {
            this.isAssignmentPresent |= term.containsAssignments();
            super.visit(term);
        }
    }
}
