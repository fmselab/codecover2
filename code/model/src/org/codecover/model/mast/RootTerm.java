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

package org.codecover.model.mast;

import java.util.*;

import org.codecover.model.TestCase;

/**
 * A RootTerm is a boolean term which is not a part of another boolean term. A
 * RootTerm consists of a BooleanTerm and a CoverableItem.
 * 
 * @author Markus Wittlinger, Tilmann Scheller
 * @version 1.0 ($Id: RootTerm.java 73 2010-07-30 19:42:18Z schmidberger $)
 */
public class RootTerm extends AbstractMetaDataObject {
    private final CoverableItem coverableItem;

    private final BooleanTerm term;

    protected final Map<BasicBooleanTerm, Integer> termMap;

    protected final Map<Integer, BasicBooleanTerm> posMap;

    RootTerm(BooleanTerm term, CoverableItem coverableItem) {

        if (coverableItem == null) {
            throw new NullPointerException("coverableItem == null");
        }

        if (term == null) {
            throw new NullPointerException("term == null");
        }

        this.coverableItem = coverableItem;
        this.term = term;

        // set up mappings between positions and BasicBooleanTerms
        this.termMap = new HashMap<BasicBooleanTerm, Integer>();
        this.posMap = new HashMap<Integer, BasicBooleanTerm>();

        PosVisitor visitor = new PosVisitor();
        this.accept(null, null, visitor, null);
    }

    // visitor to calculate the positions of the BasicBooleanTerms of
    // the RootTerm
    protected final class PosVisitor extends BasicBooleanTerm.DefaultVisitor {

        private int position = 0;

        /**
         * stores position of the current BasicBooleanTerm
         * 
         * @param basicTerm
         *            the BasicBooleanTerm whose position to store
         */
        @Override
        public void visit(BasicBooleanTerm basicTerm) {
            RootTerm.this.termMap.put(basicTerm, this.position);
            RootTerm.this.posMap.put(this.position, basicTerm);
            this.position++;
        }

    }

    /**
     * returns the position of the passed BasicBooleanTerm in the RootTerm
     * throws an IllegalArgumentException in case the BasicBooleanTerm does not
     * belong to the RootTerm
     * 
     * @param basicTerm
     *            the BasicBooleanTerm whose position to get
     * @return the position of the BasicBooleanTerm in the RootTerm
     */
    public int getPositionOfTerm(BasicBooleanTerm basicTerm) {
        if (!this.termMap.containsKey(basicTerm)) {
            throw new IllegalArgumentException(
                    "BooleanTerm is not part of this RootTerm");
        }

        return this.termMap.get(basicTerm);
    }
    
    /**
     * Für Auswertungen ggf. ganz nützlich: die Anzahl der Einzelterme
     * @return
     */
    public int getAmountBasicBooleanTerms() {
    	return this.termMap.size();
    }

    /**
     * returns the BasicBooleanTerm at the specified position in the RootTerm or
     * throws an IllegalArgumentException in case the position is out of bounds
     * 
     * @param position
     *            the position of the BasicBooleanTerm
     * @return the BasicBooleanTerm at the specified position
     */
    public BasicBooleanTerm getTermAtPosition(int position) {
        if (!this.posMap.containsKey(position)) {
            throw new IllegalArgumentException("position out of bounds");
        }

        return this.posMap.get(position);
    }

    /**
     * @return the term
     */
    public BooleanTerm getTerm() {
        return this.term;
    }

    /**
     * @return the coverableItem
     */
    public CoverableItem getCoverableItem() {
        return this.coverableItem;
    }
    
 

    /**
     * Gets the result the condition contained in the {@link RootTerm} produces
     * with the given {@link BooleanAssignment}
     * <p>
     * The number of {@link BooleanResult}s contained in the
     * {@link BooleanAssignment} must the same, as the number of
     * {@link BasicBooleanTerm} leaf the condition tree contains.
     * 
     * @param booleanAssignment
     *            the assignment, whose result is required
     * @return it returns {@link Boolean#TRUE}, if the condition resolves to
     *         <code>true</code>, {@link Boolean#FALSE} if the condition
     *         resolves to <code>false</code> and <code>null</code>, if
     *         this {@link RootTerm} contained no {@link BooleanTerm}s or none
     *         of the {@link BasicBooleanTerm}s were evaluated
     */
    public Boolean getAssignmentResult(BooleanAssignment booleanAssignment) {
        return getAssignmentResult(booleanAssignment, this.term);
    }

    /**
     * Gets the result the condition contained in the {@link RootTerm} produces
     * with the given {@link BooleanAssignment}
     * <p>
     * The number of {@link BooleanResult}s contained in the
     * {@link BooleanAssignment} must the same, as the number of
     * {@link BasicBooleanTerm} leaf the condition tree contains.
     * 
     * @param booleanAssignment
     *            the assignment, whose result is required
     * @param booleanTerm
     *            the boolean term, which is the root of the condition
     * @return it returns {@link Boolean#TRUE}, if the condition resolves to
     *         <code>true</code>, {@link Boolean#FALSE} if the condition
     *         resolves to <code>false</code> and <code>null</code>, if
     *         this {@link RootTerm} contained no {@link BooleanTerm}s or none
     *         of the {@link BasicBooleanTerm}s were evaluated
     */
    public static Boolean getAssignmentResult(
            BooleanAssignment booleanAssignment, BooleanTerm booleanTerm) {
        if (booleanTerm == null) {
            return null;
        }

        if (booleanAssignment == null) {
            throw new NullPointerException("booleanAssignment == null");
        }

        if (booleanAssignment.getResults().size() != booleanTerm
                .getBasicBooleanTerms()) {
            throw new RuntimeException("Illegal number of BooleanResults");
        }

        BooleanResult booleanResult = traverseTree(booleanTerm,
                booleanAssignment.getResults(), 0);

        return getBooleanFromBooleanResult(booleanResult);
    }

    private static BooleanResult traverseTree(BooleanTerm booleanTerm,
            List<BooleanResult> results, int startOffset) {

        if (booleanTerm instanceof BasicBooleanTerm) {
            return results.get(startOffset);

        } else if (booleanTerm instanceof OperatorTerm) {

            OperatorTerm operatorTerm = (OperatorTerm) booleanTerm;
            BooleanOperator booleanOperator = operatorTerm.getOperator();
            List<BooleanResult> newResults = new Vector<BooleanResult>();

            int newStartOffset = startOffset;
            for (BooleanTerm currentTerm : operatorTerm.getOperands()) {
                int childrenCount = currentTerm.getBasicBooleanTerms();

                newResults.add(traverseTree(currentTerm, results,
                        newStartOffset));

                newStartOffset += childrenCount;
            }

            Boolean booleanValue = booleanOperator.getPossibleAssignments()
                    .get(new BooleanAssignment(newResults));

            if (booleanValue == null) {
                return BooleanResult.NOT_EVALUATED;
            }

            return booleanValue ? BooleanResult.TRUE : BooleanResult.FALSE;
        }

        // This point should never be reached.
        return null;
    }

    private static Boolean getBooleanFromBooleanResult(
            BooleanResult booleanResult) {
        Boolean result;

        switch (booleanResult) {
            case TRUE:
                result = Boolean.TRUE;
                break;
            case FALSE:
                result = Boolean.FALSE;
                break;
            case NOT_EVALUATED:
            default:
                result = null;
                break;
        }

        return result;
    }

    /**
     * Returns a hash code value for the object. This method is supported for
     * the benefit of hashtables such as those provided by
     * <code>java.util.Hashtable</code>.
     * 
     * @return a hash code value for this object.
     */
    @Override
    public int hashCode() {
        int result = getCoverableItem().getId().hashCode();

        result += getTerm().getBasicBooleanTerms();

        return result;
    }

    /**
     * The {@link org.codecover.model.mast.RootTerm.Visitor Visitor} to be used
     * for all {@link RootTerm}s
     * 
     * @author Steffen Kieß, Markus Wittlinger
     * @version 1.0 ($Id: RootTerm.java 73 2010-07-30 19:42:18Z schmidberger $)
     */
    public static interface Visitor {
        /**
         * Visits a {@link RootTerm}
         * 
         * @param term
         *            the given {@link RootTerm}
         */
        void visit(RootTerm term);
    }

    /**
     * A default, empty implementation of the
     * {@link org.codecover.model.mast.RootTerm.Visitor Visitor}
     * 
     * @author Steffen Kieß, Markus Wittlinger
     * @version 1.0 ($Id: RootTerm.java 73 2010-07-30 19:42:18Z schmidberger $)
     */
    public static class DefaultVisitor implements Visitor {
        /**
         * (non-Javadoc)
         * 
         * @see org.codecover.model.mast.RootTerm.Visitor#visit(org.codecover.model.mast.RootTerm)
         */
        public void visit(RootTerm term) {
            // Do nothing.
        }
    }

    /**
     * Operations on the {@link HierarchyLevel} can be performed here.
     * 
     * @param pre
     *            the {@link Visitor} to be called before any operations are
     *            performed
     * @param post
     *            the {@link Visitor} to be called after any operations were
     *            performed
     * @param termPre
     *            the
     *            {@link org.codecover.model.mast.BooleanTerm.Visitor Visitor}
     *            to be used in accepting the {@link BooleanTerm}
     * @param termPost
     *            the
     *            {@link org.codecover.model.mast.BooleanTerm.Visitor Visitor}
     *            to be used in accepting the {@link BooleanTerm}
     */
    public void accept(Visitor pre, Visitor post, BooleanTerm.Visitor termPre,
            BooleanTerm.Visitor termPost) {
        if (pre != null) {
            pre.visit(this);
        }
        if (termPre != null || termPost != null) {
            getTerm().accept(termPre, termPost);
        }
        if (post != null) {
            post.visit(this);
        }
    }
}
