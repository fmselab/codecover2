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

import org.codecover.model.utils.*;

/**
 * A HierarchyLevel is a program object which can contain other HierarchyLevels
 * or StatementSequences, e.g. Java packages, files, classes and functions.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: HierarchyLevel.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public class HierarchyLevel extends AbstractLocatableMetaDataObject {
    private final LocationList header;

    private final List<StatementSequence> sequences;

    private final List<HierarchyLevel> children;

    private final HierarchyLevelType type;

    private final String name;

    private final String id;

    HierarchyLevel(LocationList location, String name, LocationList header,
            HierarchyLevelType type, List<HierarchyLevel> children,
            List<StatementSequence> sequences, String id) {
        super(location);

        if (name == null) {
            throw new NullPointerException("name == null");
        }

        if (header == null) {
            throw new NullPointerException("header == null");
        }

        if (type == null) {
            throw new NullPointerException("type == null");
        }

        if (id == null) {
            throw new NullPointerException("id == null");
        }

        if (children == null) {
            throw new NullPointerException("children == null");
        }

        if (sequences == null) {
            throw new NullPointerException("sequences == null");
        }

        this.name = name;
        this.header = header;
        this.type = type;
        this.id = id;
        this.children = CollectionUtil.copy(children);
        this.sequences = CollectionUtil.copy(sequences);
    }

    /**
     * @return the children
     */
    public List<HierarchyLevel> getChildren() {
        return this.children;
    }

    /**
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    /**
     * @return the header
     */
    public LocationList getHeader() {
        return this.header;
    }

    /**
     * @return a string representation of the header
     */
    public String getHeaderString() {
        // What should we do here? For now, we simply return the content of
        // the first Location of the header.
        if (getHeader().getLocations().size() == 0) {
            return "";
        } else {
            return getHeader().getLocations().get(0).getContent();
        }
    }

    /**
     * @return the sequences
     */
    public List<StatementSequence> getSequences() {
        return this.sequences;
    }

    /**
     * @return the type
     */
    public HierarchyLevelType getType() {
        return this.type;
    }

    /**
     * Gets the id of the {@link HierarchyLevel}
     * 
     * @return the id
     */
    public String getId() {
        return this.id;
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
        int result = getSequences().size();
        for (Location location : getLocation().getLocations()) {
            result += location.getStartOffset();
            result += location.getEndOffset();
            result += location.getFile().hashCode();
            result += location.getFile().getContent().hashCode();
        }

        return result;
    }

    /**
     * Returns a debug representation of the object.
     * 
     * @see LocationList#toString()
     */
    @Override
    public String toString() {
        return getClass().toString() + " (" + getType() + ") -- "
                + getLocation().toString();
    }

    /**
     * The {@link org.codecover.model.mast.HierarchyLevel.Visitor Visitor} to be
     * used for all {@link HierarchyLevel}s
     * 
     * @author Steffen Kieß, Markus Wittlinger
     * @version 1.0 ($Id: HierarchyLevel.java 69 2010-01-27 19:31:18Z schmidberger $)
     */
    public static interface Visitor {
        /**
         * Visits a {@link HierarchyLevel}
         * 
         * @param hierarchyLevel
         *            the given {@link HierarchyLevel}
         */
        void visit(HierarchyLevel hierarchyLevel);
    }

    /**
     * A default, empty implementation of the
     * {@link org.codecover.model.mast.HierarchyLevel.Visitor Visitor}
     * 
     * @author Steffen Kieß, Markus Wittlinger
     * @version 1.0 ($Id: HierarchyLevel.java 69 2010-01-27 19:31:18Z schmidberger $)
     */
    public static class DefaultVisitor implements Visitor {
        /**
         * (non-Javadoc)
         * 
         * @see org.codecover.model.mast.HierarchyLevel.Visitor#visit(org.codecover.model.mast.HierarchyLevel)
         */
        public void visit(HierarchyLevel hierarchyLevel) {
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
     * @param statementPre
     *            the {@link org.codecover.model.mast.Statement.Visitor Visitor}
     *            to be used in accepting the {@link StatementSequence}
     * @param statementPost
     *            the {@link org.codecover.model.mast.Statement.Visitor Visitor}
     *            to be used in accepting the {@link StatementSequence}
     * @param rootTermPre
     *            the {@link org.codecover.model.mast.RootTerm.Visitor Visitor}
     *            to be used in accepting the {@link RootTerm}
     * @param rootTermPost
     *            the {@link org.codecover.model.mast.RootTerm.Visitor Visitor}
     *            to be used in accepting the {@link RootTerm}
     * @param termPre
     *            the
     *            {@link org.codecover.model.mast.BooleanTerm.Visitor Visitor}
     *            to be used in accepting the {@link BooleanTerm}
     * @param termPost
     *            the
     *            {@link org.codecover.model.mast.BooleanTerm.Visitor Visitor}
     *            to be used in accepting the {@link BooleanTerm}
     */
    public void accept(Visitor pre, Visitor post,
            Statement.Visitor statementPre, Statement.Visitor statementPost,
            RootTerm.Visitor rootTermPre, RootTerm.Visitor rootTermPost,
            BooleanTerm.Visitor termPre, BooleanTerm.Visitor termPost, QuestionMarkOperator.Visitor qmaVisitor) {
        if (pre != null) {
            pre.visit(this);
        }
        for (HierarchyLevel child : getChildren()) {
            child.accept(pre, post, statementPre, statementPost, rootTermPre,
                    rootTermPost, termPre, termPost, qmaVisitor);
        }
        if (statementPre != null || statementPost != null
                || rootTermPre != null || rootTermPost != null
                || termPre != null || termPost != null || qmaVisitor != null) {
            for (StatementSequence sequence : getSequences()) {
                sequence.accept(statementPre, statementPost, rootTermPre,
                        rootTermPost, termPre, termPost, qmaVisitor);
            }
        }
        if (post != null) {
            post.visit(this);
        }
    }

    private abstract class ChildParentMap<ChildType, ParentType> {
        private final Object lock = new Object();

        private volatile Map<ChildType, ParentType> data = null;

        /**
         * Gets the parent of the given child
         * 
         * @param child
         *            the given child
         * @return the parent.
         * @throws IllegalArgumentException
         *             when no parent was found.
         */
        public ParentType getParent(ChildType child) {
            final ParentType parent = tryGetParent(child);

            if (parent == null) {
                throw new IllegalArgumentException(
                        "Trying to get parent for child " + child
                                + " with HierarchyLevel " + HierarchyLevel.this);
            }

            return parent;
        }

        /**
         * Gets the parent of the given child. Returns null, if no parent was
         * found.
         * 
         * @param child
         *            the given child
         * @return the parent.
         */
        public ParentType tryGetParent(ChildType child) {
            if (child == null) {
                throw new NullPointerException("child == null");
            }

            Map<ChildType, ParentType> map = this.data;
            if (map == null) {
                synchronized (this.lock) {
                    map = this.data;
                    if (map == null) {
                        map = this.data = createMap();
                        if (map == null) {
                            throw new RuntimeException();
                        }
                    }
                }
            }

            return map.get(child);
        }

        private Map<ChildType, ParentType> createMap() {
            final Map<ChildType, ParentType> result = new HashMap<ChildType, ParentType>();

            addData(result);

            return Collections.unmodifiableMap(result);
        }

        protected abstract void addData(Map<ChildType, ParentType> map);
    }

    private final ChildParentMap<HierarchyLevel, HierarchyLevel> hierarchyLevelMap = new ChildParentMap<HierarchyLevel, HierarchyLevel>() {
        @Override
        protected void addData(final Map<HierarchyLevel, HierarchyLevel> map) {
            accept(new DefaultVisitor() {
                @Override
                public void visit(HierarchyLevel hierarchyLevel) {
                    for (HierarchyLevel child : hierarchyLevel.getChildren()) {
                        map.put(child, hierarchyLevel);
                    }
                }
            }, null, null, null, null, null, null, null, null);
        }
    };

    /**
     * Gets the parent {@link HierarchyLevel} of the given
     * {@link HierarchyLevel}
     * 
     * @param level
     *            the given {@link HierarchyLevel} to check
     * @return the parent {@link HierarchyLevel}
     * @throws IllegalArgumentException
     *             when <code>this == level</code>
     */
    public HierarchyLevel getParent(HierarchyLevel level) {
        if (this == level) {
            throw new IllegalArgumentException("Cannot get my own parent");
        }

        return this.hierarchyLevelMap.getParent(level);
    }

    private final ChildParentMap<StatementSequence, Object> statementSequenceMap = new ChildParentMap<StatementSequence, Object>() {
        @Override
        protected void addData(final Map<StatementSequence, Object> map) {
            accept(new DefaultVisitor() {
                @Override
                public void visit(HierarchyLevel hierarchyLevel) {
                    for (StatementSequence sequence : hierarchyLevel
                            .getSequences()) {
                        map.put(sequence, hierarchyLevel);
                    }
                }
            }, null, new Statement.DefaultVisitor() {
                @Override
                public void visit(LoopingStatement statement) {
                    map.put(statement.getBody(), statement);
                }

                @Override
                public void visit(Branch branch) {
                    map.put(branch.getSequence(), branch);
                }
            }, null, null, null, null, null, null);
        }
    };

    /**
     * Returns a HierarchyLevel, a Branch or a LoopingStatement
     * 
     * @param sequence
     *            the {@link StatementSequence} to check
     * @return the parent object.
     */
    public Object getParent(StatementSequence sequence) {
        return this.statementSequenceMap.getParent(sequence);
    }

    private final ChildParentMap<Statement, StatementSequence> statementMap = new ChildParentMap<Statement, StatementSequence>() {
        @Override
        protected void addData(final Map<Statement, StatementSequence> map) {
            accept(null, null, new Statement.DefaultVisitor() {
                @Override
                public void visit(StatementSequence sequence) {
                    for (Statement statement : sequence.getStatements()) {
                        map.put(statement, sequence);
                    }
                }
            }, null, null, null, null, null, null);
        }
    };

    /**
     * Gets the {@link StatementSequence} the given {@link Statement} belongs
     * to.
     * 
     * @param statement
     *            the given {@link Statement} to check
     * @return the parent object.
     */
    public StatementSequence getParent(Statement statement) {
        return this.statementMap.getParent(statement);
    }

    private final ChildParentMap<Branch, ConditionalStatement> branchMap = new ChildParentMap<Branch, ConditionalStatement>() {
        @Override
        protected void addData(final Map<Branch, ConditionalStatement> map) {
            accept(null, null, new Statement.DefaultVisitor() {
                @Override
                public void visit(ConditionalStatement statement) {
                    for (Branch branch : statement.getBranches()) {
                        map.put(branch, statement);
                    }
                }
            }, null, null, null, null, null, null);
        }
    };

    /**
     * Gets the {@link ConditionalStatement} the given {@link Branch} belongs
     * to.
     * 
     * @param branch
     *            the {@link Branch} to check.
     * @return the {@link ConditionalStatement}
     */
    public ConditionalStatement getParent(Branch branch) {
        return this.branchMap.getParent(branch);
    }

    private final ChildParentMap<CoverableItem, Object> coverableItemMap = new ChildParentMap<CoverableItem, Object>() {
        @Override
        protected void addData(final Map<CoverableItem, Object> map) {
            accept(null, null, new Statement.DefaultVisitor() {
                private void maybeAdd(CoverableItem item, Object o) {
                    if (o == null) {
                        throw new RuntimeException();
                    }

                    if (item != null) {
                        map.put(item, o);
                    }
                }

                private void doStatement(Statement statement) {
                    maybeAdd(statement.getCoverableItem(), statement);
                }

                @Override
                public void visit(BasicStatement statement) {
                    doStatement(statement);
                }

                @Override
                public void visit(ConditionalStatement statement) {
                    doStatement(statement);
                }

                @Override
                public void visit(LoopingStatement statement) {
                    doStatement(statement);
                    maybeAdd(statement.getNeverExecutedItem(), statement);
                    maybeAdd(statement.getOnceExecutedItem(), statement);
                    maybeAdd(statement.getMultipleExecutedItem(), statement);
                }

                @Override
                public void visit(Branch branch) {
                    maybeAdd(branch.getCoverableItem(), branch);
                }
            }, null, null, null, null, null, null);
        }
    };

    /**
     * Returns a Statement, a Branch or a RootTerm (for loop coverage the
     * LoopingStatement will be returned)
     * 
     * @param item
     *            the {@link CoverableItem} to check
     * @return the parent object.
     */
    public Object getParent(CoverableItem item) {
        return this.coverableItemMap.getParent(item);
    }

    /**
     * Returns a Statement, a Branch or a RootTerm (for loop coverage the
     * LoopingStatement will be returned)
     * 
     * @param prefix
     *            the prefix of the {@link CoverableItem} to check
     * @param id
     *            the id of the {@link CoverableItem} to check
     * @return the parent object.
     */
    public Object getParentOfCoverableItem(String prefix, String id) {
        // The new CoverableItem will work because CoverableItem implements
        // hashCode() and equals()
        return getParent(new CoverableItem(prefix, id));
    }
}
