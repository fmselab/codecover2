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

import static org.codecover.UtilsForTestingJava.TARGET;
import static org.codecover.UtilsForTestingJava.locationToString;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import org.codecover.UtilsForTestingJava;
import org.codecover.instrumentation.Instrumenter;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.BasicBooleanTerm;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.Branch;
import org.codecover.model.mast.ConditionalStatement;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.LoopingStatement;
import org.codecover.model.mast.OperatorTerm;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.Statement;
import org.codecover.model.mast.StatementSequence;

/**
 * This class runs some tests and is able to put syntaxtree information into a
 * plain file, which can then be used for regression testing with external
 * diff-tools.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: RegressionTestUtil.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class RegressionTestUtil {
    public static final DateFormat DATE_FORMAT = new SimpleDateFormat(
            "yyyy-MM-dd-HH-mm-ss-SSS");

    public static void main(String[] args) {
        File targetFile = new File(String.format("%sregessionTest",
                TARGET,
                DATE_FORMAT.format(new Date())));
        UtilsForTestingJava.clearTarget();
        UtilsForTestingJava.copyMeasurementHelpersToBin();

        Instrumenter instrumenter = UtilsForTestingJava.getDefaultJavaInstrumenter();
        MASTBuilder builder = UtilsForTestingJava.newMASTBuilder();
        HierarchyLevel hierarchyLevel = null;
        PrintWriter writer = null;

        try {
            hierarchyLevel = JavaAllInstrumentationTest.startTestAllJava(instrumenter,
                                                        builder).getCode();
            FileOutputStream fileOutputStream = new FileOutputStream(targetFile);
            OutputStreamWriter outputStreamWriter = new OutputStreamWriter(fileOutputStream, Charset.forName("UTF-8"));
            BufferedWriter bufferedWriter = new BufferedWriter(outputStreamWriter);
            writer = new PrintWriter(bufferedWriter, false);

            HierarchyLevelDumper visitorHL = new HierarchyLevelDumper(writer);
            visitorHL.visit(new HierarchyLevelLocateable(hierarchyLevel));
        } catch (Exception e) {
            UtilsForTestingJava.handleException(e);
        } finally {
            if (writer != null) {
                writer.flush();
                writer.close();
            }
        }
    }

    public static class HierarchyLevelDumper {

        private final PrintWriter writer;
        private final StringBuilder prefix;

        public HierarchyLevelDumper(PrintWriter writer) {
            this.writer = writer;
            this.prefix = new StringBuilder();
        }

        public void visit(Locateable thisLocateable) {
            SortedSet<Locateable> children = thisLocateable.getChildren();

            this.writer.printf("%s>%s>%n", this.prefix, thisLocateable.getRepresentationString());

            Iterator<Location> locationIterator = thisLocateable.getLocations().iterator();
            Location thisLocation = null; 
            if (locationIterator.hasNext()) {
                thisLocation = locationIterator.next();
                this.writer.printf(" %s>%d>%n", this.prefix, new Integer(thisLocation.getStartOffset()));
            }

            for (Locateable thisChild : children) {
                int start = getFirstStartOffset(thisChild);
                int end = getLastEndOffset(thisChild);
                if (start == -1 && end == -1) {
                    visitChild(thisChild);
                } else if (start >= 0 && end >= start) {
                    while (thisLocation != null && thisLocation.getEndOffset() <= start) {
                        this.writer.printf(" %s<%d<%n", this.prefix, new Integer(thisLocation.getEndOffset()));
                        if (locationIterator.hasNext()) {
                            thisLocation = locationIterator.next();
                            this.writer.printf(" %s>%d>%n", this.prefix, new Integer(thisLocation.getStartOffset()));
                        } else {
                            thisLocation = null;
                        }
                    }
                    visitChild(thisChild);
                } else {
                    throw new IllegalStateException(thisChild.getRepresentationString() + "start < 0 || end < start");
                }
            }

            while (thisLocation != null) {
                this.writer.printf(" %s<%d<%n", this.prefix, new Integer(thisLocation.getEndOffset()));
                if (locationIterator.hasNext()) {
                    thisLocation = locationIterator.next();
                    this.writer.printf(" %s>%d>%n", this.prefix, new Integer(thisLocation.getStartOffset()));
                } else {
                    thisLocation = null;
                }
            }

            this.writer.printf("%s<%s<%n", this.prefix, thisLocateable.getRepresentationString());
        }

        private void visitChild(Locateable locateable) {
            this.prefix.append("  ");
            visit(locateable);
            this.prefix.setLength(this.prefix.length() - 2);
        }
    }

    private static int getFirstStartOffset(Locateable locateable) {
        List<Location> locationList = locateable.getLocations();
        if (locationList.isEmpty()) {
            return -1;
        }
        return locationList.get(0).getStartOffset();
    }

    private static int getLastEndOffset(Locateable locateable) {
        List<Location> locationList = locateable.getLocations();
        if (locationList.isEmpty()) {
            return -1;
        }
        return locationList.get(locationList.size() - 1).getEndOffset();
    }

    private static interface Locateable {
        public String getRepresentationString();
        public List<Location> getLocations();
        public SortedSet<Locateable> getChildren();
    }

    private static class HierarchyLevelLocateable implements Locateable {
        private final HierarchyLevel delegate;

        HierarchyLevelLocateable(HierarchyLevel delegate) {
            super();
            this.delegate = delegate;
        }

        public String getRepresentationString() {
            return this.delegate.getName();
        }

        public List<Location> getLocations() {
            return this.delegate.getLocation().getLocations();
        }

        public SortedSet<Locateable> getChildren() {
            SortedSet<Locateable> set = new TreeSet<Locateable>(LocateableComparator.INSTANCE);

            for (HierarchyLevel thisChild : this.delegate.getChildren()) {
                set.add(LocateableFactory.toLocateable(thisChild));
            }

            for (StatementSequence thisStatementSequence : this.delegate.getSequences()) {
                for (Statement thisStatement : thisStatementSequence.getStatements()) {
                    set.add(LocateableFactory.toLocateable(thisStatement));
                }
            }

            return set;
        }
    }

    private static class StatementLocateable implements Locateable {
        private final Statement delegate;

        StatementLocateable(Statement delegate) {
            super();
            this.delegate = delegate;
        }

        public String getRepresentationString() {
            return this.delegate.getClass().getCanonicalName();
        }

        public List<Location> getLocations() {
            return this.delegate.getLocation().getLocations();
        }

        public SortedSet<Locateable> getChildren() {
            SortedSet<Locateable> set = new TreeSet<Locateable>(LocateableComparator.INSTANCE);
            
            for (RootTerm thisRootTerm : this.delegate.getTerms()) {
                set.add(LocateableFactory.toLocateable(thisRootTerm));
            }

            return set;
        }
    }

    private static class LoopingStatementLocateable extends StatementLocateable {
        private final LoopingStatement delegate;

        LoopingStatementLocateable(LoopingStatement delegate) {
            super(delegate);
            this.delegate = delegate;
        }

        public SortedSet<Locateable> getChildren() {
            SortedSet<Locateable> set = super.getChildren();

            for (Statement thisStatement : this.delegate.getBody().getStatements()) {
                set.add(LocateableFactory.toLocateable(thisStatement));
            }

            return set;
        }
    }

    private static class ConditionalStatementLocateable extends StatementLocateable {
        private final ConditionalStatement delegate;
        
        ConditionalStatementLocateable(ConditionalStatement delegate) {
            super(delegate);
            this.delegate = delegate;
        }

        public SortedSet<Locateable> getChildren() {
            SortedSet<Locateable> set = super.getChildren();

            for (Branch thisBranch : this.delegate.getBranches()) {
                for (Statement thisStatement : thisBranch.getSequence().getStatements()) {
                    set.add(LocateableFactory.toLocateable(thisStatement));
                }
            }

            return set;
        }
    }

    private static class RootTermLocateable implements Locateable {
        private final RootTerm delegate;

        RootTermLocateable(RootTerm delegate) {
            super();
            this.delegate = delegate;
        }

        public String getRepresentationString() {
            return this.delegate.getClass().getCanonicalName();
        }

        public List<Location> getLocations() {
            return Collections.<Location>emptyList();
        }

        public SortedSet<Locateable> getChildren() {
            final SortedSet<Locateable> set = new TreeSet<Locateable>(LocateableComparator.INSTANCE);

            this.delegate.accept(null, null, new BooleanTerm.Visitor() {

                public void visit(BasicBooleanTerm term) {
                    set.add(LocateableFactory.toLocateable(term));
                }

                public void visit(OperatorTerm term) {
                    set.add(LocateableFactory.toLocateable(term));
                }
                
            }, null);

            return set;
        }
    }

    private static class BooleanTermLocateable implements Locateable {
        private static final SortedSet<Locateable> EMPTY_SET =
            new TreeSet<Locateable>(LocateableComparator.INSTANCE);

        private final BooleanTerm delegate;

        BooleanTermLocateable(BooleanTerm delegate) {
            super();
            this.delegate = delegate;
        }

        public String getRepresentationString() {
            if (this.delegate.getLocation().getLocations().isEmpty()) {
                return this.delegate.getClass().getCanonicalName();
            }

            return locationToString(this.delegate.getLocation());
        }

        public List<Location> getLocations() {
            return this.delegate.getLocation().getLocations();
        }

        public SortedSet<Locateable> getChildren() {
            return EMPTY_SET;
        }
    }
    
    private static class OperatorTermLocateable extends BooleanTermLocateable {
        private final OperatorTerm delegate;

        public OperatorTermLocateable(OperatorTerm delegate) {
            super(delegate);
            this.delegate = delegate;
        }

        @Override
        public String getRepresentationString() {
            return this.delegate.getOperator().getName();
        }
    }

    private static class LocateableFactory {
        public static Locateable toLocateable(Object o) {
            if (o instanceof HierarchyLevel) {
                return new HierarchyLevelLocateable((HierarchyLevel) o);
            }
            if (o instanceof ConditionalStatement) {
                return new ConditionalStatementLocateable((ConditionalStatement) o);
            }
            if (o instanceof LoopingStatement) {
                return new LoopingStatementLocateable((LoopingStatement) o);
            }
            if (o instanceof Statement) {
                return new StatementLocateable((Statement) o);
            }
            if (o instanceof RootTerm) {
                return new RootTermLocateable((RootTerm) o);
            }
            if (o instanceof OperatorTerm) {
                return new OperatorTermLocateable((OperatorTerm) o);
            }
            if (o instanceof BooleanTerm) {
                return new BooleanTermLocateable((BooleanTerm) o);
            }
            throw new IllegalArgumentException(o.getClass().getName());
        }
    }

    private static class LocateableComparator implements Comparator<Locateable> {
        public static final Comparator<Locateable> INSTANCE = new LocateableComparator();

        public int compare(Locateable o1, Locateable o2) {
            int returnValue;

            returnValue = getFirstStartOffset(o1) - getFirstStartOffset(o2);

            if (returnValue == 0) {
                returnValue = o1.getRepresentationString().compareTo(o2.getRepresentationString());
            }
            if (returnValue == 0) {
                returnValue = locationToString(o1.getLocations()).compareTo(locationToString(o2.getLocations()));
            }
            if (returnValue == 0) {
                returnValue = o1.getLocations().toString().compareTo(o2.getLocations().toString());
            }
            return returnValue;
        }
    }
}
