///////////////////////////////////////////////////////////////////////////////
//
// $Id: Instrumenter.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 22:30:53
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.cobol85;

import java.io.Reader;
import java.io.Writer;

import org.gbt2.instrumentation.cobol85.manipulators.DefaultBranchManipulator;
import org.gbt2.instrumentation.cobol85.manipulators.DefaultConditionManipulator;
import org.gbt2.instrumentation.cobol85.manipulators.DefaultStaticPartManipulator;
import org.gbt2.instrumentation.cobol85.manipulators.DefaultLoopManipulator;
import org.gbt2.instrumentation.cobol85.manipulators.DefaultStatementManipulator;
import org.gbt2.instrumentation.cobol85.parser.CobolParser;
import org.gbt2.instrumentation.cobol85.visitor.InstrumentationVisitor;
import org.gbt2.instrumentation.ParseException;
import org.gbt2.instrumentation.cobol85.syntaxtree.CompilationUnit;
import org.gbt2.instrumentation.criteria.BranchCoverage;
import org.gbt2.instrumentation.criteria.ConditionCoverage;
import org.gbt2.instrumentation.criteria.LoopCoverage;
import org.gbt2.instrumentation.criteria.StatementCoverage;

/**
 * This class is the main class of the COBOL instrumenter. It is derived from
 * the Instrumenter and override the instrumentThis method to start the
 * instrumentation visitor.
 * 
 * @author Stefan Franke
 * @version 1.0 - 28.03.2007
 * 
 */
public class Instrumenter extends org.gbt2.instrumentation.Instrumenter {

    CobolParser cobolParser;

    InstrumentationVisitor instrumentationVisitor;

    CounterProvider counterProvider;

    /**
     * Constructor
     */
    public Instrumenter() {
        super();
    }

    /**
     * Starts the parser, adds the chosen coverage criteria and starts
     * instrumentation.
     * 
     * @param source
     * @param target
     * @throws ParseException
     */
    @Override
    protected void instrumentThis(Reader source, Writer target)
            throws ParseException {
        this.cobolParser = new CobolParser(source);
        CompilationUnit compilationUnit = this.cobolParser.CompilationUnit();
        this.instrumentationVisitor = new InstrumentationVisitor(target);
        this.counterProvider = new CounterProvider();

        if (super.isCriterionSet(StatementCoverage.getInstance())) {
            this.instrumentationVisitor
                    .setStatementManipulator(new DefaultStatementManipulator(
                            this.counterProvider));
        }
        if (super.isCriterionSet(BranchCoverage.getInstance())) {
            this.instrumentationVisitor
                    .setBranchManipulator(new DefaultBranchManipulator(
                            this.counterProvider));
        }
        if (super.isCriterionSet(ConditionCoverage.getInstance())) {
            this.instrumentationVisitor
                    .setConditionManipulator(new DefaultConditionManipulator(
                            this.counterProvider));
        }
        if (super.isCriterionSet(LoopCoverage.getInstance())) {
            this.instrumentationVisitor
                    .setLoopManipulator(new DefaultLoopManipulator(
                            this.counterProvider));
        }
        if (!super.getCriteria().isEmpty()) {
            this.instrumentationVisitor
                    .setStaticPartManipulator(new DefaultStaticPartManipulator(
                            this.counterProvider));
        }
        this.instrumentationVisitor.visit(compilationUnit);
    }
}
