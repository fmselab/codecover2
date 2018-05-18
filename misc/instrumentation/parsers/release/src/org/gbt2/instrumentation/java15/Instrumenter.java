///////////////////////////////////////////////////////////////////////////////
//
// $Id: Instrumenter.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 17:40:48
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

import org.gbt2.instrumentation.ParseException;
import org.gbt2.instrumentation.criteria.BranchCoverage;
import org.gbt2.instrumentation.criteria.ConditionCoverage;
import org.gbt2.instrumentation.criteria.LoopCoverage;
import org.gbt2.instrumentation.criteria.StatementCoverage;
import org.gbt2.instrumentation.java15.manipulators.BranchManipulator;
import org.gbt2.instrumentation.java15.manipulators.ConditionManipulator;
import org.gbt2.instrumentation.java15.manipulators.DefaultBranchManipulator;
import org.gbt2.instrumentation.java15.manipulators.DefaultConditionManipulator;
import org.gbt2.instrumentation.java15.manipulators.DefaultLoopManipulator;
import org.gbt2.instrumentation.java15.manipulators.DefaultStatementManipulator;
import org.gbt2.instrumentation.java15.manipulators.LoopManipulator;
import org.gbt2.instrumentation.java15.manipulators.StatementManipulator;
import org.gbt2.instrumentation.java15.measurement.CoverageResultLog;
import org.gbt2.instrumentation.java15.measurement.CoverageResultLogFile;
import org.gbt2.instrumentation.java15.parser.JavaParser;
import org.gbt2.instrumentation.java15.syntaxtree.CompilationUnit;
import org.gbt2.instrumentation.java15.visitor.InstrumentationVisitorWithException;

/**
 * This is a Instrumenter for the Java programming language.
 * 
 * @author Christoph Müller, Stefan Franke
 */
public class Instrumenter extends org.gbt2.instrumentation.Instrumenter {
    /**
     * A constructor of a java instrumenter.
     */
    public Instrumenter() {
        super();
    }

    /**
     * Reads the source file from the source instruments into the target.
     * 
     * @param source
     *            A reader of the source file.
     * @param target
     *            A writer of the instrumented source file.
     * @throws ParseException
     *             If an exceptions occurs during the parsing:
     *             {@link JavaParser#CompilationUnit()}.
     * @throws IOException
     *             On the occurance of write exceptions.
     * 
     * TODO coveragResultLog must be abstracted by using it as an instrumenter
     * decision.
     */
    @Override
    protected void instrumentThis(Reader source, Writer target)
            throws ParseException, IOException {
        JavaParser javaParser;
        CompilationUnit compilationUnit;
        InstrumentationVisitorWithException instrumentationVisitor;
        StatementManipulator statementManipulator;
        BranchManipulator branchManipulator;
        ConditionManipulator conditionManipulator;
        LoopManipulator loopManipulator;
        Class<? extends CoverageResultLog> coveragResultLog;

        javaParser = new JavaParser(source);
        compilationUnit = javaParser.CompilationUnit();
        // assert: there was no ParseException

        coveragResultLog = CoverageResultLogFile.class;
        instrumentationVisitor = new InstrumentationVisitorWithException(target,
                coveragResultLog);

        if (super.isCriterionSet(StatementCoverage.getInstance())) {
            statementManipulator = new DefaultStatementManipulator();
            instrumentationVisitor.setStatementManipulator(statementManipulator);
        }

        if (super.isCriterionSet(BranchCoverage.getInstance())) {
            branchManipulator = new DefaultBranchManipulator();
            instrumentationVisitor.setBranchManipulator(branchManipulator);
        }

        if (super.isCriterionSet(ConditionCoverage.getInstance())) {
            conditionManipulator = new DefaultConditionManipulator();
            instrumentationVisitor.setConditionManipulator(conditionManipulator);
        }

        if (super.isCriterionSet(LoopCoverage.getInstance())) {
            loopManipulator = new DefaultLoopManipulator();
            instrumentationVisitor.setLoopManipulator(loopManipulator);
        }

        instrumentationVisitor.visit(compilationUnit);
    }
}
