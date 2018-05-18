///////////////////////////////////////////////////////////////////////////////
//
// $Id: DefaultLoopManipulator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 17:31:25
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.manipulators;

import java.io.IOException;

import org.gbt2.instrumentation.java15.syntaxtree.DoStatement;
import org.gbt2.instrumentation.java15.syntaxtree.ForStatement;
import org.gbt2.instrumentation.java15.syntaxtree.WhileStatement;

import static org.gbt2.instrumentation.java15.visitor.TreeDumperWithException.LINE_SEPERATOR;

/**
 * @author Christoph Müller
 */
public class DefaultLoopManipulator extends AbstractDefaultManipulator
        implements LoopManipulator {

    private static final String TRY_KEY_WORD = "try";

    private static final String FINALLY_KEY_WORD = "finally";

    /**
     * Always true.
     * 
     * @return true;
     */
    public boolean requiresBlockExpansionsForBranches() {
        return true;
    }

    /**
     * Always true.
     * 
     * @return true;
     */
    public boolean requiresBlockExpansionsForLoops() {
        return true;
    }

    public void manipulateBefore(ForStatement n, String primaryLoopID)
            throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write(TRY_KEY_WORD);
        super.getWriter().write(" {");
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("  ");
        this.getCounterIDManager().writeLoopHelperSetZero(primaryLoopID);
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("  ");
    }

    public void manipulateInner(ForStatement n, String primaryLoopID)
            throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        this.getCounterIDManager().writeLoopHelperIncrementing(primaryLoopID);
    }

    public void manipulateAfter(ForStatement n, String primaryLoopID)
            throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("} ");
        super.getWriter().write(FINALLY_KEY_WORD);
        super.getWriter().write(" {");
        super.getWriter().write(LINE_SEPERATOR);
        this.getCounterIDManager().writeHelperEvaluationForLoop(primaryLoopID,
                true);
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("}");
        super.getWriter().write(LINE_SEPERATOR);
    }

    public void manipulateBefore(WhileStatement n, String primaryLoopID)
            throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write(TRY_KEY_WORD);
        super.getWriter().write(" {");
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("  ");
        this.getCounterIDManager().writeLoopHelperSetZero(primaryLoopID);
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("  ");
    }

    public void manipulateInner(WhileStatement n, String primaryLoopID)
            throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        this.getCounterIDManager().writeLoopHelperIncrementing(primaryLoopID);
    }

    public void manipulateAfter(WhileStatement n, String primaryLoopID)
            throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("} ");
        super.getWriter().write(FINALLY_KEY_WORD);
        super.getWriter().write(" {");
        super.getWriter().write(LINE_SEPERATOR);
        this.getCounterIDManager().writeHelperEvaluationForLoop(primaryLoopID,
                true);
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("}");
        super.getWriter().write(LINE_SEPERATOR);
    }

    public void manipulateBefore(DoStatement n, String primaryLoopID)
            throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write(TRY_KEY_WORD);
        super.getWriter().write(" {");
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("  ");
        this.getCounterIDManager().writeLoopHelperSetZero(primaryLoopID);
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("  ");
    }

    public void manipulateInner(DoStatement n, String primaryLoopID)
            throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        this.getCounterIDManager().writeLoopHelperIncrementing(primaryLoopID);
    }

    public void manipulateAfter(DoStatement n, String primaryLoopID)
            throws IOException {
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("} ");
        super.getWriter().write(FINALLY_KEY_WORD);
        super.getWriter().write(" {");
        super.getWriter().write(LINE_SEPERATOR);
        this.getCounterIDManager().writeHelperEvaluationForLoop(primaryLoopID,
                false);
        super.getWriter().write(LINE_SEPERATOR);
        super.getWriter().write("}");
        super.getWriter().write(LINE_SEPERATOR);
    }
}
