///////////////////////////////////////////////////////////////////////////////
//
// $Id: CoverageResultLog.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 22.03.2007 22:47:35
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.measurement;


/**
 * This is an interface where coverage results can be written to. It extends the
 * interface {@link CoverageCounterLog}<br>
 * <br>
 * For example, this interface can be implemented as a coverage log file. See
 * {@link CoverageResultLogFile}<br>
 * <br>
 * This log accepts the start and the end of a named test tase, the start of a
 * section and long counters. All accesses to this logfile must be processed
 * after the {@link startLog} and befor {@link #closeLog()}.
 * 
 * @author Christoph Müller
 */
public interface CoverageResultLog extends CoverageCounterLog {
    /**
     * This is the name of static method all childs will have to get the single
     * instance.
     */
    public static final String GET_INSTANCE_METHOD_NAME = "getInstance";
    
    /**
     * Starts the logging.
     */
    public void startLog();

    /**
     * Starts the logging. Additionally a header comment can be passes.
     * 
     * @param comment
     *            A possible header comment.
     */
    public void startLog(String comment);

    /**
     * Notifies the start of a test case.
     * 
     * @param testCaseName
     *            The name of the test case.
     * @param timestamp 
     *            A long representing a timestamp of the start of the test case.
     */
    public void startTestCase(String testCaseName, long timestamp);

    /**
     * Notifies the start of a test case.
     * 
     * @param testCaseName
     *            The name of the test case.
     * @param timestamp 
     *            A long representing a timestamp of the start of the test case.
     * @param comment
     *            An comment for the test case.
     */
    public void startTestCase(String testCaseName, long timestamp,
            String comment);

    /**
     * Notifies the end of the test case.
     * 
     * @param testCaseName
     *            The name of the test case.
     * @param timestamp 
     *            A long representing a timestamp of the end of the test case.
     */
    public void endTestCase(String testCaseName, long timestamp);

    /**
     * Puts out a comment line.
     * 
     * @param comment
     *            A possible comment.
     */
    public void lineComment(String comment);

    /**
     * Closes the logging.
     */
    public void closeLog();

    /**
     * Closes the logging. Additionally a footer comment can be passes.
     * 
     * @param comment
     *            A possible footer comment.
     */
    public void closeLog(String comment);
}
