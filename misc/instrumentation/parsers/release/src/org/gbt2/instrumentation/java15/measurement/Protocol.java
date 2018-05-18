///////////////////////////////////////////////////////////////////////////////
//
// $Id: Protocol.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 02.04.2007 22:29:24
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.measurement;

/**
 * This is the protocol class for starting and ending test cases for coverage
 * measurement.<br>
 * <br>
 * For starting you can use either {@link #startTestCaseImpl(String, String)} or
 * {@link #startTestCaseImpl(String)}. For ending a test case you can use
 * either {@link #endTestCaseImpl(String)} or {@link #endTestCaseImpl()}. If no
 * starting or ending test case is recognized, all the coverage data is
 * measured&mdash;from the beginning till the end of the execution.<br>
 * If this class is used just for compiling the SUT without compiler errors and
 * simple execution, this class will do exactly nothing. If the SUT is
 * instrumented another protocoll class is used in addition:
 * {@link ProtocolImpl}. That class replaces the single {@link #instance} of
 * this class by an instance of {@link ProtocolImpl}. So the
 * {@link ProtocolImpl} instance is informed of every start end ending of test
 * cases.
 * 
 * @author Christoph Müller
 * @see ProtocolImpl
 */
public class Protocol {
    static Protocol instance = new Protocol();

    /**
     * The coverage measurement is booked on the test case with the given name.<br>
     * <br>
     * If another test case has been started befor, but not ended, it is ended
     * implicitly.
     * 
     * @param name
     *            The name of the test case.
     */
    public static void startTestCase(String name) {
        instance.startTestCaseImpl(name);
    }

    /**
     * The coverage measurement is booked on the test case with the given name.<br>
     * <br>
     * If another test case has been started befor, but not ended, it is ended
     * implicitly.
     * 
     * @param name
     *            The name of the test case.
     * @param comment
     *            A comment for the test case.
     */
    public static void startTestCase(String name, String comment) {
        instance.startTestCaseImpl(name, comment);
    }

    /**
     * The coverage measurement for the current test case with the given name is
     * finished.<br>
     * <br>
     * All counters are stored persistently. If the current test case hasn't got
     * the given name, this method call is ignored.
     * 
     * @param name
     *            The name of the test case.
     */
    public static void endTestCase(String name) {
        instance.endTestCaseImpl(name);
    }

    /**
     * The coverage measurement for the current test case is finished.<br>
     * <br>
     * All counters are stored persistently.
     */
    public static void endTestCase() {
        instance.endTestCaseImpl();
    }
    
    /**
     * The coverage measurement is booked on the test case with the given name.<br>
     * <br>
     * If another test case has been started befor, but not ended, it is ended
     * implicitly.
     * 
     * @param name
     *            The name of the test case.
     * @param comment
     *            A comment for the test case.
     */
    @SuppressWarnings("unused")
    protected void startTestCaseImpl(String name, String comment) {
        // do nothing
    }

    /**
     * The coverage measurement is booked on the test case with the given name.<br>
     * <br>
     * If another test case has been started befor, but not ended, it is ended
     * implicitly.
     * 
     * @param name
     *            The name of the test case.
     */
    @SuppressWarnings("unused")
    protected void startTestCaseImpl(String name) {
        // do nothing
    }

    /**
     * The coverage measurement for the current test case with the given name is
     * finished.<br>
     * <br>
     * All counters are stored persistently. If the current test case hasn't got
     * the given name, this method call is ignored.
     * 
     * @param name
     *            The name of the test case.
     */
    @SuppressWarnings("unused")
    protected void endTestCaseImpl(String name) {
        // do nothing
    }

    /**
     * The coverage measurement for the current test case is finished.<br>
     * <br>
     * All counters are stored persistently.
     */
    protected void endTestCaseImpl() {
        // do nothing
    }
}
