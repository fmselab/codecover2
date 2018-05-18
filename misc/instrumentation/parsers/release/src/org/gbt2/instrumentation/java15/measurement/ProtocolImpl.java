///////////////////////////////////////////////////////////////////////////////
//
// $Id: ProtocolImpl.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 02.04.2007 16:49:03
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.measurement;

import java.util.Set;
import java.util.TreeSet;

/**
 * This class provides methods to protocol the coverage measurement of the SUT.<br>
 * <br>
 * This class is used for coverage measurement&mdash;in constrast to
 * {@link Protocol} which is just used as an interface for testers. To capture
 * all start and end test case notifications the instance
 * {@link Protocol#instance} is overwritten by an instance of this class. By the
 * way all calls of the start and end testcase methods of {@link Protocol} are
 * delegated to this single object. For this reasong the start and end methodsof
 * this class are <b>not</b> directly called.<br>
 * It is implemented as an Singleton: {@link #getInstance()} is used to get the
 * instance.<br>
 * All <b>instrumented</b> classes add their inner class
 * {@link CounterContainer} to this class by using the method
 * {@link #addObservedContainer(CounterContainer)}.<br>
 * If someone calls {@link #startTestCase(String)} then all counters are reset
 * using {@link CounterContainer#reset()}. From this time on all counters are
 * incrementing till the {@link #endTestCase(String)} method is called. Then all
 * {@link CounterContainer}s are ordered to
 * {@link CounterContainer#serializeAndReset(CoverageCounterLog)}.<br>
 * If no {@link #startTestCase(String)} is called, all counters are stored at
 * the end of the running programm.<br>
 * To get to know of normal and abnormal shutdowns of the Java VM, a
 * ShutdownHook is used({@link Runtime#addShutdownHook(Thread)}).<br>
 * All the methods of this class are made thread safe using a {@link #LOCK}
 * object and synchronized blocks.
 * 
 * @author Christoph Müller
 * @see CounterContainer
 * @see Protocol
 * 
 */
public class ProtocolImpl extends Protocol implements Runnable {
    /** This is the name of {@link #addObservedContainer(CounterContainer)}. */
    public static final String ADD_OBSERVED_CONTAINER_METHOD_NAME = "addObservedContainer";

    /** This is the name of {@link #getInstance(CoverageResultLog)}. */
    public static final String GET_INSTANCE_METHOD_NAME = "getInstance";

    /** used for synchronized statements */
    private static final Object LOCK = new Object();

    /**
     * The name of the test case, if there occur no
     * {@link #startTestCaseImpl(String)} calls and only one single test case is
     * created.
     * 
     * @see #UNNAMED_TESTCASE_COMMENT
     */
    private static final String UNNAMED_TESTCASE_NAME = "UNNAMED TESTCASE";

    /**
     * The comment of the test case, if there occur no
     * {@link #startTestCaseImpl(String)} calls and only one single test case is
     * created.
     * 
     * @see #UNNAMED_TESTCASE_NAME
     */
    private static final String UNNAMED_TESTCASE_COMMENT = "This is test case "
            + "containing all coverage results cause no startTestCase methods "
            + "where captured.";

    private static final String COMMENT_START_SESSION = "Start Session";

    private static final String COMMENT_END_SESSION = "End Session";

    /** The single instance */
    private static ProtocolImpl instanceImpl = null;
    
    /**
     * The {@link CoverageResultLog} all results are written to.
     */
    private final CoverageResultLog coverageResultLog;
    
    /**
     * A set with observed {@link CounterContainer}s.
     */
    private final Set<CounterContainer> observedCounterContainers;

    /**
     * The current state of this object
     * 
     * @see {@link ProtocolState}
     */
    private ProtocolState state;

    /**
     * This is the {@link MeasurementTimer} that is used to write all the
     * timestamps to the {@link CoverageResultLog}.
     */
    private MeasurementTimer timer;

    /**
     * The name of the current test case.
     */
    private String currentTestCaseName;
    
    /**
     * Gets the single instance.<br>
     * <br>
     * If the instance is not created yet, it is done now. By the way some
     * initializations are made:
     * <ul>
     * <li>replace {@link Protocol#instance} by {@link #instanceImpl}</li>
     * <li>use {@link Runtime#addShutdownHook(Thread)}</li>
     * </ul>
     * <br>
     * If the {@link ProtocolImpl} is already initialized, the
     * {@link CoverageResultLog} is ignored.
     * 
     * @param coverageResultLog
     *            The {@link CoverageResultLog} all coverage data is written to.
     * 
     * @return The single instance.
     */
    public static ProtocolImpl getInstance(CoverageResultLog coverageResultLog) {
        synchronized (LOCK) {
            if (instanceImpl == null) {
              instanceImpl = new ProtocolImpl(coverageResultLog);
              Protocol.instance = instanceImpl;
              Runtime.getRuntime().addShutdownHook(instanceImpl.new ShutdownHook());
            }
        }

        return instanceImpl;
    }

    /**
     * Private constructor.
     * 
     * @param coverageResultLog
     *            The {@link CoverageResultLog} all coverage data is written to.
     */
    private ProtocolImpl(CoverageResultLog coverageResultLog) {
        this.coverageResultLog = coverageResultLog;
        this.observedCounterContainers = new TreeSet<CounterContainer>();
        this.state = ProtocolState.NO_TEST_CASE_STARTED_YET;
        this.currentTestCaseName = null;
        this.timer = new MeasurementTimer();
        
        this.coverageResultLog.startLog();
        this.coverageResultLog.lineComment("///////////////////////////////");
        this.coverageResultLog.lineComment("");
        this.coverageResultLog.lineComment(COMMENT_START_SESSION);
        this.coverageResultLog.lineComment("");
        this.coverageResultLog.lineComment(this.timer.getOverallStart());
        this.coverageResultLog.lineComment("");
        this.coverageResultLog.lineComment("///////////////////////////////");
    }

    /**
     * Adds a {@link CounterContainer} to be observed by this protocol.
     * 
     * @param observedCounterContainer The observed container.
     */
    public synchronized void addObservedContainer(
            CounterContainer observedCounterContainer) {
        this.observedCounterContainers.add(observedCounterContainer);
    }

    @Override
    protected synchronized void startTestCaseImpl(String name, String comment) {
        switch (this.state) {
        case TEST_CASE_STARTED:
            endTestCaseImpl(); //includes tellAlltoSerializeAndReset()
            break;
        case NO_TEST_CASE_STARTED_YET:
            tellAlltoReset();
            break;
        case TEST_CASE_ENDED:
            tellAlltoReset();
            break;
        case FINISHED:
            // something got wrong - reject
            return;
        }
        
        this.timer.setPhaseStart();
        this.coverageResultLog.lineComment(this.timer.getPhaseStart());
        
        this.currentTestCaseName = name;
        // write start test case to the coverage result log
        if (comment != null) {
            this.coverageResultLog.startTestCase(this.currentTestCaseName,
                    this.timer.getPhaseStartLong(), comment);
        } else {
            this.coverageResultLog.startTestCase(this.currentTestCaseName,
                    this.timer.getPhaseStartLong());           
        }

        this.state = ProtocolState.TEST_CASE_STARTED;
    }

    @Override
    protected synchronized void startTestCaseImpl(String name) {
        startTestCaseImpl(name, null);
    }

    @Override
    protected synchronized void endTestCaseImpl(String name) {
        switch (this.state) {
        case TEST_CASE_STARTED:
            if (this.currentTestCaseName != name) {
                // something got wrong - reject
                return;
            }
            break;
        case NO_TEST_CASE_STARTED_YET:
            // something got wrong - reject
            return;
        case TEST_CASE_ENDED:
            // something got wrong - reject
            return;
        case FINISHED:
            // something got wrong - reject
            return;
        }
        
        // assert: a test case with the given name has been started
        tellAlltoSerializeAndReset();

        this.timer.setPhaseEnd();
        this.coverageResultLog.lineComment(this.timer.getPhaseEnd());
        this.coverageResultLog.lineComment(this.timer.getPhaseDuration());
        this.coverageResultLog.endTestCase(this.currentTestCaseName,
                this.timer.getPhaseEndLong());

        this.currentTestCaseName = null;
        this.state = ProtocolState.TEST_CASE_ENDED;
    }

    @Override
    protected synchronized void endTestCaseImpl() {
        switch (this.state) {
        case TEST_CASE_STARTED:
            endTestCaseImpl(this.currentTestCaseName);
            break;
        case NO_TEST_CASE_STARTED_YET:
            // something got wrong - reject
            return;
        case TEST_CASE_ENDED:
            // something got wrong - reject
            return;
        case FINISHED:
            // something got wrong - reject
            return;
        }
    }

    private synchronized void finish() {
        switch (this.state) {
        case TEST_CASE_STARTED:
            // end the last open test case
            endTestCaseImpl(this.currentTestCaseName);
            break;
        case NO_TEST_CASE_STARTED_YET:
            // we have to generate a test case for everything
            this.coverageResultLog.lineComment(this.timer.getOverallStart());
            this.coverageResultLog.startTestCase(UNNAMED_TESTCASE_NAME,
                    this.timer.getOverallStartLong(), UNNAMED_TESTCASE_COMMENT);

            tellAlltoSerializeAndReset();

            this.timer.setOverallEnd();
            this.coverageResultLog.lineComment(this.timer.getOverallEnd());
            this.coverageResultLog.lineComment(this.timer.getOverallDuration());
            this.coverageResultLog.endTestCase(UNNAMED_TESTCASE_NAME,
                    this.timer.getOverallEndLong());
            break;
        case TEST_CASE_ENDED:
            break;
        case FINISHED:
            // something got wrong - reject
            return;
        }

        this.timer.setOverallEnd();
        this.coverageResultLog.lineComment("///////////////////////////////");
        this.coverageResultLog.lineComment("");
        this.coverageResultLog.lineComment(COMMENT_END_SESSION);
        this.coverageResultLog.lineComment("");
        this.coverageResultLog.lineComment(this.timer.getOverallEnd());
        this.coverageResultLog.lineComment(this.timer.getOverallDuration());
        this.coverageResultLog.lineComment("");
        this.coverageResultLog.lineComment("///////////////////////////////");
        this.coverageResultLog.closeLog();
        this.state = ProtocolState.FINISHED;
    }

    /**
     * Calls {@link CounterContainer#reset()} for all
     * {@link #observedCounterContainers}.
     */
    private synchronized void tellAlltoReset() {
        for (CounterContainer thisCC : this.observedCounterContainers) {
            thisCC.reset();
        }
    }
    
    /**
     * Calls {@link CounterContainer#serializeAndReset(CoverageCounterLog)} for
     * all {@link #observedCounterContainers}.
     */
    private synchronized void tellAlltoSerializeAndReset() {
        for (CounterContainer thisCC : this.observedCounterContainers) {
            thisCC.serializeAndReset(this.coverageResultLog);
        }
    }

    /**
     * Cause of {@link Runnable} interface.<br>
     * <br>
     * Is used by the {@link ShutdownHook} to notfiy the shutdown of the Java
     * VM.
     */
    public void run() {
        finish();
    }
   
    private enum ProtocolState {
        /**
         * A test case is explicitly started using method
         * {@link Protocol#startTestCase(String)}.
         */
        TEST_CASE_STARTED,

        /** A Test case is ended using {@link Protocol#endTestCase()}. */
        TEST_CASE_ENDED,

        /** No start test case method has been called yet. */
        NO_TEST_CASE_STARTED_YET,

        /**
         * The {@link Protocol#finish()} method has been called - no starts or
         * ends can be processed
         */
        FINISHED
    }

    /**
     * Is used as a Shutdownhook by the
     * {@link ProtocolImpl#getInstance(CoverageResultLog)}. It's behavior is
     * described in {@link Runtime#addShutdownHook(Thread)}.
     * 
     * @author Christoph Müller
     */
    private class ShutdownHook extends Thread {
        private static final String THREAD_NAME = "Gbt2 Shutdown Hook";

        /**
         * Constructs a new Shutdown Hook.
         */
        public ShutdownHook() {
            super(ProtocolImpl.this, THREAD_NAME);
        }
    }
}
