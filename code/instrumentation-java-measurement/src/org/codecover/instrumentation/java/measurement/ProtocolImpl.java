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

package org.codecover.instrumentation.java.measurement;

import java.lang.management.ManagementFactory;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import javax.management.MBeanServer;
import javax.management.ObjectName;

import org.codecover.instrumentation.java.measurement.jmx.LiveNotification;
import org.codecover.instrumentation.measurement.CoverageCounterLog;
import org.codecover.instrumentation.measurement.CoverageResultLog;

/**
 * This class provides methods to protocol the coverage measurement of the SUT.<br>
 * <br>
 * This class is used for coverage measurement&mdash;in contrast to
 * {@link Protocol}, which is just used as an interface for testers. To capture
 * all start and end test case notifications the instance
 * {@link Protocol#instance} is overwritten by an instance of this class. By the
 * way all calls of the start and end test case methods of {@link Protocol} are
 * delegated to this single object. For this reason the start and end methods of
 * this class are <b>not</b> directly called.<br>
 * It is implemented as an Singleton:
 * {@link #getInstance(CoverageResultLog, String)} is used to get the instance.<br>
 * All <b>instrumented</b> classes add their inner class
 * {@link CounterContainer} to this class by using the method
 * {@link #addObservedContainer(CounterContainer)}.<br>
 * If someone calls {@link #startTestCase(String)} then all counters are reset
 * using {@link CounterContainer#reset()}. From this time on all counters are
 * incrementing till the {@link #endTestCase(String)} method is called. Then all
 * {@link CounterContainer}s are ordered to
 * {@link CounterContainer#serializeAndReset(CoverageCounterLog)}.<br>
 * If no {@link #startTestCase(String)} is called, all counters are stored at
 * the end of the running program.<br>
 * To get to know of normal and abnormal shutdowns of the Java VM, a
 * ShutdownHook is used({@link Runtime#addShutdownHook(Thread)}).<br>
 * All the methods of this class are made thread safe using a {@link #LOCK}
 * object and synchronized blocks.<br>
 * A remote feature is added in the method {@link #initializeMBean()}. This
 * initializes an MBean {@link LiveNotification} to allow remote calls of the 
 * start and end test case methods.<br>
 * <b>Pay Attention</b> this source file can be used with Java 1.5 or higher.
 * There is a source file "ProtocolImpl.java14" too, which is just an adapted
 * copy of this file, which can be used with Java 1.4 or higher. The reason why
 * to do this lies in the extension of this class to register at an
 * {@link MBeanServer}, which is not compatible to Java 1.4.
 * 
 * @author Christoph Müller
 * @version 1.0_Java15 ($Id: ProtocolImpl.java 1 2007-12-12 17:37:26Z t-scheller $)
 * @see CounterContainer
 * @see Protocol
 */
public class ProtocolImpl extends Protocol implements Runnable {
    /** This is the name of {@link #addObservedContainer(CounterContainer)}. */
    public static final String ADD_OBSERVED_CONTAINER_METHOD_NAME = "addObservedContainer";

    /** This is the name of {@link #getInstance(CoverageResultLog, String)}. */
    public static final String GET_INSTANCE_METHOD_NAME = "getInstance";

    /** used for synchronized statements */
    private static final Object LOCK = new Object();
    
    /** No start test case method has been called yet. */
    private static final int NO_TEST_CASE_STARTED_YET = 0;

    /**
     * A test case is explicitly started using method
     * {@link Protocol#startTestCase(String)}.
     */
    private static final int TEST_CASE_STARTED = 100;

    /**
     * A test case is started by {@link Protocol#startJUnitTestCase(Class, String)}.
     */
    private static final int JUNIT_TEST_CASE_STARTED = 150;

    /** A Test case is ended using {@link Protocol#endTestCase()}. */
    private static final int TEST_CASE_ENDED = 200;

    /**
     * The {@link Protocol#finishImpl()} method has been called - no starts or
     * ends can be processed
     */
    private static final int FINISHED = 300;

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
    private static final String UNNAMED_TESTCASE_COMMENT = "This is a test case "
            + "containing all coverage results cause no startTestCase methods "
            + "where captured.";

    private static final String COMMENT_START_SESSION = "Start Session";

    private static final String COMMENT_END_SESSION = "End Session";
    
    private static final String MBEAN_OBJECT_NAME = "CodeCover:type=LiveNotificationMBean";

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
     * This is the {@link MeasurementTimer} that is used to write all the
     * timestamps to the {@link CoverageResultLog}.
     */
    private MeasurementTimer timer;

    /**
     * The name of the current test case.
     */
    private String currentTestCaseName;

    /**
     * The class of a JUnit test case. 
     */
    private String currentTestClass;

    /**
     * The UID of the TestSessionContainer. 
     */
    private String testSessionContainerUID;

    private int state;

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
     * @param testSessionContainerUID
     *            The UID of the TestSessionContainer.
     * 
     * @return The single instance.
     */
    public static ProtocolImpl getInstance(CoverageResultLog coverageResultLog,
            String testSessionContainerUID) {
        synchronized (LOCK) {
            if (instanceImpl == null) {
              instanceImpl = new ProtocolImpl(coverageResultLog, testSessionContainerUID);
              // restore the state of Protocol
              if (Protocol.instance.isFinished) {
                  instanceImpl.state = FINISHED;
              } else if (Protocol.instance.testCaseStartedClass != null) {
                  instanceImpl.startJUnitTestCaseImpl(Protocol.instance.testCaseStartedClass,
                          Protocol.instance.testCaseStartedName);
              } else if (Protocol.instance.testCaseStartedName != null) {
                  instanceImpl.startTestCaseImpl(Protocol.instance.testCaseStartedName,
                          Protocol.instance.testCaseStartedComment);
              }
              Runtime.getRuntime().addShutdownHook(instanceImpl.new ShutdownHook());

              // overwrite the Protocol.instance, which is informed of all
              // starts and ends
              Protocol.instance = instanceImpl;
            }
        }

        return instanceImpl;
    }
    
    


    /**
     * Private constructor.
     * 
     * @param coverageResultLog
     *            The {@link CoverageResultLog} all coverage data is written to.
     * @param testSessionContainerUID
     *            The UID of the TestSessionContainer.            
     */
    private ProtocolImpl(CoverageResultLog coverageResultLog,
            String testSessionContainerUID) {
        this.coverageResultLog = coverageResultLog;
        this.testSessionContainerUID = testSessionContainerUID;
        this.observedCounterContainers = new TreeSet<CounterContainer>();
        this.state = NO_TEST_CASE_STARTED_YET;
        this.currentTestCaseName = null;
        this.currentTestClass = null;
        this.timer = new MeasurementTimer();

        this.coverageResultLog.startLog();
        this.coverageResultLog.lineComment("///////////////////////////////");
        this.coverageResultLog.lineComment("");
        this.coverageResultLog.lineComment(COMMENT_START_SESSION);
        this.coverageResultLog.lineComment("");
        this.coverageResultLog.lineComment(this.timer.getOverallStart());
        this.coverageResultLog.lineComment("");
        this.coverageResultLog.lineComment("///////////////////////////////");
        
        initializeMBean();
    }

    /**
     * TODO comment
     */
    private void initializeMBean() {
        MBeanServer server = ManagementFactory.getPlatformMBeanServer();
        try {
            final ObjectName objectName = new ObjectName(MBEAN_OBJECT_NAME);
            if (!server.isRegistered(objectName)) {
                server.registerMBean(new LiveNotification(), objectName);
            }
        } catch (Exception e) {
            throw new Error(e);
        }
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
        case JUNIT_TEST_CASE_STARTED:
            // JUnit test cases have priority
            return;
        case NO_TEST_CASE_STARTED_YET:
            tellAlltoReset();
            break;
        case TEST_CASE_ENDED:
            tellAlltoReset();
            break;
        case FINISHED:
            // something went wrong - reject
            return;
        }

        this.timer.setPhaseStart();
        this.coverageResultLog.lineComment(this.timer.getPhaseStart());

        this.currentTestCaseName = name;
        // write start test case to the coverage result log
        if (comment != null) {
            this.coverageResultLog.startTestCase(this.testSessionContainerUID,
                    this.currentTestCaseName, this.timer.getPhaseStartLong(),
                    comment);
        } else {
            this.coverageResultLog.startTestCase(this.testSessionContainerUID,
                    this.currentTestCaseName, this.timer.getPhaseStartLong());           
        }

        this.state = TEST_CASE_STARTED;
    }

    @Override
    protected synchronized void startTestCaseImpl(String name) {
        startTestCaseImpl(name, null);
    }

    @Override
    protected synchronized void endTestCaseImpl(String name, String resultComment) {
        switch (this.state) {
        case TEST_CASE_STARTED:
            if (this.currentTestCaseName != name) {
                // something went wrong - reject
                return;
            }
            break;
        case JUNIT_TEST_CASE_STARTED:
            // something went wrong or the junit test case had priority
            return;
        case NO_TEST_CASE_STARTED_YET:
            // we did not get the start of the first test case -> start it now
            startTestCase(name);
        case TEST_CASE_ENDED:
            // something went wrong - reject
            return;
        case FINISHED:
            // something went wrong - reject
            return;
        }

        // assert: a test case with the given name has been started
        tellAlltoSerializeAndReset();

        this.timer.setPhaseEnd();
        this.coverageResultLog.lineComment(this.timer.getPhaseEnd());
        this.coverageResultLog.lineComment(this.timer.getPhaseDuration());
        this.coverageResultLog.endTestCase(this.currentTestCaseName,
                this.timer.getPhaseEndLong(), resultComment);

        this.currentTestCaseName = null;
        this.state = TEST_CASE_ENDED;
    }

    @Override
    protected synchronized void endTestCaseImpl() {
        switch (this.state) {
        case TEST_CASE_STARTED:
            endTestCaseImpl(this.currentTestCaseName, null);
            break;
        case JUNIT_TEST_CASE_STARTED:
            // something went wrong or the junit test case had priority
            return;
        case NO_TEST_CASE_STARTED_YET:
            // something went wrong - reject
            return;
        case TEST_CASE_ENDED:
            // something went wrong - reject
            return;
        case FINISHED:
            // something went wrong - reject
            return;
        }
    }

    @Override
    protected synchronized void startJUnitTestCaseImpl(String jUnitTestClassName,
            String testName) {
        switch (this.state) {
        case TEST_CASE_STARTED:
            // end the last open test case
            endTestCaseImpl(); //includes tellAlltoSerializeAndReset()
            break;
        case JUNIT_TEST_CASE_STARTED:
            if (this.currentTestClass == null) {
                // something went wrong
                tellAlltoReset();
                break;
            }
            // something went wrong, but end the last JUnit test case
            endJUnitTestCase(this.currentTestClass, testName, null);
            break;
        case NO_TEST_CASE_STARTED_YET:
            tellAlltoReset();
            break;
        case TEST_CASE_ENDED:
            tellAlltoReset();
            break;
        case FINISHED:
            // something went wrong - reject
            return;
        }

        this.timer.setPhaseStart();
        this.coverageResultLog.lineComment(this.timer.getPhaseStart());

        this.currentTestClass = jUnitTestClassName;
        this.currentTestCaseName = testName;
        String fullTestCaseName = getFullTestCaseName(this.currentTestClass,
                this.currentTestCaseName);

        // write start test case to the coverage result log
        this.coverageResultLog.startTestCase(this.testSessionContainerUID,
                fullTestCaseName, this.timer.getPhaseStartLong());           

        this.state = JUNIT_TEST_CASE_STARTED;
    }

    @Override
    protected synchronized void endJUnitTestCaseImpl(String jUnitTestClassName,
            String testName, String resultComment) {
        switch (this.state) {
        case TEST_CASE_STARTED:
            // something went wrong or the junit test case had priority
            return;
        case JUNIT_TEST_CASE_STARTED:
            // the default case
            break;
        case NO_TEST_CASE_STARTED_YET:
            // something went wrong - reject
            return;
        case TEST_CASE_ENDED:
            // something went wrong - reject
            return;
        case FINISHED:
            // something went wrong - reject
            return;
        }

        // assert: a test case with the given class has been started
        tellAlltoSerializeAndReset();

        String fullTestCaseName = getFullTestCaseName(this.currentTestClass,
                this.currentTestCaseName);

        this.timer.setPhaseEnd();
        this.coverageResultLog.lineComment(this.timer.getPhaseEnd());
        this.coverageResultLog.lineComment(this.timer.getPhaseDuration());
        this.coverageResultLog.endTestCase(fullTestCaseName,
                this.timer.getPhaseEndLong(), resultComment);

        this.currentTestClass = null;
        this.currentTestCaseName = null;
        this.state = TEST_CASE_ENDED;
    }

    @Override
    protected synchronized void finishImpl() {
        switch (this.state) {
        case TEST_CASE_STARTED:
            // end the last open test case
            endTestCaseImpl(this.currentTestCaseName, null);
            break;
        case JUNIT_TEST_CASE_STARTED:
            // something went wrong
            break;
        case NO_TEST_CASE_STARTED_YET:
            // we have to generate a test case for everything
            this.coverageResultLog.lineComment(this.timer.getOverallStart());
            this.coverageResultLog.startTestCase(this.testSessionContainerUID, 
                    UNNAMED_TESTCASE_NAME, this.timer.getOverallStartLong(),
                    UNNAMED_TESTCASE_COMMENT);

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
            // something went wrong - reject
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
        this.state = FINISHED;
    }

    /**
     * Calls {@link CounterContainer#reset()} for all
     * {@link #observedCounterContainers}.
     */
    private synchronized void tellAlltoReset() {
        Iterator<CounterContainer> iterator = this.observedCounterContainers.iterator();
        while (iterator.hasNext()) {
            iterator.next().reset();
        }
    }

    /**
     * Calls {@link CounterContainer#serializeAndReset(CoverageCounterLog)} for
     * all {@link #observedCounterContainers}.
     */
    private synchronized void tellAlltoSerializeAndReset() {
        Iterator<CounterContainer> iterator = this.observedCounterContainers.iterator();
        while (iterator.hasNext()) {
            iterator.next().serializeAndReset(this.coverageResultLog);
        }
    }

    private String getFullTestCaseName(String testCaseClass, String testCaseName) {
        if (testCaseName == null) {
            // the JUnit TestCases are used as test cases 
            return testCaseClass;
        }
        // JUnit test methods are used as test cases
        return testCaseClass + ":" + testCaseName;
    }

    /**
     * Cause of {@link Runnable} interface.<br>
     * <br>
     * Is used by the inner class ShutdownHook to notfiy the shutdown of the Java
     * VM.
     */
    public void run() {
        finishImpl();
    }

    /**
     * Is used as a Shutdownhook by the
     * {@link ProtocolImpl#getInstance(CoverageResultLog)}. It's behavior is
     * described in {@link Runtime#addShutdownHook(Thread)}.
     * 
     * @author Christoph Müller
     * @version 1.0 ($Id: ProtocolImpl.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    private class ShutdownHook extends Thread {
        private static final String THREAD_NAME = "CodeCover Shutdown Hook";

        /**
         * Constructs a new Shutdown Hook.<br>
         * <br>
         * Calls the super constructor
         * {@link Thread#Thread(ThreadGroup, Runnable, String)} and hands over
         * as the {@link Runnable} the instance of {@link ProtocolImpl}. So the
         * method {@link ProtocolImpl#run()} will be called after
         * {@link Runtime#exit(int)}.
         */
        public ShutdownHook() {
            super(ProtocolImpl.this, THREAD_NAME);
        }
    }
}
