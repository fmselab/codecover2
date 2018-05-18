///////////////////////////////////////////////////////////////////////////////
//
// $Id: Protocol.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 21.03.2007 11:52:14
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.measurement;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.util.Set;
import java.util.TreeSet;

/**
 * @author Christoph Müller
 * @version 1.0 - 21.03.2007
 * 
 */
public class Protocol {
    private static CoverageResultLog coverageLog = new CoverageResultPrinter();

    private static boolean isInitialized = false;

    private static Set<CounterContainer> observedCounterContainers;

    private static String testCaseName;

    public static void initialize() {
        if (!isInitialized) {
            observedCounterContainers = new TreeSet<CounterContainer>();
            testCaseName = null;
            Runtime.getRuntime().addShutdownHook(new ShutdownHook());

            isInitialized = true;
        }
    }

    public static void addObservedContainer(CounterContainer observedCounter) {
        initialize();

        observedCounterContainers.add(observedCounter);
    }

    public static void startTestCase(String name) {
        initialize();

        if (testCaseName != null) {
            endTestCase();
        }

        testCaseName = name;
    }

    public static void endTestCase() {
        initialize();

        coverageLog.startTestCase(testCaseName);

        for (CounterContainer thisCounterContainer : observedCounterContainers) {
            thisCounterContainer.serializeAndReset(coverageLog);
        }

        coverageLog.endTestCase(testCaseName);

        testCaseName = null;
    }

    public static void finish() {
        coverageLog.close();
    }

    private static class CoverageResultPrinter implements CoverageResultLog {
        private static final String COVERAGE_LOG_DIR = "stuff" + File.separator;

        private PrintWriter printer;

        public CoverageResultPrinter() {
            try {
                observedCounterContainers = new TreeSet<CounterContainer>();
                testCaseName = null;

                FileOutputStream fileOutputStream = new FileOutputStream(
                        COVERAGE_LOG_DIR
                                + Long.toHexString(System.currentTimeMillis())
                                + ".clf");
                OutputStreamWriter outputStreamWriter = new OutputStreamWriter(
                        fileOutputStream, Charset.forName("UTF-8"));
                BufferedWriter bufferedWriter = new BufferedWriter(
                        outputStreamWriter);
                this.printer = new PrintWriter(bufferedWriter);
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }

        public void startFile(String fileName) {
            this.printer.printf("START TEST CASE \"%s\"%n", fileName);
        }

        public void endTestCase(String testCaseName) {
            this.printer.printf("START TEST CASE \"%s\"%n", testCaseName);
        }

        public void startTestCase(String testCaseName) {
            this.printer.printf("END TEST CASE \"%s\"%n", testCaseName);
        }

        public void writeCounter(String counterName, long counterValue) {
            this.printer.print(counterName);
            this.printer.print(" ");
            this.printer.println(counterValue);
        }

        public void close() {
            printer.flush();
            printer.close();
        }
    }

    private static class ShutdownHook extends Thread implements Runnable {
        public ShutdownHook() {
            super("Gbt2 Shutdown Hook");
        }

        @Override
        public void run() {
            Protocol.finish();
        }
    }
}
