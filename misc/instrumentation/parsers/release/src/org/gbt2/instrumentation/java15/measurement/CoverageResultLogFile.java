///////////////////////////////////////////////////////////////////////////////
//
// $Id: CoverageResultLogFile.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 02.04.2007 16:57:37
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.measurement;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.util.Formatter;

/**
 * This is an implementation of {@link CoverageResultLog} using an output file.
 * <br>
 * <br>
 * The instance of {@link CoverageResultLogFile} should be used just for one
 * coverage log. After the usage of {@link #closeLog(String)} no other
 * notifications can be written, cause with {@link #closeLog(String)} the
 * internal {@link BufferedWriter} is closed. <br>
 * Exceptions for writing are not thrown but captured. The
 * {@link FileNotFoundException} is also not thrown, but the stack trace is
 * written.<br>
 * UTF-8 is used.
 * 
 * @author Christoph Müller
 * @see CoverageResultLog
 */
public class CoverageResultLogFile implements CoverageCounterLog,
        CoverageResultLog {
    /** The sepeerator for lines: '\n' */
    public static final String LINE_SEPERATOR = "\n";
  
    /** The identifier for comment lines: <b>//</b> */
    public static final String COMMENT_IDENTIFIER = "// ";

    /**
     * The format String, which is written at the start of the file if there is
     * no comment.
     * 
     * @see Formatter
     */
    public static final String START_FILE_FORMAT = "";

    /**
     * The format String, which is written at the start of the file if there is
     * a comment.
     * 
     * @see Formatter
     */
    public static final String START_FILE_FORMAT_COMMENT = COMMENT_IDENTIFIER
            + "%s%n";

    /**
     * The format String, which is written at the start of a test case if there
     * is no test case comment.
     * 
     * @see Formatter
     */
    public static final String START_TEST_CASE_FORMAT = "START TEST CASE " +
                "\"%s\" \"%d\"%n";

    /**
     * The format String, which is written at the start of a test case if there
     * is a test case comment.
     * 
     * @see Formatter
     */
    public static final String START_TEST_CASE_FORMAT_COMMENT = "START TEST CASE " +
                "\"%s\" \"%d\" \"%s\"%n";

    /**
     * The format String, which is written at the end of a test case if there is
     * no test case comment.
     * 
     * @see Formatter
     */
    public static final String END_TEST_CASE_FORMAT = "END TEST CASE " +
                "\"%s\" \"%d\"%n";

    /**
     * The format String, which is written at the start of a section.
     * 
     * @see Formatter
     */
    public static final String START_SECTION_FORMAT = "START SECTION \"%s\"%n";

    /**
     * The format String, which is used for passing counter ID and value.
     * 
     * @see Formatter
     */
    public static final String COUNTER_FORMAT = "%s %d%n";

    /**
     * The format String, which is written at the end of the file if there is no
     * comment.
     * 
     * @see Formatter
     */
    public static final String END_FILE_FORMAT = "";

    /**
     * The format String, which is written at the end of the file if there is a
     * comment.
     * 
     * @see Formatter
     */
    public static final String END_FILE_FORMAT_COMMENT = COMMENT_IDENTIFIER
            + "//%s";

    private static final Charset CHARSET = Charset.forName("UTF-8");

    private static final int BUFFER_SIZE = 8192;

    private static final String TARGET_PATH = "coverage_log.clf";

    private static CoverageResultLogFile instance = null;

    private File targetFile;

    private Writer writer;

    /**
     * Returns the single instance of {@link CoverageResultLogFile} or creates
     * one.<br>
     * <br>
     * If a {@link FileNotFoundException} occurs during opening of the
     * {@link FileOutputStream}, the exception message is put out to the
     * {@link System#err}.
     * 
     * @return The single instance.
     */
    public static CoverageResultLogFile getInstance() {
        if (instance == null) {
            instance = new CoverageResultLogFile();
        }

        return instance;
    }

    /**
     * A new {@link CoverageResultLogFile} is created. The target file is opened
     * for writing.
     */
    private CoverageResultLogFile() {
        try {
            this.targetFile = new File(TARGET_PATH);
            FileOutputStream fileOutputStream = new FileOutputStream(
                    this.targetFile);
            OutputStreamWriter outputStreamWriter = new OutputStreamWriter(
                    fileOutputStream, CHARSET);
            this.writer = new BufferedWriter(outputStreamWriter, BUFFER_SIZE);
        } catch (FileNotFoundException e) {
            System.err.println(e.getMessage());
        }
    }

    public void startLog() {
        startLog(null);
    }

    public void startLog(String comment) {
        try {
            if (comment == null) {
                this.writer.write(START_FILE_FORMAT);
            } else {
                this.writer.write(String.format(START_FILE_FORMAT_COMMENT,
                        comment));
            }
        } catch (IOException e) {
            // just catch
        }
    }

    public void startTestCase(String testCaseName, long timestamp) {
        startTestCase(testCaseName, timestamp, null);
    }

    public void startTestCase(String testCaseName, long timestamp,
            String comment) {
        try {
            if (comment == null) {
                this.writer.write(String.format(START_TEST_CASE_FORMAT,
                        testCaseName, new Long(timestamp)));
            } else {
                this.writer.write(String.format(START_TEST_CASE_FORMAT_COMMENT,
                        testCaseName, new Long(timestamp), comment));
            }
        } catch (IOException e) {
            // just catch
        }
    }

    public void endTestCase(String testCaseName, long timestamp) {
        try {
            this.writer.write(String.format(END_TEST_CASE_FORMAT, testCaseName,
                    new Long(timestamp)));
        } catch (IOException e) {
            // just catch
        }
    }

    public void startNamedSection(String sectionName) {
        try {
            this.writer.write(String.format(START_SECTION_FORMAT, sectionName));
        } catch (IOException e) {
            // just catch
        }
    }

    public void passCounter(String counterID, long counterValue) {
        try {
            this.writer.write(String.format(COUNTER_FORMAT, counterID,
                    new Long(counterValue)));
        } catch (IOException e) {
            // just catch
        }
    }

    public void lineComment(String comment) {
        try {
            this.writer.write(COMMENT_IDENTIFIER);
            this.writer.write(comment);
            this.writer.write(LINE_SEPERATOR);
        } catch (IOException e) {
            // just catch
        }
    }

    public void closeLog() {
        closeLog(null);
    }

    public void closeLog(String comment) {
        try {
            if (comment == null) {
                this.writer.write(END_FILE_FORMAT);
            } else {
                this.writer.write(String.format(END_FILE_FORMAT_COMMENT,
                        comment));
            }
            this.writer.flush();
            this.writer.close();
        } catch (IOException e) {
            // just catch
        }
    }
}
