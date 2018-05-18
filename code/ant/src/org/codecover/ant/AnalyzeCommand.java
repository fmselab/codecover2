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

package org.codecover.ant;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.UnsupportedCharsetException;
import java.util.Date;

import org.apache.tools.ant.BuildException;
import org.codecover.instrumentation.measurement.CoverageResultLogReader;
import org.codecover.instrumentation.measurement.MeasurementConstants;
import org.codecover.instrumentation.measurement.parser.CoverageLogParser;
import org.codecover.instrumentation.measurement.parser.ParseException;
import org.codecover.instrumentation.measurement.parser.WrongUIDException;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: AnalyzeCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class AnalyzeCommand extends Command {

    String containerId;

    File coverageLog;

    String name;

    String comment = "";

    String charset;

    /**
     * Sets the containerId.
     * 
     * @param containerId
     *                the containerId to set
     */
    public void setContainerId(String containerId) {
        this.containerId = containerId;
    }

    /**
     * Sets the coverageLog.
     * 
     * @param coverageLog
     *                the coverageLog to set
     */
    public void setCoverageLog(File coverageLog) {
        this.coverageLog = coverageLog;
    }

    /**
     * Sets the name.
     * 
     * @param name
     *                the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Sets the comment.
     * 
     * @param comment
     *                the comment to set
     */
    public void setComment(String comment) {
        this.comment = comment;
    }

    /**
     * Sets the charset.
     * 
     * @param charset
     *                the charset to set
     */
    public void setCharset(String charset) {
        this.charset = charset;
    }

    @Override
    public void run(Context context) {
        if (this.containerId == null) {
            throw new BuildException("The attribute 'containerId' is missing.");
        }

        if (this.coverageLog == null) {
            throw new BuildException("The attribute 'coverageLog' is missing.");
        }

        if (this.name == null) {
            throw new BuildException("The attribute 'name' is missing.");
        }

        if (this.comment == null) {
            throw new BuildException("The attribute 'comment' is missing.");
        }

        Charset coverageLogCharset;
        if (this.charset != null) {
            try {
                coverageLogCharset = Charset.forName(this.charset);
            } catch (UnsupportedCharsetException e) {
                context.getLogger()
                        .fatal(
                                "The charset \"" + this.charset
                                        + "\" is not supported", e);
                throw new RuntimeException();
            }
        } else {
            coverageLogCharset = MeasurementConstants.CHARSET;
        }

        TestSessionContainer testSessionContainer = context
                .getTestSessionContainer(this.containerId);

        TestSession testSession = testSessionContainer.createTestSession(
                this.name, this.comment, new Date());
        // If the name of the test session already existed, the user should
        // know, that the session is called something like
        // %test session name (1)%
        if (!this.name.equals(testSession.getName())) {
            context
                    .getLogger()
                    .warning(
                            "A test session with the name \""
                                    + this.name
                                    + "\" already exists! The created test session can be found under the name: \""
                                    + testSession.getName() + "\"");
        }

        try {
            CoverageLogParser logParser = new CoverageLogParser(
                    this.coverageLog, coverageLogCharset);

            CoverageResultLogReader coverageResultLogReader = new CoverageResultLogReader(
                    testSession, context.getMASTBuilder());

            logParser.CompilationUnit(coverageResultLogReader,
                    testSessionContainer.getId());

            // Success
        } catch (IOException e) {
            context.getLogger().fatal("Error accessing the coverage log file",
                    e);
        } catch (WrongUIDException e) {
            context
                    .getLogger()
                    .fatal(
                            "The coverage log file does not fit to the session container! Process aborted.",
                            e);
        } catch (ParseException e) {
            context.getLogger().fatal("Error parsing the coverage log", e);
        }
    }
}
