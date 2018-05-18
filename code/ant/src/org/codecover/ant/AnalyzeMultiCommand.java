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
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.util.FileUtils;
import org.codecover.instrumentation.measurement.CoverageResultLogReader;
import org.codecover.instrumentation.measurement.MeasurementConstants;
import org.codecover.instrumentation.measurement.parser.CoverageLogParser;
import org.codecover.instrumentation.measurement.parser.ParseException;
import org.codecover.instrumentation.measurement.parser.WrongUIDException;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.utils.Logger;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: AnalyzeMultiCommand.java 50 2009-06-01 09:59:34Z ahija $)
 */
public class AnalyzeMultiCommand extends Command {

    private static final FileUtils FILE_UTILS = FileUtils.getFileUtils();

    String containerId;

    FileSet coverageLogs;

    String baseName;

    String baseComment = "";

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
     * Sets the coverageLogs.
     *
     * @param coverageLogSet
     *                the coverageLogs to set
     */
    public void addConfiguredCoverageLogs(FileSet coverageLogSet) {
        if (this.coverageLogs != null) {
            throw new BuildException("There are multiple <coverageLogs> elements");
        }
        this.coverageLogs = coverageLogSet;
    }

    /**
     * Sets the name.
     *
     * @param baseName
     *                the name to set
     */
    public void setBaseName(String baseName) {
        this.baseName = baseName;
    }

    /**
     * Sets the comment.
     *
     * @param baseComment
     *                the comment to set
     */
    public void setBaseComment(String baseComment) {
        this.baseComment = baseComment;
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

        if (this.coverageLogs == null) {
            throw new BuildException("The attribute 'coverageLogs' is missing.");
        }

        if (this.baseName == null) {
            throw new BuildException("The attribute 'baseName' is missing.");
        }

        if (this.baseComment == null) {
            throw new BuildException("The attribute 'baseComment' is missing.");
        }

        Logger logger = context.getLogger();

        Charset coverageLogCharset;
        if (this.charset != null) {
            try {
                coverageLogCharset = Charset.forName(this.charset);
            } catch (UnsupportedCharsetException e) {
                context.getLogger().fatal("The charset \"" + this.charset + "\" is not supported", e);
                throw new RuntimeException();
            }
        } else {
            coverageLogCharset = MeasurementConstants.CHARSET;
        }

        TestSessionContainer testSessionContainer = context.getTestSessionContainer(this.containerId);

        File baseDir = this.coverageLogs.getDir(getProject());
        String[] coverageLogPaths = this.coverageLogs.getDirectoryScanner(getProject()).getIncludedFiles();

        for (int i = 0; i < coverageLogPaths.length; i++) {
            final String thisCoverageLogPath = coverageLogPaths[i];
            final File coverageLogFile = FILE_UTILS.resolveFile(baseDir, thisCoverageLogPath);

            final String sessionName = String.format("%s [%03d]", this.baseName, Integer.valueOf(i + 1));
            final String sessionComment = String.format("[%03d] %s%n%s", Integer.valueOf(i + 1),
                    coverageLogFile.getName(), this.baseComment);

            logger.info("Analyzing " + sessionName + " from file " + coverageLogFile);

            TestSession testSession = testSessionContainer.createTestSession(sessionName, sessionComment,
                    new Date());
            // If the name of the test session already existed, the user should
            // know, that the session is called something like
            // %test session name (1)%
            if (!sessionName.equals(testSession.getName())) {
                logger.warning("A test session with the name \"" + sessionName
                        + "\" already exists! The created test session can be found under the name: \""
                        + testSession.getName() + "\"");
            }

            try {
                CoverageLogParser logParser = new CoverageLogParser(coverageLogFile, coverageLogCharset);

                CoverageResultLogReader coverageResultLogReader = new CoverageResultLogReader(testSession,
                        context.getMASTBuilder());

                logParser.CompilationUnit(coverageResultLogReader, testSessionContainer.getId());

                // Success
            } catch (IOException e) {
                context.getLogger().fatal("Error accessing the coverage log file", e);
            } catch (WrongUIDException e) {
                context.getLogger().fatal(
                        "The coverage log file does not fit to the session container! Process aborted.", e);
            } catch (ParseException e) {
                context.getLogger().fatal("Error parsing the coverage log", e);
            }
        }
    }
}
