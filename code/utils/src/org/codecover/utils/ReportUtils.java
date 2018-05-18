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

package org.codecover.utils;

import java.io.*;
import java.nio.charset.*;
import java.util.*;
import java.util.Map.Entry;
import java.util.regex.*;

import org.codecover.report.*;
import org.codecover.report.exceptions.*;
import org.codecover.metrics.*;
import org.codecover.model.*;
import org.codecover.model.exceptions.*;
import org.codecover.model.extensions.*;
import org.codecover.model.utils.*;
import org.codecover.model.utils.criteria.*;
import org.codecover.model.utils.file.*;

/**
 * Instrumentation utility methods.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: ReportUtils.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public final class ReportUtils {
    private ReportUtils() {
    }
    
    private static final FileCreationHandler nullFileCreationHandler = new FileCreationHandler() {
            public void setLogger(Logger logger) {
            }

            public OutputStream createFile(String filepath, String contentType) throws FileCreationException {
                if (filepath == null) {
                    throw new NullPointerException("filepath == null");
                }
                
                if (contentType == null) {
                    throw new NullPointerException("contentType == null");
                }
                
                return new OutputStream() {
                    public void write(byte[] b) {
                    }

                    public void write(byte[] b, int off, int len) {
                    }

                    public void write(int b) {
                    }
                };
            }
        };
    
    public static void report(Logger logger, PluginManager pluginManager, ProgressHandler progressHandler, boolean pretend, File destination, File template, List<TestCase> testCases) {
        if (logger == null) {
            throw new NullPointerException("logger == null");
        }
        if (progressHandler == null) {
            throw new NullPointerException("progressHandler == null");
        }
        if (destination == null) {
            throw new NullPointerException("destination == null");
        }
        if (template == null) {
            throw new NullPointerException("template == null");
        }
        if (testCases == null) {
            throw new NullPointerException("testCases == null");
        }

        Report r = new Report(pluginManager,
                MetricProvider.getAvailabeMetrics(pluginManager, logger),
                logger);
        r.setProgressHandler(progressHandler);
        if (pretend) {
            r.setCreationHandler(nullFileCreationHandler);
        } else {
            try {
                r.setFileSystemPath(destination);
            } catch (FileCreationException e) {
                /*
                logger.fatal("The given destination is not valid. Please " +
                             "ensure, that this is a valid filename and you " +
                             "have the rights to write there.");
                */
                logger.fatal("Could not create output file: " + e.getMessage(), e);
            }
        }
        r.setTemplate(template);
        r.setTestCases(testCases);
        progressHandler.setProgress(0.0f);
        try {
            r.generateReport();
        } catch (TemplateException e) {
            logger.fatal("An error occured while processing the template: " + e.getMessage(), e);
        } catch (LoadReportGeneratorException e) {
            logger.fatal("Could not load a ReportGenerator fitting to the " +
                         "template.", e);
        } catch (ReportException e) {
            logger.fatal("An error occured while writing the report: " + e.getMessage(), e);
        }
    }
}
