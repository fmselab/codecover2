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

import java.io.File;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * This class is used to get to know the path of the coverage log file and to
 * add additional files to the instrumentation target.<br>
 * <br>
 * The process of getting the target for the coverage log file goes the
 * following. If the path of the coverage log file is found on a level, it is
 * returned. If it is not found, the lower level is asked.
 * <ol>
 * <li>{@link System#getProperty(String)} is asked for "{@link CoverageLogPath#PROPERTY_PATH_VARIABLE}"</li>
 * <li>{@link System#getenv(String)} is asked for "{@link CoverageLogPath#ENVIRONMENT_PATH_VARIABLE}"</li>
 * <li>the {@link #getDefaultPath()} method is asked for it</li>
 * </lo>
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: CoverageLogPath.java 8 2008-03-27 19:46:02Z ahija $)
 */
public class CoverageLogPath {

    private static final String DEFAULT_TARGET_FILE_NAME = "coverage-log";

    private static final String DEFAULT_TARGET_FILE_EXTENSION = "clf";

    /**
     * The default path of the coverage log file.
     */
    private static final String DEFAULT_TARGET_FILE = DEFAULT_TARGET_FILE_NAME +
        "." + DEFAULT_TARGET_FILE_EXTENSION;

    /**
     * The format for the suffix of the coverage-log:<br>
     * <br>
     * <code>yyyy-MM-dd-HH-mm-ss-SSS</code>
     * 
     * @see SimpleDateFormat
     */
    public static final DateFormat COVERAGE_LOG_DATE_SUFFIX = new SimpleDateFormat(
    "yyyy-MM-dd-HH-mm-ss-SSS");

    /**
     * The name of the property, used to get the path of the coverage log file: {@value} .
     * @see System#getProperty(String)
     */
    public static final String PROPERTY_PATH_VARIABLE = "org.codecover.coverage-log-file";

    /**
     * The name of the property, used to allow overwriting of the coverage log: {@value} .<br> To allow overwriting, the property must be set and its value must not be <code>false</code> or <code>no</code>.
     * @see System#getProperty(String)
     */
    public static final String PROPERTY_OVERWRITE_VARIABLE = "org.codecover.coverage-log-overwrite";

    /**
     * The name of the environment variable, used to get the path of the coverage log file:  {@value} .
     * @see System#getenv(String)
     */
    public static final String ENVIRONMENT_PATH_VARIABLE = "org.codecover.coverage-log-file";

    /**
     * This is the pattern, which is used to get a coverage log file name and make it unique:  {@link #getAlternativeFile(File)} .<br> <br> The pattern looks like:  {@value} <br>
     * @see #getAlternativeFile(File)  for usage information
     */
    public static final String ALTERNATIVE_FILE_PATTERN_STRING = ".*\\((\\d{1,18}+)\\)";

    private static final Pattern ALTERNATIVE_FILE_PATTERN = Pattern.compile(ALTERNATIVE_FILE_PATTERN_STRING);
    
    private static final Object LOCK = new Object();

    /**
     * Stores the path of the coverage log file after {@link #getCoverageLogFile(String)}  has decided its target.
     */
    private static File usedPath = null;

    /**
     * Change this return value to a relative or absolute path, where ever the
     * coverage log file should be written to.
     * 
     * @return the path to the coverage log file
     */
    private static String getDefaultPath() {
        return DEFAULT_TARGET_FILE_NAME + "-" + 
               COVERAGE_LOG_DATE_SUFFIX.format(new Date()) + "." +
               DEFAULT_TARGET_FILE_EXTENSION;
    }

    /**
     * Gets the path, where the Coverage Log file will be created.
     * 
     * @param preferredPath The preferred path of the clf, if now property is 
     * set. 
     * 
     * @return The path of the Coverage Log File.
     * @see CoverageLogPath
     * @see CoverageResultLogFile#getInstance()
     */
    public static File getCoverageLogFile(String preferredPath) {
        // just one Thread can create the path of the coverage log file 
        synchronized (LOCK) {
            if (usedPath != null) {
                return usedPath;
            }

            String clfPath = null;

            // try the PROPERTY_PATH_VARIABLE
            try {
                clfPath = System.getProperty(PROPERTY_PATH_VARIABLE);
            } catch (Throwable t) {
                // access was not allowed
            }

            // try the ENVIRONMENT_PATH_VARIABLE
            if (clfPath == null) {
                try {
                    clfPath = System.getenv(ENVIRONMENT_PATH_VARIABLE);
                } catch (Throwable t) {
                    // access was not allowed
                }
            }

            // try the preferredPath
            if (clfPath == null) {
                clfPath = preferredPath;
            }

            // user the #getDefaultPath
            if (clfPath == null) {
                clfPath = getDefaultPath();
            }

            File clfFile = new File(clfPath);
            if (clfFile.isDirectory()) {
                clfPath = clfPath + File.separatorChar
                + DEFAULT_TARGET_FILE;
                clfFile = new File(clfPath);
            }

            clfFile = clfFile.getAbsoluteFile();

            try {
                String overwriteProp = System.getProperty(PROPERTY_OVERWRITE_VARIABLE); 
                if (overwriteProp == null ||
                        overwriteProp.equals("false") ||
                        overwriteProp.equals("no")) {
                    clfFile = getAlternativeFile(clfFile).getAbsoluteFile();
                }
            } catch (Throwable t) {
                // access was not allowed
            }

            // store the decision in used Path
            usedPath = clfFile;
            return clfFile;
        }
    }

    /**
     * Ensures, that a file with the given name does not exist. This is done by
     * adding <code>(1), (2)</code> and so on.<br>
     * <br>
     * Detailed description:<br>
     * We use a {@link Pattern} for this job: {link #ALTERNATIVE_FILE_PATTERN_STRING}.<br>
     * At the beginning of the file name any character is allowed by the pattern.
     * Then there might be a number in brackets: <code>(12)</code>. This number
     * has at least one, at most 18 digits. So it can be casted into a {@link Long}.<br>
     * If the pattern does not match, we
     * can add a <code>(1)</code> to the name of the file, to get an alternative
     * file name.<br>
     * If the pattern matches, we can get the number of the capturing group 1, 
     * <code>12</code>, cast it into a {@link Long} and increment it. Perhaps
     * this file name is not used.
     * 
     * @param clfFile The file to use as start.
     * 
     * @return The file, based on clfFile, that does not exists yet. 
     * 
     * @see #ALTERNATIVE_FILE_PATTERN_STRING
     */
    public static File getAlternativeFile(File clfFile) {
        while (clfFile.exists()) {
            String parentPath = clfFile.getParentFile().getAbsolutePath();
            String fileName = clfFile.getName();
            String extension = getExtension(clfFile);
            if (extension.length() > 0) {
                // remove the extension and the dot from the file name
                fileName = fileName.substring(0,
                        fileName.length() - extension.length() - 1);
                extension = '.' + extension;
            }

            // matches the pattern?
            Matcher matcher = ALTERNATIVE_FILE_PATTERN.matcher(fileName);
            if (matcher.matches()) {
                // first get the number
                String numberAsString = fileName.substring(matcher.start(1),
                        matcher.end(1));
                // then truncate the file name until the opening (
                fileName = fileName.substring(0, matcher.start(1));
                long number = Long.parseLong(numberAsString);
                fileName += Long.toString(number + 1) + ')';
            } else {
                // we can add a (1)
                fileName += "(1)";
            }

            clfFile = new File(parentPath + File.separatorChar + fileName +
                    extension);
        }
        return clfFile;
    }

    /**
     * Gets the extension of a file.<br>
     * <br>
     * An extension is defined as the String after the last dot. The extensions
     * is always returned in lower case. If there is no dot, the extensions is
     * defined as "".
     * 
     * <pre>
     *      getExtension(Main.java) == &quot;java&quot;
     *      getExtension(Main.java.in) == &quot;in&quot;
     *      getExtension(Main) == &quot;&quot;
     * </pre>
     * 
     * Copied from <code>ExtensionFileFilter#getExtension(File)</code> that this
     * class needs not to be shipped as source file too.
     * 
     * @param file
     *            The given file.
     * @return The extension of the file.
     * 
     */
    public static String getExtension(File file) {
        if (file == null) {
            return "";
        }
        // file != null

        String name = file.getName();
        // name != null

        String extensions = "";
        int i = name.lastIndexOf('.');
        if (i >= 0 && i < name.length() - 1) {
            extensions = name.substring(i + 1).toLowerCase();
        }
        return extensions;
    }
}