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

package org.codecover.instrumentation.java15;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;

import org.codecover.CodeCoverInfo;
import org.codecover.instrumentation.java.measurement.Protocol;
import org.codecover.instrumentation.measurement.CoverageCounterLog;
import org.codecover.instrumentation.measurement.CoverageResultLog;

/**
 * This class is a helper to copy the sources of helper classes to the default
 * directory of the instrumented java sources. These helpers are used for
 * measuring the coverage and protocol it into a coverage log file.<br>
 * We ship the sources, that the user can compile the helpers with his VM rather
 * than shipping the already compiled class files.<br>
 * In addition, the source files are selected depending on the flag
 * <code>java14Compatibilit</code> of {@link #copyMeasurementHelpersToInstrumentFolder(File, boolean, Charset)}.<br>
 * Last not least, we reencode the helpers to use the Charset that is used for
 * the instrumented source files rather than using their original UTF-8 charset.
 * 
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: JavaInstrumentationHelper.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class JavaInstrumentationHelper {
    private static final String MEASUREMENT_PACKAGE = "org/codecover/instrumentation/measurement/";

    private static final String JAVA_MEASUREMENT_PACKAGE = "org/codecover/instrumentation/java/measurement/";

    private static final String JMX_PACKAGE = JAVA_MEASUREMENT_PACKAGE + "jmx/";

    private static final MeasurementHelperFile[] HELPERS_ALL_VERSIONS = {
        new MeasurementHelperFile(MEASUREMENT_PACKAGE, "CoverageCounterLog.java"),
        new MeasurementHelperFile(MEASUREMENT_PACKAGE, "CoverageResultLog.java"),
        new MeasurementHelperFile(MEASUREMENT_PACKAGE, "MeasurementConstants.java"),

        new MeasurementHelperFile(JAVA_MEASUREMENT_PACKAGE, "ConditionCounter.java"),
        new MeasurementHelperFile(JAVA_MEASUREMENT_PACKAGE, "CounterContainer.java"),
        new MeasurementHelperFile(JAVA_MEASUREMENT_PACKAGE, "CoverageLogPath.java"),
        new MeasurementHelperFile(JAVA_MEASUREMENT_PACKAGE, "CoverageResultLogFile.java"),
        new MeasurementHelperFile(JAVA_MEASUREMENT_PACKAGE, "LargeConditionCounter.java"),
        new MeasurementHelperFile(JAVA_MEASUREMENT_PACKAGE, "LongContainer.java"),
        new MeasurementHelperFile(JAVA_MEASUREMENT_PACKAGE, "MeasurementTimer.java"),
        new MeasurementHelperFile(JAVA_MEASUREMENT_PACKAGE, "MediumConditionCounter.java"),
        new MeasurementHelperFile(JAVA_MEASUREMENT_PACKAGE, "Protocol.java"),
        new MeasurementHelperFile(JAVA_MEASUREMENT_PACKAGE, "SmallOneConditionCounter.java"),
        new MeasurementHelperFile(JAVA_MEASUREMENT_PACKAGE, "SmallTwoConditionCounter.java"),
        new MeasurementHelperFile(JAVA_MEASUREMENT_PACKAGE, "TestMethod.java")
    };

    private static final MeasurementHelperFile[] HELPERS_JAVA14 = {
        new MeasurementHelperFile(JAVA_MEASUREMENT_PACKAGE, "ProtocolImpl.java14",
                                  JAVA_MEASUREMENT_PACKAGE, "ProtocolImpl.java")
    };

    private static final MeasurementHelperFile[] HELPERS_JAVA15 = {
        new MeasurementHelperFile(JAVA_MEASUREMENT_PACKAGE, "ProtocolImpl.java"),
        new MeasurementHelperFile(JMX_PACKAGE, "LiveNotification.java"),
        new MeasurementHelperFile(JMX_PACKAGE, "LiveNotificationMBean.java"),
        new MeasurementHelperFile(JMX_PACKAGE, "JMXFileTransferException.java")
    };
    
    /**
     * Creates the source code of the classes in:
     * <code>org.codecover.instrumentation.java.measurement</code>,
     * the {@link CoverageCounterLog} and {@link CoverageResultLog}.
     * 
     * @param defaultPackageFolder
     *            Where is the default package of the source code? The
     *            classes will be created in the sub directories referring to
     *            their packages.
     * @param java14Compatibility
     *            true &rarr; The copied sources are compatible with Java 1.4;
     *            false &rarr; The copied sources use special Java 1.5 features. 
     * @param targetCharset
     *            The charset, the helper sources should have &rrar; expected
     *            to be the charset of the instrumented source files.
     * @throws IOException
     *             If there occurs an {@link IOException} when writing.
     */
    public static void copyMeasurementHelpersToInstrumentFolder(File defaultPackageFolder,
            boolean java14Compatibility,
            Charset targetCharset)
    throws IOException  {
        if (defaultPackageFolder.isFile()) {
            throw new RuntimeException("defaultPackageFolder.isFile()");
        }

        for (int i = 0; i < HELPERS_ALL_VERSIONS.length; i++) {
            copyClassToInstrumentFolder(HELPERS_ALL_VERSIONS[i],
                    defaultPackageFolder, targetCharset);
        }
        if (java14Compatibility) {
            for (int i = 0; i < HELPERS_JAVA14.length; i++) {
                copyClassToInstrumentFolder(HELPERS_JAVA14[i],
                        defaultPackageFolder, targetCharset);
            }
        } else {
            for (int i = 0; i < HELPERS_JAVA15.length; i++) {
                copyClassToInstrumentFolder(HELPERS_JAVA15[i],
                        defaultPackageFolder, targetCharset);
            }
        }
    }

    private static void copyClassToInstrumentFolder(MeasurementHelperFile fileToCopy,
            File defaultPackageFolder,
            Charset targetCharset) throws IOException {
        // we use a class as reference for the stream, that is guaranteed to
        // be in the same distribution directory/jar like all files to be copied 
        InputStream sourceInputStream = Protocol.class.getResourceAsStream("/" + fileToCopy.getFullSourcePath());
        if (sourceInputStream == null) {
            throw new FileNotFoundException("Helper file not found: " + fileToCopy.getFullSourcePath());
        }
        InputStreamReader sourceReader = new InputStreamReader(sourceInputStream,
                CodeCoverInfo.CODE_FILE_CHARSET);

        String targetSourcePath = defaultPackageFolder.getAbsolutePath() + "/"
        + fileToCopy.getFullTargetPath();
        File targetSourceFile = new File(targetSourcePath).getAbsoluteFile();
        File parentOfTargetSourceFile = targetSourceFile.getParentFile();
        if (parentOfTargetSourceFile != null) {
            parentOfTargetSourceFile.mkdirs();
        }
        FileOutputStream targetOutputStream = new FileOutputStream(targetSourceFile);
        OutputStreamWriter targetWriter = new OutputStreamWriter(targetOutputStream, targetCharset);

        char[] buff = new char[2048];
        while (true) {
            int iReadSize = sourceReader.read(buff);
            if (iReadSize > 0) {
                targetWriter.write(buff, 0, iReadSize);
            } else {
                break;
            }
        }

        sourceReader.close();
        targetWriter.flush();
        targetWriter.close();
    }

    /**
     * This is a class, that contains information of a measurement helper,
     * that has to be copied to the default package of the instrumented sources.<br>
     * <br>
     * We need a special container because we have to decide, which classes we
     * use for Java 1.4 and Java 1.5.
     * 
     * @author Christoph Müller 
     *
     * @version 1.0 ($Id: JavaInstrumentationHelper.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public static class MeasurementHelperFile {
        private String fullSourcePath;

        private String fullTargetPath;
        
        /**
         * Creates a new {@link MeasurementHelperFile}, that has an equal
         * source and target path and name.<br>
         * <br>
         * The path is the sub directory under the file has to be
         * placed&mdash;e.g. <code>org/codecover/</code>. The name is the
         * name of the file including the extension&mdash;e.g.
         * <code>Helper.java</code>.
         * 
         * @param sourcePath
         *                The source and target path.
         * @param sourceName
         *                The source and target name.
         * 
         * @see JavaInstrumentationHelper.MeasurementHelperFile#MeasurementHelperFile(String,
         *      String, String, String)
         */
        public MeasurementHelperFile(String sourcePath, String sourceName) {
            this(sourcePath, sourceName, sourcePath, sourceName);
        }

        /**
         * Creates a new {@link MeasurementHelperFile}, that has an unequal
         * source and target paths or names.<br>
         * <br>
         * The path is the sub directory under the file has to be
         * placed&mdash;e.g. <code>org/codecover/</code>. The name is the
         * name of the file including the extension&mdash;e.g.
         * <code>Helper.java</code>.
         * 
         * @param sourcePath
         *                The source and target path.
         * @param sourceName
         *                The source and target name.
         * @param targetPath
         *                The target and target path.
         * @param targetName
         *                The target and target name.
         * 
         * @see JavaInstrumentationHelper.MeasurementHelperFile#MeasurementHelperFile(String,
         *      String)
         */
        public MeasurementHelperFile(String sourcePath, String sourceName,
                String targetPath, String targetName) {
            this.fullSourcePath = sourcePath.replace('\\', '/');
            if (this.fullSourcePath.endsWith("/")) {
                this.fullSourcePath += sourceName;
            } else {
                this.fullSourcePath += '/' + sourceName;
            }

            this.fullTargetPath = targetPath.replace('\\', '/');
            if (this.fullTargetPath.endsWith("/")) {
                this.fullTargetPath += targetName;
            } else {
                this.fullTargetPath += '/' + targetName;
            }
        }

        /**
         * The full source path under the source default directory<br>
         * <br>
         * Full means, including the file name and extension. For example:
         * <code>org/codecover/Helper.java</code>.
         * 
         * @return The full source path.
         */
        public String getFullSourcePath() {
            return this.fullSourcePath;
        }
        
        /**
         * The full target path under the target default directory<br>
         * <br>
         * Full means, including the file name and extension. For example:
         * <code>org/codecover/Helper.java</code>.
         * 
         * @return The full source path.
         */
        public String getFullTargetPath() {
            return this.fullTargetPath;
        }
    }
}
