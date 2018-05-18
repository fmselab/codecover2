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

package org.codecover.instrumentation.java15.measurement;

import static org.codecover.UtilsForTestingJava.TARGET;
import static org.codecover.UtilsForTestingJava.checkAreMeasurementHelpersCopied;
import static org.codecover.UtilsForTestingJava.copyMeasurementHelpersToBin;
import static org.codecover.UtilsForTestingJava.handleException;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.nio.charset.Charset;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJava;
import org.codecover.instrumentation.java15.JavaInstrumentationHelper;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: JavaInstrumentationHelperTest.java 1895 2007-09-17
 *          11:16:31Z kiesssn $)
 */
public class JavaInstrumentationHelperTest extends TestCase {

    /**
     * Test method for
     * {@link JavaInstrumentationHelper#copyMeasurementHelpersToInstrumentFolder(File, boolean, Charset)}.
     */
    public void testCopyMeasurementHelpersToInstrumentFolderJava15()
            throws Exception {
        // if we want to test this, the source files have to be in the bin
        // folder
        copyMeasurementHelpersToBin();

        File target = new File(TARGET);
        try {
            JavaInstrumentationHelper.copyMeasurementHelpersToInstrumentFolder(
                    target, false, Charset.forName("UTF-16"));
        } catch (Exception e) {
            handleException(e);
        }

        checkAreMeasurementHelpersCopied();
        
        // now we want to check, whether we have copied the correct ProtocolImpl
        FileInputStream fileInputStream = new FileInputStream(TARGET + "org/codecover/instrumentation/java/measurement/ProtocolImpl.java");
        InputStreamReader reader = new InputStreamReader(fileInputStream, Charset.forName("UTF-16"));
        LineNumberReader lineReader = new LineNumberReader(reader);
        try {
            while (lineReader.ready()) {
                String thisLine = lineReader.readLine();
                if (thisLine.contains("import java.lang.management.ManagementFactory;")) {
                    return;
                }
            }
            Assert.fail("We have not found the ManagementFactory import");
        } finally {
            lineReader.close();
        }
    }

    /**
     * Test method for
     * {@link JavaInstrumentationHelper#copyMeasurementHelpersToInstrumentFolder(File, boolean, Charset)}.
     */
    public void testCopyMeasurementHelpersToInstrumentFolderJava14()
            throws Exception {
        // if we want to test this, the source files have to be in the bin
        // folder
        copyMeasurementHelpersToBin();

        File target = new File(TARGET);
        try {
            JavaInstrumentationHelper.copyMeasurementHelpersToInstrumentFolder(
                    target, true, Charset.forName("UTF-16"));
        } catch (Exception e) {
            handleException(e);
        }

        checkAreMeasurementHelpersCopied(new String[][] {
                UtilsForTestingJava.FILES_TO_COPY_BASIC,
                UtilsForTestingJava.FILES_TO_COPY_MEASUREMENT
        });

        // now we want to check, whether we have copied the correct ProtocolImpl
        FileInputStream fileInputStream = new FileInputStream(TARGET + "org/codecover/instrumentation/java/measurement/ProtocolImpl.java");
        InputStreamReader reader = new InputStreamReader(fileInputStream, Charset.forName("UTF-16"));
        LineNumberReader lineReader = new LineNumberReader(reader);
        try {
            while (lineReader.ready()) {
                String thisLine = lineReader.readLine();
                if (thisLine.contains("import java.lang.management.ManagementFactory;")) {
                    Assert.fail("We have found the ManagementFactory import.");
                }
            }
        } finally {
            lineReader.close();
        }
    }
}
