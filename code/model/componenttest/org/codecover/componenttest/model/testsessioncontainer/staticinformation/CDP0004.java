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

package org.codecover.componenttest.model.testsessioncontainer.staticinformation;

import java.util.*;
import java.io.*;

import org.codecover.model.*;
import org.codecover.model.mast.*;
import org.codecover.model.exceptions.*;
import org.codecover.model.utils.*;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: CDP0004.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDP0004 extends junit.framework.TestCase {

    /**
     * CDP0004: TSC date
     */
    public void testCDP0004() throws Exception {
        final TimeZone oldDefault = TimeZone.getDefault();
        try {
            //1.
            final TimeZone utc = new SimpleTimeZone(0, "UTC");
            TimeZone.setDefault(utc);
            
            java.text.DateFormat format = new java.text.SimpleDateFormat("dd.MM.yyyy HH:mm:ss zzz");

            //2.
            String singleContainerLocation = "../../qa/testdata/containers/singlefile/statement.xml";
            Logger logger = new SimpleLogger();
            MASTBuilder builder = new MASTBuilder(logger);
            TestSessionContainer tsc1 = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder, singleContainerLocation);
            
            //3.
            Date date = tsc1.getDate();
            
            //4.
            FileReader fileReader = new FileReader("../../qa/testdata/containers/singlefile/statement.utctime");
            final String expectedDate;
            try {
                expectedDate = new BufferedReader(fileReader).readLine();
            } finally {
                fileReader.close();
            }
            assertEquals(expectedDate, format.format(date));
            
        } finally {
            // clean up, so that the other test cases aren't influenced
            TimeZone.setDefault(oldDefault);
        }
    }
}
