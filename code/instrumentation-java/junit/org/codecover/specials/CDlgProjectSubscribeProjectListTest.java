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

package org.codecover.specials;

import static org.codecover.UtilsForTestingJava.SOURCE;
import static org.codecover.UtilsForTestingJava.TARGET;
import static org.codecover.UtilsForTestingJava.handleException;
import static org.codecover.UtilsForTestingJava.simpleTestSessionContainerTests;

import java.io.File;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJava;
import org.codecover.instrumentation.Instrumenter;
import org.codecover.instrumentation.exceptions.InstrumentationException;
import org.codecover.instrumentation.java15.InstrumenterDescriptor;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.utils.file.SourceTargetContainer;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: CDlgProjectSubscribeProjectListTest.java 15 2008-05-24 20:59:06Z ahija $)
 */
public class CDlgProjectSubscribeProjectListTest extends TestCase {
    public void testLoadWorkFile() throws Exception {
        // we had Problems with the JavaCharStream in rev. 1595
        // java.lang.StringIndexOutOfBoundsException: String index out of range: -34
        // at java.lang.String.substring(Unknown Source)
        // at org.codecover.instrumentation.java15.parser.JavaCharStream.getSourceFileImage(JavaCharStream.java:347)
        // at org.codecover.instrumentation.java15.parser.JavaParserTokenManager.jjFillToken(JavaParserTokenManager.java:2000)
        // at org.codecover.instrumentation.java15.parser.JavaParserTokenManager.getNextToken(JavaParserTokenManager.java:2031)
        // at org.codecover.instrumentation.java15.parser.JavaParser.jj_ntk(JavaParser.java:10012)
        // at org.codecover.instrumentation.java15.parser.JavaParser.CompilationUnit(JavaParser.java:52)
        // at org.codecover.instrumentation.java15.Instrumenter.instrumentThis(Instrumenter.java:100)
        // at org.codecover.instrumentation.Instrumenter.instrumentJob(Instrumenter.java:422)

        UtilsForTestingJava.clearTarget();
        UtilsForTestingJava.copyMeasurementHelpersToBin();

        Instrumenter instrumenter = UtilsForTestingJava.getDefaultJavaInstrumenter();
        MASTBuilder builder = UtilsForTestingJava.newMASTBuilder();

        // instrument
        final String srcPath = SOURCE + "de/sopra06/view/dialogs/project/CDlgProjectSubscribeProjectList.java";
        final File source = new File(srcPath);
        final String targetPath = TARGET + "de/sopra06/view/dialogs/project/CDlgProjectSubscribeProjectList.java";
        final File target = new File(targetPath);
        Date dateBefore = new Date();
        TestSessionContainer testSessionContainer = null;
        Collection<SourceTargetContainer> container = Collections.<SourceTargetContainer>singleton(
                new SourceTargetContainer(source, target));

        try {
          testSessionContainer = instrumenter.instrument(new File(SOURCE),
                  new File(TARGET),
                  container,
                  builder,
                  new InstrumenterDescriptor().getDefaultDirectiveValues());
        } catch (InstrumentationException e) {
            handleException(e);
        }

        Assert.assertTrue(target.exists());
        simpleTestSessionContainerTests(dateBefore, testSessionContainer, instrumenter);
    }
}