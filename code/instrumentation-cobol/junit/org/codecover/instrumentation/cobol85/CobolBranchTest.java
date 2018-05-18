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

package org.codecover.instrumentation.cobol85;

import static org.codecover.UtilsForTestingCobol.SESSION_CONTAINER;
import static org.codecover.UtilsForTestingCobol.SOURCE;
import static org.codecover.UtilsForTestingCobol.TARGET;
import static org.codecover.UtilsForTestingCobol.handleException;
import static org.codecover.UtilsForTestingCobol.isCompileableCobol;
import static org.codecover.UtilsForTestingCobol.simpleTestSessionContainerTests;

import java.io.File;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingCobol;
import org.codecover.instrumentation.DefaultInstrumenterFactory;
import org.codecover.instrumentation.Instrumenter;
import org.codecover.instrumentation.InstrumenterFactory;
import org.codecover.instrumentation.exceptions.InstrumentationException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.file.SourceTargetContainer;

/**
 * @author Stefan Franke
 * @version 1.0 ($Id: CobolBranchTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CobolBranchTest extends TestCase {

    Instrumenter instrumenter;

    MASTBuilder builder;

    @Override
    protected void setUp() throws Exception {
        this.builder = UtilsForTestingCobol.newMASTBuilder();

        org.codecover.instrumentation.InstrumenterDescriptor descriptor = new InstrumenterDescriptor();
        InstrumenterFactory factory = new DefaultInstrumenterFactory();
        factory.setDescriptor(descriptor);
        factory.setCharset(Charset.forName("UTF-8"));
        factory.setPretendMode(false);

        factory.addCriterion(BranchCoverage.getInstance());

        this.instrumenter = factory.getInstrumenter();
    }

    @Override
    protected void tearDown() {
        this.instrumenter = null;
    }

    public void testBranchCoverage() {
        String srcPath = SOURCE + "BranchCoverage/branchCoverage.cob";
        File source = new File(srcPath);
        String targetPath = TARGET + "BranchCoverage/branchCoverage.cob";
        File target = new File(targetPath);
        Date dateBefore = new Date();
        TestSessionContainer testSessionContainer = null;
        Collection<SourceTargetContainer> container = Collections.
        <SourceTargetContainer>singleton(
                new SourceTargetContainer(source, target));

        try {
          testSessionContainer = this.instrumenter.instrument(source.getParentFile(),
                  target.getParentFile(),
                  container,
                  this.builder,
                  UtilsForTestingCobol.getInstrumenterDirectives());
        } catch (InstrumentationException e) {
            handleException(e);
        }

        Assert.assertTrue(target.exists());
        simpleTestSessionContainerTests(dateBefore, testSessionContainer, this.instrumenter);
        Assert.assertTrue(isCompileableCobol(target));

        try {
            testSessionContainer.save(SESSION_CONTAINER);
        } catch (Exception e) {
            handleException(e);
        }
        Assert.assertTrue(new File(SESSION_CONTAINER).exists());
    }
}
