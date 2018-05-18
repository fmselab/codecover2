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

package org.codecover.instrumentation;

import java.nio.charset.Charset;
import java.nio.charset.IllegalCharsetNameException;
import java.util.Set;
import java.util.TreeSet;

import org.codecover.instrumentation.exceptions.FactoryMisconfigurationException;
import org.codecover.model.utils.ProgressHandler;
import org.codecover.model.utils.criteria.Criterion;

/**
 * This is an implementation of {@link InstrumenterFactory}.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: DefaultInstrumenterFactory.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @see InstrumenterFactory for more details.
 */
public class DefaultInstrumenterFactory implements InstrumenterFactory {
    /**
     * A {@link InstrumenterDescriptor} that contains information about the
     * {@link Instrumenter}.<br>
     * <br>
     * The {@link InstrumenterDescriptor#getInstrumenter()} is used to get the
     * unconfigured instrumenter.
     */
    private InstrumenterDescriptor descriptor = null;

    /** The {@link Charset}, which is used for the source files. */
    private Charset charset = null;

    /** A set with all criteria this instrumeter supports. */
    private Set<Criterion> criteria = null;

    private boolean pretend = false;

    private boolean verbose = false;

    private ProgressHandler progressHandler;

    /**
     * Constructs a new {@link DefaultInstrumenterFactory}.<br>
     * <br>
     * {@link #reset()} is called to set all fields to null.
     */
    public DefaultInstrumenterFactory() {
        reset();
    }

    public void setDescriptor(InstrumenterDescriptor descriptor) {
        this.descriptor = descriptor;
    }

    public void setCharset(Charset charset) {
        this.charset = charset;
    }

    public void setCharset(String charsetName)
            throws IllegalCharsetNameException {
        this.charset = Charset.forName(charsetName);
    }

    public void setPretendMode(boolean pretend) {
        this.pretend = pretend;
    }

    public void setVerboseMode(boolean verbose) {
        this.verbose = verbose;
    }

    public void setProgressHandler (ProgressHandler progressHandler) {
        this.progressHandler = progressHandler;
    }
    
    public void addCriterion(Criterion criterion) {
        this.criteria.add(criterion);
    }

    public void addSupportedCriteria(InstrumenterDescriptor descriptorToUse) {
        for (Criterion supportedCriterion : descriptorToUse.getSupportedCriteria()) {
            this.criteria.add(supportedCriterion);
        }
    }

    public Instrumenter getInstrumenter()
            throws FactoryMisconfigurationException {
        // check, if every necessary property is set
        if (this.descriptor == null) {
            throw new FactoryMisconfigurationException("descriptor is unset");
        }
        if (this.charset == null) {
            if (this.descriptor.getDefaultCharset() == null) {
                throw new FactoryMisconfigurationException("charset is unset");
            }

            this.charset = this.descriptor.getDefaultCharset();
        }

        // check support of set criteria
        for (Criterion criterion : this.criteria) {
            if (!this.descriptor.isCriterionSupported(criterion)) {
                throw new FactoryMisconfigurationException(
                        "instrumenter does not support criterion: \""
                                + criterion.getName() + "\"");
            }
        }

        // assert: every check negative
        // create and configure instrumenter
        Instrumenter instrumenter = this.descriptor.getInstrumenter();
        if (instrumenter == null) {
            throw new FactoryMisconfigurationException(
                    "created instrumenter is null");
        }

        instrumenter.setCharset(this.charset);
        instrumenter.setPretendMode(this.pretend);
        instrumenter.setVerboseMode(this.verbose);
        if (this.progressHandler == null) {
            instrumenter.setProgressHandler(ProgressHandler.NULL);
        } else {
            instrumenter.setProgressHandler(this.progressHandler);
        }
        for (Criterion criterion : this.criteria) {
            instrumenter.addCriterion(criterion);
        }

        return instrumenter;
    }

    public void reset() {
        this.descriptor = null;
        this.charset = null;
        this.criteria = new TreeSet<Criterion>();
    }
}
