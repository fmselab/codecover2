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

import org.codecover.instrumentation.exceptions.FactoryMisconfigurationException;
import org.codecover.model.utils.ProgressHandler;
import org.codecover.model.utils.criteria.Criterion;

/**
 * This is an interface to get a configured Instrumenter.<br>
 * <br>
 * First an {@link InstrumenterDescriptor} must be created. This can be done by
 * using the new method of a fitting {@link InstrumenterDescriptor} child class.
 * Another way is to use <code>PluginUtils.getExtensionObjects(pluginManager, logger, InstrumenterDescriptor.class)</code>.<br>
 * Using this {@link InstrumenterDescriptor}, the method
 * {@link #setDescriptor(InstrumenterDescriptor)} <b>must</b> be called.
 * Moreover the {@link #setCharset(Charset)} <b>must</b> be called if
 * {@link InstrumenterDescriptor#getDefaultCharset()} is <code>null</code>.<br>
 * After having set all properties, the {@link #getInstrumenter()} can be used
 * to get a configured {@link Instrumenter}. All other set options are optional
 * but certainly needed in most cases.<br>
 * The method {@link #reset()} is made, to reuse an instance of
 * {@link InstrumenterFactory} more than one time and reset the properties.
 * 
 * @see DefaultInstrumenterFactory
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: InstrumenterFactory.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface InstrumenterFactory {

    /**
     * @param descriptor
     *            The {@link InstrumenterDescriptor} that contains information
     *            about the {@link Instrumenter}.<br>
     *            The {@link InstrumenterDescriptor#getInstrumenter()} is used
     *            to get the unconfigured instrumenter.
     */
    public void setDescriptor(InstrumenterDescriptor descriptor);

    /**
     * @param charset
     *            The charset to use to encode the source files.<br>
     *            If not stated, the
     *            {@link InstrumenterDescriptor#getDefaultCharset()} is used.
     */
    public void setCharset(Charset charset);

    /**
     * @param charsetName
     *            The name of the charset to use to encode the source files.<br>
     *            If not stated, the
     *            {@link InstrumenterDescriptor#getDefaultCharset()} is used.
     * 
     * @throws IllegalCharsetNameException
     *             If no support for the named charset is available in this
     *             instance of the Java virtual machine.
     * @see Charset#forName(String)
     */
    public void setCharset(String charsetName)
            throws IllegalCharsetNameException;

    /**
     * Set the pretend mode of the instrumenter.
     * 
     * @param pretend
     *            true &rarr; <i>pretend mode</i> activated; false &rarr;
     *            <i>pretend mode</i> deactivated.
     */
    public void setPretendMode(boolean pretend);

    /**
     * Set the progress handler.
     * 
     * @param progressHandler
     *            A {@link ProgressHandler} used to inform the caller of
     *            the instrumenter of the progress of the instrumentation.
     */
    void setProgressHandler (ProgressHandler progressHandler);
    
    /**
     * Set the verbose mode of the instrumenter.
     * 
     * @param verbose
     *            true &rarr; <i>verbose mode</i> activated; false &rarr;
     *            <i>verbose mode</i> deactivated.
     */
    public void setVerboseMode(boolean verbose);

    /**
     * @param criterion
     *            The coverage criteria which should be instrumented by the
     *            Instrumenter.
     */
    public void addCriterion(Criterion criterion);

    /**
     * Adds all criteria, the instrumenter supports.
     * 
     * @param descriptor
     *            An {@link InstrumenterDescriptor} Containing a Set of
     *            supported criteria:
     *            {@link InstrumenterDescriptor#getSupportedCriteria()}.
     */
    public void addSupportedCriteria(InstrumenterDescriptor descriptor);

    /**
     * This method returns the {@link Instrumenter} which belongs to the
     * {@link InstrumenterDescriptor}, taking into account the specified Charset
     * and list of criteria.
     * 
     * @return The configured {@link Instrumenter}.
     * 
     * @throws FactoryMisconfigurationException
     *             If there where problems with the configuration&mdash;e.g.
     *             unset properties or such, that are not supported by the
     *             instrumenter.
     */
    public Instrumenter getInstrumenter()
            throws FactoryMisconfigurationException;

    /**
     * This method resets the factory, all fields need to bet set again.
     * 
     */
    public void reset();
}