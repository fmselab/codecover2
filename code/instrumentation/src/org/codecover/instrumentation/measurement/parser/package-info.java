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

/**
 * This package contains a parser for a coverage log file.<br>
 * <br>
 * This <code>CoverageLogParser</code> expects a <code>CoverageResultLog</code>
 * to be informed of the parsed Ids, counters and test case names. For example
 * this can be a <code>CoverageResultLogReader</code>.
 */
package org.codecover.instrumentation.measurement.parser;