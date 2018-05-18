/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This file may be used, modifies and redistributed     *
 * under the terms of either the Eclipse Public License v1.0 which            *
 * accompanies this distribution and is available at                          *
 * http://www.eclipse.org/legal/epl-v10.html or the MIT license, available at *
 * http://www.opensource.org/licenses/mit-license.php                         *
 ******************************************************************************/

package org.codecover.model.utils;

/**
 * ProgressHandler should be implemented by classes which want to be informed
 * of
 * the progress of an operation like a report generation or an instrumentation.
 * In regular intervals the method setProgess will be called.
 *
 * @author Steffen Kieß
 * @version 1.0 ($Id: ProgressHandler.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface ProgressHandler {
    /**
     * A progress handler that simply ignores all seen progress.
     */
    public static final ProgressHandler NULL = new ProgressHandler() {
            public void setProgress(float progress) {
                // Do nothing here.
            }
        };
    
    /**
     * This method will be called in regular intervals. Implementors
     * of this method have to be aware that this method will not be called in
     * the GUI thread.
     *
     * @param progress a float between {@code 0.0} and {@code 1.0} (including)
     *                 representing a progess between 0% and 100%. 
     */
    public void setProgress(float progress);
}
