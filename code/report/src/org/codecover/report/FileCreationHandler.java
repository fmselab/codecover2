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

package org.codecover.report;

import java.io.OutputStream;

import org.codecover.model.utils.Logger;
import org.codecover.report.exceptions.FileCreationException;

/**
 * The report component uses the <code>FileCreationHandler</code> for creating
 * the auxiliary files of a multiple-files-report. Auxiliary files are saved in
 * the output directory of the report. Thus it is recommended to implement
 * <code>FileCreationHandler</code> to create files only in a specific
 * directory, the output directory. A good example is the
 * <code>{@link DefaultFileCreationHandler DefaultFileCreationHandler}</code>
 * which defines a path in its constructor all created files are saved in.
 * <p>
 * A <code>FileCreationHandler</code> is responsible for creating a file which
 * was requested by calling the method <code>createFile</code>. The parameters
 * of this method are the path to the file and its MIME content-type
 * (e.g. text/plain for plain text). The method returns an
 * <code>OutputStream</code> which can be used to write to the newly created
 * file.
 *
 * @author Johannes Langauf
 * @author Michael Starzmann
 * @author Robert Hanussek
 * @version 1.0 ($Id: FileCreationHandler.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface FileCreationHandler {

    /**
     * The name-separator used to separate filenames or names of directories in
     * filepaths.
     */
    public final static String SEPARATOR = "/";

    /**
     * Sets the logger. If not set, no logging takes place.
     */
    public void setLogger(Logger logger);
    
    /**
     * Creates a file.
     *
     * @param filepath     the path to the file which is to be created
     * @param contentType  the MIME content type of the file which is to be
     *                     created
     *
     * @throws FileCreationException  if method can not be performed,
     *                                e.g. filepath not valid
     */
    public OutputStream createFile(String filepath, String contentType)
    throws FileCreationException;
}
