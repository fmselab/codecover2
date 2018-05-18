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

import java.io.File;

import junit.framework.TestCase;

import org.codecover.model.utils.Logger;
import org.codecover.report.exceptions.FileCreationException;

/**
 * Tests the class <code>org.codecover.report.DefaultFileCreationHandler</code>.
 *
 * @author Robert Hanussek
 * @version 1.0 ($Id: DefaultFileCreationHandlerTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DefaultFileCreationHandlerTest extends TestCase {

    private File directory;

    private static final String FILEPATH_SINGLEFILE = "atestfile";
    private static final String FILEPATH_FILEINSUBDIR
            = "subdir" + FileCreationHandler.SEPARATOR + "afileinsubdir";

    protected void setUp() throws Exception {
        this.directory = new File(System.getProperty("java.io.tmpdir"),
            "testCreateFile" + (new Long(System.nanoTime())).toString());
        if(this.directory.exists()) {
            throw new Exception("Temporary output directory already exists" +
                    " (" + this.directory.getAbsolutePath() + ").");
        }
    }

    protected void tearDown() throws Exception {
        this.directory.delete();
    }

    /**
     * Tests the method <code>createFile</code> by creating a single file and
     * checking if the file exists after the method call.
     */
    public void testCreateFile_singleFile() throws FileCreationException {
        DefaultFileCreationHandler defFCHandler
            = new DefaultFileCreationHandler( this.directory.getAbsolutePath(),
                true, Logger.NULL);
        defFCHandler.createFile(FILEPATH_SINGLEFILE, "is/ignored");
        // check for existence of file
        assertTrue((new File(this.directory, FILEPATH_SINGLEFILE)).exists());
        // clean up
        (new File(this.directory, FILEPATH_SINGLEFILE)).delete();
    }

    /**
     * Tests the method <code>createFile</code> by creating a file in a
     * subdirectory and checking if the file exists after the method call.
     */
    public void testCreateFile_FileInSubdir() throws FileCreationException {
        DefaultFileCreationHandler defFCHandler
            = new DefaultFileCreationHandler( this.directory.getAbsolutePath(),
                true, Logger.NULL);
        defFCHandler.createFile(FILEPATH_FILEINSUBDIR, "is/ignored");
        // check for existence of file
        File newFile = new File(this.directory,
                FILEPATH_FILEINSUBDIR.replace(FileCreationHandler.SEPARATOR,
                        File.separator));
        assertTrue(newFile.exists());
        // clean up
        for(    ;
                !newFile.equals(this.directory);
                newFile = newFile.getParentFile()) {
            if(!newFile.delete())
                System.err.println("Test file/directory could not be deleted.");
        }
    }
}
