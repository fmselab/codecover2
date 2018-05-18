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
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStream;

import org.codecover.model.utils.Logger;
import org.codecover.report.exceptions.FileCreationException;

/**
 * Creates files in the file system.
 * <p>
 * An object is instantiated by using the constructor
 * {@link #DefaultFileCreationHandler(String, boolean, Logger)} whereas the
 * given path represents the path to the directory files are created in (via the
 * method <code>createFile</code>). All paths passed to
 * {@link #createFile(String, String)} are treated as being relative to the
 * directory set by the constructor. Passing an absolute path to
 * {@link #createFile(String, String)} that doesn't point inside the directory
 * set by the constructor is not allowed.
 * </p>
 *
 * @author Robert Hanussek
 * @version 1.0 ($Id: DefaultFileCreationHandler.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DefaultFileCreationHandler implements FileCreationHandler {

    /**
     * The directory files are created in (via the method
     * <code>createFile</code>).
     */
    private File directory;

    /**
     * The path to the directory which the user requested files to be created
     * in.
     */
    private final String directoryPath;

    /**
     * <code>true</code> if the user requested creation of the directory it (if
     * doesn't already exist), <code>false</code> otherwise.
     */
    private final boolean directoryCreationRequested;

    private Logger logger;

    /**
     * Sets the logger, mustn't be <code>null</code>, set to
     * <code>Logger.NULL</code> if you don't need a logger.
     * 
     * @param logger    the logger
     * 
     * @throws NullPointerException if the given logger is <code>null</code>
     */
    public void setLogger(Logger logger) {
        if(logger == null) {
            throw new NullPointerException("logger mustn't be null");
        }
        this.logger = logger;
    }

    /**
     * Constructs a <code>FileCreationHandler</code> which creates files in the
     * file system.
     * 
     * @param dirpath   the path to the directory files are created in (via the
     *                  method <code>createFile</code>)
     * @param createDir if <code>true</code> and the directory does not already
     *                  exist, it is created
     * @param logger    the logger, mustn't be <code>null</code>, set to
     *                  <code>Logger.NULL</code> if you don't need a logger
     * 
     * @throws NullPointerException     if the given logger is <code>null</code>
     */
    public DefaultFileCreationHandler(String dirpath, boolean createDir,
            Logger logger) throws NullPointerException {
        this.setLogger(logger);
        this.directoryPath = dirpath;
        this.directoryCreationRequested = createDir;
    }

    /**
     * Creates a file in the file system and returns an
     * <code>OutputStream</code> to write to the created file. If the file
     * already exists and OutputStream to the existing file is returned.
     *
     * @param filepath     the path to the file to be created in the file system
     * @param contentType  the MIME content type of the file to be created
     * 
     * @throws FileCreationException    if the file could not be created because
     *                                  of a malformed filepath or missing
     *                                  access permissions<br>
     *                                  <em>or</em><br>
     *                                  (can only happen at the first call to
     *                                  this method) if the path to the
     *                                  directory (which was specified at the
     *                                  constructor call) doesn't point to a
     *                                  directory or access to the given path is
     *                                  denied or the directory does not already
     *                                  exist or the directory could not be
     *                                  created
     */
    public OutputStream createFile(String filepath, String contentType)
    throws FileCreationException {
        String[] pathSegs;
        String newDirsPath;
        File newDirs;
        String newFilePath;
        File newFile;
        FileOutputStream fos;

        this.logger.debug("Creating requested file \""+ filepath +"\"...");

        // create directory if not already done
        if(this.directory == null) {
            // may throw FileCreationException
            this.directory = DefaultFileCreationHandler.createDirectory(
                    this.directoryPath,
                    this.directoryCreationRequested);
        }

        // split the given filepath into segments
        pathSegs = explodePath(filepath);   // may throw FileCreationException

        /*
         * create directories for every segment but the last one (which is the
         * file to create
         */
        if(pathSegs.length > 1) {
            newDirsPath = implodePath(pathSegs, pathSegs.length-1);
            newDirs = new File(this.directory.getAbsolutePath(), newDirsPath);
            try {
                if(!newDirs.mkdirs()) {
                    throw new FileCreationException("Directory hierarchy '"
                            + newDirsPath + "' could not be created.");
                }
            } catch(SecurityException e) {
                throw new FileCreationException(e);
            }
        }

        // create the file
        newFilePath = implodePath(pathSegs, pathSegs.length);
        newFile = new File(this.directory.getAbsolutePath(), newFilePath);
        try {
            fos = new FileOutputStream(newFile);
        } catch (FileNotFoundException e) {
            throw new FileCreationException(e);
        }

        this.logger.debug("Done creating requested file.");
        return fos;
    }

    /**
     * Checks if the given path points to a directory or creates it if
     * requested.
     * 
     * @param dirpath   the path of the directory to check or create
     * @param createDir if <code>true</code>, the directory is created if it
     *                  doesn't exist
     * 
     * @return  the path to the checked or created directory
     * 
     * @throws FileCreationException    if the given path doesn't point to a
     *                                  directory or access to the given path
     *                                  is denied or the directory does not
     *                                  already exist or the directory could not
     *                                  be created
     */
    private static File createDirectory(String dirpath, boolean createDir)
    throws FileCreationException {
        File directory = new File(dirpath);
        try {
            if(directory.exists()) {
                if(!directory.isDirectory()) {
                    directory = null;
                    throw new FileCreationException("Path '" + dirpath
                            + "' is not a directory.");
                }
            } else if(!directory.exists()) {
                if(createDir) {
                    if(!directory.mkdir()) {
                        directory = null;
                        throw new FileCreationException("Directory'" + dirpath
                                + "' doesn't exist and could not be created.");
                    }
                } else {
                    directory = null;
                    throw new FileCreationException("Directory'" + dirpath
                            + "' does not exist.");
                }
            }
        } catch(SecurityException e) {
            directory = null;
            throw new FileCreationException(e);
        }

        return directory;
    }

    /**
     * Splits the given path around matches of
     * <code>FileCreationHandler.SEPARATOR</code>. The path is also checked
     * for occurrences of <code>File.separator</code> and empty path
     * segments. If either of them is found an exception is thrown.
     *
     * @param path  the path to split
     *
     * @return      an array with the segments of the given path
     *
     * @throws FileCreationException
     * if the path contains <code>File.separator</code> or empty segments
     */
    private static String[] explodePath(String path)
    throws FileCreationException {
        String[] pathSegs
                = path.split("\\Q"+FileCreationHandler.SEPARATOR+"\\E");
        // check if filepath contains a path separator of the current platform
        if(!File.separator.equals(FileCreationHandler.SEPARATOR)
            && path.contains(File.separator)) {
                throw new FileCreationException("The path contains a path" +
                        " separator which differs from the path seperator used" +
                        " by DefaultFileCreationHandler (" +
                        FileCreationHandler.SEPARATOR + "). This could" +
                        " raise unexpected results.");
        }
        // check for empty path segments
        if(path.endsWith(FileCreationHandler.SEPARATOR)) {
            throw new FileCreationException("Last segment of path must be a" +
            " filename.");
        }
        for(String pathSeg : pathSegs) {
            if(pathSeg.length() == 0) {
                throw new FileCreationException("Path segment with length 0" +
                        " not allowed.");
            }
        }
        return pathSegs;
    }

    /**
     * Joins the given path segments to a path with
     * <code>File.separator</code> between each segment. The maximum
     * number of segments to join can be limited.
     *
     * @param pathSegs  the path segments to join
     * @param limit     limits the maximum number of segments to join
     * @return          the joined path
     */
    private static String implodePath(String[] pathSegs, int limit) {
        StringBuilder path = new StringBuilder();
        int pathSegNbr = 0;
        for(    pathSegNbr=0;
                pathSegNbr < pathSegs.length-1 && pathSegNbr < limit-1;
                pathSegNbr++) {
            path.append(pathSegs[pathSegNbr] + File.separator);
        }
        if(pathSegNbr < pathSegs.length && pathSegNbr < limit) {
            path.append(pathSegs[pathSegNbr]);
        }
        return path.toString();
    }

}
