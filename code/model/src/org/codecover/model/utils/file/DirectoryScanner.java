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

package org.codecover.model.utils.file;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.TreeSet;

import org.codecover.model.utils.file.listener.FileFoundListener;

/**
 * This {@link DirectoryScanner} is a tool to scan a directory:
 * {@link #scan(File, FileFoundListener)}, and divide all files found into
 * <code>included</code>, <code>not included</code>, and <code>ignored</code>.<br>
 * <br>
 * This selection is done by using patterns and extensions:<br>
 * A File is ignored, if it fits one of the <code>ignore patterns</code>
 * ({@link #addIgnorePattern(String)}).<br>
 * A {@link File} is included, if it matches at least one
 * <code>include pattern</code> ({@link #addIncludePattern(String)}), is
 * accepted by the {@link FileFilter} ({@link #setFileFilter(FileFilter)}),
 * matches none of the <code>exclude patterns</code> ({@link #addExcludePattern(String)})
 * and none of the <code>ignore patterns</code> ({@link #addIgnorePattern(String)}).<br>
 * A {@link File} is not included, if it matches no <code>include pattern</code>
 * ({@link #addIncludePattern(String)}),
 * is rejected by the {@link FileFilter} ({@link #setFileFilter(FileFilter)}),
 * or machtes one of the <code>exclude patterns</code> ({@link #addExcludePattern(String)})
 * and none of the <code>ignore patterns</code> ({@link #addIgnorePattern(String)}).<br>
 * These patterns have the syntax, <b>apache ant</b> is using (<a
 * href="http://ant.apache.org/manual/dirtasks.html#patterns">Description</a>):
 * <ul>
 * <li>? matches one character</li>
 * <li>* matches zero or more characters</li>
 * <li>** matches zero or more 'directories' in a path</li>
 * </ul>
 * <br>
 * The patterns are only used for files. Directories are only partly matched
 * against the pattern ({@link AntPathMatcher#matchStart(String, String)}).<br>
 * The feature of the {@link FileFilter} is added cause source files often require a
 * specific file extension&mdash;e.g. <code>java</code> under Java&mdash;or other
 * specialities of the programming language. To honor these requirements, you
 * can set a {@link FileFilter}, which is asked, whether to accept a file or not
 * ({@link FileFilter#accept(File)}). If the {@link FileFilter} is never set, or
 * set to <code>null</code>, it is ignored.<br>
 * <hr>
 * An example Usage:
 * <pre>
 * try {
 *   DirectoryScanner scanner = new DirectoryScanner();
 *   scanner.addIgnorePatterns(DefaultIgnores.getIgnorePatterns());
 *   // includes any java source files
 *   scanner.addIncludePattern("**\\*.java");
 *   // except its ending with Test
 *   scanner.addExcludePattern("**\\*Test.java");
 *   IncludedFileFoundListener listener = new IncludedFileFoundListener();
 *   scanner.scan(new File("src"), listener);
 *   Collection<File> includedFiles = listener.getIncludedFiles();
 *   // [..]
 * } catch (IOException e) {
 *   e.printStackTrace();
 * }
 * </pre>
 * 
 * @see AntPathMatcher for some examples of the patterns.
 * @see FileFilter
 * @see DefaultIgnores
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: DirectoryScanner.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DirectoryScanner {

    private String rootDirPath;

    private int rootDirPrefix;

    private final AntPathMatcher pathMatcher;

    private final TreeSet<String> includePatterns;

    private final TreeSet<String> excludePatterns;

    private final TreeSet<String> ignorePatterns;

    private FileFoundListener fileFoundListener;

    private FileFilter fileFilterToAsk;

    private boolean considerNotIncluded;

    private boolean isEverythingIncluded;

    /**
     * The constructor for initializations.
     */
    public DirectoryScanner() {
        this.pathMatcher = new AntPathMatcher();
        this.includePatterns = new TreeSet<String>();
        this.excludePatterns = new TreeSet<String>();
        this.ignorePatterns = new TreeSet<String>();
        this.fileFilterToAsk = null;
    }

    /**
     * Adds a pattern, included files can match.
     * 
     * @param includePattern A <code>include pattern</code>.
     */
    public void addIncludePattern(String includePattern) {
        this.includePatterns.add(this.pathMatcher.normalizePattern(includePattern));
    }

    /**
     * Adds patterns, included files can match.
     * 
     * @param includePatternArray Some <code>include patterns</code>.
     */
    public void addIncludePatterns(String[] includePatternArray) {
        for (String thisPattern : includePatternArray) {
            addIncludePattern(thisPattern);
        }
    }

    /**
     * @return The number of include patterns added. 
     */
    public int getIncludePatternCount() {
        return this.includePatterns.size();
    }

    /**
     * Adds a pattern, included files must not match.
     * 
     * @param excludePattern A <code>exclude pattern</code>.
     */
    public void addExcludePattern(String excludePattern) {
        this.excludePatterns.add(this.pathMatcher.normalizePattern(excludePattern));
    }

    /**
     * Adds patterns, included files must not match.
     * 
     * @param excludePatternArray Some <code>exclude patterns</code>.
     */
    public void addExcludePatterns(String[] excludePatternArray) {
        for (String thisPattern : excludePatternArray) {
            addExcludePattern(thisPattern);
        }
    }

    /**
     * Adds a pattern for files totally ignored.
     * 
     * @param ignorePattern A <code>ignore pattern</code>.
     */
    public void addIgnorePattern(String ignorePattern) {
        this.ignorePatterns.add(this.pathMatcher.normalizePattern(ignorePattern));
    }

    /**
     * Adds patterns for files totally ignored.
     * 
     * @param ignorePatternArray Some <code>ignore patterns</code>.
     */
    public void addIgnorePatterns(String[] ignorePatternArray) {
        for (String thisPattern : ignorePatternArray) {
            addIgnorePattern(thisPattern);
        }
    }

    /**
     * Adds a possible {@link FileFilter}, which is asked, if a file is
     * accepted.<br>
     * <br>
     * This {@link FileFilter} is ignored, if it is null.
     * 
     * @param fileFilter
     *          A {@link FileFilter} to aks, whether a file is accepted.
     */
    public void setFileFilter(FileFilter fileFilter) {
        this.fileFilterToAsk = fileFilter;
    }

    /**
     * Starts the scan for a directory.<br>
     * <br>
     * The set {@link FileFoundListener} is informed of included and not
     * included {@link File}s.
     * 
     * @param rootDir
     *            The directory to scan.
     * @param fileFoundListenerToUse
     *            The {@link FileFoundListener} to use.
     * 
     * @throws IOException
     *             If there is a Exception resolving File names:
     *             {@link File#getCanonicalPath()}
     */
    public synchronized void scan(File rootDir, FileFoundListener fileFoundListenerToUse)
        throws IOException {
        this.rootDirPath = rootDir.getCanonicalPath() + File.separatorChar;
        File newRootDir = new File(this.rootDirPath); 
        this.rootDirPrefix = this.rootDirPath.length();
        this.fileFoundListener = fileFoundListenerToUse;
        this.considerNotIncluded = this.fileFoundListener.considerNotIncluded();

        this.isEverythingIncluded = false;
        if (this.includePatterns.contains("**")) {
            this.isEverythingIncluded = true;
        }

        // if nothing is included -> skip
        if (this.includePatterns.isEmpty() || !newRootDir.exists()) {
            return;
        }
        scan(newRootDir, this.rootDirPath);
        // cleares the cached tonkenized patterns
        this.pathMatcher.clearCache();
    }

    /**
     * Recursive method for scanning.
     */
    private void scan(File dirToScan, final String dirPath) throws IOException {
        File[] filesOfDir = dirToScan.listFiles();
        if (filesOfDir == null) {
            throw new IOException("could not list files of dir: " + 
                    dirPath);
        }

        FileListing: for (File thisFile : filesOfDir) {
            String newFilesPath = thisFile.getCanonicalPath();
            String realtivePath = newFilesPath.substring(this.rootDirPrefix);
            // normalize this path, replcae \ by /
            realtivePath = this.pathMatcher.normalizePath(realtivePath);
            thisFile = new File(newFilesPath);

            if (isIgnored(realtivePath)) {
                // this file or folder is ignored, we needn't consider it
                continue FileListing;
            }

            // assert: this file is not ignored
            if (thisFile.isDirectory()) {
                // - we have to start the recursion, if we want to collect the not
                // included files or if this folder might contain included files
                // - we cannot check for exclude patterns, because they are only
                // considered for files and if we exclude "src/org", we
                // attempted to exclude the file "src/org" and not a directory
                if (this.considerNotIncluded || isIncluded(realtivePath, false)) {
                    scan(thisFile, newFilesPath);
                }
            } else {
                if (isIncluded(realtivePath, true) && isAccepted(thisFile) &&
                        !isExcluded(realtivePath)) {
                    // an included file
                    this.fileFoundListener.includedFileFound(thisFile, realtivePath);
                } else if (this.considerNotIncluded) {
                    // a not included file
                    this.fileFoundListener.notIncludedFileFound(thisFile, realtivePath);
                }
            }
        } // endif FileListing: for
    }

    private boolean isIncluded(String matchString, boolean fullMatchReq) {
        if (this.isEverythingIncluded) {
            return true;
        }
        for (String includePattern : this.includePatterns) {
            if (this.pathMatcher.doMatch(includePattern, matchString, fullMatchReq)) {
                return true;
            }
        }
        return false;
    }

    private boolean isAccepted(File file) {
        return this.fileFilterToAsk == null || this.fileFilterToAsk.accept(file); 
    }

    private boolean isExcluded(String matchString) {
        for (String excludePattern : this.excludePatterns) {
            if (this.pathMatcher.doMatch(excludePattern, matchString, true)) {
                return true;
            }
        }

        return false;
    }

    private boolean isIgnored(String matchString) {
        for (String ignorePattern : this.ignorePatterns) {
            if (this.pathMatcher.doMatch(ignorePattern, matchString, true)) {
                return true;
            }
        }

        return false;
    }
}
