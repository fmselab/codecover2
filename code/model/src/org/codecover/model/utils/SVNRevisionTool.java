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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.codecover.CodeCoverInfo;
import org.codecover.model.utils.file.FileTool;
import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.wc.SVNClientManager;
import org.tmatesoft.svn.core.wc.SVNInfo;
import org.tmatesoft.svn.core.wc.SVNRevision;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: SVNRevisionTool.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @see #main(String[])
 */
public class SVNRevisionTool
{
    private static final int PARAM_COUNT = 2;

    private static final String USAGE = "Usage: SVNRevision <RootDir> <CodeCoverInfo>";

    /** looks like <code>String REVISION = "1000";</code>*/
    private static final Pattern REVISION_PATTERN = Pattern.compile("String REVISION = \"([\\d\\?]*)\";");

    /** looks like <code>String DATE = "Wed Jun 13 07:01:56 CEST 2007";</code>*/
    private static final Pattern DATE_PATTERN = Pattern.compile("String DATE = \"([\\w \\:\\?]*)\";");

    /**
     * This method is used to get SVN revision information while building CodeCover.<br>
     * <br>
     * Therefore the SVN revision and date are parsed, using the <code>RootDir</code>.
     * Then the <code>CodeCoverInfo</code>, which will be a copy of the
     * {@link CodeCoverInfo}, will be updated by replacing the CodeCoverInfo.REVISION
     * and CodeCoverInfo.DATE in the source code by the revision and date got from
     * the svn working copy.  
     * 
     * @param args <code>{The path to the root directory,<br>
     * The file, which should be created}</code>
     * @throws IllegalArgumentException {@link #USAGE}.
     * @throws IOException While writing or reading the <code>CodeCoverInfo</code>
     * file.
     */
    public static void main(String[] args) throws IOException
    {
        if (args.length != PARAM_COUNT)
            {
                throw new IllegalArgumentException(USAGE);
            }

        String rootDirPath = args[0];
        File rootDir = new File(rootDirPath).getAbsoluteFile();
        String codeCoverInfoPath = args[1];
        File codeCoverInfoFile = new File(codeCoverInfoPath).getAbsoluteFile();

        String contentOfInfoFile = FileTool.getContentFromFile(codeCoverInfoFile);

        try {
            // get the revision of the working copy -> "svn update" needed before
            // WORKING will cause the SVNWCCClient to use the revision of the working
            // copy rather than checking the repository
            SVNRevision rev = SVNRevision.WORKING;

            SVNInfo info = SVNClientManager.newInstance().getWCClient().doInfo(rootDir, rev);
            String revision = Long.toString(info.getRevision().getNumber());
            String date = info.getCommittedDate().toString();

            // find and replace the REVISION = "1000"; by the correct revision
            Matcher revisionMatcher = REVISION_PATTERN.matcher(contentOfInfoFile);
            if (!revisionMatcher.find()) {
                throw new RuntimeException("could not find the constant REVISION in " + codeCoverInfoPath);
            }
            contentOfInfoFile = revisionMatcher.replaceFirst("String REVISION = \""+ revision + "\";");
    
            // find and replace the REVISION = "1000"; by the correct revision
            Matcher dateMatcher = DATE_PATTERN.matcher(contentOfInfoFile);
            if (!dateMatcher.find()) {
                throw new RuntimeException("could not find the constant DATE in " + codeCoverInfoPath);
            }
            contentOfInfoFile = dateMatcher.replaceFirst("String DATE = \""+ date + "\";");
        } catch (SVNException e) {
            // print and ignore error; we might be not in a svn working dir
            System.err.println(e);
        }
    
        // overwrite the CodeCoverInfoPath
        FileOutputStream fileOutputStream = new FileOutputStream(codeCoverInfoFile);
        OutputStreamWriter outputStreamWriter = new OutputStreamWriter(fileOutputStream, CodeCoverInfo.CODE_FILE_CHARSET);
        outputStreamWriter.write(contentOfInfoFile);
        outputStreamWriter.flush();
        outputStreamWriter.close();
    }
}
