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

package org.codecover.batch.commands;

import java.io.File;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Vector;

import org.codecover.batch.BatchLogger;
import org.codecover.batch.CommandLine;
import org.codecover.batch.HelpFormatter;
import org.codecover.batch.Option;
import org.codecover.batch.OptionSet;
import org.codecover.batch.Options;
import org.codecover.batch.SimpleCommand;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;

/**
 * InfoCommand
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: InfoCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InfoCommand extends SimpleCommand {
    private static final String NAME = "name";

    private static final String COMMENT = "comment";

    private static final String DATE = "date";

    private static final DateFormat CUSTOM_FORMAT=new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z (EEE, dd MMM yyyy)");

    private static final int FORMATTED_DATE_STRING_LENGTH =CUSTOM_FORMAT.format(new Date())
    .length();

    private static final InfoCommand instance = new InfoCommand();

    /**
     * Gets an instance of this class
     * 
     * @return the instance of the class
     */
    public static InfoCommand getInstance() {
        return instance;
    }

    private InfoCommand() {
        super(null,
              "info",
              "shows information about a session container",
              new OptionSet(new Option[] {(Options.container)},
                            new Option[] {(Options.testSession),
                                          (Options.showTestCase)}));
    }

    @Override
    protected int run(CommandLine cl, BatchLogger logger, org.codecover.model.extensions.PluginManager pluginManager) {
        StringBuilder output = new StringBuilder();

        // Create the testSessionContainer and fill it with the data
        // from the given file
        TestSessionContainer testSessionContainer = null;
        String containerLocation = cl.getOptionValue(Options.container);
        try {
            MASTBuilder builder = new MASTBuilder(logger);
            testSessionContainer = TestSessionContainer.loadInfoOnly(pluginManager,
                                                                     logger,
                                                                     builder,
                                                                     containerLocation);
        } catch (FileLoadException e) {
            logger.fatal("An error occured during loading", e);
        }

        if (testSessionContainer == null) {
            logger.fatal("testSessionContainer == null");
        }

        List<TestSession> sessionList = new Vector<TestSession>();
        List<String> notFoundSessionNameList = new Vector<String>();

        if (cl.hasOption(Options.testSession)) {
            List<String> givenSessionNames = cl.getOptionValues(Options.testSession);

            for (String givenSessionName : givenSessionNames) {

                TestSession testSession = testSessionContainer.getTestSessionWithName(givenSessionName);

                if (testSession != null) {
                    sessionList.add(testSession);
                } else {
                    // For later usage add the session names
                    // to a list, which do not correspond
                    // to a test session in the current
                    // testsessioncontainer.
                    notFoundSessionNameList.add(givenSessionName);
                }
            }
        } else {
            sessionList.addAll(testSessionContainer.getTestSessions());
        }

        output.append(HelpFormatter.getDividerWithLength(75, '-'));
        output.append("\n");
        output.append("test session container: "
                + new File(containerLocation).getName());
        output.append("\n");

        if (cl.hasOption(Options.showTestCase)) {
            printDetailedTestSessionList(cl, output, sessionList);
        } else {
            List<String> names = new Vector<String>();
            List<String> comments = new Vector<String>();
            List<Date> dates = new Vector<Date>();

            for (TestSession testSession : sessionList) {
                names.add(testSession.getName());
                comments.add(testSession.getComment());
                dates.add(testSession.getDate());
            }

            printList(cl, output, "test sessions:", names, comments, dates);
        }
        output.append(HelpFormatter.getDividerWithLength(75, '-'));
        output.append("\n");

        // We should not use the logger here since the logger will write to
        // error output.
        System.out.print(output.toString());

        return 0;
    }

    /**
     * Creates a string of the following kind, if verbose
     * <p>
     * <code>
     * headerText:<br>
     * name | date | comment <br>
     * ---------------------
     * </code>
     * <p>
     * or, if not verbose
     * <p>
     * <code>
     * headerText:<br>
     * name | date<br>
     * -----------
     * </code>
     * 
     * @param headerText
     *            the header text
     * @param maxNameLength
     *            the maximum length of a name int the whole list, for cosmetic
     *            purposes
     * @param maxCommentLength
     *            the maximum length of a comment in the whole list, for
     *            cosmetic purposes
     * @return the created string, as shown above
     */
    private String createListHeader(CommandLine cl, String headerText,
            int maxNameLength, int maxCommentLength) {
        String header = "\n" + headerText + "\n" + NAME;

        for (int i = 0; i < maxNameLength - NAME.length(); i++) {
            header += " ";

        }
        header += " | ";

        header += DATE;

        for (int i = 0; i < FORMATTED_DATE_STRING_LENGTH - DATE.length(); i++) {
            header += " ";
        }

        if (isVerbose(cl)) {
            header += " | ";

            header += COMMENT;
        }

        header += "\n" + HelpFormatter.getDividerWithLength(75, '-') + "\n";
        

        return header;
    }

    /**
     * Prints a message of the following kind, if verbose
     * <p>
     * <code>
     * test session name:    foo<br>
     * test session date:    bar<br>
     * test session comment: foobar<br>
     * <p>
     * test cases: <br>
     * name | date | comment <br>
     * --------------------- <br>
     * fooo | baar | foobar  
     * </code>
     * <p>
     * or, if not verbose
     * <p>
     * <code>
     * test session name:    foo<br>
     * test session date:    bar<br>
     * test session comment: foobar<br>
     * <p>
     * test cases: <br>
     * name | date<br>
     * -----------<br>
     * fooo | baar  
     * </code>
     * 
     * @param sb
     *            the stream on which is written
     * @param testSessions
     *            the list of {@link TestSession}s to be written
     */
    private void printDetailedTestSessionList(CommandLine cl, StringBuilder sb,
            List<TestSession> testSessions) {

        for (TestSession testSession : testSessions) {
            sb.append(HelpFormatter.getDividerWithLength(75, '='));
            sb.append("\ntest session name:    " + testSession.getName());
            sb.append("\ntest session date:    "
                    + CUSTOM_FORMAT.format(testSession.getDate()));
            sb.append("\ntest session comment: " + testSession.getComment()
                    + "\n");

            List<String> names = new Vector<String>();
            List<String> comments = new Vector<String>();
            List<Date> dates = new Vector<Date>();

            for (TestCase testCase : testSession.getTestCases()) {
                names.add(testCase.getName());
                comments.add(testCase.getComment());
                dates.add(testCase.getDate());
            }

            printList(cl, sb, "test cases:", names, comments, dates);
        }
    }

    /**
     * Prints a message of the following kind, if verbose
     * <p>
     * <code>
     * headerText: <br>
     * name | date | comment <br>
     * --------------------- <br>
     * fooo | baar | foobar  
     * </code>
     * <p>
     * or, if not verbose
     * <p>
     * <code>
     * headerText: <br>
     * name | date<br>
     * -----------<br>
     * fooo | baar  
     * </code>
     * <p>
     * The lists of names, comments and dates must be the same size
     * 
     * @param sb
     *            the stream on which is written
     * @param testSessions
     *            the list of {@link TestSession}s to be written
     * @param headerText
     *            the header text of the list
     * @param names
     *            the list of names
     * @param comments
     *            the list of comments
     * @param dates
     *            the list of dates
     */
    private void printList(CommandLine cl, StringBuilder sb, String headerText,
            List<String> names, List<String> comments, List<Date> dates) {

        if (names.size() != comments.size() || comments.size() != dates.size()) {
            throw new IllegalArgumentException("List sizes must be equal");
        }

        int maxNameLength = 0;
        int maxCommentLength = 0;

        for (int i = 0; i < names.size(); i++) {
            maxNameLength = Math.max(maxNameLength, names.get(i).length());
            maxCommentLength = Math.max(maxCommentLength, comments.get(i)
                                                                  .length());
        }

        sb.append(createListHeader(cl,
                                   headerText,
                                   maxNameLength,
                                   maxCommentLength));

        for (int i = 0; i < names.size(); i++) {
            sb.append(names.get(i));
            for (int a = 0; a < Math.max(maxNameLength, NAME.length())
                    - names.get(i).length(); a++) {
                sb.append(" ");
            }
            sb.append(" | ");

            sb.append(CUSTOM_FORMAT.format(dates.get(i)));

            if (isVerbose(cl)) {
                sb.append(" | ");

                sb.append(comments.get(i));
            }

            sb.append("\n");
        }
    }
}
