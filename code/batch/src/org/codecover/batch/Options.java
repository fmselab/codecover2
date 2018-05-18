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

package org.codecover.batch;

/**
 * Class containing all the {@link Option}s.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: Options.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public final class Options {
    /**
     * The charset option
     */
    public static final Option charset = new OptionBuilder()
            .withShortOpt('a')
            .withLongOpt("charset")
            .withArgName("charset")
            .withDescription("the charset to use")
            .hasArg()
            .withArgumentType(new ArgumentType.SuggestionList("utf-8", "latin1"))
            .create();

    /**
     * The comment option
     */
    public static final Option comment = new OptionBuilder()
            .withShortOpt('m')
            .withLongOpt("comment")
            .withDescription("comment describing the test case or test session")
            .hasArg()
            .withArgName("text")
            .create();

    /**
     * The container option
     */
    public static final Option container = new OptionBuilder()
            .withShortOpt('c')
            .withLongOpt("container")
            .withDescription("the test session container to use")
            .hasArg()
            .withArgName("file")
            .withArgumentType(new ArgumentType.File())
            .create();

    /**
     * The copyUninstrumented option
     */
    public static final Option copyUninstrumented = new OptionBuilder()
            .withShortOpt('u')
            .withLongOpt("copy-uninstrumented")
            .withDescription("copy all files under the root directory " +
                             "that are not instrumented")
            .create();

    /**
     * The coverageLog option
     */
    public static final Option coverageLog = new OptionBuilder()
            .withShortOpt('g')
            .withLongOpt("coverage-log")
            .withDescription("the coverage log")
            .hasArg()
            .withArgName("file")
            .withArgumentType(new ArgumentType.File())
            .create();

    /**
     * The criterion option
     */
    public static final Option criterion = new OptionBuilder()
            .withShortOpt('o')
            .withLongOpt("criterion")
            .withDescription("one or more of (all, st, br, co, lo)")
            .hasArg()
            .withArgumentType(new ArgumentType.SuggestionList("all", "st", "br", "co", "lo"))
            .withArgName("crit")
            .multipleTimes()
            .create();

    /**
     * The destination option
     */
    public static final Option destination = new OptionBuilder()
            .withShortOpt('d')
            .withLongOpt("destination")
            .withDescription("the destination file / directory")
            .hasArg()
            .withArgName("filename")
            .withArgumentType(new ArgumentType.File())
            .create();

    /**
     * The destinationContainer option
     */
    public static final Option destinationContainer = new OptionBuilder()
            .withShortOpt('d')
            .withLongOpt("destination-container")
            .withDescription("the (maybe new) destination session container")
            .hasArg()
            .withArgName("file")
            .withArgumentType(new ArgumentType.File())
            .create();

    /**
     * The directive option
     */
    public static final Option directive = new OptionBuilder()
            .withShortOpt('D')
            .withLongOpt("directive")
            .withDescription("a directive for the instrumenter to enable " +
                             "special features; has the form \"key=value\"")
            .hasArg()
            .withArgName("key=value")
            .multipleTimes()
            .create();

    /**
     * The exclude option
     */
    public static final Option exclude = new OptionBuilder()
            .withShortOpt('e')
            .withLongOpt("exclude")
            .withDescription("a exclude pattern, can occur more than one time")
            .hasArg()
            .withArgName("pattern")
            .multipleTimes()
            .create();

    /**
     * The excludesFile option
     */
    public static final Option excludesFile = new OptionBuilder()
            .withShortOpt('x')
            .withLongOpt("excludes-file")
            .withDescription("a file containing a list of exclude patterns - " +
                             "separated by a line break")
            .hasArg()
            .withArgName("file")
            .withArgumentType(new ArgumentType.File())
            .create();

    /**
     * The help option
     */
    public static final Option help = new OptionBuilder()
            .withLongOpt("help")
            .withDescription("shows help-page")
            .withShortOpt('h')
            .create();

    /**
     * The include option
     */
    public static final Option include = new OptionBuilder()
            .withShortOpt('i')
            .withLongOpt("include")
            .withDescription("a include pattern, can occur more than one time")
            .hasArg()
            .withArgName("pattern")
            .multipleTimes()
            .create();

    /**
     * The includesFile option
     */
    public static final Option includesFile = new OptionBuilder()
            .withShortOpt('f')
            .withLongOpt("includes-file")
            .withDescription("a file containing a list of include patterns - " +
                             "separated by a line break")
            .hasArg()
            .withArgName("file")
            .withArgumentType(new ArgumentType.File())
            .create();

    /**
     * The instrumenter option
     */
    public static final Option instrumenter = new OptionBuilder()
            .withShortOpt('I')
            .withLongOpt("instrumenter")
            .withDescription("the unique key of the instrumenter; can be got " +
                             "by using the command instrumenter-info")
            .hasArg()
            .withArgName("key")
            .create();

    /**
     * The language option
     */
    public static final Option language = new OptionBuilder()
            .withShortOpt('l')
            .withLongOpt("language")
            .withDescription("e.g. java, cobol")
            .hasArg()
            .withArgumentType(new ArgumentType.SuggestionList("java", "cobol"))
            .withArgName("lang")
            .create();

    /**
     * The name option
     */
    public static final Option name = new OptionBuilder()
            .withShortOpt('n')
            .withLongOpt("name")
            .withDescription("the new name of the test case or test session")
            .hasArg()
            .withArgName("name")
            .create();

    /**
     * The noProgressBar option
     */
    public static final Option noProgressBar = new OptionBuilder()
            .withLongOpt("no-progress-bar")
            .withDescription("print no progress bar")
            .create();

    /**
     * The pretend option
     */
    public static final Option pretend = new OptionBuilder()
            .withShortOpt('p')
            .withLongOpt("pretend")
            .withDescription("no data changes, only simulation")
            .create();

    /**
     * The progressBar option
     */
    public static final Option progressBar = new OptionBuilder()
            .withLongOpt("progress-bar")
            .withDescription("print a progress bar")
            .exclude(noProgressBar)
            .create();

    /**
     * The quiet option
     */
    public static final Option quiet = new OptionBuilder()
            .withShortOpt('q')
            .withLongOpt("quiet")
            .withDescription("print no information at all")
            .create();

    /**
     * The removeOldTestCases option
     */
    public static final Option removeOldTestCases = new OptionBuilder()
            .withShortOpt('R')
            .withLongOpt("remove-old-test-cases")
            .withDescription("indicates, if the test cases, that " +
                             "were merged, are removed after merging")
            .create();

    /**
     * The removeOldTestSessions option
     */
    public static final Option removeOldTestSessions = new OptionBuilder()
            .withShortOpt('R')
            .withLongOpt("remove-old-test-sessions")
            .withDescription("indicates, if the test sessions, that " +
                             "were merged, are removed after merging")
            .create();

    /**
     * The rootDirectory option
     */
    public static final Option rootDirectory = new OptionBuilder()
            .withShortOpt('r')
             .withLongOpt("root-directory")
             .withDescription("the root directory of the source files " +
                              "(e.g. the default package)")
             .hasArg()
             .withArgumentType(new ArgumentType.Directory())
             .withArgName("directory")
             .create();

    /**
     * The showTestCase option
     */
    public static final Option showTestCase = new OptionBuilder()
            .withShortOpt('T')
            .withLongOpt("test-cases")
            .withDescription("showing test-case information")
            .create();

    /**
     * The template option
     */
    public static final Option template = new OptionBuilder()
            .withShortOpt('t')
            .withLongOpt("template")
            .withDescription("template file defining the layout")
            .hasArg()
            .withArgName("file")
            .withArgumentType(new ArgumentType.File())
            .create();

    /**
     * The testCase option
     */
    public static final Option testCase = new OptionBuilder()
            .withShortOpt('t')
            .withLongOpt("test-case")
            .withDescription("the old name of the test case")
            .hasArg()
            .withArgName("name")
            .create();

    /**
     * The testCases option
     */
    public static final Option testCases = new OptionBuilder()
            .withShortOpt('t')
            .withLongOpt("test-case")
            .withDescription("the name(s) of test case(s)")
            .hasArg()
            .withArgName("name")
            .multipleTimes()
            .create();

    /**
     * The testSession option
     */
    public static final Option testSession = new OptionBuilder()
            .withShortOpt('s')
            .withLongOpt("session")
            .withDescription("the name of the test session")
            .hasArg()
            .withArgName("name")
            .create();

    /**
     * The testSessions option
     */
    public static final Option testSessions = new OptionBuilder()
            .withShortOpt('s')
            .withLongOpt("session")
            .withDescription("names of test sessions")
            .hasArg()
            .withArgName("name")
            .multipleTimes()
            .create();

    /**
     * The verbose option
     */
    public static final Option verbose = new OptionBuilder()
        .withShortOpt('v')
        .withLongOpt("verbose")
        .withDescription("print more information as usual")
        .exclude(quiet)
        .create();

    /**
     * The version option
     */
    public static final Option version = new OptionBuilder()
            .withShortOpt('V')
            .withLongOpt("version")
            .withDescription("shows version information")
            .create();

    /**
     * The add-plugin-dir option
     */
    public static final Option addPluginDir = new OptionBuilder()
            .withLongOpt("add-plugin-dir")
            .withDescription("use all plugins in this directory")
            .hasArg()
            .withArgName("directory")
            .withArgumentType(new ArgumentType.Directory())
            .multipleTimes()
            .create();

    /**
     * The no-default-plugin-dir option
     */
    public static final Option noDefaultPluginDir = new OptionBuilder()
            .withLongOpt("no-default-plugin-dir")
            .withDescription("do not use the plugins from the default plugin directory")
            .create();

    /**
     * The show-stack-trace option
     */
    public static final Option showStackTrace = new OptionBuilder()
            .withLongOpt("show-stack-trace")
            .withDescription("show stack traces on errors")
            .create();
}
