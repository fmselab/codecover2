<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: cli-reference.html.m4 28 2008-05-28 20:31:17Z ahija $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`CLI Reference')

    <h2>Command Line Interface</h2>
    <p><code>usage: codecover %command% [%options%]</code></p>
    <p>CodeCover itself, without using a command, supports the two options:</p>
    <table border="1">
        <tr>
          <th>Option</th>
          <th>Explanation</th>
        </tr>
        <tr>
          <td><code>-h, --help</code> </td>
          <td>shows all available commands or, if followed by a command, the help page of this command</td>
        </tr>
        <tr>
          <td><code>-V, --version</code></td>
          <td>shows version information.</td>
        </tr>
    </table>
    <h3>Command Overview</h3>
    <table border="1">
      <tr>
        <th>Command</th>
        <th>Description</th>
      </tr>
      <tr>
        <td><a href="#AlterSession"><code>as, alter-session</code></a></td>
        <td>changes information of a test session</td>
      </tr>
      <tr>
        <td><a href="#AlterTestCase"><code>at, alter-test-case</code></a></td>
        <td>changes information about a test case</td>
      </tr>
      <tr>
        <td><a href="#Analyze"><code>an, analyze</code></a></td>
        <td>inserts a coverage log into a test session container</td>
      </tr>
      <tr>
        <td><a href="#CopySessions"><code>cs, copy-sessions</code></a></td>
        <td>copies test-sessions from one session container to another</td>
      </tr>
      <tr>
        <td><a href="#Help"><code>h, help</code></a></td>
        <td>a help page containing this command overview or an option and parameter overview for a given command.</td>
      </tr>
      <tr>
        <td><code><a href="#Info">info</a></code></td>
        <td>shows information about a session container</td>
      </tr>
      <tr>
        <td><code><a href="#InstrumenterInfo">ii, instrumenter-info</a></code></td>
        <td>prints information about available instrumenters</td>
      </tr>
      <tr>
        <td><a href="#Instrument"><code>in, instrument</code></a></td>
        <td>instruments source files</td>
      </tr>
      <tr>
        <td><a href="#MergeSessions"><code>ms, merge-sessions</code></a></td>
        <td>merges test-sessions within the same session container</td>
      </tr>
      <tr>
        <td><a href="#MergeTestCases"><code>mt, merge-test-cases</code></a></td>
        <td>merges test cases of the same session</td>
      </tr>
      <tr>
        <td><a href="#RemoveSessions"><code>rs, remove-sessions</code></a></td>
        <td>removes test-sessions from a session container</td>
      </tr>
      <tr>
        <td><a href="#RemoveTestCases"><code>rt, remove-test-cases</code></a></td>
        <td>removes test cases from a session container</td>
      </tr>
      <tr>
        <td><a href="#Report"><code>re, report</code></a></td>
        <td>generates a report from a test-session</td>
      </tr>
      <tr>
        <td><a href="#TouchContainer"><code>touch-container</code></a></td>
        <td>reads and rewrites all given test session containers</td>
      </tr>
    </table>
    <div>
      <h3>General Options</h3>
      <p>For every command a set of general options is supported. In addition to specific command options, all commands support the following parameterless options. They are not required but change the behavior of the software when used.</p>
      <table border="1">
        <tr>
          <th>Option</th>
          <th>Explanation</th>
        </tr>
        <tr>
          <td><code>-v, --verbose</code> </td>
          <td> Orders the software to print more information as usual. For example this can be a description of the actions being done. This option is the opposite of <code>--quiet</code>.</td>
        </tr>
        <tr>
          <td><code>-q, --quiet</code></td>
          <td>Orders the software not to print information to the shell. This option is the opposite to <code>--verbose</code>.</td>
        </tr>
        <tr>
          <td><code>-p, --pretend</code></td>
          <td>Orders the software not to perform any actions affecting the data persistently but to print information about what the software would do instead. Using <code>--pretend</code> the actor can make sure that his command has the correct syntax and would be successfully executed.</td>
        </tr>
        <tr>
          <td><code>-h, --help</code></td>
          <td>Prints an option and parameter overview of the given command. Has the same effect like <code>codecover help \%command\%</code>.</td>
        </tr>
        <tr>
          <td><code>--add-plugin-dir</code></td>
          <td>Uses all plugins in this directory.</td>
        </tr>
        <tr>
          <td><code>--no-default-plugin-dir</code></td>
          <td>Does not use the plugins from the default plugin directory.</td>
        </tr>
        <tr>
          <td><code>--progress-bar</code></td>
          <td>prints a progress bar</td>
        </tr>
        <tr>
          <td><code>--no-progress-bar</code></td>
          <td>prints no progress bar</td>
        </tr>
        <tr>
          <td><code>--show-stack-trace</code></td>
          <td>shows stack trace on errors</td>
        </tr>
      </table>
      <!-- Intrument-Info Command -->
      <a name="InstrumenterInfo" id="InstrumenterInfo"></a></div>
    <div class="commandDiv">
      <h3>Instrumenter-Info</h3>
      <p>This command is used to get information of all available instrumenters. If there are two or more instrumenters available for a given programming language, you can get to know the unique keys of the possible instrumenters. These key can than be used for the option <code>instrumenter</code> of the <a href="#Instrument">instrument command</a>.</p>
      <h4>Usage</h4>
      <pre>
<code>codecover (instrumenter-info|ii) [options]</code>
</pre>
      <h4>Options</h4>
      The following table lists all the options, that are available for use with this command.
      <table border="1">
        <tr>
          <th>Option</th>
          <th>Description</th>
          <th style="text-align: center">Required</th>
        </tr>
        <tr>
          <td><code>-l, --language &lt;lang&gt;</code></td>
          <td>(<code>java</code> | <code>cobol</code>)</td>
          <td></td>
        </tr>
      </table>
      <h4>Examples</h4>
      <pre>
<code>codecover instrumenter-info -l java -v</code>
</pre>
      <p>Print out verbose information of all instrumenters, that support the programming language java.</p>
    </div>
    <!-- Intrument Command -->
    <a name="Instrument" id="Instrument"></a>
    <div class="commandDiv">
      <h3>Instrument</h3>
      <p>This command is used to instrument source code files so that the coverage will be measured when these files will be executed.</p>
      <h4>Usage</h4>
      <pre>
<code>codecover (instrument|in) [options]</code>
</pre>
      <h4>Options</h4>
      The following table lists all the options, that are available for use with this command.
      <table border="1">
        <tr>
          <th>Option</th>
          <th>Description</th>
          <th style="text-align: center">Required</th>
        </tr>
        <tr>
          <td><code>-c, --container &lt;file&gt;</code></td>
          <td>the new session container</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-d, --destination &lt;dir&gt;</code></td>
          <td>the destination directory for the instrumented files</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-r, --root-directory &lt;dir&gt;</code></td>
          <td >the root directory of the source files</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-l, --language &lt;lang&gt;</code></td>
          <td> (<code>java</code> | <code>cobol</code>)</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-a, --charset &lt;charset&gt;</code></td>
          <td>the character encoding of the source files</td>
          <td style="text-align: center"></td>
        </tr>
        <tr>
          <td><code>-u, --copy-uninstrumented</code></td>
          <td> advices the software to copy all files of the root-directory, that where not instrumented, to the destination</td>
          <td style="text-align: center"></td>
        </tr>
        <tr>
          <td><code>-o, --criterion &lt;crit&gt;</code></td>
          <td>one of (<code>all</code>, <code>st</code>, <code>br</code>, <code>co</code>, <code>lo</code>); this argument can occur more than one time -- once for every criterion</td>
          <td style="text-align: center"></td>
        </tr>
        <tr>
          <td><code>-D, --directive</code></td>
          <td><p>arguments of the style <code>key=value</code> to enable special features of the instrumenter; the instrumenter-info command prints out a list of directives, an instrumenter supports.</p></td>
          <td style="text-align: center"></td>
        </tr>
        <tr>
          <td><code>-e, --exclude &lt;pattern&gt;</code></td>
          <td>a relative exclude pattern; this argument can occur more than one time</td>
          <td style="text-align: center"></td>
        </tr>
        <tr>
          <td><code>-x, --excludes-file &lt;file&gt;</code></td>
          <td>a file containing a list of relative exclude patterns separated by new line</td>
          <td style="text-align: center"></td>
        </tr>
        <tr>
          <td><code>-i, --include &lt;pattern&gt;</code></td>
          <td>a relative include pattern; this argument can occur more than one time</td>
          <td style="text-align: center"></td>
        </tr>
        <tr>
          <td><code>-f, --includes-file &lt;file&gt;</code></td>
          <td>a file containing a list of relative include patterns separated by new line</td>
          <td style="text-align: center"></td>
        </tr>
        <tr>
          <td><code>-I, --instrumenter &lt;key&gt;</code></td>
          <td> the unique key of the instrumenter to use; use the command <a href="#InstrumenterInfo">instrumenter-info</a> to get the correct key</td>
          <td style="text-align: center"></td>
        </tr>
      </table>
      <p>The arguments of option <code>criteria</code> stand for:</p>
      <table border="1">
        <tr>
          <th style="text-align: center">Criteria abbreviation</th>
          <th>Explanation</th>
        </tr>
        <tr>
          <td style="text-align: center"><code>all</code></td>
          <td>all criteria</td>
        </tr>
        <tr>
          <td style="text-align: center"><code>st</code></td>
          <td>statement coverage</td>
        </tr>
        <tr>
          <td style="text-align: center"><code>br</code></td>
          <td>branch coverage</td>
        </tr>
        <tr>
          <td style="text-align: center"><code>co</code></td>
          <td>condition coverage</td>
        </tr>
        <tr>
          <td style="text-align: center"><code>lo</code></td>
          <td>loop coverage</td>
        </tr>
      </table>
      <h4>Patterns</h4>
      <p>For a more detailed selection, include and exclude patterns can be used. These patterns allow wildcards and are adopted from the <a href="http://ant.apache.org/">apache ant project</a> (see their <a href="http://ant.apache.org/manual/dirtasks.html">pattern description</a>).</p>
      <p>The wildcard <code>?</code> matches one character. The wildcard <code>*</code> matches zero or more characters. Examples:</p>
      <ul>
        <li> <code>?.java</code> matches <code>x.java </code>and <code>A.java</code>, but not <code>.java</code> or <code>xyz.java</code> (both don't have one character before <code>.java</code>).</li>
        <li><code>*.java</code> matches <code>.java</code>, <code>x.java</code> and <code>FooBar.java</code>, but not <code>FooBar.xm</code>l (does not end with <code>.java</code>).</li>
      </ul>
      <p>Combinations of <code>*</code>'s and <code>?</code>'s are allowed.</p>
      <p>To match a complete directory tree, or a file anywhere in the directory tree, use <code>**</code>. When <code>**</code> is used as the name of a directory in the pattern, it matches zero or more directories. Example:</p>
      <ul>
        <li><code>src/**/*.java</code> matches all java files under <code>src/</code>, such as <code>src/x.java</code>, or <code>src/foo/bar/MyObject.java</code>, but not <code>Main.java</code> or <code>test/AllTests.java</code> (Do not lie under test}).</li>
      </ul>
      <p>The instrumenter uses some ignore patterns for files, that are totally ignored:</p>
      <ul>
        <li><code>**/*~, **/#*#, **/.#*, **/%*%&quot;, **/._*&quot;</code></li>
        <li><code>**/.svn/**, **/_svn/**, **/CVS/**, **/.cvsignore</code></li>
        <li><code>**/SCCS/**, **/vssver.scc, **/.DS_Store, **/Thumbs.db</code></li>
      </ul>
      <h4>Examples</h4>
      <pre><code>codecover instrument -r src/ 
                     -d bin/instr/ 
                     -c container.xml 
                     -l java</code></pre>
      <p>Find all java source files in the directory &quot;src&quot;, parse and instrument these files into the directory &quot;bin/instr/&quot;. The source files are instrumented for all criteria and the static information compiled during the instrumentation is stored in the &quot;container.xml&quot;. </p>
      <pre><code>codecover instrument -r src/ 
                     -d bin/instr/ 
                     -i &quot;org/pak1/**&quot;
                     -e &quot;**/*Test.java&quot;
                     -c container.xml 
                     -l java
                     -I CodeCover_Java_1.5
                     -o st lo
                     -a UTF-8
                     -u</code></pre>
      <p>Find all java source files under the directory &quot;org/pak1&quot;, exclude files ending with &quot;Test.java&quot;, parse these files with the &quot;UTF-8&quot; charset and instrument them into the directory &quot;bin/instr/&quot;. The source files are instrumented for statement and loop coverage, and the static information compiled during the instrumentation is stored in the &quot;container.xml&quot;. All files under &quot;src&quot;, which are not instrumented, are copied to &quot;bin/instr/&quot;. The code cover instrumenter for java is used. It is specified by its unique key <em>CodeCover\_Java\_1.5</em>.</p>
    </div>
    <!-- Analyze Command -->
    <a name="Analyze" id="Analyze"></a>
    <div class="commandDiv">
      <h3>Analyze</h3>
      <p>This command is used to import a coverage-log, that was produced by an instrumented program, into a specified test-session container.</p>
      <h4>Usage</h4>
      <pre>
<code>codecover (analyze|an) [options]</code>
</pre>
      <h4>Options</h4>
      The following table lists all the options, that are available for use with this command.
      <table border="1">
        <tr>
          <th>Option</th>
          <th>Description</th>
          <th style="text-align: center">Required</th>
        </tr>
        <tr>
          <td><code>-c, --container &lt;file&gt;</code></td>
          <td>the test-session container the coverage data should be added to</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-g, --coverage-log &lt;file&gt;</code></td>
          <td>the coverage log produced by the executed program</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-n, --name &lt;name&gt;</code></td>
          <td>the name of the new test-session containing the coverage results</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-m, --comment &lt;text&gt;</code></td>
          <td>a comment describing the test-session</td>
          <td style="text-align: center"></td>
        </tr>
        <tr>
          <td><code>-a, --charset &lt;charset&gt;</code></td>
          <td>the character encoding of the coverage log file</td>
          <td style="text-align: center"></td>
        </tr>
      </table>
      <h4>Examples</h4>
      <pre>
<code>codecover analyze -c container.xml
                  -g coverageLog.clf
                  -n &quot;test-session 1&quot;</code></pre>
      <p>Load the test-session container from &quot;container.xml&quot;, as well as parse the generated coverage log &quot;coverageLog.clf&quot;, and add the data contained in the coverage log to the test-session container under the name &quot;test-session 1&quot;.</p>
      <pre>
<code>codecover analyze -c container.xml
                  -g coverageLog.clf
                  -n &quot;test-session 2&quot;
                  -m &quot;Another test-session&quot;</code></pre>
      <p>Load the test-session container from &quot;container.xml&quot;, as well as parse the generated coverage log &quot;coverageLog2.clf&quot;, and add the data contained in the coverage log to the test-session container under the name &quot;test-session 2&quot; and the comment &quot;Another test-session&quot;.</p>
    </div>
    <!-- Report Command -->
    <a name="Report" id="Report"></a>
    <div class="commandDiv">
      <h3>Report</h3>
      <p>Creates a report of a test-session. A template which specifies the appearance and format is used to generate the report.</p>
      <h4>Usage</h4>
      <pre>
<code>codecover (report|re) [options]</code>
</pre>
      <h4>Options</h4>
      The following table lists all the options, that are available for use with this command.
      <table border="1">
        <tr>
          <th>Option</th>
          <th>Description</th>
          <th>Required</th>
        </tr>
        <tr>
          <td><code>-c, --container &lt;file&gt;</code></td>
          <td>the session container to use</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-s, --session &lt;name&gt;</code></td>
          <td>the name of the test-session for the report</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-t, --template &lt;file&gt;</code></td>
          <td>the template file containing transformation descriptions</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-d, --destination &lt;file&gt;</code></td>
          <td>the destination for the report</td>
          <td style="text-align: center">X</td>
        </tr>
      </table>
      <h4>Examples</h4>
      <p>An example <a href="../tutorials/SimpleJavaAppReport.html">CodeCover report</a></p>
    </div>
    <!-- Info Command -->
    <a name="Info" id="Info"></a>
    <div class="commandDiv">
      <h3>Info</h3>
      <p>Shows information about a test-session container. The options allow to manipulate the output, as detailed in <a href="#InfoExamples">Examples</a>.</p>
      <h4>Usage</h4>
      <pre>
<code>codecover info [options]</code>
</pre>
      <h4>Options</h4>
      The following table lists all the options, that are available for use with this command.
      <table border="1">
        <tr>
          <th>Option</th>
          <th>Description</th>
          <th>Required</th>
        </tr>
        <tr>
          <td><code>-c, --container &lt;file&gt;</code></td>
          <td>the test-session container</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-s, --session &lt;name&gt;</code></td>
          <td>the name of a test-session</td>
          <td style="text-align: center"></td>
        </tr>
        <tr>
          <td><code>-T, --test-cases</code></td>
          <td>showing test case information</td>
          <td style="text-align: center"></td>
        </tr>
      </table>
      <h4><a name="InfoExamples" id="InfoExamples"></a>Examples</h4>
      <h4>General Example</h4>
      <p>The following shows the different information levels CodeCover provides. Calling the info command with only the test-session container, results in a list of all the test-session contained in this test-session container, with their name, and the date, they were created on. By naming test-sessions present in the test-session container, you can tailor the output to your specific needs.</p>
      <pre><code>root@deepthought:~&gt; codecover info -c container.xml
---------------------------------------------------------------------
test-session container: container.xml

test-sessions:
name         | date                                        
---------------------------------------------------------------------
TestSession0 | 2007-05-18 17:47:32 +0200 (Fri, 18 May 2007)
TestSession1 | 2007-05-18 17:47:32 +0200 (Fri, 18 May 2007)
TestSession2 | 2007-05-18 17:47:32 +0200 (Fri, 18 May 2007)
---------------------------------------------------------------------</code></pre>
      <h4>Example with <code>verbose</code></h4>
      <p>Adding &quot;verbose&quot; as an option to the above call, additionally displays the comment of the test-sessions in the test-session container.</p>
      <pre><code>root@deepthought:~&gt; codecover info --test-cases 
                                   -c container.xml 
                                   -s TestSession0
---------------------------------------------------------------------
test-session container: container.xml
=====================================================================
test-session name:    TestSession0
test-session date:    2007-05-18 17:47:32 +0200 (Fri, 18 May 2007)
test-session comment: 42

test cases:
name        | date                                        
---------------------------------------------------------------------
TestCase0   | 2007-05-18 17:47:32 +0200 (Fri, 18 May 2007)
TestCase99  | 2007-05-18 17:47:32 +0200 (Fri, 18 May 2007)
TestCase198 | 2007-05-18 17:47:32 +0200 (Fri, 18 May 2007)
TestCase297 | 2007-05-18 17:47:32 +0200 (Fri, 18 May 2007)
---------------------------------------------------------------------</code></pre>
      <h4>Example with <code>test-cases</code> and <code>verbose</code></h4>
      <p>As before, calling the info command with &quot;test-cases&quot; option, as well as the &quot;verbose&quot; option, adds the comments to the list of test cases in the given test-session</p>
      <pre><code>root@deepthought:~&gt; codecover info --verbose 
                                   --test-cases 
                                   -c container.xml 
                                   -s TestSession0
---------------------------------------------------------------------
test-session container: container.xml
=====================================================================
test-session name:    TestSession0
test-session date:    2007-05-18 17:47:32 +0200 (Fri, 18 May 2007)
test-session comment: 42

test cases:
name        | date                                         | comment
---------------------------------------------------------------------
TestCase0   | 2007-05-18 17:47:32 +0200 (Fri, 18 May 2007) | 42
TestCase99  | 2007-05-18 17:47:32 +0200 (Fri, 18 May 2007) | 42
TestCase198 | 2007-05-18 17:47:32 +0200 (Fri, 18 May 2007) | 42
TestCase297 | 2007-05-18 17:47:32 +0200 (Fri, 18 May 2007) | 42
---------------------------------------------------------------------</code></pre>
    </div>
    <!-- Merge Sessions Command -->
    <a name="MergeSessions" id="MergeSessions"></a>
    <div class="commandDiv">
      <h3>Merge Sessions</h3>
      <p>This command is used to merge one or more test-sessions of a session container into a new test-session.</p>
      <h4>Usage</h4>
      <pre><code>codecover (merge-sessions|ms) [options]</code></pre>
      <h4>Options</h4>
      The following table lists all the options, that are available for use with this command.
      <table border="1">
        <tr>
          <th>Option</th>
          <th>Description</th>
          <th>Required</th>
        </tr>
        <tr>
          <td><code>-c, --container &lt;file&gt;</code></td>
          <td>the session container to use</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-s, --session &lt;name&gt;</code></td>
          <td>a name of a test-session participating in the merging; this argument can occur more than one time -- once for every participant</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-n, --name &lt;name&gt;</code></td>
          <td>the name of the merged test-session</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-m, --comment &lt;text&gt;</code></td>
          <td>a comment describing the merged test-session</td>
          <td style="text-align: center"></td>
        </tr>
        <tr>
          <td><code>-R, --remove-old-test-sessions</code></td>
          <td>indicates, whether or not the test-sessions, that were merged, are removed after merging</td>
          <td style="text-align: center"></td>
        </tr>
      </table>
      <h4>Examples</h4>
      <pre><code>codecover merge-sessions -c container.xml 
                         -n &quot;Merged test-session&quot; 
                         -s &quot;test-session 1&quot; &quot;test-session 2&quot;</code></pre>
      <p>Loads the test-session container from the file &quot;container.xml&quot; and merges the two test-sessions &quot;test-session 1&quot; and &quot;test-session 2&quot; into the new &quot;Merged test-session&quot;. By default the merged test-sessions - &quot;test-session 1&quot; and &quot;test-session 2&quot; - are <em>not</em> deleted.</p>
      <pre><code>codecover merge-sessions --remove-old-test-sessions 
                         -c container.xml 
                         -s &quot;test-session 1&quot; &quot;test-session 2&quot;
                         -n &quot;Merged test-session&quot;</code></pre>
      <p>Adding the &quot;remove-old-test-sessions&quot; to the command performs the operation described above, but deletes the merged test-sessions - &quot;test-session 1&quot; and &quot;test-session 2&quot; - from the test-session container.</p>
    </div>
    <!-- Alter Session Command -->
    <a name="AlterSession" id="AlterSession"></a>
    <div class="commandDiv">
      <h3>Alter Session</h3>
      <p>This command is used to modify the name and/or the comment of a test-session.</p>
      <h4>Usage</h4>
      <pre><code>codecover (alter-session|as) [options]</code></pre>
      <h4>Options</h4>
      The following table lists all the options, that are available for use with this command.
      <table border="1">
        <tr>
          <th>Option</th>
          <th>Description</th>
          <th>Required</th>
        </tr>
        <tr>
          <td><code>-c, --container &lt;file&gt;</code></td>
          <td>the session container to use</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-s, --session &lt;name&gt;</code></td>
          <td>the old name of the test-session</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-n, --name &lt;name&gt;</code></td>
          <td>a new name of the test-session</td>
          <td style="text-align: center"></td>
        </tr>
        <tr>
          <td><code>-m, --comment &lt;text&gt;</code></td>
          <td>a new comment describing the test-session</td>
          <td style="text-align: center"></td>
        </tr>
      </table>
      <h4>Examples</h4>
      <pre><code>codecover alter-session -c container.xml 
                        -s &quot;Session 1&quot;
                        -n &quot;New Name&quot; 
                        -m &quot;New Comment&quot;</code></pre>
      <p>Load the test-session container from &quot;container.xml&quot;, rename the test-session with the name &quot;Session 1&quot; to &quot;New Name&quot; and change the comment of it to &quot;New Comment&quot;. The option for the comment is not required, so a test-session can be renamed without changing its comment.</p>
    </div>
    <!-- Copy Sessions Command -->
    <a name="CopySessions" id="CopySessions"></a>
    <div class="commandDiv">
      <h3>Copy Sessions</h3>
      <p>This command is used to copy one or more test-sessions from one session container to another.</p>
      <p>If the destination test-session container does not exists, the source test-session container is copied to the location of the destination test-session container, while containing only the specified test-sessions.</p>
      <p>If the destination test-session container already contains a test-session, that is to be copied into it, from the source test-session container, the copied test-session is renamed along the lines of &quot;Test Session&quot; to &quot;Test Session (1)&quot;. The test-session that was already present in the destination test-session container is not modified. </p>
      <h4>Usage</h4>
      <pre><code>codecover (copy-sessions|cs) [options]</code></pre>
      <h4>Options</h4>
      The following table lists all the options, that are available for use with this command.
      <table border="1">
        <tr>
          <th>Option</th>
          <th>Description</th>
          <th>Required</th>
        </tr>
        <tr>
          <td><code>-c, --container &lt;file&gt;</code></td>
          <td>the source session container</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-s, --session &lt;name&gt;</code></td>
          <td>a name of a test-session participating at the copy; this argument can occur more than one time -- once for every participant</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-d, --destination &lt;file&gt;</code></td>
          <td>the destination session container</td>
          <td style="text-align: center">X</td>
        </tr>
      </table>
      <h4>Examples</h4>
      <pre><code>codecover copy-sessions -c container.xml 
                        -s &quot;Session 1&quot; 
                        -s &quot;Session 3&quot; 
                        -d container2.xml</code></pre>
      <p>Load the test-session containers from &quot;container.xml&quot;, copies the sessions named &quot;Session 1&quot; and &quot;Session 3&quot; to the test-session container from &quot;container2.xml&quot;.</p>
    </div>
    <!-- Remove Sessions Command -->
    <a name="RemoveSessions" id="RemoveSessions"></a>
    <div class="commandDiv">
      <h3>Remove Sessions</h3>
      <p>This command is used to remove one or more test-sessions from a session container.</p>
      <h4>Usage</h4>
      <pre><code>codecover (remove-sessions|rs) [options]</code></pre>
      <h4>Options</h4>
      The following table lists all the options, that are available for use with this command.
      <table border="1">
        <tr>
          <th>Option</th>
          <th>Description</th>
          <th>Required</th>
        </tr>
        <tr>
          <td><code>-c, --container &lt;file&gt;</code></td>
          <td>the session container to remove from</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-s, --session &lt;name&gt;</code></td>
          <td>the name of the test-session to be removed; this argument can occur more than one time -- once for every test-session</td>
          <td style="text-align: center">X</td>
        </tr>
      </table>
      <h4>Examples</h4>
      <pre><code>codecover remove-sessions -c container.xml 
                          -s &quot;Session 1&quot;</code></pre>
      <p>Load the test-session container from &quot;container.xml&quot;, remove the test-session with the name &quot;Session 1&quot; of the container.</p>
    </div>
    <!-- Merge Test Case Command -->
    <a name="MergeTestCases" id="MergeTestCases"></a>
    <div class="commandDiv">
      <h3>Merge Test Cases</h3>
      <p>This command is used to merge one or more test-cases of a test-session into a new test-case.</p>
      <h4>Usage</h4>
      <pre><code>codecover (merge-test-cases|mt) [options]</code></pre>
      <h4>Options</h4>
      The following table lists all the options, that are available for use with this command.
      <table border="1">
        <tr>
          <th>Option</th>
          <th>Description</th>
          <th>Required</th>
        </tr>
        <tr>
          <td><code>-c, --container &lt;file&gt;</code></td>
          <td>the session container to use</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-s, --session &lt;name&gt;</code></td>
          <td>the name of the test-session</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-t, --test-case &lt;name&gt;</code></td>
          <td>a name of a test case participating in the merging; this argument can occur more than one time -- once for every participant</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-n, --name &lt;name&gt;</code></td>
          <td>the name of the merged test case</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-R, --remove-old-test-cases</code></td>
          <td>indicates, whether or not the test cases, that were merged, are removed after merging</td>
          <td style="text-align: center"></td>
        </tr>
        <tr>
          <td><code>-m, --comment &lt;text&gt;</code></td>
          <td>a comment describing the merged test case</td>
          <td style="text-align: center"></td>
        </tr>
      </table>
      <h4>Examples</h4>
      <pre><code>codecover merge-test-cases -c container.xml 
                           -s &quot;Session 1&quot;
                           -t &quot;test-case 1&quot; &quot;test-case 2&quot; 
                           -n &quot;Merged test-case&quot;</code></pre>
      <p>Loads the test-session container from the file &quot;container.xml&quot; and merges the two test-cases &quot;test-case 1&quot; and &quot;test-case 2&quot; into the new test-case &quot;Merged test-case&quot;. By default the merged test-cases - &quot;test-case 1&quot; and &quot;test-case 2&quot; - are <em>not</em> deleted.</p>
      <pre><code>codecover merge-test-cases --remove-old-test-cases 
                           -c container.xml 
                           -s &quot;Session 1&quot;
                           -t &quot;test-case 1&quot; &quot;test-case 2&quot; 
                           -n &quot;Merged test-case&quot;</code></pre>
      <p>Adding the &quot;remove-old-test-cases&quot; to the command performs the operation described above, but deletes the merged test-cases - &quot;test-case 1&quot; and &quot;test-case 2&quot; - from the test-session container.</p>
    </div>
    <!-- Alter Test Cases Command -->
    <a name="AlterTestCase" id="AlterTestCase"></a>
    <div class="commandDiv">
      <h3>Alter Test Case</h3>
      <p>This command is used to modify the name and/or the comment of a test-case.</p>
      <h4>Usage</h4>
      <pre><code>codecover (alter-test-case|at) [options]</code></pre>
      <h4>Options</h4>
      The following table lists all the options, that are available for use with this command.
      <table border="1">
        <tr>
          <th>Option</th>
          <th>Description</th>
          <th>Required</th>
        </tr>
        <tr>
          <td><code>-c, --container &lt;file&gt;</code></td>
          <td>the session container to use</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-s, --session &lt;name&gt;</code></td>
          <td>the name of the test-session</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-t, --test-case &lt;name&gt;</code></td>
          <td>the old name of the test case</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-n, --name &lt;name&gt;</code></td>
          <td>the new name of the test case</td>
          <td style="text-align: center"></td>
        </tr>
        <tr>
          <td><code>-m, --comment &lt;text&gt;</code></td>
          <td>a new comment describing the test case</td>
          <td style="text-align: center"></td>
        </tr>
      </table>
      <h4>Examples</h4>
      <pre><code>codecover alter-test-case -c container.xml 
                          -s &quot;Session 1&quot;
                          -t &quot;Test Case 1&quot;
                          -n &quot;New Name&quot;
                          -m &quot;New Comment&quot;</code></pre>
      <p>Load the test-session container from &quot;container.xml&quot;, rename the test-case with the name &quot;Test Case 1&quot; to &quot;New Name&quot; and change the comment of the test-case to &quot;New Comment&quot;. The option for the comment is not required, so a test-case can be renamed without changing its comment.</p>
    </div>
    <!-- Remove Test Cases Command -->
    <a name="RemoveTestCases" id="RemoveTestCases"></a>
    <div class="commandDiv">
      <h3>Remove Test Cases</h3>
      <p>This command is used to remove a test-case from a test-session.</p>
      <h4>Usage</h4>
      <pre><code>codecover (remove-test-cases|rt) [options]</code></pre>
      <h4>Options</h4>
      The following table lists all the options, that are available for use with this command.
      <table border="1">
        <tr>
          <th>Option</th>
          <th>Description</th>
          <th>Required</th>
        </tr>
        <tr>
          <td><code>-c, --container &lt;file&gt;</code></td>
          <td>the session container to use</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-s, --session &lt;name&gt;</code></td>
          <td>the name of the test-session</td>
          <td style="text-align: center">X</td>
        </tr>
        <tr>
          <td><code>-t, --test-case &lt;name&gt;</code></td>
          <td>the name of the test case to be removed; this argument can occur more than one time -- once for every test case</td>
          <td style="text-align: center">X</td>
        </tr>
      </table>
      <h4>Examples</h4>
      <pre><code>codecover remove-test-cases -c container.xml 
                            -s &quot;Session 1&quot;
                            -t &quot;Test Case 1&quot;</code></pre>
      <p>Load the test-session container from &quot;container.xml&quot;, remove the test-case with the name &quot;Test Case 1&quot; of the session &quot;Session 1&quot;.</p>
    </div>
    <!-- Help Command -->
    <a name="Help" id="Help"></a>
    <div class="commandDiv">
      <h3>Help</h3>
      <p>The help command can be used either to show a help page containing an overview of all command, or to show all information about one given command, including all options and parameters of it.</p>
      <h4>Usage</h4>
      <pre><code>codecover (help|h) [\%command\%]</code></pre>
      <h4>Options</h4>
      <p>The help command has no options (except --help telling exactly this)</p>
      <h4>Examples</h4>
      <pre><code>codecover in --help</code></pre>
      <p>or</p>
      <pre><code>codecover h in</code></pre>
      <p>Show the usage, the available options and parameters of the instrument command.
        The output for this command will be:</p>
      <pre><code>root@deepthought:~&gt; codecover in --help
usage: codecover instrument &lt;options&gt;
instruments source files

This command requires a root directory with all source files, a destination for
the instrumented source files, a container, where static information of the
source files are placed - the so called MAST (More Abstract Syntax Tree) and th

language of the source files - e.g. java or cobol.

To select the files to instrument more detailed, patters can be used. There are
include and exclude patterns. A file is instrumented, if it fits one of the
include patterns and none of the exclude patterns. These patterns are relative
paths under the root directory and can include wildcards:
 ? matches one character
 * matches zero or more characters
** matches zero or more directories in a path
see http://ant.apache.org/manual/dirtasks.html#patterns for details

REQUIRED options:
 -c, --container       the test session container to use
 -d, --destination     the destination file / directory
 -l, --language        e.g. java, cobol
 -r, --root-directory  the root directory of the source files (e.g. the default
                       package)

OPTIONAL options:
     --add-plugin-dir         use all plugins in this directory
 -a, --charset                the charset to use
 -u, --copy-uninstrumented    copy all files under the root directory that are
                              not instrumented
 -o, --criterion              one or more of (all, st, br, co, lo)
 -D, --directive              a directive for the instrumenter to enable
                              special features; has the form "key=value"
 -e, --exclude                a exclude pattern, can occur more than one time
 -x, --excludes-file          a file containing a list of exclude patterns -
                              separated by a line break
 -h, --help                   shows help-page
 -i, --include                a include pattern, can occur more than one time
 -f, --includes-file          a file containing a list of include patterns -
                              separated by a line break
 -I, --instrumenter           the unique key of the instrumenter; can be got by
                              using the command instrumenter-info
     --no-default-plugin-dir  do not use the plugins from the default plugin
                              directory
     --no-progress-bar        print no progress bar
 -p, --pretend                no data changes, only simulation
     --progress-bar           print a progress bar
 -q, --quiet                  print no information at all
     --show-stack-trace       show stack traces on errors
 -v, --verbose                print more information as usual</code></pre>
    </div>
  <!-- Touch Container Command -->
    <a name="TouchContainer" id="TouchContainer"></a>
    <div class="commandDiv">
    <h3>Touch Container</h3>
    <p>Reads and rewrites all given session containers.</p>
    </div>

m4_web_create_footer
