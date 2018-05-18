<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: how_to_batch.html.m4 28 2008-05-28 20:31:17Z ahija $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`HOWTO Use CodeCover with the command-line interface')

    <h2>HOWTO Use CodeCover with the command-line interface</h2>
    <p>This section contains step-by-step instructions on how to use CodeCover with the command-line interface. A piece of software is instrumented, compiled and executed. The measured coverage data is then collected in an HTML report.</p>
    <h3>General Preconditions</h3>
    <ul>
      <li>The name of the software used in this example is <em>SimpleJavaApp</em>.</li>
      <li><em>SimpleJavaApp</em> is written in Java.</li>
      <li><em>SimpleJavaApp</em>'s sources are encoded using UTF-8.</li>
      <li>CodeCover is executed with the <code>codecover</code> command.</li>
      <li><code>CODECOVER_HOME</code>$ refers to the installation directory of CodeCover.</li>
      <li>You can get the SimpleJavaApp here, as <a href="SimpleJavaApp.zip">.zip</a> or <a href="SimpleJavaApp.tar.bz2">.tar.bz2</a>.</li>
    </ul>
    <h3>Instrument</h3>
    <p>The first step is the instrumentation of<em>SimpleJavaApp</em><em> </em>with the following command:</p>
    <pre><code>codecover instrument --root-directory <em>SimpleJavaApp</em>/src
                     --destination <em>SimpleJavaApp</em>/instrumentedSrc 
                     --container <em>SimpleJavaApp</em>/test-session-container.xml
                     --language java 
                     --charset UTF-8</code></pre>
    <ul>
      <li>The option <code>root-directory</code> refers to the directory of the software that holds the top-level package(s) of the code to be instrumented.</li>
      <li>The option <code>destination</code> refers to the directory where the instrumented source files are stored.</li>
      <li>The option <code>container</code> refers to the test session container that holds the static information about the instrumented code, as well as the collected coverage data.</li>
      <li>The option <code>language</code> refers to the language of the code to be instrumented.</li>
      <li> The option <code>charset</code> refers to the character set, in which the source files are saved.</li>
    </ul>
    <p>You will begin the instrumentation process with the execution of this command. The duration depends on the size of the given source code. As a result a number of new, instrumented source files are created in the specified destination. Also, a test session container file is created which holds the static information about the source code.</p>
    <h3>Compile</h3>
    <p>The instrumentation of the software is followed by the compilation of the instrumented source code. As this exceeds the scope of CodeCover, this is your job. It must be noted that any additional source files created by CodeCover must be compiled with the instrumented source code.</p>
    <h3>Execute</h3>
    <p>You can now execute the compiled program and perform your test activities. The termination of the program will result in a coverage log file. This file holds the coverage data measured during the execution. In the next step it will be entered into the created test session container.</p>
    <h3>Analyze</h3>
    <p>To enter the created coverage log file into the test session container use the following command:</p>
    <pre><code>codecover analyze --container <em>SimpleJavaApp</em>/test-session-container.xml 
                  --coverage-log <em>SimpleJavaApp</em>/instrumentedSrc/coverage_log.clf 
                  --name TestSession1 
                  --comment &quot;The first test session&quot;</code></pre>
    <ul>
      <li> The option <code>container</code> refers to the test session container into which the coverage data shall be inserted, usually the test session container created during the instrumentation process.</li>
      <li> The option <code>coverage-log</code> refers to the coverage log file whose coverage data shall be inserted into the test session container.<br />
        The option <code>name</code> refers to the name of the test session the coverage data is attributed to.</li>
      <li> The option <code>comment</code> refers to an optional comment that can be added to a test session.</li>
    </ul>
    <p>You will enter the coverage data of the coverage log file into the test session container with the execution of this command. The coverage log file is no longer needed after the successful completion of the command.</p>
    <h3>Interlude</h3>
    <p>You can now repeat the execution of the software and enter any resulting coverage log into further test sessions. CodeCover provides you with a command that can merge existing test sessions into a single test session. If you did not create multiple test sessions, or do not want to merge the test sessions, then you can skip the following step and go on to the <a href="#GenerateReport">creation of the report</a>.</p>
    <h3>Merge test sessions</h3>
    <p>Multiple test sessions can be merged into a single test session, by the following command:</p>
    <pre><code>codecover merge-sessions --container <em>SimpleJavaApp</em>/test-session-container.xml 
                         --session TestSession1 
                         --session TestSession2 
                         --name &quot;TestSession1+2&quot;
                         --comment &quot;TestSession1 and TestSession2&quot;</code></pre>
    <ul>
      <li> The option <code>container</code> refers to the test session container which holds the test sessions to be merged.</li>
      <li> The option <code>session</code> refers to individual test sessions that are to be merged. It is assumed here that you created a second test session with the name &quot;TestSession2&quot;.</li>
      <li> The option <code>name</code> refers to the name of the merged test session.</li>
      <li> The option <code>comment</code> refers to an optional comment that can be added to the merged test session.</li>
    </ul>
    <p>Execution of this command creates a new test session with the name &quot;TestSession1+2&quot;, that holds all the coverage data of the previously separate test sessions.</p>
    <h3><a name="GenerateReport" id="GenerateReport"></a>Generate Report</h3>
    <p>Finally CodeCover can generate a report of one of the test sessions in the test session container with the following command:</p>
    <pre><code>codecover report --container <em>SimpleJavaApp</em>/test-session-container.xml 
                 --destination <em>SimpleJavaApp</em>/report/<em>SimpleJavaApp</em>Report.html 
                 --session &quot;TestSession1+2&quot;
                 --template CODECOVER_HOME/report-templates/HTML_Report_hierarchic.xml</code></pre>
    <ul>
      <li> The option <code>container</code> refers to the test session container which holds the test session from which a report is generated.</li>
      <li> The option <code>destination</code> refers to the HTML file that servers as the initial page of the report.</li>
      <li> The option <code>session</code> refers to the name of the test session from which a report is generated. In this case it is assumed that you merged two test sessions into the given one. Was this not the case, simply substitute the name of the test session with one of a test session from which you want to generate a report.</li>
      <li> The option <code>template</code> refers to the template used in the generation of the report.</li>
    </ul>
    <p>Execution of this command will generate a report of the given test session at the specified location, for you to examine and concludes this tutorial.</p>

m4_web_create_footer
