<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: how_to_junit.html.m4 32 2009-03-09 12:33:36Z schmidberger $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`HOWTO Coverage measurement with JUnit support')

    <h2>HOWTO Coverage measurement with JUnit support</h2>
    <p><em>CodeCover</em> has a support for JUnit. Using the <code>TestCases</code> of JUnit, you can partition your coverage measurement. The test cases of <em>CodeCover</em> have then the same names as the ones of JUnit.</p>
    <p>This section contains step-by-step instructions on how to measure coverage with JUnit support using the command-line interface of <em>CodeCover</em> and JUnit. A technical overview can be found in <a href="m4_web_rootdir/documentation/references/javaMeasurement.html#JUnit" title="Measurement under Java">Measurement under Java</a>. A part of this tutorial mentions <em>instrumentation</em>, <em>analysis</em> and <em>report generation</em>. A more detailed explanation can be found in <a href="how_to_batch.html" title="Use CodeCover with the command-line interface">Use CodeCover with the command-line interface</a>.</p>
    <p><em>CodeCover</em> is integrated into Eclipse too, so you can also use JUnit in Eclipse to create test cases. (see <a href="m4_web_rootdir/documentation/references/eclManual.html#JUnit" title="CodeCover Measurement for JUnit in Eclipse">CodeCover Measurement for JUnit in Eclipse</a>.</p>

    <h3>Preconditions</h3>
    <p>To illustrate the coverage measurement with JUnit support, we discuss a small example. Let's say you have an ordinary project with the following structure:</p>
    <ul>
        <li><code>src/</code>
        <ul>
            <li><code>Person.java</code></li>
        </ul></li>
        <li><code>junit/</code>
        <ul>
            <li><code>PersonTest.java</code></li>
        </ul></li>
        <li><code>bin/</code></li>
        <li><code>lib/</code>
        <ul>
            <li><code>junit3.8.1.jar</code></li>
            <li><code>JUnit-TestRunner.jar</code> (from the <em>CodeCover</em> release dir)</li>
        </ul></li>
    </ul>
    <p>Moreover, you have scripts or other methods to compile your <code>src</code> and <code>junit</code> directory into <code>bin</code>.</p>

    <h3>Instrumentation</h3>
    <p>You can start the instrumentation with the following command:</p>
    <pre><code>codecover instrument --root-directory src
                     --destination src.instr
                     --container person_tsc.xml
                     --language java
                     --verbose</code></pre>
    <p>This will instrument all the source files from the folder <code>src</code> into the folder <code>src.instr</code>. The test session container will be saved in <code>person.tsc</code>.</p>

    <h3>Coverage measurement</h3>
    <p>Now you have to adapt your compile scripts to compile the instrumented source files from <code>src.instr</code> rather than the original source files from <code>src</code> into <code>bin</code>. Do not forget to compile the helper classes in the newly added package <code>org.codecover.instrumentation</code>.</p>
    <p>Running the test is easy now &ndash; just call one of the <em>CodeCover</em> <code>TestRunners</code> &ndash; e.g. the Swing-version. It registers some listeners and then calls the JUnit <code>Swing TestRunner</code> itself:</p>
    <pre><code>java -cp bin;lib\junit3.8.1.jar;lib\JUnit-TestRunner.jar
     -methodsAsTestCases
     org.codecover.junit3.swing.TestRunner PersonTest</code></pre>
    <p>The flag <code>-methodsAsTestCases</code> tells the <code>TestRunner</code> to use a <em>CodeCover</em> test cases for each JUnit test method. Otherwise a <em>CodeCover</em> test case is used for every JUnit test case class.</p>
    <p>After you executed the tests, there will be a file in your project folder such as <code>coverage-log-2007-11-27-19-23-53-077.clf</code>. This file contains the coverage data.</p>

     <h3>Analysis</h3>
     <p>To analyze the coverage data, you have to load it into your test session container <code>person_tsc.xml</code>:</p>
     <pre><code>codecover analyze --container person_tsc.xml
                  --coverage-log coverage-log-2007-11-27-19-23-53-077.clf
                  --name JUnitTestSession
                  --comment &quot;Two errors occurred.&quot;</code></pre>
     <p>A brief <code>info</code> will tell you which test cases have been captured:</p>
     <pre><code>codecover info --container person_tsc.xml
               --test-cases
               --verbose</code></pre>

     <h3>Report</h3>
     <p>Finally, you can generate a report out of the test session container <code>person_tsc.xml</code>.</p>
     <pre><code>codecover report --container person_tsc.xml
                 --destination report.html 
                 --session &quot;JUnitTestSession&quot;
                 --template CODECOVER_HOME/report-templates/HTML_Report_hierarchic.xml</code></pre>

m4_web_create_footer
