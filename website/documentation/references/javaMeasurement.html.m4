<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: javaMeasurement.html.m4 28 2008-05-28 20:31:17Z ahija $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`Measurement under Java')

<!-- TODO <div class="content" id="DIV1" onclick="return DIV1_onclick()"> -->
    <h2>Measurement under Java</h2>
    <ul>
      <li><a href="#CoverageCriteria">Coverage criteria</a>
        <ul>
          <li><a href="#StatementCovereage">Statement coverage</a></li>
          <li><a href="#BranchCovereage">Branch criteria</a></li>
          <li><a href="#ConditionCoverage">Condition coverage</a></li>
          <li><a href="#LoopCoverage">Loop coverage</a></li>
        </ul>
      <li><a href="#TestCaseSelection">Test case selection</a>
        <ul>
          <li><a href="#OneTestCase">One test case</a></li>
          <li><a href="#Comments">Comments</a></li>
          <li><a href="#MethodCalls">Method calls</a></li>
          <li><a href="#JUnit">JUnit</a></li>
        </ul>
      </li>
      <li><a href="#CoverageResult">Coverage result</a></li>
      <li><a href="#SpecialCharacteristics">Special characteristics</a></li>
    </ul>
    <p>The following sections describe some special features of the instrumentation and coverage measurement process, which are provided by the Java instrumenter shipped together with <em>CodeCover</em>.</p>


    <h3><a name="CoverageCriteria" id="CoverageCriteria"></a>Coverage criteria</h3>
    <p>CodeCover can measure a number of so called coverage criteria. The basics are explained on the feature page <a href="../../features/coverage.html">Code Coverage</a> and in the <a href="../../development/Specification.pdf">Specification</a>. There are some specifics for Java, that are necessary to mention, which is done in the next few lines.</p>

    <h4><a name="StatementCovereage" id="StatementCovereage"></a>Statement coverage</h4>
    <p>Statement coverage is only measured for the following statement types:</p>
    <ul>
        <li>assignments and method calls (<code>a = b</code> or <code>System.out.println(person.getName)</code>)</li>
        <li><code>break</code></li>
        <li><code>continue</code></li>
        <li><code>return</code></li>
        <li><code>throw</code></li>
        <li>empty statements <code>;</code></li>
        <li>variable and member declarations with an assignment (<code>int i = 0</code>)</li>
    </ul>
    <p>In consequence, all conditional statements (e.g. <code>if</code>) and looping statements (e.g. <code>while</code>) are not considered for statement coverage. If a statement is executed and it causes an exception &ndash; e.g. a <code>NullPointerException</code> &ndash; the statement will nevertheless be covered. But the next statement will be uncovered, which can be a hint for an exception.</p>

    <h4><a name="BranchCovereage" id="BranchCovereage"></a>Branch coverage</h4>
    <p>Branches are created by:</p>
        <ul>
        <li><code>if</code> and <code>else</code></li>
        <li><code>switch</code>-branches: <code>case</code> and <code>default</code></li>
        <li><code>try</code> has a branch for each <code>catch</code> and one single branch for an uncatched exception and the successful execution of the whole <code>try</code>-block</li>
    </ul>
    <p>The branch coverage, that is measured for Java, does not consider the body of a looping statement as a branch. Moreover the branches, created by possible exceptions &ndash; e.g. a <code>NullPointerException</code> &ndash; are ignored too.</p>

    <h4><a name="ConditionCoverage" id="ConditionCoverage"></a>Condition coverage</h4>
    <p>Boolean terms within the following statements are considered:</p>
    <ul>
        <li><code>if</code></li>
        <li><code>for</code>, if it is no <a href="http://java.sun.com/docs/books/jls/third_edition/html/statements.html#14.14.2">enhanced for statement</a></li>
        <li><code>while</code></li>
        <li><code>do .. while</code></li>
    </ul>
    <p>Boolean expressions in assignments, method parameters and ternary operators (<code>a ? b : c</code>) are ignored.</p>
    <p>Our approach for conditional coverage needs a special instrumentation of Java boolean terms. Unfortunately this approach makes a boolean expression more complex. If assignments are used in boolean terms of <code>if</code>, <code>while</code> and so on, the compilers might get problems, to decide about the <a href="http://java.sun.com/docs/books/jls/third_edition/html/defAssign.html">definite assignment</a> when analyzing the control flow. Take a look at the following example:</p>
    <pre><code>public class B {
    public Object bar() { return null; }
    public Object foo(Object b) throws Exception {
        Object a;
        if (b == null || ((a = bar()) == null)) { throw new Exception();}
        return a.toString();
    }
}</code></pre>
    <p>Getting to know, whether <code>a</code> is definitely assigned before its usage is very complex after instrumentation. For this reason, CodeCover <b>does not</b> consider boolean expression, when there occurs at least one assignment (<code>a = b</code>) operator.</p>

    <h4><a name="LoopCoverage" id="LoopCoverage"></a>Loop coverage</h4>
    <p>Loop coverage is measured for the following statements:</p>
    <ul>
        <li><code>for</code>, even if it has a fixed number of executions</li>
        <li><code>while</code></li>
        <li><code>do .. while</code> (the coverable item for zero-execution is not required!)</li>
    </ul>


    <h3><a name="TestCaseSelection" id="TestCaseSelection"></a>Test case selection</h3>
    <p>All coverage data collected during a test run is called a test session. You can subdivide this test session into test cases. There is a number of methods to do this.</p>

    <h4><a name="OneTestCase" id="OneTestCase"></a>One test case</h4>
    <p>If you just instrument your classes and run it on your own, only one test case containing
      all coverage data will be produced. This test case has the name <em>UNNAMED TESTCASE</em>.</p>

    <h4><a name="Comments" id="Comments"></a>Comments</h4>
    <p>You can use specific comments in source files, that will be instrumented. These comments will be translated into method calls during the instrumentation process. For this reason, it is important, that you place these comments at such positions in your source files, where a method call is allowed. Otherwise you will receive compiler errors.</p>
    <p>The possible comments are: </p>
    <pre><code>// startTestCase("NAME");
// startTestCase("NAME", "COMMENT");
// endTestCase();
// endTestCase("NAME");
// endTestCase("NAME", "COMMENT");
// finishTestSession();</code></pre>
    <p>You have to ensure, that a comment looks exactly like one of these patterns. Otherwise a comment will be ignored. </p>
    <p>The start and end test case comments explicitly start or end a test case. The start of a test case implicitly ends a prior test case, if it has not ended yet. So two test cases can not overlap.</p>
    <p>The finish test session comment forces the coverage measurement to stop, flush all remaining coverage counters and close the log. No more coverage results will be collected after this call. You can use this method for example to finish the coverage measurement of a Java dynamic web project. Here the <code>ShutdownHook</code> approach of <em>CodeCover</em> might not catch the shutdown of the server or <code>webapp</code>.</p>

    <h4><a name="MethodCalls" id="MethodCalls"></a>Method calls</h4>
    <p>If you like the method of the comments, but want to use dynamic <code>Strings</code> as parameters, you can use specific method calls in your own test suite. For this approach, you need to add a special <code>jar</code> file to your class path:</p>
    <p><code>%CodeCoverRelease%/lib/java-protocol-dummy.jar</code></p>
    <p>And you have to call one of the methods of the class:</p>
    <p><code>org.codecover.instrumentation.java.measurement.Protocol</code></p>
    <p>Here is an example for this approach:</p>
    <pre><code>try {
    Person person = null;
    for (int i = 0; i < MAX; i++ {
        person = persons[i];
        Protocol.startTestCase(person.getName());
        person.prepare();
        person.callTestMethods();

        // throws Exception
        String result = person.checkResults();
        Protocol.endTestCase(person.getName(), result);
    }
} catch (Exception e) {
    Protocol.endTestCase(person.getName(),
        "An Exception occurred " + e.getMessage()) ;
}</code></pre>

    <h4><a name="JUnit" id="JUnit"></a>JUnit</h4>
    <p><em>CodeCover</em> supports JUnit for the test case selection too. For this approach you don't need to instrument your JUnit <code>TestCases</code>. But you have to use so called <code>TestRunners</code> provided by <em>CodeCover</em> in the jar:</p>
    <p><code>JUnit-TestRunner.jar</code></p>
    <p>There are a number of <code>TestRunners</code> available:</p>
    <ul>
      <li><code>org.codecover.junit3.swing.TestRunner:</code><br />
        a Swing UI for JUnit 3.x<br />
      </li>
      <li><code>org.codecover.junit3.awt.TestRunner:</code><br />
        an AWT UI for JUnit 3.x<br />
      </li>
      <li><code>org.codecover.junit3.text.TestRunner:</code><br />
        a command line UI for JUnit 3.x<br />
      </li>
      <li><code>org.codecover.junit4.core.TestRunner:</code><br />
        a command line UI for JUnit 4.x<br />
      </li>
    </ul>
    <p>These test runners can be called by:</p>
    <pre><code>java -cp junit.jar:JUnit-TestRunner.jar:bin
     org.codecover.junit.swing.TestRunner
     [-methodsAsTestCases]
     de.myprogram.AllTests</code></pre>
    <p>Where <code>AllTests</code> is a JUnit test suite. There is an optional argument: <code>-methodsAsTestCases</code>. <br />
      This tells <em>CodeCover</em> whether to use JUnit test cases or the methods of JUnit test cases as test cases in the understanding of the software. <em>CodeCover</em> will explicitly start end end test cases synchronously to JUnit. If there are own calls of start test case, using one of the methods described above, they might be ignored, because JUnit test cases have a higher priority.</p>

    <h3><a name="CoverageResult" id="CoverageResult"></a>Coverage result</h3>
    <p>The coverage results of a test run is stored in a so called <code>coverage log</code> file. Per default its file name has the following format:</p>
    <p><code>coverage-log-yyyy-MM-dd-HH-mm-ss-SSS.clf</code></p>
    <p>This file is stored in the current directory. To change the target of this coverage log file, you can use one of the following methods:</p>
    <ol>
      <li>System property:<br />
        You can set a so called system property when starting your instrumented program. The name of the property is:<br />
        <code>org.codecover.coverage-log-file.</code><br />
        An example call:
        <pre><code>java -Dorg.codecover.coverage-log-file=archiv/coverage1.clf
     -cp bin:library.jar
     Main</code></pre>
      </li>
      <li>Environment variable:<br />
        You can set a system variable with the name:<br />
        <code>org.codecover.coverage-log-file</code><br />
      </li>
      <li><code>CoverageLogPath:</code><br />
        After having instrumented your source files, additionally classes have been added. One of them is: <code>org.codecover.instrumentation.java.measurement.CoverageLogPath</code><br />
        You just can change the return value of the method <code>getDefaultPath()</code> to another file name or another path. Afterwards you have to recompile this class. </li>
    </ol>
    <p>If a coverage log file with the same name already exists, the name of the new <code>coverage log</code> file will be extended by <em>(1)</em>. If you want to enable overwriting, set the following system variable to <code>true</code> or <code>on</code>:</p>
    <code>org.codecover.coverage-log-overwrite</code>
    <p>Example:</p>
    <pre><code>java -Dorg.codecover.coverage-log-file=archiv/coverage2.clf
     -Dorg.codecover.coverage-log-overwrite=true
     -cp bin:library.jar
     Main</code></pre>
    <p>Note: Only if there was at least one coverage data&mdash;e.g. one statement executed&mdash;the coverage log file is produced. Otherwise this file is not created, indicating that no coverage was made.</p>

    <h3><a name="SpecialCharacteristics" id="SpecialCharacteristics"></a>Special characteristics</h3>
    <p>When instrumenting your Java source files, some additional files will be added. They are in the following package:</p>
    <p><code>org.codecover</code></p> 
    <p>You have to compile them and add them to your class path when executing your program, because these files are needed for the coverage measurement.</p>
    <p>The coverage measurement under Java is <b>not</b> threadsafe. For this reason, the number of executions for a statement might not be correct, when your program uses multithreading. Nevertheless, CodeCover gets to know when a statement is covered correctly.</p>
    <p>The measurement approach under Java uses a so called <code>ShutdownHook</code>, to guarantee, that all measurement results are captured. If you use an own <code>ShutdownHook</code> its effected coverage data might not be captured.</p>
    <p>If you use an own <code>ClassLoader</code> you have to ensure, that this classloader is not instrumented because the instrumentation causes problems in this specific case.</p>

m4_web_create_footer
