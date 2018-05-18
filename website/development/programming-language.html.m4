<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: programming-language.html.m4 30 2008-05-28 20:49:16Z ahija $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`Add a new programming language')

    <h2>Add a new programming language</h2>
    <ul>
        <li><a href="#preface">Preface</a></li>
        <li><a href="#parser">Parser</a>
        <ul>
            <li><a href="#parser_grammar">Grammar</a></li>
            <li><a href="#parser_test">Test the grammar</a></li>
            <li><a href="#parser_nodecounter">Instrumentable item counter</a></li>
            <li><a href="#parser_jtb">Java Tree Builder (JTB)</a></li>
            <li><a href="#parser_create">Create the final parser</a></li>
            <li><a href="#parser_adapt">Adapt the parser</a></li>
        </ul>
        </li>
        <li><a href="#instrumenter">Instrumenter</a>
        <ul>
            <li><a href="#instrumenter_instrumenter">Instrumenter</a></li>
            <li><a href="#instrumenter_hierarchy">HierarchyLevel</a></li>
            <li><a href="#instrumenter_descriptor">InstrumenterDescriptor</a></li>
            <li><a href="#instrumenter_criteria">Criteria</a></li>
            <li><a href="#instrumenter_visitor">InstrumentationVisitor</a></li>
        </ul>
        </li>
        <li><a href="#instrumentation">Instrumentation</a>
        <ul>
            <li><a href="#instrumentation_design">Design the coverage measurement</a></li>
            <li>
            <ul>
                <li><a href="#instrumentation_covdata">Coverage data</a></li>
                <li><a href="#instrumentation_design_statement">Statement coverage</a></li>
                <li><a href="#instrumentation_design_branch">Branch coverage</a></li>
                <li><a href="#instrumentation_design_loop">Loop coverage</a></li>
                <li><a href="#instrumentation_design_condition">Condition coverage</a></li>
            </ul>
            </li>
            <li><a href="#instrumentation_manipulator">The manipulator concept</a></li>
            <li><a href="#instrumentation_real">Real instrumentation</a></li>
            <li>
            <ul>
                <li><a href="#instrumentation_real_counterprovider">CounterIDProvider</a></li>
                <li><a href="#instrumentation_real_statement">Statement Instrumentation</a></li>
                <li><a href="#instrumentation_real_branch">Branch Instrumentation</a></li>
                <li><a href="#instrumentation_real_loop">Loop Instrumentation</a></li>
                <li><a href="#instrumentation_real_condition">Condition Instrumentation</a></li>
            </ul>
            </li>
            <li><a href="#instrumentation_log">Coverage log file</a></li>
        </ul>
        </li>
        <li><a href="#mast">MAST</a></li>
        <li>
        <ul>
            <li><a href="#mast_model">The syntaxtree model</a></li>
            <li><a href="#mast_attic">StatementAttic</a></li>
            <li><a href="#mast_programunit"><code>PROGRAM</code> unit</a></li>
            <li><a href="#mast_basicstatement">The <code>BasicStatements</code></a></li>
            <li><a href="#mast_if">The <code>ConditionalStatements</code></a></li>
            <li><a href="#mast_while">The <code>LoopingStatement</code></a></li>
        </ul>
        </li>
     <!--   <li><a href="#advanced">Advanced features</a></li>
        <ul>
            <li><a href="#advanced_hierarchy">Hierarchy levels</a></li>
            <li><a href="#advanced_logger">Logger</a></li>
            <li><a href="#advanced_directives">Instrumenter directives</a></li>
        </ul>
        -->
        <li><a href="#download">Download sources</a></li>
    </ul>
    <br/>
    <hr/>



    <!--
        PREFACE CHAPTER
      -->
    <h2><a name="preface">Preface</a></h2>
    <p>CodeCover is shipped with support for <i>Java 1.4</i>, <i>Java 1.5</i> and <i>VS COBOL 1985</i>. These pages describe how to use CodeCover with another programming language.</p>
    <p>Before we start to explain what you need and how you can integrate your programming language, it is import to understand our process of coverage measurement. Therefore, you can take a look at the design document &ndash; the chapter <i>2.1 Process chain</i> gives an overview of the process: instrumentation, compilation and execution, analysing, creating the report (see the <a href="Design.pdf">design document</a>).</p>
    <p>CodeCover is written in Java 1.5 and designed to support various programming languages. This is achieved by having strict interfaces. On the one side of these interfaces is the individual programming language. It has a special grammar and its semantic. On the other side of the interfaces are:</p><ul>
    <li>an abstract representation of the source code,</li>
    <li>a generic metric model</li>
    <li>and a report generation, using these two element.</li></ul>
    <p>If you want to enable support for a new programming language, it is your task to write a collection of Java classes making your programming language compatible with these interfaces.</p>
    <p>Although you can implement these interfaces in the order you want, it is recommended to use the approach we have used for Java &ndash; respectively COBOL. Why? Because we can describe this approach and we know, that it works.</p>
    This approach has the following steps:
    <ul>
        <li>create the parser
        <ul>
            <li>create a javacc grammar or use one of the available grammars</li>
            <li>use the Java Tree Builder (JTB) to generate a syntaxtree and an annotated grammar</li>
            <li>create a parser</li>
            <li>adapt the generated files</li>
        </ul>
        </li>
        <li>create the instrumenter</li>
        <li>instrument the code files
        <ul>
            <li>design the coverage measurement</li>
            <li>add counting statement</li>
            <li>implement the counter logging</li>
            <li>test case handling</li>
        </ul>
        </li>
        <li>create the <i>More Abstract Syntaxtree</i> (MAST)</li>
    </ul>
    <p>Because examples are a very good way for learning, we use an imaginary programming language <i>Xampil</i> to explain the steps of our approach. Of course, this imaginary programming language has no compiler, so code in this language can not be executed. Moreover, this programming language &ndash; especially its grammar &ndash; is not complete. We just want to give you an overview, what you have to do in each step and how time-consuming each step might be for your programming language. To sum this up: we think that by explaining the required steps for <i>Xampil</i>, you learn, what you have to do for your programming language.</p>
    <p>We provide a lot of source code examples that grow step by step. If you want to have a look at the final instrumenter, take a look at the download section at the end.</p>
    <p>How <i>Xampil</i> looks like shows <a href="programming-language-files/example.xpl"><code>example.xpl</code></a>:</p>
    <pre><code>// example.xpl
DECLARATION
    BOOLEAN b := FALSE

    INTEGER i := -1

    STRING characters := &quot;unset&quot;
PROGRAM
    i := 0

    IF TRUE THEN
        i := 1
    ELSE
        i := 0
    ENDIF

    WHILE ((i &lt;&gt; 0) AND (i &lt;= 10)) DO
        i := i + 1
    ENDWHILE

    SWITCH i
        CASE 1 :
            characters := &quot;i was 1&quot;
        ENDCASE
        CASE 10 :
            characters := &quot;i was 10&quot;
        ENDCASE
        DEFAULT :
            characters := &quot;i was not set to 1 or 10&quot;
        ENDCASE
    ENDSWITCH

    FILE OVERWRITE &quot;target.log&quot; i
ENDPROGRAM</code></pre>
    <p>There in one requirement each programming language has to fullfill to be used with CodeCover:</p>
    <ul>
        <li>it has to be able to write strings and integers into files with a common character encoding (UTF-8 is recommended)</li>
    </ul>



    <!--
        PARSER CHAPTER
      -->
    <h2><a name="parser">Parser</a></h2>
    <p>The basis of the whole concept is a parser, that parses source files of your programming language. In addition to that, it builds up a syntaxtree of your source file. Later, this syntaxtree can be traversed by visitors. We use javacc and the Java Tree Builder for this purpose. They are both purely java and we used them successfully.</p>

    <h3><a name="parser_grammar">Grammar</a></h3>
    <p>To generate the parser, you need a grammar of your programming language. If you are not so advanced in building compilers and using grammars, this step might be a little bit difficult. The javacc grammars are based on BNF and regular expressions.</p>
    <p>You can have a look at the <a href="https://javacc.dev.java.net/servlets/ProjectDocumentList?folderID=110&amp;expandFolder=110&amp;folderID=109">javacc grammar repository</a>. There is a list of grammars for the most important programming languages. Maybe, your programming language is in the list.</p>
    <p>If not, you have to create an own grammar. We can not tell you how to do this in these chapters, but there is a documentation shipped with the javacc release.</p>
    <p>At this point, we want to start with our example programming language <i>Xampil</i>. Therefore, we have provided a javacc <a href="programming-language-files/xampil.jj">grammar file</a> for <i>Xampil</i>.</p>
    <p>As you can see, the grammar reminds you of java code which is enriched by regular expressions and tags similar to XML. There are different sections in each grammar file.</p>
    <h4>A section for options</h4>
    <pre><code>options {
  UNICODE_INPUT = true;
  ERROR_REPORTING = true;
  USER_CHAR_STREAM = true;
  STATIC = false;
  JDK_VERSION = &quot;1.5&quot;;
  FORCE_LA_CHECK = true;
}</code></pre>
    <p>This tells the javacc parser generator, how to create the parser. Moreover some options affect the performance of the generated parser. Activated <code>ERROR_REPORTING</code> allows you to check, why a code file is rejected by the parser. A more verbose <code>ParseException</code> is thrown. But the performance decreases. The flag <code>STATIC</code> tells the generator, whether to have one static parser or instances of the parser. <code>UNICODE_INPUT</code> tells the parser to read Unicode instead of ASCII characters. The feature <code>USER_CHAR_STREAM</code> is needed later, so ensure that it is enabled. It tells the parser to use an interface as the <code>CharStream</code> instead of directly using a <code>SimpleCharStream</code>. All the options are explained in the <code>javaccgrm.html</code> of the javacc documentation.</p>
    <h4>A section for the Parser</h4>
    <p>Here you can give the parser class a name and you can put code that you want to add to the generated code.</p>
    <pre><code>PARSER_BEGIN(XampilParser)
package org.codecover.instrumentation.xampil.parser;

public class XampilParser
{/* you can paste you code here */}

PARSER_END(XampilParser)</code></pre>
    <h4>A section for the tokens</h4>
    <pre><code>[..]
SPECIAL_TOKEN :
{
    &lt;SPACE_CHAR: &quot; &quot; | &quot;\t&quot; | &quot;\f&quot;&gt;
}
[..]
TOKEN :
{
    &lt; PROGRAM: &quot;PROGRAM&quot; &gt;
  | &lt; ENDPROGRAM: &quot;ENDPROGRAM&quot; &gt;

  | &lt; IF: &quot;IF&quot; &gt;
  | &lt; THEN: &quot;THEN&quot; &gt;
  | &lt; ELSE: &quot;ELSE&quot; &gt;
  | &lt; WHILE: &quot;WHILE&quot; &gt;
  | &lt; DO: &quot;DO&quot; &gt;
  | &lt; END: &quot;END&quot; &gt;
}</code></pre>
    <p>The tokens can be subdivided into <code>TOKEN</code>, <code>SPECIAL_TOKEN</code>, <code>SKIP</code> and <code>MORE</code>. The skip tokens are just ignored by the parser. The special tokens are not used for the BNF productions, but added to the generated tokens. The real Tokens are used for the BNF. Code created by the Java Tree Builder will transform these tokens to nodes of a syntaxtree.</p>
    <p><b>Important hint:</b> ensure, that your grammar has declared all kind of white spaces (<code>&quot; &quot;</code>, <code>&quot;\n&quot;</code>, <code>&quot;\r&quot;</code>, <code>&quot;\t&quot;</code>, ...) as <code>SPECIAL_TOKEN</code>s rather than <code>SKIP</code> tokens. This is important for the offset calculation and reproduction.</p>
    <h4>A section for the productions</h4>
<pre><code>void IfStatement():
{}
{
    &lt;IF&gt; BooleanExpression() &lt;THEN&gt; &lt;EOL&gt;
        ( Statement() )*
    (
      &lt;ELSE&gt; &lt;EOL&gt;
        ( Statement() )*
    )?
    &lt;END&gt; &lt;EOL&gt;
}
[..]</code></pre>
    <p>The productions of a javacc grammar describe BNF productions. You can use identifiers of other productions or predefined tokens within a production declaration.</p>
    <p>The syntax of the javacc grammar file is explained in the <code>javaccgrm.html</code> of the javacc documentation.</p>

    <h3><a name="parser_test">Test the grammar</a></h3>
    <p>After you have created the grammar, you have to ensure, that it is correct. This can be done by simply invoking the javacc parser generator. It will tell you, if there are <code>ParseExceptions</code>. If you have not downloaded it yet, you have to do it now. Look at <a href="https://javacc.dev.java.net/">javacc.dev.java.net</a> and download the javacc 4.0 parser generator. After you have extracted the zip file, you will find a <code>doc</code> folder with the javacc documentation and a <code>bin</code> folder with the <code>javacc.jar</code>. Now you can run the parser generator:</p>
    <pre><code>java -cp javacc-4.0/bin/lib/javacc.jar javacc
     -output_directory=src/org/codecover/instrumentation/xampil/parser/
      xampil.jj</code></pre>
    <p>If your grammar is correct, you will see an output like this:</p>
    <pre><code>Java Compiler Compiler Version 4.0 (Parser Generator)
  (type &quot;javacc&quot; with no arguments for help)
Reading from file xampil.jtb.jj . . .
Note: UNICODE_INPUT option is specified. Please make sure you create
  the parser/lexer using a Reader with the correct character encoding.
File &quot;TokenMgrError.java&quot; does not exist.  Will create one.
File &quot;ParseException.java&quot; does not exist.  Will create one.
File &quot;Token.java&quot; does not exist.  Will create one.
File &quot;SimpleCharStream.java&quot; does not exist.  Will create one.
Parser generated with 0 errors and 0 warnings.</code></pre>
    <p>If there are warnings or errors, you have to solve them before continuing with the next step!</p>
    <p>We recommend to use a script &ndash; such as <a href="http://ant.apache.org/">apache ant</a> &ndash; to run the javacc command. Why? You will see that you have to run the parser generator very often when you adapt you grammar. By using a script, you needn't type in the commands and you are sure, that you use the same command every time.</p>
    <p>To test the parser, you can use a short java <code>main</code> method like this:</p>
    <pre><code>File targetFile = new File(&quot;example.xpl&quot;);
FileInputStream fileInputStream = new FileInputStream(targetFile);
InputStreamReader inputStreamReader = new InputStreamReader(
        fileInputStream, Charset.forName(&quot;UTF-8&quot;));
BufferedReader bufferedReader = new BufferedReader(
        inputStreamReader);
XampilParser parser = new XampilParser(bufferedReader);
parser.CompilationUnit();
bufferedReader.close();</code></pre>
    <p>What is this code doing? It is just parsing the file <a href="programming-language-files/example.xpl"><code>example.xpl</code></a> with respect to the given grammar. We have not provided any code, that should be executed, when a token is found. This will do the Java Tree Builder.</p>

    <h3><a name="parser_nodecounter">Instrumentable item counter</a></h3>
    <p>This section is needed because of the special properties of our programming language <i>Xampil</i>. Although you might not understand every detail of this section, it is the best way to apply these changes now.</p>
    <p>The reason for all this trouble lies in the declaration of possible counters. We have to declare additional integer variables to count the execution of statements and branches. <i>Xampil</i> does not allow to declare variables when you need them, you have to declare them in the <code>DECLARATION</code> section. In the consequence we get into trouble when instrumenting: we have to declare counting variables before we get to know how much variables we need. There are a number of solutions for this problem. Two of them are:</p>
    <ul>
        <li>Visit the syntaxtree twice. First we count the number of statements. In the second run we can declare the exact number of counters.</li>
        <li>Count the number of <code>Statement()</code> productions while parsing a source file.</li>
    </ul>
    <p>For being more performat, we choose the second approach. This requires changes in the grammar, before going on to the next step. If you need this approach for your programming language too, we recommend to do these changes now. If you have a programming language like Java, you can skip this step. Why? You can position declarations like this:</p>
    <pre><code>public class Person {
    String name, address;

    public Person(String name) {
        this.name = name;
        statementCounter[0]++;
        this.address = null;
        statementCounter[1]++;
    }

    public static int statementCounter = new int[2];
}</code></pre>
    <p>The counters can be declared <b>after</b> all statements where visited. For this reason, you can count the required counters and set the number afterwards.</p>
    <p>But back to <i>Xampil</i>, where this method is not working. We will add a class that is counting the statements, branches, loops and boolean expressions in the source file. This is done during the parsing and will be logged in a special object: <a href="programming-language-files/parser/InstrumentableItemCounter.java"><code>InstrumentableItemCounter</code></a>. You have to put this file into the <code>parser folder</code>.</p>
    <p>Now we want to adapt the grammar because we have to add some statements to notify the <code>InstrumentableItemCounter</code>. Some examples are shown in the following extract. The whole file is called <a href="programming-language-files/xampil.counter.jj"><code>xampil.counter.jj</code></a>.</p>
    <pre><code>[..]

public class XampilParser
{ <b>private InstrumentableItemCounter counter = new InstrumentableItemCounter();</b> }

[..}

void CompilationUnit(<b>InstrumentableItemCounter counter</b>):
{ <b>this.counter = counter;</b> }
{
    Declaration()
    Program()
    ( &lt;EOL&gt; )?
    &lt;EOF&gt;
    <b>{ this.counter = new InstrumentableItemCounter(); }</b>
}

[..]

void Statement():
<b>{ this.counter.incrementStatementCount(); }</b>
{
    AssignmentStatement()
  | IfStatement()
  | WhileStatement()
  | SwitchStatement()
  | FileStatement()
}

[..]

void IfStatement():
{}
{
    &lt;IF&gt; Expression() &lt;THEN&gt; &lt;EOL&gt;
    <b>{ this.counter.incrementBranchCount(); }</b>
        ( Statement() )*
    (
        &lt;ELSE&gt; &lt;EOL&gt;
            <b>{ this.counter.incrementBranchCount(); }</b>
            ( Statement() )*
    )?
    &lt;ENDIF&gt; &lt;EOL&gt;
}

[..]

void WhileStatement():
{
    <b>this.counter.incrementLoopCount();</b>
}

[..]

void SwitchStatement():
{}
{
    &lt;SWITCH&gt; &lt;IDENTIFIER&gt; &lt;EOL&gt;
    (
        &lt;CASE&gt; Expression() &lt;COLON&gt; ( &lt;EOL&gt; )?
            <b>{ this.counter.incrementBranchCount(); }</b>
            ( Statement() )*
        &lt;ENDCASE&gt; &lt;EOL&gt;
    )+
    (
        &lt;CASE_DEFAULT&gt; &lt;COLON&gt; ( &lt;EOL&gt; )?
            <b>{ this.counter.incrementBranchCount(); }</b>
            ( Statement() )*
        &lt;ENDCASE&gt; &lt;EOL&gt;
    )?
    &lt;ENDSWITCH&gt; &lt;EOL&gt;
}

[..]</code></pre>
    <p>The counter for the conditions is a little bit more complex. We do not only need the number of conditions but the number of basic boolean terms for each condition. Here we use an <code>IntegerContainer</code>, that we hand over to the <code>Expression()</code> production. This collects the number of basic booleans recursively. Afterwards, we know which condition has how many basic booleans. This is only an extract of the changes:</p>
    <pre><code>public class XampilParser
{
    private InstrumentableItemCounter counter = new InstrumentableItemCounter();

    <b>static interface IntegerContainer {
        public void set(int value);
        public int get();
    }

    static class RealIntegerContainer implements IntegerContainer {
        int value = 0;

        public void set(int value) {
            this.value = value;
        }

        public int get() {
            return this.value;
        }
    }

    static class DummyIntegerContainer implements IntegerContainer {
        public void set(int value) {}

        public int get() {
            return 0;
        }
    }

    static final DummyIntegerContainer DUMMY_CONTAINER = new DummyIntegerContainer();</b>
}

[..]

void AssignmentStatement():
{}
{
    &lt;IDENTIFIER&gt; &lt;ASSIGN&gt; Expression(<b>DUMMY_CONTAINER</b>) &lt;EOL&gt;
}

void IfStatement():
{
    <b>RealIntegerContainer basicBooleanCounter = new RealIntegerContainer();</b>
}
{
    &lt;IF&gt; Expression(<b>basicBooleanCounter</b>)
    <b>{ this.counter.incrementConditionCount(basicBooleanCounter.get()); }</b>
    &lt;THEN&gt; &lt;EOL&gt;

[..]

void Expression(<b>IntegerContainer basicBooleanCounter</b>):
{}
{
   OrExpression(<b>basicBooleanCounter</b>)
}

[..]

void EqualityExpression(IntegerContainer basicBooleanCounter):
{
    int basicBooleanCountBefore = basicBooleanCounter.get();
}
{
    RelationalExpression(<b>basicBooleanCounter</b>)
    ( ( &lt;EQ&gt; | &lt;NEQ&gt; ) RelationalExpression(<b>basicBooleanCounter</b>)
      <b>{ basicBooleanCounter.set(basicBooleanCountBefore + 1); }</b> ) ?
}

[..]

void BasicExpression(<b>IntegerContainer basicBooleanCounter</b>):
{}
{
    &lt;IDENTIFIER&gt; <b>{ basicBooleanCounter.set(basicBooleanCounter.get() + 1); }</b>
  | &lt;INTEGER_LITERAL&gt;
  | &lt;STRING_LITERAL&gt;
  | &lt;TRUE&gt;
  | &lt;FALSE&gt;
  | &lt;LPAREN&gt; Expression(<b>basicBooleanCounter</b>) &lt;RPAREN&gt;
}</code></pre>
    <p>After we have added this feature and regenerated the parser using javacc, we can pimp up our <code>main</code> method:</p>
    <pre><code>File targetFile = new File(&quot;example.xpl&quot;);
FileInputStream fileInputStream = new FileInputStream(targetFile);
InputStreamReader inputStreamReader = new InputStreamReader(
        fileInputStream, Charset.forName(&quot;UTF-8&quot;));
BufferedReader bufferedReader = new BufferedReader(
        inputStreamReader);
XampilParser parser = new XampilParser(bufferedReader);
<b>InstrumentableItemCounter counter = new InstrumentableItemCounter();</b>
parser.CompilationUnit(<b>counter</b>);
bufferedReader.close();

<b>System.out.println(&quot;Statements:  &quot; + counter.getStatementCount());
System.out.println(&quot;Branches:    &quot; + counter.getBranchCount());
System.out.println(&quot;Loops:       &quot; + counter.getLoopCount());
int conditionCount = counter.getConditionCount();
System.out.println(&quot;Conditions:  &quot; + conditionCount);
for (int i = 0; i &lt; conditionCount; i++) {
    System.out.println(&quot;Condition &quot; + i + &quot;: &quot; + counter.getBasicBooleanCount(i));
}</b></code></pre>
    <p>Now we see the following output for <a href="programming-language-files/example.xpl"><code>exampel.xpl</code></a>:</p>
    <pre><code>Statements:  12
Branches:    6
Loops:       1
Conditions:  2
Condition 0: 0
Condition 1: 2</code></pre>

    <h3><a name="parser_jtb">Java Tree Builder (JTB)</a></h3>
    You have created a javacc grammar of your programming language and have successfully generated a simple parser. Now you need the <a href="http://compilers.cs.ucla.edu/jtb/">JTB</a>. You will receive a <code>jtb132.jar</code>, which is everything you need. Now you can run the JTB:
    <pre><code>java -jar jtb132.jar
     -p org.codecover.instrumentation.xampil // the package of the generated files
     -o xampil.jtb.jj                        // the output grammar
     -printer                                // generate a TreeDumper
     -jd                                     // JavaDoc-friendly comments
     -pp                                     // parent pointers
     -tk                                     // special tokens into the tree
     xampil.counter.jj                       // the source grammar</code></pre>
     <p>After you have run the command, you will see an output like this:</p>
     <pre><code>JTB version 1.3.2
JTB:  Reading from xampil.counter.jj...
JTB:  Input file parsed successfully.
JTB:  &quot;xampil.jtb.jj&quot; generated to current directory.
JTB:  Syntax tree Java source files generated to directory &quot;syntaxtree&quot;.

JTB:  &quot;GJVisitor.java&quot; generated to directory &quot;visitor&quot;.
JTB:  &quot;Visitor.java&quot; generated to directory &quot;visitor&quot;.
JTB:  &quot;GJNoArguVisitor.java&quot; generated to directory &quot;visitor&quot;.
JTB:  &quot;GJVoidVisitor.java&quot; generated to directory &quot;visitor&quot;.
JTB:  &quot;GJDepthFirst.java&quot; generated to directory &quot;visitor&quot;.
JTB:  &quot;DepthFirstVisitor.java&quot; generated to directory &quot;visitor&quot;.
JTB:  &quot;GJNoArguDepthFirst.java&quot; generated to directory &quot;visitor&quot;.
JTB:  &quot;GJVoidDepthFirst.java&quot; generated to directory &quot;visitor&quot;.

JTB:  &quot;TreeDumper.java&quot; generated to directory &quot;visitor&quot;.
JTB:  &quot;TreeFormatter.java&quot; generated to directory &quot;visitor&quot;.

0 warnings, 0 errors.</code></pre>
    <p>What has the JTB done? It has parsed the <code>xampil.jj</code> grammar file. For every production of the BNF, a syntaxtree node is generated. The syntaxtree is a collection of class files in the directory <code>syntaxtree</code>, that all implement the interface <code>Node</code>.</p>
    <p>Then there is a directory <code>visitor</code>. It contains visitors to traverse the syntaxtree nodes by using the <i>visitor design pattern</i>.</p>
    <p>Last not least the original grammar file is annotated to <code>xampil.jtb.jj</code>, which has about four times the size of the original one. JTB has added code to the original grammar file to tell the parser how to build up the syntaxtree nodes for every production that is found.</p>
    <p>You can have a look at these files, but they can do nothing without the parser.</p>

    <h3><a name="parser_create">Create the final parser</a></h3>
    Now you have to generate the final parser. This is done by using the exact same command as used for the test &ndash; just with another grammar file:
    <pre><code>java -cp javacc-4.0/bin/lib/javacc.jar javacc
     -output_directory=src/org/codecover/instrumentation/xampil/parser/
      <b>xampil.jtb.jj</b></code></pre>
    <p>Now you can test this parser with a short java <code>main</code> method:</p>
    <pre><code>File targetFile = new File(&quot;example.xpl&quot;);
FileInputStream fileInputStream = new FileInputStream(targetFile);
InputStreamReader inputStreamReader = new InputStreamReader(
        fileInputStream, Charset.forName(&quot;UTF-8&quot;));
BufferedReader bufferedReader = new BufferedReader(
        inputStreamReader);
XampilParser parser = new XampilParser(bufferedReader);
InstrumentableItemCounter counter = new InstrumentableItemCounter();<!-- TODO ?? </b> -->
<b>CompilationUnit compilationUnit = parser.CompilationUnit(counter);
Visitor visitor = new TreeDumper(System.out);
visitor.visit(compilationUnit);</b>

bufferedReader.close();</code></pre>
    <p>If everything is working, you should see the code of the source file in the standard output. But what is this <code>main</code> method doing? First, the source file is parsed. But this time, a syntaxtree is generated. The entry point of the BNF is the token <code>CompilationUnit()</code>. So the root node of our generated syntaxtree is an object of the class with the same name: <code>CompilationUnit</code>.</p>
    <p>As said before, visitors are used to traverse the syntaxtree. Each visitor is given a root node. Then the visitor visits all the children of the root node and their children recursively. The leaves of the syntaxtree are <code>NodeToken</code>. These are nodes having just an image like <code>&quot;IF&quot;</code>, <code>&quot;ENDPROGRAM&quot;</code> or <code>&quot;&gt;=&quot;</code>. Each visitor can decide what should be done, when visiting a node. The <code>TreeDumper</code> used in this <code>main</code> method just prints out all the <code>NodeToken</code> to the standard output. This is why you see the source file in your shell.</p>

    <h3><a name="parser_adapt">Adapt the parser</a></h3>
    <p>To use the parser effectively, you have to change a number of source files manually. This step is needed to provide all the information needed for the further steps. Ensure that you can easily reapply these changes if you have to generate the parser again and overwrite the manual changes. A combination of a version control system and an ant script seems to be perfect for our approach. We recommend to make following changes in an atomic step, because the code files won't compile during the changes.</p>
    <p>You have to use a <code>CharStream</code>, that differs a little bit to the standard character stream generated by javacc. Furthermore you have to use an instance of <code>CharStream</code>, that fits to the new requirements: <code>SimpleCharStream</code>. All files you have to copy &ndash; or overwrite &ndash; are listed below. They are ordered by target folder:</p>
    <ul>
        <li><code>parser</code>
        <ul>
            <li><a href="programming-language-files/parser/CharStream.java"><code>CharStream</code></a></li>
            <li><a href="programming-language-files/parser/ParseException.java"><code>ParseException</code></a></li>
            <li><a href="programming-language-files/parser/SimpleCharStream.java"><code>SimpleCharStream</code></a></li>
            <li><a href="programming-language-files/parser/Token.java"><code>Token</code></a></li>
        </ul>
        </li>
        <li><code>syntaxtree</code>
        <ul>
            <li><a href="programming-language-files/syntaxtee/NodeToken.java"><code>NodeToken</code></a></li>
        </ul>
        </li>
        <li><code>visitor</code>
        <ul>
            <li><a href="programming-language-files/visitor/StartOffset.java"><code>StartOffset</code></a></li>
            <li><a href="programming-language-files/visitor/EndOffset.java"><code>EndOffset</code></a></li>
            <li><a href="programming-language-files/visitor/StringTreeDumper.java"><code>StringTreeDumper</code></a></li>
            <li><a href="programming-language-files/visitor/TreeDumper.java"><code>TreeDumper</code></a></li>
        </ul>
        </li>
    </ul>
    <p>The rest of the changes, you have to do manually. First you have to go to <code>XampilParser</code> and change two lines in the inner class <code>JTBToolkit</code>. The following constructor call occurs twice. Change</p>
    <pre><code>new NodeToken(t.image.intern(), t.kind, t.beginLine, t.beginColumn,
              t.endLine, t.endColumn)</code></pre>
    <p>into</p>
    <pre><code>new NodeToken(t.image.intern(), t.kind, t.beginLine, t.endLine,
              t.beginColumn, t.endColumn, t.beginOffset, t.endOffset)</code></pre>
    <p>Pay attention to the change of the parameter order.</p>
    <p>The last change considers the <code>XampilParserTokenManager</code>. Search for the method <code>jjFillToken()</code>. There you have to add two lines:</p>
    <pre><code>t.beginLine = input_stream.getBeginLine();
t.beginColumn = input_stream.getBeginColumn();
t.endLine = input_stream.getEndLine();
t.endColumn = input_stream.getEndColumn();
<b>t.beginOffset = input_stream.getBeginOffset();</b>
<b>t.endOffset = input_stream.getEndOffset();</b>
return t;</code></pre>
    <p>Why do you have to change all these classes? The approach we have made to describe the position within a source file is based on <i>offsets</i>. This means, that all the tokens have a start and an end offset. The ordinary <code>SimpleCharStream</code> does not provide such information. So we had to add this feature. The <code>XampilParserTokenManager</code> creates tokens. This token manager has to save the current offset information in an object of the type <code>Token</code>. The <code>JTBToolkit</code>, that transforms <code>Token</code> into <code>NodeToken</code>, has to save these information by handing them over to the constructor of <code>NodeToken</code>. All these changes are important for the creation of <i>MAST</i> elements (see <a href="#mast">MAST</a>).</p>
    <p>If you try to compile the <code>org.codecover.instrumentation.xampil</code> folder now, there should be no compiler errors. If you want to run our <code>main</code> method, you have to do some changes here too:</p>
    <pre><code>File targetFile = File(&quot;example.xpl&quot;);
FileInputStream fileInputStream = new FileInputStream(targetFile);
InputStreamReader inputStreamReader = new InputStreamReader(
        fileInputStream, Charset.forName(&quot;UTF-8&quot;));
BufferedReader bufferedReader = new BufferedReader(
        inputStreamReader);
CharStream charStream = new SimpleCharStream(bufferedReader);
XampilParser parser = new XampilParser(charStream);
CompilationUnit compilationUnit = parser.CompilationUnit();
<b>PrintWriter writer = new PrintWriter(System.out);
Visitor visitor = new TreeDumper(writer);</b>
visitor.visit(compilationUnit);
<b>writer.flush();</b>

bufferedReader.close();</code></pre>



    <!--
        INSTRUMENTER CHAPTER
      -->
    <h2><a name="instrumenter"></a>Instrumenter</h2>
    <p>After creating and adapting the parser you have to create the instrumenter. When you finished this chapter you will have an instrumenter, an instrumenter descriptor and an instrumentation visitor. All these components will be integrated into CodeCover, so it should be possible to call your instrumenter from the batch interface of CodeCover. But the instrumenter we build in this chapter won't instrument a file, it just puts out the source file. The real instrumentation is topic of the next chapter. This chapter contains essential preparatory work that has to be done before you can start with the real instrumentation.</p>
    <p>CodeCover contains some interfaces and abstract classes you should know about. In this chapter we discuss the most important ones and give an example for our Xampil programming language.</p>
    <ul>
        <li><b>Instrumenter</b>: This is an abstract Instrumenter implementing all the needed basic method.</li>
        <li><b>InstrumenterDescriptor</b>: It's purpose is to provide information of the corresponding Instrumenter, e.g. which programming language the instrumenter works with.</li>
        <li><b>HierarchyLevel</b>: A HierarchyLevel is a program object which can contain other HierarchyLevels or StatementSequences, e.g. Java packages, files, classes and functions.</li>
        <li><b>Criterion</b>: This interface describes a criterion for code coverage.</li>
        <li><b>InstrumentationVisitor</b>: Traverses the syntax tree and does the &quot;real&quot; work. </li>
    </ul>
    <h3><a name="instrumenter_instrumenter">Instrumenter</a></h3>
    <p>First of all, you have to create an instrumenter class which extends <code>org.codecover.instrumentation.instrumenter</code> class. The methods instrumentThis, getPackageHierarchyLevelType and allowsFileListInstrumentation needs to be implemented. Here is an example of <a href="programming-language-files/xampil/Instrumenter.java"><code>Instrumenter.java</code></a> for the Xampil programming language.</p>
    <pre><code>package org.codecover.instrumentation.xampil;

class Instrumenter extends org.codecover.instrumentation.Instrumenter {

    public Instrumenter() {
        super();
    }

    protected void instrumentThis(Reader source,
            Writer target,
            MASTBuilder database,
            SourceFile sourceFile,
            HierarchyLevelContainer hierarchyLevelContainer,
            String testSessionContainerUID,
            Map instrumenterDirectives) throws ParseException,
            IOException {
        ...
    }

    protected HierarchyLevelType getPackageHierarchyLevelType(MASTBuilder database) {
        return HierarchyLevelTypes.getSourceFileType(database);
    }

    public boolean allowsFileListInstrumentation() {
        return false;
    }
}</code></pre>
    <p>The allowsFileListInstrumentation method states whether or not this instrumenter allows the instrumentation of more than one source file at a run. The instrumenter for our Xampil programming language does not allow it, that is why the method returns false.</p>
    <p>To explain the use of the getPackageHierarchyLevelType method you need some understanding of the hierarchy level concept of CodeCover which is described later in this guide. At this point we just set the hierarchy level to the <code>SourceFileType</code>. We will create the class <code>HierarchyLevelTypes</code> in the next subsection.</p>
    <p>The <code>instrumentThis</code> method controls the instrumentation process. That means it starts the parsing of the source file and the traversal of the syntax tree. Before we go further into this we put our focus on the parameters.</p>
    <p>Target and source are easy to understand. The target is the writer that writes the instrumented file and the source is the reader that reads the original source file.</p>
    <p>More complex is the MASTBuilder. CodeCover transforms the parsed source code into an own syntax tree which we named MAST. This syntax tree contains a lot of special information like offsets or coverage data. It is your job to create the MAST objects during the instrumentation. There is a whole chapter about MAST at the end of this guide.</p>
    <p>The <code>SourceFile</code> is a container for the source code and the name of the file. It is part of the CodeCover database. This object is needed during the instrumentation to create objects which contain the location of a certain token in the source file.</p>
    <p>Every test session container has a unique ID. This ID is needed as part of the coverage log file and for this reason have to be given into the instrumentation process.</p>
    <p>The map of instrumenter directives contains the name of all registered directives and a corresponding object which could be used for a specific behavior of the instrumentation.</p>
    <p>As it is stated above, the <code>instrumentThis</code> method controls the instrumentation process, so you have to implement this method. In the last chapter you have seen how to parse and traverse the source file. Some of this code we will need now again. Here is an example of the <code>instrumentThis</code> method for the Xampil programming language.</p>
    <pre><code>SimpleCharStream simpleCharStream = new SimpleCharStream(source);
XampilParser xampilParser = new XampilParser(simpleCharStream);
CompilationUnit compilationUnit = xampilParser.CompilationUnit();
PrintWriter targetPrintWriter = new PrintWriter(target);
InstrumentationVisitor instrumentationVisitor = new InstrumentationVisitor(
    targetPrintWriter, database, sourceFile, hierarchyLevelContainer,
    testSessionContainerUID);
instrumentationVisitor.visit(compilationUnit);
targetPrintWriter.flush();</code></pre>
    <p>After parsing the source code an instrumentation visitor object is created. This class is explained later in this chapter. Afterwards we start the traversal.</p>
    <h3><a name="instrumenter_hierarchy">HierarchyLevel</a></h3>
    <p>The hierarchy levels are needed for programming languages that structures the program into packages and files like Java. Our Xampil language don't need more than one hierarchy level, the source file level. In the instrumenter class we used a class named HierarchyLevelTypes. This class provides the HierarchyLevelType object for source files and have to be written by you. Here is an example of <code>HierarchyLevelTypes.java</code> for the <i>Xampil</i> programming language.</p>
    <pre><code>package org.codecover.instrumentation.xampil;
public class HierarchyLevelTypes {
    private static final String SOURCE_FILE_INTERN = &quot;sourceFile&quot;;
    private static final String SOURCE_FILE = &quot;Xampil source file&quot;;
    private static final String PROGRAM_INTERN = &quot;program&quot;;
    private static final String PROGRAM = &quot;program&quot;;
    private static HierarchyLevelType sourceFile = null;
    private static HierarchyLevelType program = null;

    public static HierarchyLevelType getSourceFileType(MASTBuilder builder) {
        if (sourceFile == null) {
            return sourceFile = builder.createHierarchyLevelType(SOURCE_FILE,
                    SOURCE_FILE_INTERN);
        }
        return sourceFile;
    }

    public static HierarchyLevelType getProgramType(MASTBuilder builder) {
        if (program == null) {
            return program = builder.createHierarchyLevelType(PROGRAM,
                    PROGRAM_INTERN);
        }
        return program;
    }
}</code></pre>
    <p>As you can see, the class contains only two methods. We have already used the method <code>getSourceFileType</code> in the instrumenter class. This method creates a <code>HierarchyLevelType</code> for source files and returns that object. The other method <code>getProgramType</code> will be used later in the <code>InstrumentationVisitor</code> (see <a href="#mast_programunit"><code>PROGRAM</code> unit</a>).</p>
    <p>For our <i>Xampil</i> programming language this is all about hierarchy levels you need to do. If you want to learn more about it you may have a look at the advanced section at the end of this guide.</p>

    <h3><a name="instrumenter_descriptor">InstrumenterDescriptor</a></h3>
    <p>The next step is to create the <code>InstrumenterDescriptor</code> class which should be extended from <code>org.codecover.instrumentation.InstrumenterDescriptor</code>. The instrumenter descriptor contains information about the corresponding instrumenter, for example the author, the name of the programming language, the charset, the instrumenter directives or the supported code coverage criteria. CodeCover asks the descriptor if the instrumenter matches the programming language the user wants to instrument. Here is an example of <a href="programming-language-files/xampil/InstrumenterDescriptor.java"><code>InstrumenterDescriptor.java</code></a> for the Xampil programming language.</p>
    <pre><code>package org.codecover.instrumentation.xampil;
public class InstrumenterDescriptor extends
    org.codecover.instrumentation.InstrumenterDescriptor {

    private static final String INSTRUMENTER_UNIQUE_KEY = &quot;CodeCover_Xampil_2007&quot;;
    private static final String LANGUAGE = &quot;Xampil 2007&quot;;
    private static final Pattern NAME_PATTERN = Pattern.compile(&quot;Xampil(( )?(20)?07)?&quot;,
            Pattern.CASE_INSENSITIVE);
    private static final String DESCRIPTION = &quot;Instrumenter for Xampil.&quot;;
    private static final String AUTHOR = &quot;Author of this guide&quot;;
    private static final Charset DEFAULT_CHARSET = Charset.defaultCharset();

    public InstrumenterDescriptor() {
        super(INSTRUMENTER_UNIQUE_KEY);
        super.setLanguageName(LANGUAGE);
        super.setDescription(DESCRIPTION);
        super.setAuthor(AUTHOR);
        super.setDefaultCharset(DEFAULT_CHARSET);
    }

    @Override
    public boolean isLanguageSupported(String languageNameToCheck) {
        return NAME_PATTERN.matcher(languageNameToCheck).matches();
    }

    @Override
    protected Instrumenter getInstrumenter() {
        return new org.codecover.instrumentation.xampil.Instrumenter();
    }

    @Override
    public boolean accept(File file) {
        return FileTool.getExtension(file).equals(&quot;xpl&quot;);
    }
}</code></pre>
    <p>Most part of the code should be understandable. First, we set all the information like author or criteria. Afterwards we have to override some methods. The <code>getInstrumenter</code> returns an instance of a instrumenter child class, that fits to the information provided by this descriptor. With the accept method you can decide which files should be excepted by the instrumenter for instrumentation. In the case of Xampil we choose the file extension &quot;xpl&quot;. FileTool is a utility class shipped by CodeCover. It is in the <code>org.codecover.model.utils.file</code> package.</p>
    <p>As you may have noticed, the name is realized as pattern, because that way it is easier to check whether the instrumenter supports a certain programming language or not. In addition, the user can type in Xampil in upper case or lower, it may be Xampil with 2007 or without and a lot of other typing the user likes. The <code>isLanguageSupported</code> method enables CodeCover to ask the instrumenter descriptor for the language support the instrumenter supports.</p>
    <h3><a name="instrumenter_criteria">Criteria</a></h3>
    <p>There are a lot of code coverage criteria defined in books and papers. We decided to use the most important and most known criteria. That are the statement, branch, condition and loop coverage. In the package <code>org.codecover.model.utils.criteria</code> you can find an abstract criterion class, you should use if you want to write your own criteria, and the implementation of the four criteria.</p>
    <p>Back on our example language we decided to show you all four criteria. To inform CodeCover that your instrumenter is able to instrument a certain criterion you have to use the <code>addSupportedCriteria</code> method of the instrumenter descriptor class. Add the following lines into the constructor of our <code>InstrumenterDescriptor</code> class.</p>
    <pre><code>super.addSupportedCriteria(StatementCoverage.getInstance());
super.addSupportedCriteria(BranchCoverage.getInstance());
super.addSupportedCriteria(ConditionCoverage.getInstance());
super.addSupportedCriteria(LoopCoverage.getInstance());</code></pre>
    <h3><a name="instrumenter_visitor">InstrumentationVisitor</a></h3>
    <p>In the <code>instrumentThis</code> method of our instrumenter class we create an instrumentation visitor. This is the main class in the instrumentation process. At this point of the guide we just need a basic class with a constructor to reach the goal. Here is an example of  <a href="programming-language-files/visitor/InstrumentationVisitor.1.java"><code>InstrumentationVisitor.java</code></a> for the <i>Xampil</i> programming language.</p>
    <pre><code>package org.codecover.instrumentation.xampil.visitor;
public InstrumentationVisitor(PrintWriter writer,
                              MASTBuilder builder,
                              SourceFile sourceFile,
                              HierarchyLevelContainer hierarchyLevelContainer,
                              String testSessionContainerUID) {
    super(writer);
    this.builder = builder;
    this.sourceFile = sourceFile;
    this.hierarchyLevelContainer = hierarchyLevelContainer;
    this.testSessionContainerUID = testSessionContainerUID;
}</code></pre>
    <p>Now you are ready to start your instrumenter by CodeCover. The only thing you have to do is packing all the files into a jar-file and adding this file to the java class path. It should be possible to start the <i>Xampil</i> instrumenter by giving &quot;xampil&quot; to the CodeCover <code>-l</code> option.</p>



    <!--
        INSTRUMENTATION CHAPTER
      -->
    <h2><a name="instrumentation">Instrumentation</a></h2>
    <h3><a name="instrumentation_design">Design the coverage measurement</a></h3>
    <h4><a name="instrumentation_covdata">Coverage data</a></h4>
    <p>Before you start to implement the instrumentation, you have to get to know, how you are going to instrument. In other words: you have to design how you want to collect coverage data. Coverage data can be measured for statements, branches, loops and boolean expressions. Coverage data is just a number, that represents, how often a statement is executed or a branch is entered. An element is called covered, if it is executed at least once.</p>
    <p>You can decide, which <i>criteria</i> you want to instrument. This affects, which <i>coverage metrics</i> you can calculate. For example the coverage metric <code>org.codecover.metrics.coverage.StatementCoverage</code> needs the criteria <code>org.codecover.model.utils.criteria.StatementCoverage</code> instrumented in the source files. Then the metric can calculate, how many statements are covered &ndash; executed at least once &ndash; and which statements are not covered.</p>
    <p>CodeCover has already implemented the following criteria for the programming languages Java and COBOL:</p>
    <ul>
        <li>Statement coverage</li>
        <li>Branch coverage</li>
        <li>Loop coverage</li>
        <li>Strict condition coverage</li>
    </ul>
    <p>If you have questions understanding these terms, you can have a look in the <a href="Specification.pdf">Specification</a>. The glossary at the end of the specification explains all these terms.</p>
    <p>We now explain theoretically the instrumentation approach for all four coverage criteria. The final implementation is considered afterwards.</p>

    <h4><a name="instrumentation_design_statement">Statement coverage</a></h4>
    <p>The coverage measurement can be achieved by using counters. This is a very simple method, but it leads to a successful coverage measurement. Now you can start to think of your programming language. Create a small example program with as many language constructs, as you can think of. Now try to add counters after every statement<!--TODO ?? </b>-->, you want to measure coverage of.</p>
    <p>An example for instrumentation of statement coverage of our <a href="programming-language-files/example.xpl"><code>exampel.xpl</code></a> is shown in <a href="programming-language-files/example.instr-st.xpl"><code>exampel.instr-st.xpl</code></a>. The changes to the original source file are highlighted.</p>
    <pre><code>// example.instr-st.xpl
DECLARATION
    BOOLEAN b := FALSE

    INTEGER i := -1

    STRING characters := &quot;unset&quot;

    <b>INTEGER countSt1 := 0
    INTEGER countSt2 := 0
    INTEGER countSt3 := 0
    INTEGER countSt4 := 0
    INTEGER countSt5 := 0
    INTEGER countSt6 := 0
    INTEGER countSt7 := 0
    INTEGER countSt8 := 0
    INTEGER countSt9 := 0
    INTEGER countSt10 := 0
    INTEGER countSt11 := 0</b>
PROGRAM
    i := 0
    <b>countSt1 := countSt1 + 1</b>

    IF TRUE THEN
        i := 1
        <b>countSt2 := countSt2 + 1</b>
    ELSE
        i := 0
        <b>countSt3 := countSt3 + 1</b>
    ENDIF
    <b>countSt4 := countSt4 + 1</b>

    WHILE ((i &lt;&gt; 0) AND (i &lt;= 10)) DO
        i := i + 1
        <b>countSt5 := countSt5 + 1</b>
    ENDWHILE
    <b>countSt6 := countSt6 + 1</b>

    SWITCH i
        CASE 1 :
            characters := &quot;i was 1&quot;
            <b>countSt7 := countSt7 + 1</b>
        ENDCASE
        CASE 10 :
            characters := &quot;i was 10&quot;
            <b>countSt8 := countSt8 + 1</b>
        ENDCASE
        DEFAULT :
            characters := &quot;i was not set to 1 or 10&quot;
            <b>countSt9 := countSt9 + 1</b>
        ENDCASE
    ENDSWITCH
    <b>countSt10 := countSt10 + 1</b>

    FILE OVERWRITE &quot;target.log&quot; i
    <b>countSt11 := countSt11 + 1</b>
ENDPROGRAM</code></pre>
    <p>As you can see, for every basic statement a counter is declared. It is set to zero in its declaration. After the related statement, each counter is incremented. This manual procedure might be a little bit time-consuming, but it gives answers to the following questions:</p>
    <ul>
        <li>which statements should be instrumented and by the way counted</li>
        <li>where can the counters be placed (e.g. the <code>return</code> statement in Java does not allow another statement afterwards)</li>
        <li>where can the counters be declared</li>
        <li>where can the counters be initialized</li>
        <li>can you use integer arrays or do you have to declare a variable for each counter</li>
    </ul>

    <h4><a name="instrumentation_design_branch">Branch coverage</a></h4>
    <p>Now we consider the branches. Branches occur in the program flow at branching statements like <code>if</code> or <code>switch</code>. A branch is covered, if it is entered at least once. To get to know when a branch is entered, we use counters too. The statement and branch instrumentation of <a href="programming-language-files/example.xpl"><code>exampel.xpl</code></a> is shown in <a href="programming-language-files/example.instr-br.xpl"><code>exampel.instr-br.xpl</code></a>:</p>
    <pre><code>// example.instr-br.xpl
DECLARATION
    [..]

    <b>INTEGER countBr1 := 0
    INTEGER countBr2 := 0
    INTEGER countBr3 := 0
    INTEGER countBr4 := 0
    INTEGER countBr5 := 0</b>
PROGRAM
    [..]

    IF TRUE THEN
        <b>countBr1 := countBr1 + 1</b>
        i := 1
        countSt2 := countSt2 + 1
    ELSE
        <b>countBr2 := countBr2 + 1</b>
        i := 0
        countSt3 := countSt3 + 1
    ENDIF

    [..]

    SWITCH i
        CASE 1 :
            <b>countBr3 := countBr3 + 1</b>
            characters := &quot;i was 1&quot;
            countSt7 := countSt7 + 1
        ENDCASE
        CASE 10 :
            <b>countBr4 := countBr4 + 1</b>
            characters := &quot;i was 10&quot;
            countSt8 := countSt8 + 1
        ENDCASE
        DEFAULT :
            <b>countBr5 := countBr5 + 1</b>
            characters := &quot;i was not set to 1 or 10&quot;
            countSt9 := countSt9 + 1
        ENDCASE
    ENDSWITCH

    [..]
ENDPROGRAM
</code></pre>

    <h4><a name="instrumentation_design_loop">Loop coverage</a></h4>
    <p>How is loop coverage meant? Loop coverage considers looping statements like <code>while</code>, <code>repeat until</code> or <code>for</code>. The loop coverage criterion divides the program flow at looping statements into:</p>
    <ul>
        <li>the loop condition is false for the first evaluation &rarr; loop body skipped</li>
        <li>loop body entered exactly once</li>
        <li>loop body entered more than once</li>
    </ul>
    <p>The related metric can use these information to tell you, for example that a loop body of a specific loop has never been skipped. The coverage data is again collected by counters. But this time, we need a temporary variable to count the number of loops, a loop body is entered. The statement, branch and loop instrumentation of <a href="programming-language-files/example.xpl"><code>exampel.xpl</code></a> is shown in <a href="programming-language-files/example.instr-lo.xpl"><code>exampel.instr-lo.xpl</code></a>:</p>
    <pre><code>// example.instr-lo.xpl
DECLARATION
    [..]

    <b>INTEGER countLo1_temp := 0
    INTEGER countLo1_0 := 0
    INTEGER countLo1_1 := 0
    INTEGER countLo1_2 := 0</b>
PROGRAM
    [..]

    <b>countLo1_temp := 0</b>
    WHILE ((i &lt;&gt; 0) AND (i &lt;= 10)) DO
        <b>countLo1_temp := countLo1_temp + 1</b>
        i := i + 1
        countSt5 := countSt5 + 1
    ENDWHILE
    <b>SWITCH countLo1_temp
        CASE 0 :
            countLo1_0 := countLo1_0 + 1
        ENDCASE
        CASE 1 :
            countLo1_1 := countLo1_1 + 1
        ENDCASE
        DEFAULT :
            countLo1_2 := countLo1_2 + 1
        ENDCASE
    ENDSWITCH</b>

    [..]
ENDPROGRAM
</code></pre>

    <h4><a name="instrumentation_design_condition">Condition Coverage</a></h4>
    <p>Condition coverage is a very interesting topic, because there are a number of criteria that all consider the evaluation of boolean terms. They vary in the definition, when a basic boolean term is called covered. We have to introduce the term &quot;basic boolean term&quot;, which is a boolean expression that can not be subdivided into other boolean terms. Where <code>a = 4 AND NOT FALSE</code> can be subdivided, <code>a = 4</code> or <code>vector.isEmpty()</code> can not.</p>
    <p>Different kinds of condition coverage vary in the following ways to decide whether or not a basic boolean term is covered:</p>
    <ul>
        <li>a basic boolean term must be evaluated to <code>TRUE</code> and <code>FALSE</code> or needn't</li>
        <li>there must be two different evaluations of a basic boolean term that have had effect to the result of the evaluation of the whole condition or needn't</li>
        <li>if a basic boolean term has this effect, all other basic boolean terms have to be constant</li>
        <li>how short circuit semantic is handled (having <code>b1 &amp;&amp; b2</code>, Java never evaluates <code>b2</code>, if <code>b1</code> is already evaluated to <code>false</code>)</li>
    </ul>
    <p>To sum this up: we had to introduce a complex model of the boolean terms. When using this model after the execution, we can calculate nearly every condition coverage metric that is is used in common. Therefore, we have to describe the result of every basic boolean term for every evaluation of the whole condition. We distinguish the results of a basic boolean term the following:</p>
    <ul>
        <li>evaluated to <code>FALSE</code></li>
        <li>evaluated to <code>TRUE</code></li>
        <li>not evaluated</li>
    </ul>
    <p>For being very specific for every programming language, the condition coverage approach differs too. We now describe the approach for <i>Xampil</i>, that can be used, if the evaluation of boolean terms has no side effects and short circuit semantic is not used. For another approach you can have a look in the <a href="Design.pdf">design document</a> in <i>Appendix A: &quot;Formal Proof Of Conditional Coverage Instrumentation&quot;</i>.</p>
    <p>The statement, branch, loop and conditional instrumentation of <a href="programming-language-files/example.xpl"><code>exampel.xpl</code></a> is shown in <a href="programming-language-files/example.instr-co.xpl"><code>exampel.instr-co.xpl</code></a>:</p>
    <pre><code>// example.instr-co.xpl
DECLARATION
    [..]

    <b>BOOLEAN countCo1_temp1
    BOOLEAN countCo1_temp2
    INTEGER countCo1_1010 := 0
    INTEGER countCo1_1011 := 0
    INTEGER countCo1_1110 := 0
    INTEGER countCo1_1111 := 0</b>
PROGRAM
    [..]
    <b>countCo1_temp1 := i &lt;&gt; 0
    countCo1_temp2 := i &lt;= 10
    IF countCo1_temp1 THEN
        IF countCo1_temp2 THEN
            countCo1_1111 := countCo1_1111 + 1
        ELSE
            countCo1_1110 := countCo1_1110 + 1
        ENDIF
    ELSE
        IF countCo1_temp2 THEN
            countCo1_1011 := countCo1_1011 + 1
        ELSE
            countCo1_1010 := countCo1_1010 + 1
        ENDIF
    ENDIF</b>
    WHILE ((<b>countCo1_temp1</b>) AND (<b>countCo1_temp2</b>)) DO
        countLo1_temp := countLo1_temp + 1;
        i := i + 1
        countSt5 := countSt5 + 1
    ENDWHILE

    [..]
ENDPROGRAM</code></pre>
    <p>As you can see, we need a lot of code for instrumentation and it is very time-consuming to do this by hand. What have we done in these lines? We have substituted the evaluations in the <code>WHILE</code> statement by <code>BOOLEAN</code> variables, which are evaluated before. Then we combine the states of the two <code>BOOLEAN</code> variables by using an <code>IF</code> cascade. Hereby, we can distinguish four different states of the whole expression:</p>
    <ul>
        <li><code>countCo1_temp1</code> is <code>TRUE</code> and <code>countCo1_temp2</code> is <code>TRUE</code> &rarr; <code>countCo1_1111</code> is incremented</li>
        <li><code>countCo1_temp1</code> is <code>TRUE</code> and <code>countCo1_temp2</code> is <code>FALSE</code> &rarr; <code>countCo1_1110</code> is incremented</li>
        <li><code>countCo1_temp1</code> is <code>FALSE</code> and <code>countCo1_temp2</code> is <code>TRUE</code> &rarr; <code>countCo1_1011</code> is incremented</li>
        <li><code>countCo1_temp1</code> is <code>FALSE</code> and <code>countCo1_temp2</code> is <code>FALSE</code> &rarr; <code>countCo1_1010</code> is incremented</li>
    </ul>
    <p>Why is the variable for example called <code>countCo1_<b>1110</b></code>? By the binary decoding <code>1110</code>, we describe that:</p>
    <ul>
        <li>the first variable is evaluated: <code><b>1</b>110</code></li>
        <li>the first variable has the value <code>TRUE</code>: <code>1<b>1</b>10</code></li>
        <li>the second variable is evaluated: <code>11<b>1</b>0</code></li>
        <li>the second variable has the value <code>FALSE</code>: <code>111<b>0</b></code></li>
    </ul>
    <p>This convention is needed later again.</p>
    <p>Note that the boolean expression of the <code>if</code> is not instrumented, because it is always <code>TRUE</code> and contains no basic boolean term. <code>FALSE</code> is for us just a boolean operator with arity zero.</p>
    <p>By counting all the four states separately, we can afterwards count nearly every condition coverage metric and get to know the value of the whole condition by reproducing the <code>AND</code> evaluation. All in all this instrumentation is the most complex and therefore explained at the end.</p>

    <p>Doing all these instrumentations manually in your programming language for the four coverage criteria is very important. You make fundamental decisions that should be made <i>before</i> starting to implement the instrumentation! Moreover, you get to know some problems of your programming language that make the instrumentation more difficult and it is a good way to handle special cases. Finally you can check your instrumented file against the automatically instrumented file, that you will hopefully receive at the end of this chapter.</p>

    <h3><a name="instrumentation_manipulator">The manipulator concept</a></h3>
    <p>Before we go on with the automatic instrumentation, we want to explain the manipulator concept. This concept is used to delegate all the changes, that should be made by the <code>InstrumentationVisitor</code>. The <code>InstrumentationVisitor</code> gets four <code>Manipulators</code>, one for each criterion but does not know what the <code>Manipulators</code> manipulate. This is a good practice to hide implementation details and we have found a variant how to skip the manipulation for a criterion. This feature is needed, because the user can select which criteria he wants to instrument and which not. All in all the <code>InstrumentationVisitor</code> will know at which positions which manipulator is called for manipulation, but only the specific manipulator will know whether to instrument and what to instrument.</p>
    <p>So we create a sub package <code><b>manipulators</b></code> in the <code>xampil</code> package and an interface for all manipulators: <a href="programming-language-files/manipulator-empty/Manipulator.java"><code>Manipulator</code></a>:</p>
    <pre><code>package org.codecover.instrumentation.xampil.manipulator;

import java.io.PrintWriter;

public interface Manipulator {
    public void setWriter(PrintWriter writer);
}</code></pre>
    <p>Than we create an abstract class <a href="programming-language-files/manipulator-empty/AbstractDummyManipulator.java"><code>AbstractDummyManipulator</code></a>, which will be used by all manipulators, that just implement their interface but do not manipulate anything, because the related criterion is not selected for instrumentation:</p>
    <pre><code>package org.codecover.instrumentation.xampil.manipulator;

import java.io.PrintWriter;

public abstract class AbstractDummyManipulator implements Manipulator {
    public void setWriter(PrintWriter writer) {}
}</code></pre>
    <p>The next abstract class is the <a href="programming-language-files/manipulator-empty/AbstractDefaultManipulator.java"><code>AbstractDefaultManipulator</code></a>, that will be inherited by all classes that have manipulation tasks to do:</p>
    <pre><code>package org.codecover.instrumentation.xampil.manipulator;

import java.io.PrintWriter;

public abstract class AbstractDefaultManipulator implements Manipulator {
    private PrintWriter writer = null;

    public void setWriter(PrintWriter writer) {
        this.writer = writer;
    }

    protected PrintWriter getWriter() {
        return this.writer;
    }
}</code></pre>
    <p>Now you have to create four empty interfaces &ndash; one for each criterion. For example <a href="programming-language-files/manipulator-empty/StatementManipulator.java"><code>StatementManipulator</code></a>:</p>
    <pre><code>package org.codecover.instrumentation.xampil.manipulator;

public interface StatementManipulator extends Manipulator {

}</code></pre>
    <p>And the others:</p>
    <ul>
        <li><a href="programming-language-files/manipulator-empty/BranchManipulator.java"><code>BranchManipulator</code></a></li>
        <li><a href="programming-language-files/manipulator-empty/LoopManipulator.java"><code>LoopManipulator</code></a></li>
        <li><a href="programming-language-files/manipulator-empty/ConditionManipulator.java"><code>ConditionManipulator</code></a></li>
    </ul>
    <p>These interfaces will later receive manipulation statements. But not now &ndash; we just want to create the architecture.</p>
    <p>The next steps is to create four dummy manipulators &ndash; one for each criterion. They implement their interface and extend <code>AbstractDummyManipulator</code>. For example <a href="programming-language-files/manipulator-empty/DummyStatementManipulator.java"><code>DummyStatementManipulator</code></a>:</p>
    <pre><code>package org.codecover.instrumentation.xampil.manipulator;

public class DummyStatementManipulator extends AbstractDummyManipulator
        implements StatementManipulator {

}</code></pre>
    <p>And the others:</p>
    <ul>
        <li><a href="programming-language-files/manipulator-empty/DummyBranchManipulator.java"><code>DummyBranchManipulator</code></a></li>
        <li><a href="programming-language-files/manipulator-empty/DummyLoopManipulator.java"><code>DummyLoopManipulator</code></a></li>
        <li><a href="programming-language-files/manipulator-empty/DummyConditionManipulator.java"><code>DummyConditionManipulator</code></a></li>
    </ul>
    <p>To complete the manipulator collection, you need four default manipulators. Each implements a manipulator interface and extends <code>AbstractDefaultManipulator</code>. For example <a href="programming-language-files/manipulator-empty/DefaultStatementManipulator.java"><code>DefaultStatementManipulator</code></a>:</p>
    <pre><code>package org.codecover.instrumentation.xampil.manipulator;

public class DefaultStatementManipulator extends AbstractDefaultManipulator
        implements StatementManipulator {

}</code></pre>
    <p>And the others:</p>
    <ul>
        <li><a href="programming-language-files/manipulator-empty/DefaultBranchManipulator.java"><code>DefaultBranchManipulator</code></a></li>
        <li><a href="programming-language-files/manipulator-empty/DefaultLoopManipulator.java"><code>DefaultLoopManipulator</code></a></li>
        <li><a href="programming-language-files/manipulator-empty/DefaultConditionManipulator.java"><code>DefaultConditionManipulator</code></a></li>
    </ul>
    <p>After you have created these empty classes, you should 15 files in the folder <code>manipulator</code>:</p>
    <ul>
        <li>the <code>Manipulator</code> interface</li>
        <li>an <code>AbstractDummyManipulator</code> class</li>
        <li>an <code>AbstractDefaultManipulator</code> class</li>
        <li>four interfaces inheriting <code>Manipulator</code></li>
        <li>four dummy classes used for not manipulating</li>
        <li>four default classes where the manipulations will be made later</li>
    </ul>
    <p>What should happen with these manipulators now? We go to the method <code>Instrumenter.instrumentThis(..)</code>. A method of the super <code>Instrumenter</code> is <code>isCriterionSet(Criterion criterion)</code>. We use this method to decide which manipulators the <code>InstrumentationVisitor</code> will get. Add the following lines to the method <code>Instrumenter.instrumentThis(..)</code> before the visit call is done:</p>
    <pre><code>[..]

InstrumentationVisitor instrumentationVisitor = new InstrumentationVisitor(
        targetPrintWriter, database, sourceFile, hierarchyLevelContainer,
    testSessionContainerUID);

<b>if (super.isCriterionSet(StatementCoverage.getInstance())) {
    instrumentationVisitor.setStatementManipulator(
        new DefaultStatementManipulator());
} else {
    instrumentationVisitor.setStatementManipulator(
        new DummyStatementManipulator());
}

if (super.isCriterionSet(BranchCoverage.getInstance())) {
    instrumentationVisitor.setBranchManipulator(
        new DefaultBranchManipulator());
} else {
    instrumentationVisitor.setBranchManipulator(
        new DummyBranchManipulator());
}

if (super.isCriterionSet(LoopCoverage.getInstance())) {
    instrumentationVisitor.setLoopManipulator(
        new DefaultLoopManipulator());
} else {
    instrumentationVisitor.setLoopManipulator(
        new DummyLoopManipulator());
}

if (super.isCriterionSet(ConditionCoverage.getInstance())) {
    instrumentationVisitor.setConditionManipulator(
        new DefaultConditionManipulator());
} else {
    instrumentationVisitor.setConditionManipulator(
        new DummyConditionManipulator());
}</b>

instrumentationVisitor.visit(compilationUnit);

targetPrintWriter.flush();

[..]</code></pre>
    <p>Of course, you have to implement the <code>set</code> methods of the <code>InstrumentationVisitor</code>. There you have to pay attention to set the <code>writer</code> to the new manipulators. For example:</p>
    <pre><code>public void setStatementManipulator(StatementManipulator statementManipulator) {
    this.statementManipulator = statementManipulator;
    this.statementManipulator.setWriter(super.getTargetWriter());
}</code></pre>

    <h3><a name="instrumentation_real">Real instrumentation</a></h3>
    <h4><a name="instrumentation_real_counterprovider">CounterIDProvider</a></h4>
    <p>To manage the number of counters and their format, we have provided the class <a href="programming-language-files/xampil/CounterIDProvider.java"><code>CounterIDProvider</code></a>. This class is responsible to provide an ID for every statement, branch, loop and condition. Therefore counters are held and incremented for the current IDs. The <code>CounterIDProvider</code> has methods to increment the number of one of the current counters &ndash; e.g. for statements. We add the class by creating a new object in the constructor of <code>InstrumentationVisitor</code>. It will be used later.</p>
    <pre><code>[..]

<b>private CounterIDProvider counterIDProvider;</b>

public InstrumentationVisitor(PrintWriter writer,
        MASTBuilder builder,
        SourceFile sourceFile,
        HierarchyLevelContainer hierarchyLevelContainer,
        String testSessionContainerUID) {
    super(writer);
    this.builder = builder;
    this.sourceFile = sourceFile;
    this.hierarchyLevelContainer = hierarchyLevelContainer;
    this.testSessionContainerUID = testSessionContainerUID;
    <b>this.counterIDProvider = new CounterIDProvider();</b>
}

[..]</code></pre>

    <h4><a name="instrumentation_real_statement">Statement Instrumentation</a></h4>
    <p>Now we start to instrument. We begin to consider statements. What do we have to do? We create the method <code>InstrumentationVisitor.visit(Statement n)</code>:</p>
    <pre><code>@Override
public void visit(Statement n) {
   super.visit(n);
   String statementID = this.counterIDProvider.nextStatementID();
   this.statementManipulator.manipulate(n, statementID);
}</code></pre>
    <p>The super call tells the <code>TreeDumper</code> and its father to visit all nodes under <code>Statement</code> and print them to the target <code>writer</code>. The <code>manipulate</code> method tells the <code>StatementManipulator</code>: <i>&quot;I have found a statement. You can now add additional code for instrumentation purpose.&quot;</i> This method has to be created in the interface <code>StatementManipulator</code>, in the <code>DummyStatementManipulator</code> and in the <code>DefaultStatementManipulator</code>. Where the dummy can leave the method empty, the default manipulator has to do the following:</p>
    <pre><code>public void manipulate(Statement n, String statementID) {
    super.getWriter().printf(&quot;%1$s%2$s := %1$s%2$s + 1%n&quot;,
                             CounterIDProvider.VARIABLE_PREFIX,
                             statementID);
}</code></pre>
    <p>That's all. After the visitor has printed all the original token images with the writer, the manipulator adds a statement that increments the counter. The counter name has a prefix for uniqueness and the <code>statementID</code>. For that reason, this counter should be unique and is related to the statement with the given ID.</p>
    <p>We modify our <code>main</code> method to test this manipulation:</p>
    <pre><code>FileInputStream fileInputStream = new FileInputStream(&quot;example.xpl&quot;);
InputStreamReader inputStreamReader = new InputStreamReader(
        fileInputStream, Charset.forName(&quot;UTF-8&quot;));
BufferedReader bufferedReader = new BufferedReader(
        inputStreamReader);
CharStream charStream = new SimpleCharStream(bufferedReader);
XampilParser parser = new XampilParser(charStream);
InstrumentableItemCounter counter = new InstrumentableItemCounter();
CompilationUnit compilationUnit = parser.CompilationUnit(counter);
PrintWriter writer = new PrintWriter(System.out);
<b>InstrumentationVisitor visitor = new InstrumentationVisitor(writer,
                                             null,
                                             null,
                                             null,
                                             null);
visitor.setStatementManipulator(new DefaultStatementManipulator());
visitor.setBranchManipulator(new DefaultBranchManipulator());
visitor.setLoopManipulator(new DefaultLoopManipulator());
visitor.setConditionManipulator(new DefaultConditionManipulator());
visitor.visit(compilationUnit);</b>
writer.flush();

bufferedReader.close();</code></pre>
    <p>And what do we see? An instrumented <a href="programming-language-files/example.autoinstr-st.xpl"><code>example.xpl</code></a>:</p>
    <pre><code>// example.xpl
DECLARATION
    BOOLEAN b := FALSE

    INTEGER i := -1

    STRING characters := &quot;unset&quot;
PROGRAM
    i := 0

    <b>CodeCoverCoverageCounter_S1 := CodeCoverCoverageCounter_S1 + 1</b>
IF TRUE THEN
        i := 1
    <b>CodeCoverCoverageCounter_S2 := CodeCoverCoverageCounter_S2 + 1</b>
ELSE
        i := 0
    <b>CodeCoverCoverageCounter_S3 := CodeCoverCoverageCounter_S3 + 1</b>
ENDIF

    <b>CodeCoverCoverageCounter_S4 := CodeCoverCoverageCounter_S4 + 1</b>
WHILE ((i &lt;&gt; 0) AND (i &lt;= 10)) DO
        i := i + 1
    <b>CodeCoverCoverageCounter_S5 := CodeCoverCoverageCounter_S5 + 1</b>
ENDWHILE

    <b>CodeCoverCoverageCounter_S6 := CodeCoverCoverageCounter_S6 + 1</b>
SWITCH i
        CASE 1 :
            characters := &quot;i was 1&quot;
        <b>CodeCoverCoverageCounter_S7 := CodeCoverCoverageCounter_S7 + 1</b>
ENDCASE
        CASE 10 :
            characters := &quot;i was 10&quot;
        <b>CodeCoverCoverageCounter_S8 := CodeCoverCoverageCounter_S8 + 1</b>
ENDCASE
        DEFAULT :
            characters := &quot;i was not set to 1 or 10&quot;
        <b>CodeCoverCoverageCounter_S9 := CodeCoverCoverageCounter_S9 + 1</b>
ENDCASE
    ENDSWITCH

    <b>CodeCoverCoverageCounter_S10 := CodeCoverCoverageCounter_S10 + 1</b>
FILE OVERWRITE &quot;target.log&quot; i
<b>CodeCoverCoverageCounter_S11 := CodeCoverCoverageCounter_S11 + 1</b>
ENDPROGRAM</code></pre>
    <p>This code is not formatted very well, but the statement instrumentation is working. The only thing, that is missing, is the declaration of the counters. Therefore we have to extend the interface <code>StatementManipulator</code>. We add a method that advises a manipulator to add all required declarations for the counters:</p>
    <pre><code>public interface StatementManipulator extends Manipulator {

    <b>public void writeDeclarations(int statementCount);</b>

    public void manipulate(Statement n, String statementID);
}</code></pre>
    <p>The <code>DummyStatementManipulator</code> does not need this method &ndash; it does not instrument and needs no counters. Consequently, the <code>DummyStatementManipulator</code> has only an empty <code>writeDeclarations()</code> method. But the <code>DefaultStatementManipulator</code> implements this method the following:</p>
    <pre><code>public void writeDeclarations(int statementCount) {
    PrintWriter writer = super.getWriter();
    for (int i = 1; i &lt;= instrumentableItemCount; i++) {
        writer.printf(&quot;INTEGER %s%s := 0%n&quot;,
                CounterIDProvider.VARIABLE_PREFIX,
                CounterIDProvider.generateStatementID(i));
    }
}</code></pre>
    <p>We get the statement count and for every expected statement, we add a counter declaration. Again we use the <code>CounterIDProvider</code> to tell us, how a statement ID with a given number is formatted. After we have implemented this <code>writeDeclarations()</code> method, we should not forget that it is not used yet. We have to call it from the <code>InstrumentationVisitor</code>. For that reason, we have to add the following code (highlighted):</p>
    <pre><code>public InstrumentationVisitor(PrintWriter writer,
        <b>InstrumentableItemCounter counter,</b>
        MASTBuilder builder,
        SourceFile sourceFile,
        HierarchyLevelContainer hierarchyLevelContainer,
        String testSessionContainerUID) {
    super(writer);
    <b>this.counter = counter;</b>
    this.builder = builder;
    this.sourceFile = sourceFile;
    this.hierarchyLevelContainer = hierarchyLevelContainer;
    this.testSessionContainerUID = testSessionContainerUID;
    this.counterIDProvider = new CounterIDProvider();
}

[..]

<b>/**
 * f0 -&gt; Declaration()
 * f1 -&gt; Program()
 * f2 -&gt; ( &lt;EOL&gt; )?
 * f3 -&gt; &lt;EOF&gt;
 */
@Override
public void visit(CompilationUnit n) {
   n.f0.accept(this);
   this.statementManipulator.writeDeclarations(this.counter.getStatementCount());
   n.f1.accept(this);
   n.f2.accept(this);
   n.f3.accept(this);
}</b>

[..]
</code></pre>
    <p>Of course, you have to update all calls of the constructor of the <code>InstrumentationVisitor</code> too. There you have to add the visitor as the second parameter.</p>
    <p>If we run the corrected <code>main</code> method now, we see that the statements are instrumented and the declarations are added too. This instrumented <a href="programming-language-files/example.autoinstr-st2.xpl"><code>example.xpl</code></a> would compile now &ndash; if we had a compiler.</p>
    <pre><code>[..]

    STRING characters := &quot;unset&quot;
    <b>INTEGER CodeCoverCoverageCounter_S1 := 0
    INTEGER CodeCoverCoverageCounter_S2 := 0
    INTEGER CodeCoverCoverageCounter_S3 := 0
    INTEGER CodeCoverCoverageCounter_S4 := 0
    INTEGER CodeCoverCoverageCounter_S5 := 0
    INTEGER CodeCoverCoverageCounter_S6 := 0
    INTEGER CodeCoverCoverageCounter_S7 := 0
    INTEGER CodeCoverCoverageCounter_S8 := 0
    INTEGER CodeCoverCoverageCounter_S9 := 0
    INTEGER CodeCoverCoverageCounter_S10 := 0
    INTEGER CodeCoverCoverageCounter_S11 := 0</b>
PROGRAM

[..]</code></pre>

    <h4><a name="instrumentation_real_branch">Branch Instrumentation</a></h4>
    <p>After we have discussed the instrumentation of statements in detail, you should have got a clue how the other instrumentations may work. For this reason, we do not discuss every detail anymore.</p>
    <p>We start in the <code>InstrumentationVisitor</code> again. We have to add methods to instrument branches. Branches are created by <code>IF</code>, <code>ELSE</code>, <code>CASE</code> and <code>DEFAULT</code>. So we add manipulator calls there. Therefore we have to overwrite <code>visit()</code> methods again. We have to call the <code>accept()</code> methods that tell each <code>Node</code> to accept a visiting. Then we have to add the <code>manipulate</code> calls of the <code>BranchManipulator</code>. To allow a more detailed description, we show an extract of the <code>IF</code> instrumentation. The important lines are highlighted.</p>
    <pre><code>[..]

/**
 * f0 -&gt; &lt;IF&gt;
 * f1 -&gt; Expression(basicBooleanCounter)
 * f2 -&gt; &lt;THEN&gt;
 * f3 -&gt; &lt;EOL&gt;
 * f4 -&gt; ( Statement() )*
 * f5 -&gt; ( &lt;ELSE&gt; &lt;EOL&gt; ( Statement() )* )?
 * f6 -&gt; &lt;ENDIF&gt;
 * f7 -&gt; &lt;EOL&gt;
 */
@Override
public void visit(IfStatement n) {
   n.f0.accept(this);
   n.f1.accept(this);
   n.f2.accept(this);
   n.f3.accept(this);
   <b>String ifBranchID = this.counterIDProvider.nextBranchID();
   this.branchManipulator.manipulateIf(n, ifBranchID);</b>
   n.f4.accept(this);

   NodeOptional elseOption = n.f5;
   String elseBranchID = this.counterIDProvider.nextBranchID();
   if (elseOption.present()) {
       // the else is present
       NodeSequence elseSequence = (NodeSequence) elseOption.node;
       // &lt;ELSE&gt;
       elseSequence.nodes.get(0).accept(this);
       // &lt;EOL&gt;
       elseSequence.nodes.get(1).accept(this);

       <b>this.branchManipulator.manipulateElse(n, elseBranchID, false);</b>

       // ( Statement() )*
       elseSequence.nodes.get(2).accept(this);
   } else {
       <b>// there was no else branch -&gt; create it
       this.branchManipulator.manipulateElse(n, elseBranchID, true);</b>
   }
   n.f6.accept(this);
   n.f7.accept(this);
}

[..]</code></pre>
    <p>We have added a manipulating statement in the if branch. This was expectable. But why do we need two <code>manipualeElse</code> calls? If there already exists an else branch, then we have to visit the <code>ELSE</code> keyword and write our instrumentation afterwards. But if there is no <code>ELSE</code>, we say, that this branch is <i><a name="words_implicit">implicit</a></i>.</p>
    <pre><code>IF a &gt; 5 THEN
    a := 5
ENDIF</code></pre>
    <p>The else branch is not explicit written, but it could be:</p>
    <pre><code>IF a &gt; 5 THEN
    a := 5
<b>ELSE</b>
ENDIF</code></pre>
    <p>The coverage measurement requires that we get to know, if an implicit else branch is &quot;entered&quot; or not. For this reason, we have to add the <code>ELSE</code> keyword in such a case. The <code>DefaultBranchManipulator</code> looks like this:</p>
    <pre><code>public void manipulateIf(IfStatement n, String ifBranchID) {
    super.getWriter().printf(&quot;%1$s%2$s := %1$s%2$s + 1%n&quot;,
            CounterIDProvider.VARIABLE_PREFIX,
            ifBranchID);
}

public void manipulateElse(IfStatement n, String elseBranchID, boolean isImplicit) {
    PrintWriter writer = super.getWriter();
    <b>if (isImplicit) {
        writer.printf(&quot;ELSE%n&quot;);
    }</b>
    writer.printf(&quot;%1$s%2$s := %1$s%2$s + 1%n&quot;,
            CounterIDProvider.VARIABLE_PREFIX,
            elseBranchID);
}</code></pre>
    <p>As you can see, we use exactly the same instrumentation approach as used for instrumenting statements. The handling of an implicit else branch is highlighted.</p>
    <p>We do not discuss the instrumentation of <code>switch</code>, because it is similar. What we have to do, is the declaration of the branch counters. This is done same way as for statement instrumentation:</p>
    <pre><code>@Override
public void visit(CompilationUnit n) {
   n.f0.accept(this);
   this.statementManipulator.writeDeclarations(this.counter.getStatementCount());
   <b>this.branchManipulator.writeDeclarations(this.counter.getBranchCount());</b>
   n.f1.accept(this);
   n.f2.accept(this);
   n.f3.accept(this);
}</code></pre>
    <p>The only thing left is to create the <code>writeDeclarations()</code> method in the <code>BranchManipulator</code>, <code>DummyBranchManipulator</code> and in the <code>DefaultBranchManipulator</code>. Everything is the same, except that you have to use branch IDs this time:</p>
    <pre><code>public class DefaultBranchManipulator extends AbstractDefaultManipulator
        implements BranchManipulator {

    <b>public void writeDeclarations(int statementCount) {
        PrintWriter writer = super.getWriter();
        for (int i = 1; i &lt;= statementCount; i++) {
            writer.printf(&quot;INTEGER %s%s := 0%n&quot;,
                    CounterIDProvider.VARIABLE_PREFIX,
                    CounterIDProvider.generateBranchID(i));
        }
    }</b>

[..]

}</code></pre>
    <p>Our <code>main</code> method will now instrument the input for the measureemnt of statement and branch coverage. The instrumented file can be found here: <a href="programming-language-files/example.autoinstr-br.xpl"><code>example.autoinstr-br.xpl</code></a></p>

    <h4><a name="instrumentation_real_loop">Loop Instrumentation</a></h4>
    <p>The loop instrumentation is a little bit more advanced. Here, we need more than a single counter to be incremented. See <a href="#instrumentation_design_loop">design the loop instrumentation section</a>, if you do not remember, how we want to instrument loops.</p>
    <p>Again, we start in the <code>InstrumentationVisitor</code>. The only loop construct of <i>Xampil</i> is the <code>WHILE</code> loop. So we overwrite its <code>visit</code> method:</p>
    <pre><code>@Override
public void visit(WhileStatement n) {
    String loopID = this.counterIDProvider.nextLoopID();
    <b>this.loopManipulator.manipulateBeforeWhile(n, loopID);</b>

    // &lt;WHILE&gt;
    n.f0.accept(this);
    // Expression(basicBooleanCounter)
    n.f1.accept(this);
    // &lt;DO&gt;
    n.f2.accept(this);
    // &lt;EOL&gt;
    n.f3.accept(this);

    <b>this.loopManipulator.manipulateInWhile(n, loopID);</b>

    // ( Statement() )*
    n.f4.accept(this);
    // &lt;ENDWHILE&gt;
    n.f5.accept(this);
    // &lt;EOL&gt;
    n.f6.accept(this);

    <b>this.loopManipulator.manipulateAfterWhile(n, loopID);</b>
}</code></pre>
    <p>All in all, we need three manipulation methods. Consequently the <code>LoopManipulator</code> and its children will have three methods. The <code>DefaultLoopManipulator</code> has the following implementation:</p>
    <pre><code>private static final String TEMP_COUNTER_SUFFIX = &quot;_temp&quot;;

public void manipulateBeforeWhile(WhileStatement n, String loopID) {
    super.getWriter().printf(&quot;%s%s%s := 0%n&quot;,
                             CounterIDProvider.VARIABLE_PREFIX,
                             loopID,
                             TEMP_COUNTER_SUFFIX);
}

public void manipulateInWhile(WhileStatement n, String loopID) {
    super.getWriter().printf(&quot;%1$s%2$s%3$s := %1$s%2$s%3$s + 1%n&quot;,
                             CounterIDProvider.VARIABLE_PREFIX,
                             loopID,
                             TEMP_COUNTER_SUFFIX);
}

public void manipulateAfterWhile(WhileStatement n, String loopID) {
    super.getWriter().printf(&quot;&quot; +
                 &quot;SWITCH %1$s%2$s%3$s%n&quot; +
                 &quot;    CASE 0 : %1$s%4$s := %1$s%4$s + 1%n&quot; +
                 &quot;    ENDCASE%n&quot; +
                 &quot;    CASE 1 : %1$s%5$s := %1$s%5$s + 1%n&quot; +
                 &quot;    ENDCASE%n&quot; +
                 &quot;    DEFAULT : %1$s%6$s := %1$s%6$s + 1%n&quot; +
                 &quot;    ENDCASE%n&quot; +
                 &quot;ENDSWITCH%n&quot;,
       /* 1$ */  CounterIDProvider.VARIABLE_PREFIX,
       /* 2$ */  loopID,
       /* 3$ */  TEMP_COUNTER_SUFFIX,
       /* 4$ */  CounterIDProvider.generateLoopSubIDZero(loopID).replace('-', '_'),
       /* 5$ */  CounterIDProvider.generateLoopSubIDOne(loopID).replace('-', '_'),
       /* 6$ */  CounterIDProvider.generateLoopSubIDAbove(loopID).replace('-', '_'));
}</code></pre>
    <p>To finish the loop instrumentation, we just have to call the <code>writeDeclarations</code> method in the <code>InstrumentationVisitor</code> and implement it in the <code>DefaultLoopManipulator</code>:</p>
    <pre><code>private static final String TEMP_COUNTER_SUFFIX = &quot;_temp&quot;;

public void writeDeclarations(int loopCount) {
    PrintWriter writer = super.getWriter();
    for (int i = 1; i &lt;= loopCount; i++) {
        String thisLoopID = CounterIDProvider.generateStatementID(i);
        writer.printf(&quot;    INTEGER %1$s%2$s%3$s%n&quot; +
                      &quot;    INTEGER %1$s%4$s := 0%n&quot; +
                      &quot;    INTEGER %1$s%5$s := 0%n&quot; +
                      &quot;    INTEGER %1$s%6$s := 0%n&quot;,
            CounterIDProvider.VARIABLE_PREFIX,
            thisLoopID,
            TEMP_COUNTER_SUFFIX,
            CounterIDProvider.generateLoopSubIDZero(thisLoopID).replace('-', '_'),
            CounterIDProvider.generateLoopSubIDOne(thisLoopID).replace('-', '_'),
            CounterIDProvider.generateLoopSubIDAbove(thisLoopID).replace('-', '_'));
}</code></pre>
    <p>Compared to the other implementations of <code>writeDeclarations</code>, this method is a little bit more complex. We need three counters for each loop. We get their ID from the <code>CounterIDProvider</code>. These IDs contain a minus (<code>-</code>), that is not allowed in the <i>Xampil</i> grammar within an identifier. For this reason, we have to replace each minus by an underscore. Last not least, we have to declare the temporary variable, that counts the number of loops of a while. This temporary variable does not need to be set to zero in the declaration, this is done in front of the related while loop.</p>
    <p>Our <code>main</code> method will now instrument its input for statement, branch and loop coverage. The instrumented file can be found here: <a href="programming-language-files/example.autoinstr-lo.xpl"><code>example.autoinstr-lo.xpl</code></a></p>

    <h4><a name="instrumentation_real_condition">Condition Instrumentation</a></h4>
    <p>For the condition instrumentation we need to parse a conditional expression and store the logical structure of the expression. We create a new visitor for this purpose, namely the <code>XampilExpressionParser</code>. This visitor provides a method to parse an expression. It returns an object of <code>InstrBooleanTerm</code> which contains the logical structure and the basic boolean terms. The <code>InstrBooleanTerm</code> is defined by CodeCover and can be found in the package <code>org.codecover.instrumentation.booleanterms</code></p>
    <pre><code>public class XampilExpressionParser extends GJNoArguDepthFirst&lt;InstrBooleanTerm>
    implements GJNoArguVisitor&lt;InstrBooleanTerm> {

    public InstrBooleanTerm parse(Expression n) {
        return n.accept(this);
    }
}</code></pre>
    <p>The expression parser works like any other visitor, you just overwrite the visit method of the node you want the visitor to do special things. But we have to be sure that every child node of expression returns an <code>InstrBooleanTerm</code> object. For that, we have to implement the <code>visit(Expression n)</code> method.</p>
    <pre><code>public class XampilExpressionParser extends GJNoArguDepthFirst&lt;InstrBooleanTerm>
    implements GJNoArguVisitor&lt;InstrBooleanTerm> {

    public InstrBooleanTerm parse(Expression n) {
        return n.accept(this);
    }

<b>    /**
     * f0 -> OrExpression(basicBooleanCounter)
     */
    public InstrBooleanTerm visit(Expression n) {
        return n.f0.accept(this);
    }</b>
}</code></pre>
    <p>As you can see, we just accept the expression and assume that the visited <code>OrExpression</code> returns an <code>InstrBooleanTerm</code> object. Of course, we have to implement this as well. It is very helpfull to copy the grammar production from the node class to identify the visit method you implement. Here is the code of the <code>OrExpression</code> visit method.</p>
    <pre><code>    /**
     * f0 -> AndExpression(basicBooleanCounter)
     * f1 -> ( &lt;OR&gt; AndExpression(basicBooleanCounter) )*
     */
    @Override
    public InstrBooleanTerm visit(OrExpression n) {
        InstrBooleanTerm returnTerm = n.f0.accept(this);
        for (Node node : n.f1.nodes) {
            NodeSequence nodeSequence = (NodeSequence) node;
            NodeToken operatorToken = (NodeToken) nodeSequence.nodes.get(0);
            InstrBooleanTerm term = nodeSequence.nodes.get(1).accept(this);
            returnTerm = new InstrOperatorTerm(returnTerm,
                    XampilBooleanOperators.getOrOperator(), term,
                    operatorToken.startOffset, operatorToken.endOffset);
        }
        return returnTerm;
    }</code></pre>
    <p>This method is the first step we have to do. First we visit the <code>AndExpression</code> and store the return (returnTerm). After that the grammar allows zero or multiple sequences of <code>OR</code> and <code>AndExpression</code>. So we iterate over all sequences, store the <code>InstrBooleanTerm</code> of the <code>AndExpression</code> (term) and create a new <code>InstrOperatorTerm</code> with the operator <code>OR</code> and operands returnTerm and term.</p>
    <p>The next visit method that has to be implemented is for the node <code>AndExpression</code>.</p>
    <pre><code>    /**
     * f0 -> NotExpression(basicBooleanCounter)
     * f1 -> ( &lt;AND&gt; NotExpression(basicBooleanCounter) )*
     */
    @Override
    public InstrBooleanTerm visit(AndExpression n) {
        InstrBooleanTerm returnTerm = n.f0.accept(this);
        for (Node node : n.f1.nodes) {
            NodeSequence nodeSequence = (NodeSequence) node;
            NodeToken operatorToken = (NodeToken) nodeSequence.nodes.get(0);
            InstrBooleanTerm term = nodeSequence.nodes.get(1).accept(this);
            returnTerm = new InstrOperatorTerm(returnTerm,
                    XampilBooleanOperators.getAndOperator(), term,
                    operatorToken.startOffset, operatorToken.endOffset);
        }
        return returnTerm;
    }</code></pre>
    <p>This is quite the same as above. If an <code>AND</code> operator occurs we create a new <code>InstrOperatorTerm</code> with the operator <code>AND</code> and operands returnTerm and term. The next visit methods should be easy to understand. Here is the listing.</p>
    <pre><code>    /**
     * f0 -> ( &lt;NOT&gt; )?
     * f1 -> EqualityExpression(basicBooleanCounter)
     */
    @Override
    public InstrBooleanTerm visit(NotExpression n) {
        InstrBooleanTerm returnTerm = n.f1.accept(this);
        if (n.f0.present()) {
            NodeToken operatorToken = (NodeToken) n.f0.node;
            returnTerm = new InstrOperatorTerm(XampilBooleanOperators.getNotOperator(),
                    returnTerm,
                    operatorToken.startOffset, operatorToken.endOffset);
        }
        return returnTerm;
    }

    /**
     * f0 -> RelationalExpression(basicBooleanCounter)
     * f1 -> ( ( &lt;EQ&gt; | &lt;NEQ&gt; ) RelationalExpression(basicBooleanCounter) )?
     */
    @Override
    public InstrBooleanTerm visit(EqualityExpression n) {
        InstrBooleanTerm returnTerm = n.f0.accept(this);
        if (n.f1.present()) {
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n);
        }
        return returnTerm;
    }

    /**
     * f0 -> AdditiveExpression(basicBooleanCounter)
     * f1 -> ( ( &lt;LT&gt; | &lt;GT&gt; | &lt;LE&gt; | &lt;GE&gt; ) AdditiveExpression(basicBooleanCounter) )?
     */
    @Override
    public InstrBooleanTerm visit(RelationalExpression n) {
        InstrBooleanTerm returnTerm = n.f0.accept(this);
        if (n.f1.present()) {
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n);
        }
        return returnTerm;
    }

    /**
     * f0 -> MultiplicativeExpression(basicBooleanCounter)
     * f1 -> ( ( &lt;PLUS&gt; | &lt;MINUS&gt; ) MultiplicativeExpression(basicBooleanCounter) )*
     */
    @Override
    public InstrBooleanTerm visit(AdditiveExpression n) {
        InstrBooleanTerm returnTerm = n.f0.accept(this);
        if (n.f1.present()) {
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n);
        }
        return returnTerm;
    }

    /**
     * f0 -> BasicExpression(basicBooleanCounter)
     * f1 -> ( ( &lt;STAR&gt; | &lt;SLASH&gt; ) BasicExpression(basicBooleanCounter) )*
     */
    @Override
    public InstrBooleanTerm visit(MultiplicativeExpression n) {
        InstrBooleanTerm returnTerm = n.f0.accept(this);
        if (n.f1.present()) {
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n);
        }
        return returnTerm;
    }</code></pre>
    <p>The <code>InstrBasicBooleanVisitor</code> is a visitor which converts a node to an <code>InstrBasicBooleanTerm</code> by capturing all <code>NodeToken</code>s, startOffset and endOffset. After writing the last visit method we will discuss the <code>InstrBasicBooleanVisitor</code>. This last method is needed for processing a <code>BasicExpression</code>.</p>
    <pre><code>    /**
     * f0 -> &lt;IDENTIFIER&gt;
     *       | &lt;INTEGER_LITERAL&gt;
     *       | &lt;STRING_LITERAL&gt;
     *       | &lt;TRUE&gt;
     *       | &lt;FALSE&gt;
     *       | &lt;LPAREN&gt; Expression(basicBooleanCounter) &lt;RPAREN&gt;
     */
    @Override
    public InstrBooleanTerm visit(BasicExpression n) {
        InstrBooleanTerm returnTerm = null;
        if (n.f0.which == 0) {
            returnTerm = InstrBasicBooleanVisitor.convertToInstrBasicBoolean(n);
        } else if (n.f0.which == 3) {
            NodeToken token = (NodeToken) n.f0.choice;
            returnTerm = new InstrOperatorTerm(XampilBooleanOperators.getTrueOperator(),
                    token.startOffset, token.endOffset);
        } else if (n.f0.which == 4) {
            NodeToken token = (NodeToken) n.f0.choice;
            returnTerm = new InstrOperatorTerm(XampilBooleanOperators.getFalseOperator(),
                    token.startOffset, token.endOffset);
        } else if (n.f0.choice instanceof NodeSequence) {
            NodeSequence nodeSequence = (NodeSequence) n.f0.choice;
            InstrBooleanTerm expressionTerm = nodeSequence.nodes.get(1).accept(this);
            returnTerm = new InstrBracketTerm(expressionTerm);
        } else {
            throw new RuntimeException("Integer or string literal used as" +
                                            "basic boolean term.");
        }
        return returnTerm;
    }</code></pre>
    <p>A <code>BasicExpression</code> can be a basic boolean term, true, false, an integer or string, or a nested expression. If it is a basic boolean term, an <code>InstrBasicBoolean</code> is created. For true and false, an <code>InstrOperatorTerm</code> with the arity zero is created and nested expression are visited by the <code>visit(Expression n)</code> method.</p>
    <p>After discussing the <code>XampilExpressionParser</code> we change the focus to some helper classes we need for condition instrumentation. The first one we look at is the class <code>InstrBasicBooleanVisitor</code>.</p>
    <pre><code>public class InstrBasicBooleanVisitor extends DepthFirstVisitor {
    private StringWriter writer;

    private int foundStartOffset = -1;

    private int foundEndOffset = -1;

    private InstrBasicBooleanVisitor() {
        this.writer = new StringWriter();
    }

    public void visit(NodeToken n) {
        if (n.numSpecials() > 0) {
            for (NodeToken nt : n.specialTokens) {
                this.writer.write(nt.tokenImage);
            }
        }

        this.writer.write(n.tokenImage);
        if (this.foundStartOffset == -1) {
            this.foundStartOffset = n.startOffset;
        }
        this.foundEndOffset = n.endOffset;
    }

    public static InstrBasicBooleanTerm convertToInstrBasicBoolean(Node n) {
        InstrBasicBooleanVisitor treeStringDumper = new InstrBasicBooleanVisitor();
        n.accept(treeStringDumper);

        return new InstrBasicBooleanTerm(treeStringDumper.writer.toString().trim(),
                treeStringDumper.foundStartOffset,
                treeStringDumper.foundEndOffset);
    }
}</code></pre>
    <p>This visitor dumps all node tokens into a <code>StringWriter</code> for a node given to the <code>convertToInstrBasicBoolean(Node n)</code> method. In addition, the start and end offset of the given node is gathered. All the information is used to create a new <code>InstrBasicBooleanTerm</code> which is returned.</p>
    <p>The second helper class is <code>XampilBooleanOperators</code>. We used its methods in the <code>XampilExpressionParser</code> to generate operator terms. The following listing shows the source.</p>
    <pre><code>public class XampilBooleanOperators {
    public static final String OR = "OR";
    public static final String NOT = "NOT";
    public static final String AND = "AND";
    public static final String TRUE_DESCRIPTION = "TRUE";
    public static final String FALSE_DESCRIPTION = "FALSE";
    private static InstrBooleanOperator orOperator = null;
    private static InstrBooleanOperator andOperator = null;
    private static InstrBooleanOperator notOperator = null;
    private static InstrBooleanOperator trueOperator = null;
    private static InstrBooleanOperator falseOperator = null;

    ...

}</code></pre>
    <p>This class basically contains methods that return <code>InstrBooleanOperator</code>s which store information about possible boolean assignments of an operator. For example two operands connected by an <code>OR</code> operator returns true if one of the two is true. Such information has to be given for every operator your programming language is capable of. In <i>Xampil</i> we have to define the <code>OR</code>, <code>AND</code>, <code>NOT</code>, <code>true</code>, and <code>false</code> operator. True and false are operators with the arity zero in CodeCover!</p>
    <pre><code>
    public static InstrBooleanOperator getOrOperator() {
        if (orOperator == null) {
            Map&lt;BooleanAssignment, Boolean&gt; possibleAssignments =
                    new HashMap&lt;BooleanAssignment, Boolean&gt;();
            BooleanAssignment assignment;

            assignment = new BooleanAssignment(FALSE, FALSE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(FALSE, TRUE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            assignment = new BooleanAssignment(TRUE, FALSE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            assignment = new BooleanAssignment(TRUE, TRUE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            orOperator = InstrBooleanOperator.getTwoArgumentOperator(
                    XampilBooleanOperators.OR, "OR", possibleAssignments);
        }

        return orOperator;
    }

    public static InstrBooleanOperator getAndOperator() {
        if (andOperator == null) {
            Map&lt;BooleanAssignment, Boolean&gt; possibleAssignments =
                    new HashMap&lt;BooleanAssignment, Boolean&gt;();
            BooleanAssignment assignment;

            assignment = new BooleanAssignment(FALSE, FALSE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(FALSE, TRUE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(TRUE, FALSE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            assignment = new BooleanAssignment(TRUE, TRUE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            andOperator = InstrBooleanOperator.getTwoArgumentOperator(
                    XampilBooleanOperators.AND, "AND", possibleAssignments);
        }

        return andOperator;
    }

    public static InstrBooleanOperator getNotOperator() {
        if (notOperator == null) {
            Map&lt;BooleanAssignment, Boolean&gt; possibleAssignments =
                    new HashMap&lt;BooleanAssignment, Boolean&gt;();
            BooleanAssignment assignment;

            assignment = new BooleanAssignment(FALSE);
            possibleAssignments.put(assignment, Boolean.TRUE);

            assignment = new BooleanAssignment(TRUE);
            possibleAssignments.put(assignment, Boolean.FALSE);

            notOperator = InstrBooleanOperator.getOneArgumentOperator(
                    XampilBooleanOperators.NOT, "NOT", false, possibleAssignments);
        }

        return notOperator;
    }

    public static InstrBooleanOperator getTrueOperator() {
        if (trueOperator == null) {
            BooleanAssignment assignment = new BooleanAssignment();

            trueOperator = InstrBooleanOperator.getConstantOperator(
                    XampilBooleanOperators.TRUE_DESCRIPTION, "TRUE",
                    Collections.singletonMap(assignment, Boolean.TRUE));
        }

        return trueOperator;
    }

    public static InstrBooleanOperator getFalseOperator() {
        if (falseOperator == null) {
            BooleanAssignment assignment = new BooleanAssignment();

            falseOperator = InstrBooleanOperator.getConstantOperator(
                    XampilBooleanOperators.FALSE_DESCRIPTION, "FALSE",
                    Collections.singletonMap(assignment, Boolean.FALSE));
        }

        return falseOperator;
    }</code></pre>
    <p>After doing all this stuff, we are now ready to add the instructions into the <code>InstrumentationVisitor</code>. To do so, we have to find out where conditional expressions can occure. In <i>Xampil</i> it is the if and while statement. So we insert the following piece of code:</p>
    <pre><code>
    public void visit(IfStatement n) {
<b>        XampilExpressionParser xampilExpressionParser = new XampilExpressionParser();
        InstrBooleanTerm instrBooleanTerm = xampilExpressionParser.parse(n.f1);
        List&lt;InstrBasicBooleanTerm&gt; termList = new LinkedList&lt;InstrBasicBooleanTerm&gt;();
        instrBooleanTerm.getAllBasicBooleanTerms(termList);
        this.conditionManipulator.manipulate(ifConditionID, termList);</b>

        n.f0.accept(this);
        ....
    }

    public void visit(WhileStatement n) {
<b>        XampilExpressionParser xampilExpressionParser = new XampilExpressionParser();
        InstrBooleanTerm instrBooleanTerm = xampilExpressionParser.parse(n.f1);
        List&lt;InstrBasicBooleanTerm&gt; termList = new LinkedList&lt;InstrBasicBooleanTerm&gt;();
        instrBooleanTerm.getAllBasicBooleanTerms(termList);
        this.conditionManipulator.manipulate(whileConditionID, termList);</b>

        n.f0.accept(this);
        ....
    }
</code></pre>
    <p>On the top of that, we have to insert a call of the condition manipulator's <code>writeDeclaration()</code> method. See next listing:</p>
    <pre><code>    public void visit(CompilationUnit n) {
       n.f0.accept(this);
       this.statementManipulator.writeDeclarations(this.counter.getStatementCount());
       this.branchManipulator.writeDeclarations(this.counter.getBranchCount());
       <b>this.conditionManipulator.writeDeclarations(this.counter);</b>
       this.loopManipulator.writeDeclarations(this.counter.getLoopCount());
       n.f1.accept(this);
       n.f2.accept(this);
       n.f3.accept(this);
    }</code></pre>
    <p>And now the last step for condition instrumentation can be taken, namely the condition manipulator. We need to implement the manipulate and writeDeclarations method.</p>
    <pre><code>public class DefaultConditionManipulator extends AbstractDefaultManipulator
        implements ConditionManipulator {

    private static final String END_IF = "ENDIF %n    ";

    private static final String ELSE = "ELSE %n    ";

    private static final String IF_NOT = "IF NOT(%s) THEN %n    ";

    private static final String ONE = "1";

    private static final String ZERO = "0";

    private static final String CONDITION_COUNTER =
            "%1$s%2$s_%3$s := %1$s%2$s_%3$s + 1%n    ";

    private int truthTableLine;

    private List&lt;InstrBasicBooleanTerm> basicBooleanTerms;

    ....

}</code></pre>
    <p>First, we take a look at the writeDeclarations method.</p>
    <pre><code>    public void writeDeclarations(InstrumentableItemCounter counter) {
        PrintWriter writer = super.getWriter();
        OUTER_LOOP : for (int i = 0; i &lt; counter.getConditionCount(); i++) {
            int booleanTerms = counter.getBasicBooleanCount(i);
            if (booleanTerms == 0) {
                continue OUTER_LOOP;
            }
            int basicBooleanCounters = 1 &lt;&lt; booleanTerms;
            String conditionPrimaryID = CounterIDProvider.generateConditionPrimaryID(i);
            for (int j = 0; j &lt; basicBooleanCounters; j++) {
                writer.printf("    INTEGER %s%s_%s := 0%n",
                        CounterIDProvider.VARIABLE_PREFIX,
                        conditionPrimaryID,
                        getTruthTableLine(booleanTerms, j));
            }
        }
        writer.printf("%n");
    }</code></pre>
    <p>This method writes all declarations for every condition to the output writer. To do that, we loop over all contitions and write to the output writer in an inner loop write for each line in the truth table. The second method we have to implement is the <code>manipulate</code> method.</p>
    <pre><code>    public void manipulate(String conditionID, List&lt;InstrBasicBooleanTerm&gt; termList) {
        if (termList.isEmpty()) {
            return;
        }
        this.truthTableLine = 0;
        this.basicBooleanTerms = termList;
        this.generateNestedIfBlock(0, conditionID);
    }</code></pre>
    <p>It seems to be very simple on the first look, but the whole complexity of generating the nested if block is put into an extra method. This method is <code>generateNestedIfBlock</code> which is implemented as follows.</p>
    <pre><code>    private void generateNestedIfBlock(int basicBooleanTerm, String conditionID) {
        String basicBooleanTermString =
                this.basicBooleanTerms.get(basicBooleanTerm).termToString();
        super.getWriter().printf(IF_NOT, basicBooleanTermString);
        if (basicBooleanTerm &lt; this.basicBooleanTerms.size() - 1) {
            this.generateNestedIfBlock(basicBooleanTerm + 1, conditionID);
        } else {
            String truthTableLineString = this.getTruthTableLine(
                    this.basicBooleanTerms.size(), this.truthTableLine);
            super.getWriter().printf(CONDITION_COUNTER,
                    CounterIDProvider.VARIABLE_PREFIX,
                    conditionID,
                    truthTableLineString);
            this.truthTableLine++;
        }
        super.getWriter().printf(ELSE);
        if (basicBooleanTerm &lt; this.basicBooleanTerms.size() - 1) {
            this.generateNestedIfBlock(basicBooleanTerm + 1, conditionID);
        } else {
            String truthTableLineString = this.getTruthTableLine(
                    this.basicBooleanTerms.size(), this.truthTableLine);
            super.getWriter().printf(CONDITION_COUNTER,
                    CounterIDProvider.VARIABLE_PREFIX,
                    conditionID,
                    truthTableLineString);
            this.truthTableLine++;
        }
        super.getWriter().printf(END_IF);
    }</code></pre>
    <p>As you might have seen, we implemented the generation recursively. That way it's easy to write and relatively easy to understand. The last method we have to implement is the <code>getTruthTableLine</code> method.</p>
    <pre><code>    private String getTruthTableLine(int variables, int line) {
        String tTableLine = Integer.toBinaryString(line);
        while (tTableLine.length() &lt; variables) {
            tTableLine = ZERO + tTableLine;
        }
        String truthTableLineAdapted = new String("");
        for (int position = 0; position &lt; variables; position++) {
            truthTableLineAdapted = truthTableLineAdapted + ONE
                    + tTableLine.charAt(position);
        }
        return truthTableLineAdapted;
    }</code></pre>
    <p>This method simply returns a string containing the binary code for a given line in the truth table with n variables. With these methods the condition manipulator is ready for instrumentation purposes.</p>
    <p>If anything was done correctly you should be able to instrument the conditions now.</p>

    <h3><a name="instrumentation_log">Coverage log file</a></h3>
    <p>The instrumentation we have done yet is nearly finshed. Although a <i>Xampil</i> file will collect coverage data during the run, the data is not written to a file. This feature has to be implemented now.</p>
    <p>How does such a coverage log file look like? You can have a look in the <a href="Design.pdf">design document</a> &ndash; <i>Appendix B: &quot;Coverage log
    file specification&quot;</i>.</p>
    <p>How do we produce such a file? We have to add additional <code>FILE</code> statement for the output. Therefore, we add three methods in the <code>InstrumentationVisitor</code>:</p>
    <pre><code>private static final String CLF_NAME = "coverage-log.clf";

/**
 * f0 -> &lt;PROGRAM&gt;
 * f1 -> &lt;EOL&gt;
 * f2 -> ( Statement() )*
 * f3 -> &lt;ENDPROGRAM&gt;
 */
@Override
public void visit(Program n) {
   n.f0.accept(this);
   n.f1.accept(this);
   n.f2.accept(this);
   writeCoverageLogFileOutput();
   n.f3.accept(this);
}

public void writeCoverageLogFileOutput() {
    PrintWriter targetWriter = super.getTargetWriter();
    writeFileStatement(targetWriter, true, "\"TEST_SESSION_CONTAINER \\\"" +
        this.testSessionContainerUID + "\\\"\"");
    writeFileStatement(targetWriter, false,
        "\"START_TEST_CASE \\\"Single Test Case\\\"\"");
    this.statementManipulator.writeCoverageLogFileOutput(
        this.counter.getStatementCount());
    this.branchManipulator.writeCoverageLogFileOutput(
        this.counter.getBranchCount());
    this.loopManipulator.writeCoverageLogFileOutput(
        this.counter.getLoopCount());
    this.conditionManipulator.writeCoverageLogFileOutput(this.counter);
    writeFileStatement(targetWriter, false,
        "\"END_TEST_CASE \\\"Single Test Case\\\"\"");
}

public static void writeFileStatement(PrintWriter targetWriter,
                                      boolean overwrite,
                                      String message) {
    targetWriter.write("    FILE ");
    if (overwrite) {
        targetWriter.write("OVERWRITE");
    } else {
        targetWriter.write("APPEND");
    }
    targetWriter.write(" \"" + CLF_NAME + "\" ");
    targetWriter.write(message);
    targetWriter.write(" + \"\\n\"\n");
}</code></pre>
    <p>Before we accept the token <code>ENDPROGRAM</code> (<code>n.f3.accept(this)</code>), we call the method <code>writeCoverageLogFileOutput</code>. This method will create <code>FILE</code> statements, that do write the coverage log file at runtime. The only thing left to do is to implement the required methods for each <code>Manipulator</code>. An exampale is shown here. See the <code>DefaultStatementManipulator</code>:</p>
    <pre><code>public void writeCoverageLogFileOutput(int statementCount) {
    for (int i = 1; i &lt;= statementCount; i++) {
        writeFileStatement(super.getWriter(), false,
                String.format("\"%2$s \" + %1$s%2$s",
                              CounterIDProvider.VARIABLE_PREFIX,
                              CounterIDProvider.generateStatementID(i)));
    }
}</code></pre>
    <p>After the instrumentation is finished, these <code>FILE</code> statements will look like this:</p>
    <pre><code>FILE OVERWRITE "coverage-log.clf" "TEST_SESSION_CONTAINER
        \"1ac546e6-901f-4673-bd20-194e6329f389\"" + "\n"
FILE APPEND "coverage-log.clf" "START_TEST_CASE \"Single Test Case\"" + "\n"
FILE APPEND "coverage-log.clf" "S1 " + CodeCoverCoverageCounter_S1 + "\n"
FILE APPEND "coverage-log.clf" "S2 " + CodeCoverCoverageCounter_S2 + "\n"
FILE APPEND "coverage-log.clf" "S3 " + CodeCoverCoverageCounter_S3 + "\n"
[..]
FILE APPEND "coverage-log.clf" "END_TEST_CASE \"Single Test Case\"" + "\n"</code></pre>



    <!--
      MAST CHAPTER
      -->
    <h2><a name="mast">More abstract syntaxtree (MAST)</a></h2>
    <h3><a name="mast_model">The syntaxtree model</a></h3>
    <p>As we have told you in the preface chapter, we need an abstract model of all the source files. This is required to get to know, which statements and branches are related to each other. Moreover, we can subdivide the model and calculate metrics only for parts of it. For this reason, we use a <i>More abstract syntaxtree</i>. This concept is explained in detail in the <a href="Design.pdf">design document</a>. The chapter &quot;3.1 Data model&quot; gives an overview of the classes used for the MAST. There is even an example of a small mast that illustrates, how the elements are hierarchically ordered. In this document, we only want to show you the basic ideas.</p>
    <p>At the top of the MAST there are the so called <code>HierarchyLevel</code>s. They represent a source file, a package, a class or a program unit. On the one hand, they can recursively contain child <code>HierarchyLevel</code>s. On the other hand, they can contain <code>StatementSequences</code> with <code>Statements</code>.</p>
    <p>These <code>Statements</code> are subdivided into:</p>
    <ul>
        <li><code>BasicStatements</code>: arithmetic statements, method calls</li>
        <li><code>ConditionalStatements</code>: a statement with <code>Branches</code> like <code>if</code> or <code>switch</code></li>
        <li><code>LoopingStatements</code>: a statement, whose body can be executed multiple times; like <code>while</code>, <code>return until</code> or <code>for</code></li>
    </ul>
    <p>Then there are <code>BooleanTerms</code>. They represent a boolean expression that occurs for example in an <code>if</code>. We subdivide <code>BooleanTerms</code> into:</p>
    <ul>
        <li><code>BasicBooleanTerms</code>:&nbsp;&nbsp;<code>a &lt; 9</code>&nbsp;&nbsp;;&nbsp;&nbsp;<code>vector.isEmpty()</code></li>
        <li><code>OperatorTerms</code>:&nbsp;&nbsp;<code>A <b>OR</b> B</code>&nbsp;&nbsp;;&nbsp;&nbsp;<code><b>NOT</b> C</code>&nbsp;&nbsp;;&nbsp;&nbsp;<code><b>TRUE</b></code></li>
    </ul>
    <p>Nearly every element of the MAST has a <code>Location</code>. A location references a <code>SourceFile</code> and an offset in it. This offset is the position of the start and end character of the element in the source file. For example might a <code>BasicBooleanTerm</code> <code>&quot;a &gt; 0&quot;</code> occur in the <code>SourceFile</code> &quot;program.xpl&quot; at the offset <code>120..124</code> if the &quot;a&quot; is the 120th character and the &quot;0&quot; is the 124th.</p>

    <h3><a name="mast_attic">StatementAttic</a></h3>
    <p>Now we start to prepare the MAST building. First, we create a statement attic. This is a collection, similar to a stack, where we want to push all <code>Statements</code> that occur at a given level. If we have the construct:</p>
    <pre><code>PROGRAM
    i := 1
    IF a &gt; 9 THEN
        i := i + 1
        a := a * i
    ENDIF
    FILE OVERWRITE &quot;target.log&quot; i
ENDPROGRAM</code></pre>
    <p>We have <code>i := 1</code>, <code>IF</code> and <code>FILE..</code> at the same statement level. Within the <code>IF</code>, statements are collected at a lower level and later combined to a <code>Branch</code>. Then the branch is used to create the <code>ConditionalStatement</code> <code>IF</code> at the highest level. The statement attic will look like this:</p>
    <pre><code>   &rarr; push new list for the PROGRAM unit
1) (PROGRAM) {}
2) (PROGRAM) {i := 1}
   &rarr; push new list for the IF
3) (PROGRAM) {i := 1}
   (if)      {}
4) (PROGRAM) {i := 1}
   (if)      {i := i + 1}
5) (PROGRAM) {i := 1}
   (if)      {i := i + 1, a := a * i}
   &rarr; pop lowest list and create a StatementSequence for the IF branch
6) (PROGRAM) {i := 1, {i := i + 1, a := a * i}}
7) (PROGRAM) {i := 1, {i := i + 1, a := a * i}, FILE OVERWRITE &quot;target.log&quot; i}
   &rarr; pop lowest list and create a StatementSequence for the PROGRAM unit</code></pre>
   <p>So this concept will push new lists to the statement attic for every element that creates an own statement body. Ordinary statements are always pushed to the lowest list. Then the end of an element body forces the lowest list to be poped and to be transformed into a <code>StatementSequence</code>. Finally this sequence is pushed to the lowest list, too.</p>
   <p>Now we implement this feature and add some lines in the <code>InstrumentationVisitor</code>:</p>
   <pre><code>private Attic&lt;List&lt;org.codecover.model.mast.Statement&gt;&gt; statementAttic;

private void pushNewStatementLevelToAttic() {
    this.statementAttic.push(new LinkedList&lt;org.codecover.model.mast.Statement&gt;());
}

private StatementSequence createStatementSequenceFromAttic() {
    List&lt;org.codecover.model.mast.Statement&gt; statementList = this.statementAttic.pop();
    List&lt;Location&gt; locationsOfSequence = new Vector&lt;Location&gt;(statementList.size());
    for (org.codecover.model.mast.Statement thisStatement : statementList) {
        locationsOfSequence.addAll(thisStatement.getLocation().getLocations());
    }

    return this.builder.createStatementSequence(
            this.builder.createLocationList(locationsOfSequence),
            statementList);
}

private List&lt;StatementSequence&gt; createStatementSequenceListFromAttic() {
    StatementSequence statementSequence = createStatementSequenceFromAttic();

    if (statementSequence.getStatements().isEmpty()) {
        return Collections.&lt;StatementSequence&gt;emptyList();
    }

    return Collections.&lt;StatementSequence&gt;singletonList(statementSequence);
}</code></pre>
    <p>Where the method <code>pushNewStatementLevelToAttic</code> just pushes an empty list to the attic, the method <code>createStatementSequenceFromAttic</code> pops the lowest list and creates a <code>StatementSequence</code> out of it. For compatibility, CodeCover often expects a list of <code>StatementSequences</code>. We do not need this feature, so we use a singleton list to encapsulate our created <code>StatementSequence</code> in the method <code>createStatementSequenceListFromAttic</code>.</p>
    <p>To create all the elements of the MAST, a <code>MastBuilder</code> has to be used. It has various <code>create</code> methods to create every element of the MAST. We have got this <code>MastBuilder</code> in the constructor of the <code>InstrumentationVisitor</code>.</p>

    <h3><a name="mast_programunit"><code>PROGRAMM</code> unit</a></h3>
    <p>We start with the mast creation at the root. We go to the method <code>visit(CompilationUnit n)</code> and add the following code:</p>
    <pre><code>[..]

<b>private Attic&lt;List&lt;org.codecover.model.mast.Statement&gt;&gt; statementAttic;</b>

public InstrumentationVisitor(PrintWriter writer,
                              InstrumentableItemCounter counter,
                              MASTBuilder builder,
                              SourceFile sourceFile,
                              HierarchyLevelContainer hierarchyLevelContainer,
                              String testSessionContainerUID) {
    [..]
    <b>this.statementAttic = new Attic&lt;List&lt;org.codecover.model.mast.Statement&gt;&gt;();</b>
}

<b>private Location createLocation(int startOffset, int endOffset) {
    return this.builder.createLocation(this.sourceFile, startOffset, endOffset);
}

private LocationList createLocationList(int startOffset, int endOffset) {
    if (startOffset == -1 &amp;&amp; endOffset == -1) {
        return this.builder.createEmptyLocationList();
    }

    if (startOffset != -1 &amp;&amp; endOffset != -1) {
        Location location = createLocation(startOffset, endOffset);
        List&lt;Location&gt; listOfLocations = Collections.&lt;Location&gt;singletonList(location);
        return this.builder.createLocationList(listOfLocations);
    }

    String message = &quot;startOffset == -1 ^ endOffset == -1&quot;;
    Exception exception = new IllegalStateException(message);
    this.builder.getLogger().fatal(message, exception);

    // never reached cause fatal throws an Exception
    return null;
}

private void popTopLevelHieraryLevelsFromAttic(int start,
                                               int end,
                                               int headerStartOffset,
                                               int headerEndOffset) {
    if (this.statementAttic.size() != 1) {
        String message = &quot;this.statementAttic.size() != 1&quot;;
        Exception exception = new IllegalStateException(message);
        this.builder.getLogger().fatal(message, exception);
    }

    List&lt;StatementSequence&gt; programStatements = createStatementSequenceListFromAttic();
    HierarchyLevel programHL = this.builder.createHierarchyLevel(
            createLocationList(start, end),
            &quot;PROGRAM&quot;,
            createLocationList(headerStartOffset, headerEndOffset),
            HierarchyLevelTypes.getProgramType(this.builder),
            Collections.&lt;HierarchyLevel&gt;emptyList(),
            programStatements);
    this.hierarchyLevelContainer.addHierarchyLevelToRoot(programHL);
}</b>

[..]

@Override
public void visit(CompilationUnit n) {
   <b>int startOffset = 0;
   int endOffset;
   int headerStartOffset;
   int headerEndOffset;
   pushNewStatementLevelToAttic();</b>

   // Declaration()
   n.f0.accept(this);
   this.statementManipulator.writeDeclarations(this.counter.getStatementCount());
   this.branchManipulator.writeDeclarations(this.counter.getBranchCount());
   this.conditionManipulator.writeDeclarations(this.counter);
   this.loopManipulator.writeDeclarations(this.counter.getLoopCount());

   <b>headerStartOffset = StartOffset.getStartOffset(n.f1);
   headerEndOffset = headerStartOffset + 7;</b>
   // Program()
   n.f1.accept(this);
   // ( &lt;EOL&gt; )?
   n.f2.accept(this);
   // ( &lt;EOF&gt; )?
   n.f3.accept(this);

   <b>endOffset = super.getLastEndOffset();
   popTopLevelHieraryLevelsFromAttic(startOffset, endOffset,
           headerStartOffset, headerEndOffset);</b>
}</code></pre>
    <p>OK, this is a lot of code. What is it doing? The the variable <code>statementAttic</code> has been discussed in detail above. We have to initialize it in the constructor. The method <code>createLocation</code> can create a simple <code>Location</code> out of a start and an end offset. The method <code>createLocationList</code> can create a <code>LocationList</code> out of a start and an end offset. Again, the MAST often expects a <code>LocationList</code> rather than a single <code>Location</code>. For this reason, the method <code>createLocationList</code> only encapsulates a single <code>Location</code> in a <code>LocationList</code>.</p>
    <p>The next lines we have added are in the <code>visit(CompilationUnit n)</code> method. Here we get the offsets of the whole source file and of the token <code>PROGRAM</code>. Therefore we need the class <code>StartOffset</code>, that we have added earlier. This class can visit a <code>Node</code> and traverses until it has found the first <code>NodeToken</code>. Then its start offset is returned. The <code>TreeDumper</code>, that is the super class of the <code>InstrumentationVisitor</code>, has a method <code>getLastEndOffset</code>. This can be used to get the end offset of the very last token printed out.</p>
    <p>Besides the offsets, we have added <code>pushNewStatementLevelToAttic()</code> at the beginning to add a new list at the statement attic. The method <code>popTopLevelHieraryLevelsFromAttic</code> at the end can then be used to pop the highest statement list from the attic and create a <code>HierarchyLevel</code> out of it. The <code>MastBuilder.createHierarchyLevel</code> method has a lot of arguments, that have to be collected first &ndash; e.g. various <code>Locations</code>, a name and a <code>HierarchyLevelType</code>. For this type we can use the class <code>HierarchyLevelTypes</code>, that has been copied before (see <a href="#instrumenter_hierarchy">HierarchyLevel</a>).</p>
    <p>All in all, we have created the basis to create all the statements of the MAST. We will continue with this in the next section.</p>

    <h3><a name="mast_basicstatement">The <code>BasicStatements</code></a></h3>
    <p>We start with the <code>Assignment</code> and <code>File</code> statement, which are both <code>BasicStatements</code> of the MAST. We go to the method <code>InstrumentationVisitor.visit()</code> and add some lines:</p>
    <pre><code>[..]

<b>private CoverableItem createCoverableItem(String id) {
    return this.builder.createCoverableItem(this.sourceFile.getFileName(), id);
}


private void atticStatement(Node statement, String statementID) {
    if (statementID == null) {
        this.builder.getLogger().fatal(&quot;statementID == null&quot;);
    }
    int startOffset = StartOffset.getStartOffset(statement);
    int endOffset = super.getLastEndOffset();

    LocationList locationList = createLocationList(startOffset, endOffset);
    org.codecover.model.mast.Statement newStatement = this.builder
            .createBasicStatement(locationList,
                                  createCoverableItem(statementID),
                                  Collections.&lt;RootTerm&gt; emptySet());
    this.statementAttic.bottom().add(newStatement);
}</b>

@Override
public void visit(Statement n) {
    n.f0.accept(this);
    String statementID = this.counterIDProvider.nextStatementID();
    <b>if (n.f0.choice instanceof AssignmentStatement ||
        n.f0.choice instanceof FileStatement) {
        atticStatement(n.f0.choice, statementID);
    }</b>

    this.statementManipulator.manipulate(n, statementID);
}</code></pre>
    <p>The visit method has a look, whether the visited <code>Statement</code> is an <code>AssignmentStatement</code> or a <code>FileStatement</code>. Only in this condition, the node is considered to be a <code>BasicStatement</code> of the MAST. The method <code>atticStatement</code> uses the <code>MastBuilder</code> to create this <code>BasicStatement</code>. The method <code>createCoverableItem</code> is a helper method to create a coverable item that is needed for coverage measurement. For this reason, it is the exact same <code>statementID</code> as we used for coverage measurement.</p>
    <p>You want to test what you have done? This is not so simple, because we have no viewer for the MAST. But you have to change the <code>main</code> method either to avoid <code>NullPointerExceptions</code>. Try this:</p>
    <pre><code>MASTBuilder mastBuilder = new MASTBuilder(new SimpleLogger());
File targetFile = new File(&quot;example.xpl&quot;);
SourceFile sourceFile = mastBuilder.createSourceFile(targetFile.getName(),
        FileTool.getContentFromFile(targetFile));
CharStream charStream = new SimpleCharStream(new StringReader(sourceFile.getContent()));
XampilParser parser = new XampilParser(charStream);
InstrumentableItemCounter counter = new InstrumentableItemCounter();
CompilationUnit compilationUnit = parser.CompilationUnit(counter);
PrintWriter writer = new PrintWriter(System.out);
HierarchyLevelType rootType = HierarchyLevelTypes.getSourceFileType(mastBuilder);
HierarchyLevelContainer rootHierarchyLevelContainer = new HierarchyLevelContainer(
        rootType.getInternalName(), rootType, rootType);
InstrumentationVisitor visitor = new InstrumentationVisitor(writer,
        counter,
        mastBuilder,
        sourceFile,
        rootHierarchyLevelContainer,
        UUID.randomUUID().toString());
visitor.setStatementManipulator(new DefaultStatementManipulator());
visitor.setBranchManipulator(new DefaultBranchManipulator());
visitor.setLoopManipulator(new DefaultLoopManipulator());
visitor.setConditionManipulator(new DefaultConditionManipulator());
visitor.visit(compilationUnit);
writer.flush();</code></pre>

    <h3><a name="mast_if">The <code>ConditionalStatements</code></a></h3>
    <p>In the last section, we have ignored, that there are other statements &ndash; e.g. <code>IF</code>. Now, we will create a <code>ConditionalStatement</code> out of it. So we have to add the following lines:</p>
    <pre><code>[..]

<b>private Branch createExplicitBranchFromAttic(String branchID,
                                             int startOffset,
                                             int endOffset,
                                             int decisionStartOffset,
                                             int decisionEndOffset) {
    LocationList locationList = createLocationList(startOffset, endOffset);
    LocationList locationListDecision = createLocationList(decisionStartOffset,
            decisionEndOffset);
    StatementSequence statementSequence = createStatementSequenceFromAttic();

    return this.builder.createBranch(locationList,
            createCoverableItem(branchID),
            false,
            locationListDecision,
            statementSequence);
}

private Branch createImplicitBranch(String branchID) {
    LocationList locationList = this.builder.createEmptyLocationList();
    LocationList locationListDecision = this.builder.createEmptyLocationList();
    StatementSequence statementSequence = this.builder.createStatementSequence(
            this.builder.createEmptyLocationList(),
            Collections.&lt;org.codecover.model.mast.Statement&gt; emptyList());

    return this.builder.createBranch(locationList,
            createCoverableItem(branchID),
            true,
            locationListDecision,
            statementSequence);
}

private void createConditionalStatementAndPushIt(String statementID,
        int startOffset,
        int endOffset,
        RootTerm rootTerm,
        List&lt;Branch&gt; branchList,
        int keywordStartOffset,
        int keywordEndOffset) {
    LocationList locationList = createLocationList(startOffset, endOffset);
    Location keywordLocation = createLocation(keywordStartOffset, keywordEndOffset);
    Set&lt;RootTerm&gt; setRootTerms;

    if (rootTerm == null) {
        setRootTerms = Collections.&lt;RootTerm&gt; emptySet();
    } else {
        setRootTerms = new HashSet&lt;RootTerm&gt;();
        setRootTerms.add(rootTerm);
    }

    ConditionalStatement conditionalStatement = this.builder.createConditionalStatement(
            locationList,
            createCoverableItem(statementID),
            setRootTerms,
            branchList,
            keywordLocation);
    this.statementAttic.bottom().add(conditionalStatement);
}</b>

@Override
public void visit(Statement n) {
    String statementID = this.counterIDProvider.nextStatementID();
    if (n.f0.choice instanceof AssignmentStatement ||
        n.f0.choice instanceof FileStatement) {
        n.f0.accept(this);
        // create a MAST Statement here
        // the statement has to be visited BEFORE
        atticStatement(n.f0.choice, statementID);
    } <b>else if (n.f0.choice instanceof IfStatement) {
        IfStatement ifStatement = (IfStatement) n.f0.choice;
        ifStatement.statementID = statementID;
        ifStatement.accept(this);
    } else if (n.f0.choice instanceof WhileStatement) {
        WhileStatement whileStatement = (WhileStatement) n.f0.choice;
        whileStatement.statementID = statementID;
        whileStatement.accept(this);
    } else if (n.f0.choice instanceof SwitchStatement) {
        SwitchStatement switchStatement = (SwitchStatement) n.f0.choice;
        switchStatement.statementID = statementID;
        switchStatement.accept(this);
    }</b>

    this.statementManipulator.manipulate(n, statementID);
}

@Override
public void visit(IfStatement n) {
    <b>final int startOffSet = n.f0.startOffset;
    final int endOffset;
    final int keywordStartOffset = startOffSet;
    final int keywordEndOffset = n.f0.endOffset;
    final int thenStartOffset;
    final int thenEndOffset;
    final int elseStartOffset;
    final int elseEndOffset;

    final String thenBranchID = this.counterIDProvider.nextBranchID();
    final String elseBranchID = this.counterIDProvider.nextBranchID();
    final String ifConditionID = this.counterIDProvider.nextConditionID();

    final Branch thenBranch;
    final Branch elseBranch;</b>

    XampilExpressionParser xampilExpressionParser = new XampilExpressionParser();
    InstrBooleanTerm instrBooleanTerm = xampilExpressionParser.parse(n.f1);
    <b>BooleanTerm booleanTerm = instrBooleanTerm.toBooleanTerm(this.builder,
            this.sourceFile);
    RootTerm rootTerm = this.builder.createRootTerm(booleanTerm,
            createCoverableItem(ifConditionID));</b>

    // Instrumenting the boolean term
    List&lt;InstrBasicBooleanTerm&gt; termList = new LinkedList&lt;InstrBasicBooleanTerm&gt;();
    instrBooleanTerm.getAllBasicBooleanTerms(termList);

    this.conditionManipulator.manipulate(ifConditionID, termList);
    // &lt;IF&gt;
    n.f0.accept(this);
    // Expression
    n.f1.accept(this);
    // &lt;THEN&gt;
    n.f2.accept(this);
    // &lt;EOL&gt;
    n.f3.accept(this);

    this.branchManipulator.manipulateIf(n, thenBranchID);
    <b>if (n.f4.present()) {
        thenStartOffset = StartOffset.getStartOffset(n.f4);
    } else {
        thenStartOffset = -1;
    }
    pushNewStatementLevelToAttic();</b>

    // ( Statement() )*
    n.f4.accept(this);
    <b>thenEndOffset = super.getLastEndOffset();

    thenBranch = createExplicitBranchFromAttic(thenBranchID,
                                               thenStartOffset,
                                               thenEndOffset,
                                               -1, -1);</b>

    NodeOptional elseOption = n.f5;
    if (elseOption.present()) {
        // the else is present
        NodeSequence elseSequence = (NodeSequence) elseOption.node;
        // &lt;ELSE&gt;
        elseSequence.nodes.get(0).accept(this);
        // &lt;EOL&gt;
        elseSequence.nodes.get(1).accept(this);

        this.branchManipulator.manipulateElse(n, elseBranchID, false);

        <b>NodeListOptional statementList = (NodeListOptional) elseSequence.nodes.get(2);
        if (statementList.present()) {
            elseStartOffset = StartOffset.getStartOffset(statementList);
        } else {
            elseStartOffset = -1;
        }
        pushNewStatementLevelToAttic();</b>

        // ( Statement() )*
        statementList.accept(this);
        <b>elseEndOffset = super.getLastEndOffset();

        // create the explicit else branch
        elseBranch = createExplicitBranchFromAttic(elseBranchID,
                                                   elseStartOffset,
                                                   elseEndOffset,
                                                   -1, -1);</b>
    } else {
        this.branchManipulator.manipulateElse(n, elseBranchID, true);

        <b>// create the implicit else branch
        elseBranch = createImplicitBranch(elseBranchID);</b>
    }

    // &lt;ENDIF&gt;
    n.f6.accept(this);
    // &lt;EOL&gt;
    n.f7.accept(this);

    <b>endOffset = super.getLastEndOffset();
    List&lt;Branch&gt; branchList = new Vector&lt;Branch&gt;(2);
    branchList.add(thenBranch);
    branchList.add(elseBranch);

    createConditionalStatementAndPushIt(n.statementID,
                                        startOffSet,
                                        endOffset,
                                        rootTerm,
                                        branchList,
                                        keywordStartOffset,
                                        keywordEndOffset);</b>
}

[..]</code></pre>
    <p>Again, this is a lot of code we have to discuss. We start with the helper method <code>createExplicitBranchFromAttic</code>. It creates <code>LocationLists</code> for the whole branch and for the decision. The decision is meant to be the position of a value or expression, that stands for branch. For the then and else branch this decision location is not needed. For this reason the offsets can be set to <code>-1</code> and by the way leads to an empty <code>LocationList</code>. This methods pops the lowest list of the statement attic, creates a <code>StatementSequence</code> and uses the <code>MastBuilder</code> to create a <code>Branch</code>.</p>
    <p>The method <code>createImplicitBranch</code> has the same usage, but it is only for branches that are implicit. Implicit means, that the branch is not declared in the original source code, but could be (see <a href="#words_implicit">implicit</a>).</p>
    <p>The method <code>createConditionalStatementAndPushIt</code> gets a list of explicit or implicit <code>Branches</code> and creates a <code>ConditionalStatement</code> out of it. The start and end offset refer to the start and end position of the whole <code>ConditionalStatement</code>. The <code>RootTerm</code> is the boolean expression, that is for example contained in an <code>IF</code> statement. The offset of the keyword means the position of the keyword like <code>IF</code> or <code>SWITCH</code>.</p>
    <p>The <code>visit(Statement n)</code> method has to be extended to differentiate between the various <code>Statements</code>. This is needed because we have to hand over the <code>statementID</code> by setting a new field of a <code>IfStatement</code>, <code>SwitchStatement</code> and <code>WhileStatement</code>. This public field has to be created in all three classes now.</p>
    <p>And now we consider the <code>visit(IfStatement n)</code>. At the heading we declare some offset variables. Some can be initialized here &ndash; e.g.for the keyword <code>IF</code>. A few lines lower, we transform the <code>InstrBooleanTerm</code> into a <code>BooleanTerm</code> of the mast. We can use the simple method <code>toBooleanTerm</code>. The MAST requires a <code>RootTerm</code> rather than a <code>BooleanTerm</code>. So we have to encapsulate it in such a term.</p>
    <p>Around the <code>accept</code> of the <code>( Statement() )*</code> of the then branch we have on the one side the initialization of the start offset of the then branch. Very important: we have to call <code>pushNewStatementLevelToAttic()</code>. This is for all the statements that occur in the then branch.</p>
    <p>The same is done for the else. Here we have to decide, whether the else is explicit or implicit. At the end of the method, we create a <code>ConditionalStatement</code> out of all the collected objects. The method <code>createConditionalStatementAndPushIt</code> is perfect for this purpose.</p>
    <p>We do not discuss the MAST creation for switch. It is similar and can be seen in the <a href="#download">final version of the source files</a>.</p>

    <h3><a name="mast_while">The <code>LoopingStatement</code></a></h3>
    <p>The last remaining statement of the MAST is the <code>LoopingStatement</code>. We have done a lot of preparatory work, but have to add a lot of lines of code too:</p>
    <pre><code>[..]

<b>private void createLoopingStatementAntPushIt(String statementID,
        int startOffset,
        int endOffset,
        RootTerm rootTerm,
        int keywordStartOffset,
        int keywordEndOffset,
        String loopIDZero,
        String loopIDOnce,
        String loopIDAbove,
        boolean optionalBodyExecution) {
    LocationList locationList = createLocationList(startOffset, endOffset);
    Location keywordLocation = createLocation(keywordStartOffset, keywordEndOffset);
    StatementSequence statementSequence = createStatementSequenceFromAttic();
    Set&lt;RootTerm&gt; setRootTerms;

    if (rootTerm == null) {
        setRootTerms = Collections.&lt;RootTerm&gt; emptySet();
    } else {
        setRootTerms = new HashSet&lt;RootTerm&gt;();
        setRootTerms.add(rootTerm);
    }

    LoopingStatement loopingStatement = this.builder.createLoopingStatement(
            locationList,
            createCoverableItem(statementID),
            setRootTerms,
            statementSequence,
            keywordLocation,
            createCoverableItem(loopIDZero),
            createCoverableItem(loopIDOnce),
            createCoverableItem(loopIDAbove),
            optionalBodyExecution);
    this.statementAttic.bottom().add(loopingStatement);
}</b>

@Override
public void visit(WhileStatement n) {
    <b>final int startOffSet = n.f0.startOffset;
    final int endOffset;
    final int keywordStartOffset = startOffSet;
    final int keywordEndOffset = n.f0.endOffset;</b>

    final String whileLoopID = this.counterIDProvider.nextLoopID();
    final String whileConditionID = this.counterIDProvider.nextConditionID();

    XampilExpressionParser xampilExpressionParser = new XampilExpressionParser();
    InstrBooleanTerm instrBooleanTerm = xampilExpressionParser.parse(n.f1);
    <b>// create BooleanTerm BEFORE instrumenting
    BooleanTerm booleanTerm = instrBooleanTerm.toBooleanTerm(this.builder,
            this.sourceFile);
    RootTerm rootTerm = this.builder.createRootTerm(booleanTerm,
            createCoverableItem(whileConditionID));</b>

    List&lt;InstrBasicBooleanTerm&gt; termList = new LinkedList&lt;InstrBasicBooleanTerm&gt;();
    instrBooleanTerm.getAllBasicBooleanTerms(termList);
    this.conditionManipulator.manipulate(whileConditionID, termList);

    <b>pushNewStatementLevelToAttic();</b>

    this.loopManipulator.manipulateBeforeWhile(n, whileLoopID);

    // &lt;WHILE&gt;
    n.f0.accept(this);
    // Expression(basicBooleanCounter)
    n.f1.accept(this);
    // &lt;DO&gt;
    n.f2.accept(this);
    // &lt;EOL&gt;
    n.f3.accept(this);

    this.loopManipulator.manipulateInWhile(n, whileLoopID);

    // ( Statement() )*
    n.f4.accept(this);
    // &lt;ENDWHILE&gt;
    n.f5.accept(this);
    // &lt;EOL&gt;
    n.f6.accept(this);

    this.loopManipulator.manipulateAfterWhile(n, whileLoopID);

    <b>endOffset = super.getLastEndOffset();
    createLoopingStatementAntPushIt(n.statementID,
            startOffSet,
            endOffset,
            rootTerm,
            keywordStartOffset,
            keywordEndOffset,
            CounterIDProvider.generateLoopSubIDZero(whileLoopID),
            CounterIDProvider.generateLoopSubIDOne(whileLoopID),
            CounterIDProvider.generateLoopSubIDAbove(whileLoopID),
            false);</b>
}

[..]</code></pre>
    <p>The helper method <code>createLoopingStatementAntPushIt</code> is responsible to create all the <code>LocationLists</code> and pop the lowest list of the statement attic to create the loop body. Finally the <code>MastBuilder</code> is used to create the <code>LoopingStatement</code> object and the looping statement is pushed to the attic. The flag <code>optionalBodyExecution</code> of <code>MastBuilder.createLoopingStatement</code> is used to indicate whether the body has to be executed at least once (<code>false</code>) or is skipped, if the expression is false for the first execution (<code>true</code>). Another important point is, that there are four <code>CoverableItem</code>s required. One for the statement coverage and three for the loop coverage. We remember, that the loop coverage has a look at the number of executions of the loop body (see design of <a href="#instrumentation_design_loop">loop coverage</a>).</p>
    <p>And what was added in the <code>visit(WhileStatement n)</code>? Like in the <code>visit(IfStatement n)</code> we start to declare and initialize some offsets. Then we transform the instrumenter boolean term into a <code>RootTerm</code>. Before the visiting of the <code>( Statement() )*</code>, we push a new list to the statement attic.</p>
    <p>After all the visiting, we can use <code>createLoopingStatementAntPushIt</code> to create and push a new <code>ConditionalStatement</code>.</p>



    <!--
      ADVANCED FEATURES CHAPTER
    
    <h2><a name="advanced">Advanced features</a></h2>
    <h3><a name="advanced_hierarchy">Hierarchy levels</a></h3>
    <h3><a name="advanced_logger">Logger</a></h3>
    <h3><a name="advanced_directives">Instrumenter directives</a></h3>
    <br/><br/><br/><br/><br/><br/><br/><br/><br/>
    <ul>
        <li class="todo">How do we tell the reader to test the instrumenter? An own <code>main</code> method or the integration into CodeCover using the <code>DynamicClassLoader</code>.</li>
    </ul>
  -->


    <!--
      DOWNLOAD CHAPTER
      -->
    <h2><a name="download">Download sources</a></h2>
    <p>As announced before, you can download all the finished sources of the parser and the instrumenter for <i>Xampil</i>:</p>
    <ul>
        <li><a href="programming-language-files/Instrumenter-Xampil.jar"><code>Instrumenter-Xampil.jar</code></a></li>
    </ul>

m4_web_create_footer
