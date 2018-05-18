<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: velocityContext-reference.html.m4 28 2008-05-28 20:31:17Z ahija $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`CLI Reference')

    <h2>Velocity Context Reference</h2>
    <h3>Overview</h3>
    <p>In the templates for VelocityReport, you can access java objects, its members and even its methods. Velocity doesn't care for types, but java does, so you will get TypeErrors, InvocationErrors and so on, far away from the template, if you're not careful with the types and the spelling, as Velocity let's every context name it does not recognised unreplaced. Methods are called as known from many programming languages by [contextName].[methodName([parameters])]</p>
    <h3>Objects by Context name</h3>
    <h4>String</h4>
      <p>Javas String class. See the Java documentation for details.</p>
    <h4>now</h4>
       <p>The current date. See the Java documentation of Date if you need more details</p>
     <h4>dateFormatter</h4>
     <p>The <code>DateFormat</code> of the current locale. See the Java documentation of DateFormat for more details.</p>
     <h4>session</h4>
    <p>The test session this report is generated for. Methods of <code>session</code> you probably want to use are:</p>
    <table border="1">
      <tr>
        <th>Method name</th>
        <th>Explanation</th>
      </tr>
      <tr>
        <td><code>String getComment();</code> </td>
        <td>returns the comment set for this test session.</td>
      </tr>
      <tr>
        <td><code>String getName();</code></td>
        <td>returns the name of this test session.</td>
      </tr>
      <tr>
        <td><code>Date getDate()</code></td>
        <td>returns the date of this test session</td>
      </tr>
    </table>
      <h4>testcases</h4> 
      <p>A <code>List</code> of testcases this report is generated for. Methods of <code>TestCase</code> you probably want to use are:</p>
      <table border="1">
        <tr>
          <th>Method name</th>
          <th>Explanation</th>
        </tr>
        <tr>
          <td><code>String getComment();</code> </td>
          <td>returns the comment set for this testcase.</td>
        </tr>
        <tr>
          <td><code>String getName();</code></td>
          <td>returns the name of this test case.</td>
        </tr>
        <tr>
          <td><code>Date getDate()</code></td>
          <td>returns the date of this test case</td>
        </tr>
      </table>
      <h4>topmostHierarchyLevel</h4>
      <h4>currentHierarchyLevel</h4>
      <p>The <code>HierarchyLevel</code> at the top (in java, its the topmost package) and the one the output file written at this moment mainly belongs to (if such exists, otherwise it's also the one at the top).</p>
      <table border="1">
        <tr>
          <th>Method</th>
          <th>Description</th>
        </tr>
        <tr>
          <td><code>String getName()</code></td>
          <td>The name of the hierarchy level</td>
        </tr>
        <tr>
          <td><code>String getType().getInternalName()</code></td>
          <td>The name of the the type of the hierarchy level</td>
        </tr>
        <tr>
          <td><code>List&lt;HierarchyLevel&gt; getChildren()</code></td>
          <td>The children of this hierarchy level or an empty list, if there are none</td>
        </tr>
      </table>
      <h4>hierarchyLevels</h4>
      <p>A <code>List</code> of all hierarchyLevels that are successors of <code>topmostHierarchyLevel</code> meaning its children, their children, ... Access it as any other list and use for each item of the list (which is a hierarchyLevel) the methods mentioned above.</p>
      <h4>hierarchyLevelTypeCounters</h4>
      <p>This <code>List&lt;NoPerHierarchyLevelType&gt;</code> has counted the times each <code>HierarchyLevelType</code> occurs. You may want to use a foreach-loop
      </p>
      <pre><code>#foreach( $hltc in $hierarchyLevelTypeCounters)
    $hltc.HierarchyLevelType.EnglishName
    $hltc.Number
#end</code></pre>
      <h4>sorter</h4>
      <p>The sorter enables you to sort any <code>List</code> in an order you define in the template, during runtime. This works in two steps. First, you associate each item you want to sort with a sorting key.</p>
      <pre><code>void setKey(Object object, double key)
void setKey(Object object, int key)
void setKey(Object object, String key)</code></pre>
      <p>These methods set the key of the <code>object</code> to <code>key</code>. If a key was already set for this object, it is overwritten<br/>
       Items with numbers are sorted ascending followed by items with string keys sorted lexicographically. Items without a key come last in the order they were. After you have set all keys, call</p>
       <pre><code>void sort(List list)</code></pre>
       <p>to execute the sorting.</p>
      <h4>coverageMetrics</h4>
      <p>All <code>CoverageMetric</code>s that are supported by the selected session, stored in a <code>List</code>. Each coverageMetric in that list has (among others) the following methods:</p>
      <table border="1">
        <tr>
          <th>Method name</th>
          <th>Explanation</th>
        </tr>
        <tr>
          <td><code>CoverageResult getCoverage(List&lt;TestCase> testCases, HierarchyLevel hierarchyLevel)</code> </td>
          <td>returns a CoverageResult (see below)</td>
        </tr>
        <tr>
          <td><code>String getName();</code></td>
          <td>returns the name of this CoverageMetric.</td>
        </tr>
        <tr>
          <td><code>String getDescription()</code></td>
          <td>returns the description of this CoverageMetric.</td>
        </tr>
      </table>
        <p>
      CoverageResult gives you the ratio between covered and uncovered by providing the methods</p>
      <table border="1">
        <tr>
          <th>Method name</th>
          <th>Explanation</th>
        </tr>
        <tr>
          <td><code>int getCoveredItems()</code> </td>
          <td>returns the number of covered items</td>
        </tr>
        <tr>
          <td><code>int getTotalItems()</code></td>
          <td>returns the total number of items</td>
        </tr>
      </table>
      <h4>code</h4>
      <p>With the call</p>
      <pre><code>List&lt;ExtractOfCodeFile&gt; getCoveredCode (HierarchyLevel level,
                                        List&lt;TestCase&gt; testCases,
                                        List&lt;CoverageMetric&gt; metrics)</code></pre>
      <p>you get colored code, separated in files and lines. ExtractOfCodeFile has the methods</p>
      <pre><code>String getFileName()
List&lt;TextLine&gt; getTextLines()</code></pre>
      <p>and each TextLine has the methods</p>
      <table border="1">
        <tr>
          <th>Method name</th>
          <th>Explanation</th>
        </tr>
        <tr>
          <td><code>int getLineNo()</code> </td>
          <td>returns the line number of this line</td>
        </tr>
        <tr>
          <td><code>String getText()</code> </td>
          <td>returns the text in this line, enriched with html-tags for coloring</td>
        </tr>
        <tr>
          <td><code>long getExecutions()</code></td>
          <td>returns how often this line was executed</td>
        </tr>
      </table>

m4_web_create_footer
