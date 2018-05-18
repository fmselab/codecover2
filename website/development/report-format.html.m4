<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: report-format.html.m4 1 2007-12-12 17:37:26Z t-scheller $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`Adapt the report format')

    <h2>Adapt the report format</h2>
    <p>In this section, you learn how to make CodeCover write the reports the way you like them.</p>
    <p>To generate a report, CodeCover uses two components:</p>
    <ul>
      <li>A Java class derived from <code>org.codecover.report.ReportGenerator</code>, and</li>
      <li>a template file selecting this <code>ReportGenerator</code> and providing all the information it needs.</li>
    </ul>
    <h3>ReportGenerator</h3>
    <p>The <code>ReportGenerator</code> prepares all data and combines it with the (chiefly layout-) information of the template. In general you could say: whenever you need a new file format for the reports, you have to write a new <code>ReportGenerator</code> (which shouldn't happen that often.)</p>
    <h3>Template</h3>
    <p>The template is the part easier to change, responsible mainly for the layout of the report. The shipped templates can be found in the <code>eclipse/plugins/org.codecover.report.*/report-templates/</code> directorys (if you've installed CodeCover using Eclipse's update mechanism; otherwise, you may already have found them). Each template file has to use the following structure:</p>
    <pre><code>&lt;?xml version=&quot;1.0&quot; encoding=&quot;UTF-8&quot;?&gt;
&lt;report version=&quot;1.0&quot; xmlns=&quot;http://www.codecover.org/xml/report-template&quot;&gt;
  &lt;class&gt; <i>classname of the ReportGenerator</i> &lt;/class&gt;
  &lt;name xml:lang=&quot;en&quot;&gt; <i>name, can be repeated for different languages</i> &lt;/name&gt;
  &lt;description xml:lang=&quot;en&quot;&gt; <i>description, can be repeated, too</i> &lt;/description&gt;
  &lt;template version=&quot;1&quot;
      xmlns=&quot;http://www.codecover.org/xml/report-template/html-hierarchic&quot;&gt;
       
<i>information for the ReportGenerator</i>

  &lt;/template&gt;
&lt;/report&gt;
</code></pre>
    <p>Which information is needed and possible between the <code>&lt;template&gt;</code> tags depends on the <code>ReportGenerator</code> chosen in the template.</p>
    <p>CodeCover is shipped with a CSV (comma seperated values) <code>ReportGenerator</code> using a simple syntax (see below) and two HTML <code>ReportGenerator</code>s using <a href="http://velocity.apache.org/">Apache Velocity</a> to give you a huge flexibility how the reports should look like. Velocity-Statements starting with # control the generated output using (data)objects starting with $. These concepts are explained in the <a href="http://velocity.apache.org/engine/releases/velocity-1.5/user-guide.html">Velocity User Guide</a> which you should read before you continue with this document. Afterwards, see the <a href="../documentation/references/velocityContext-reference.html">Velocity Context Reference</a> for an survey of the objects you can access from the template.</p>
    <h3>SingleFileHTMLReportGenerator</h3>
    <p>With this template, you create, as the name tells, a report in one single HTML-file. Directly within the <code>&lt;template&gt;</code> tag, surroundes by</p>
    <pre><code>&lt;![CDATA[
  <i>html and Velocity code</i>
]]&gt;</code></pre>
    <p>
     is the HTML-Code for this page and are the velocity commands which are executed while the output is, line for line, written.</p>
    <h3>HierarchicalHTMLReportGenerator</h3>
    <p>This file is more difficult to understand, as more files (even of different types) are and may be created. The <code>&lt;template&gt;</code> tags contain the following parts:</p>
    <h4>Required sections</h4>
    <pre><code>
&lt;title-page&gt;&lt;![CDATA[
  <i>html and Velocity code</i>
]]&gt;&lt;/title-page&gt;</code></pre>
    <p>This file is created in the path and with the filename the user selects while creating a report. It should give a survey of the report and link to the other files. These other files are created in subfolders according to the name of their corresponding <code>HierarchyLevel</code>.</p>
    <p>The other tags create files in a subdirectory. If the index file's name is <i>INDEXFILE</i>, the files are stored in the directory <i>INDEXFILE-files</i></p>.
    <pre><code>
&lt;selection-page&gt;&lt;![CDATA[
  <i>html and Velocity code</i>
]]&gt;&lt;/selection-page&gt;</code></pre>
    <p>For each <code>HierarchyLevel</code> that is neither the topmost (this uses title-page) nor a method (this uses code-page), a selection-page with the name &quot;index.html&quot; is created in a folder with the name of the <code>HierarchyLevel</code> which lies in the folder of the levels parent. A selection-page should link to all its children.</p>
   <pre><code>
&lt;code-page&gt;&lt;![CDATA[
  <i>html and Velocity code</i>
]]&gt;&lt;/code-page&gt;</code></pre>
   <p>For each method a code-page is created. Again, it is stored in a file with the name &quot;index.html&quot; in a subfolder with same name as the method. It could, as the name tells, contain code (but the other files could, of course, contain code, too. It depends on the data you access via the objects in the Velocity Context).</p>
    <h4>Optional sections</h4>
    <p>To use a language other than English (if available), you can use:</p>
    <pre><code>
&lt;language&gt;
  <i>two letter abbreviation for language</i>
&lt;/language&gt;  </code></pre>
    <p>Use</p>
    <pre><code>
&lt;text-file filename=&quot;<i>NAME</i>&quot; content-type=&quot;<i>MIMETYPE</i>&quot;&gt;&lt;![CDATA[
  <i>plain text and Velocity code</i>
]]&gt;&lt;/text-file&gt;  </code></pre>
    <p>to create text-files with the name NAME in the <i>INDEXFILE-files</i> directory. </p>
    <p>Use</p>
    <pre><code>
&lt;binary-file filename=&quot;<i>NAME</i>&quot; content-type=&quot;<i>MIMETYPE</i>&quot;&gt;&lt;![CDATA[
  <i>base64 encoded data</i>
]]&gt;&lt;/binary-file&gt;  </code></pre>
    <p>to create binary-files with the name NAME in that directory. </p>
    <h3>CSVReport Template</h3>
    <p>A template file for the CSV export look that way:</p>
    <pre><code>&lt;?xml version="1.0" encoding="UTF-8"?&gt;
&lt;report version="1.0" xmlns="http://www.codecover.org/xml/report-template"&gt;
    &lt;plugin&gt;org.codecover.report.csv&lt;/plugin&gt;
    &lt;generator&gt;org.codecover.report.csv.CSVReportGenerator&lt;/generator&gt;
    &lt;name xml:lang="en"&gt;CSV Report&lt;/name&gt;
    &lt;description xml:lang="en"&gt;
        Generates a report with comma separated values.
    &lt;/description&gt;
    &lt;template version="1"
            xmlns="http://www.codecover.org/xml/report-template/csv"&gt;
<i>tags defining the csv table</i>
    &lt;/template&gt;
&lt;/report&gt;
    </code></pre>
  <p>Between the template-tags, there are two different kinds of tags allowed, each as often as you wish:</p>
  <ol>
  <li><code>&lt;column&gt;</code>STRING<code>&lt;/column&gt;</code> or <code>&lt;column header="HEADER"&gt;</code>STRING<code>&lt;/column&gt;</code> for each column the output should have, where HEADER is the value in the first line of the csv file (or an empty cell, if HEADER is not set) and STRING is the content of each cell in this column</li>
  <li><code>&lt;columnsPerMetric&gt;</code><i>several columns(see 1.)</i><code>&lt;/columnsPerMetric&gt;</code> for columns that shall be created for each available coverage metric.</li>
  </ol>
  <p>HEADER and STRING can contain placeholders that are replaced by the values you want to report. These placehlders can't occur everywhere:</p>
  <p>In HEADERs of columns that are part of a <code>columnsPerMetric</code> area, you can use <code>%1$s</code> which will be replaced by the name of the metric.</p>
  <p>Each STRING can contain <code>%2$s</code> which will be replaced by the name of the HierarchyLevel this line belongs to.</p>
  <p>Only STRINGS of columns that are part of a <code>columnsPerMetric</code> area can contain:</p>
  <ul>
  <li><code>%1$s</code> = the name of the metric</li>
  <li><code>%2$s</code> = the name of the HierarchyLevel</li>
  <li><code>%3$d</code> = the number of covered items this metric counts for this HierarchyLevel</li>
  <li><code>%4$d</code> = the number of uncovered items this metric counts for this HierarchyLevel</li>
  <li><code>%5$d</code> = the total number of coverable items of this HierarchyLevel for this metric (= <code>%3$d</code> + <code>%4$d</code>)</li>
  <li><code>%6$f</code> = the quotient of covered and total items = <code>%3$d</code> / <code>%5$d</code></li>
  <li><code>%7$f</code> = the 100 * <code>%6$f</code> which is the coverage in percent as you are used to read it</li>
  </ul>

m4_web_create_footer
