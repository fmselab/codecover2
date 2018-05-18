<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: report-generator.html.m4 1 2007-12-12 17:37:26Z t-scheller $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`Create a new report generator')

    <h2>Create a new report generator</h2>
    <h3>Creating a new plugin</h3>
    <p>First of all, you will want wo create a plugin for your new report generator so you have it separeted from the core of CodeCover. To do so, create a new java project and in it a child class of <code>org.codecover.model.extensions.AbstractPlugin</code> which implements a new constructor without any parameters. This constructor calls one of the two super constructors</p>
    <pre><code>
AbstractPlugin(String name
               String description, 
               Set&lt;Extension&lt;?&gt;&gt; extensions)</code></pre>
    <p>or</p>
    <pre><code>
AbstractPlugin(String name,
               String description,
               Extension&lt;?&gt;[] extensions)</code></pre>
    <p>where <code>name</code> is the name of the plugin, <code>description</code> is its description and <code>extensions</code> are <code>Extension</code>s this plugin provides.</p>
    <p>Such an extension could be child of <code>org.codecover.model.extensions.AbstractExtension&lt;ReportGenerator&gt;</code>.</p>
    <p>Example:</p>
    <pre><code>
package org.codecover.report.velocity;

import org.codecover.report.Report;
import org.codecover.report.ReportGenerator;
import org.codecover.report.exceptions.ReportException;
import org.codecover.report.exceptions.ReportIOException;

public class VelocityReportPlugin extends AbstractPlugin {

    public VelocityReportPlugin() {
        super("Velocity Report",
              "This plugin contains a velocity generator",
              new Extension&lt;?&gt;[] {new AbstractExtension&lt;ReportGenerator&gt;(
                      ReportGenerator.class,
                      "org.codecover.report.velocity.VelocityReportGenerator")
                          {
                              public ReportGenerator getObject() {
                                  return new VelocityReportGenerator();
                              }
                          }
               });
        }
}</code></pre>
    <p>where <code>org.codecover.report.velocity.VelocityReportGenerator</code> implements <code>ReportGenerator</code> (see below).
    </p>
    <p>The last thing you need is a <code>codecover-plugin.xml</code> file. The one for our example would be</p>
    <pre><code>
&lt;plugin xmlns="http://www.codecover.org/xml/plugin-descriptor-1.0"
        name="org.codecover.report.velocity"
        version="0.1"
        class="org.codecover.report.velocity.VelocityReportPlugin"&gt;
  &lt;depends name="org.codecover" version="0.1" /&gt;
&lt;/plugin&gt;</code></pre>
    <p>
    where you, of course, have to change the name of the package and class to the ones you have chosen. The XML-file must in on the topmost folder of the project. When you are ready, save the project as a jar and store it in the plugin-folder of CodeCover, specified via batch interface or in the CodeCover preferences in Eclipse.
    </p>
  <h3>ReportGenerator</h3>
  <p>As mentioned above, your report generator has to implement the interface <code>ReportGenerator</code> which requires the two methods</p>
  <pre><code>String getContentType()

void generateReport(Report report) throws ReportException, ReportIOException</code></pre>
<p>where <code>getContentType</code> returns a MIME content-type, e.g. "text/html", and <code>gerenerateReport</code> does the work. Each report is created with a template file (see <a href="report-format.html">"Adapt the report format"</a>). This template file names the ReportGenerator (in the example above, it would be <code>org.codecover.report.velocity.VelocityReportGenerator</code>) that should do the work. A <code>Report</code> is created, filled with the data from the template and the options the user set by commanding to generate a report. Not all of these data has to be set, so first of all it's recommended to check if the members of <code>report</code> you need are set. Then you can use these information (which can be - via the template - whatever you want) to create the report. The implementation of <code>generateReport</code> depends on the kind of report you want to create. <code>Report</code> gives you access to all data you may need; see the JavaDoc or the code of <code>org.codecover.report.Report</code> or its shipped implementations for details. 
  </p>

m4_web_create_footer
