<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: metric.html.m4 1 2007-12-12 17:37:26Z t-scheller $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`Create a new metric')

    <h2>Create a new metric</h2>
    <p> CodeCover is delivered with four implemented coverage metrics:</p>
    <ul>
      <li>Statement coverage</li>
      <li>Branch coverage</li>
      <li>Loop coverage</li>
      <li>Strict condition coverage</li>
    </ul>
    <p> If these metrics don't suite your needs, you can implement your own
      metric. <br/>
      This metric can use the data collected by the instrumented code in order
      to provide coverage statistics. </p>
    <h3>Implementing the <code>CoverageMetric</code> interface</h3>
    <p>
      In order to create a new metric, you have to implement the
      <code>org.codecover.metrics.coverage.CoverageMetric</code>
      interface. This interface contains methods
      to calculate the coverage for MAST nodes.
      The abstract class
      <code>org.codecover.metrics.coverage.AbstractCoverageMetric</code>
      provides default implementations for many methods in the interface.
    </p>
    <p>
      You still have to implement the <code>getCoverageLocal()</code> and
      <code>getHints()</code> methods.
    </p>
    <p>
      The <code>getCoverageLocal()</code> methods return the coverage for a
      single MAST element. If your metric doesn't create coverage information
      for a specific MAST element, you can return
      <code>CoverageResult.NULL</code> in this overload.
    </p>
    <p>
      The <code>getHints()</code> methods return additional coverage hints.
      If you don't want to return additional coverage hints, you can return
      <code>noHints</code> which is an empty
      <code>Set&lt;CoverageHint&gt;</code>.
    </p>
    <h3>Creating a plugin</h3>
    <p>
      In order to create a plugin with your new metric, you have to create a
      class implementing <code>org.codecover.model.extensions.Plugin</code>.
      You can use <code>org.codecover.model.extensions.AbstractPlugin</code>
      for an easier implementation.
    </p>
    <p>
      In order to create a plugin, you have to put all necessary
      <code>.class</code> files into a <code>.jar</code> file with a file
      called <code>codecover-plugin.xml</code> which has the following
      structure:
    </p>
    <pre><code>&lt;plugin xmlns="http://www.codecover.org/xml/plugin-descriptor-1.0"
        name="example.metric" version="4.2"
        class="example.metric.MetricPlugin"&gt;
  &lt;depends name="org.codecover" version="0.1" /&gt;
&lt;/plugin&gt;
</code></pre>
  <p>
      Here it is assumed that your plugin class has the name
      <code>example.metric.MetricPlugin</code> and you want to call your plugin
      <code>example.metric</code> with the version number <code>4.2</code>.
      The plugin will require CodeCover with a plugin ABI version of
      <code>0.x</code> with <code>x >= 1</code>.
    </p>
    <h3>Example</h3>
    <p>
      <a href="metric-example.jar">Example metric</a>
    </p>
    <p>
      In order to compile the example you might have to adjust the path of the
      <code>codecover-core.jar</code> in the <code>build.xml</code> file.
    </p>
    <p>
      The example is an implementation of a method coverage. Only methods with
      at least one basic statement will be considered.
    </p> 

m4_web_create_footer
