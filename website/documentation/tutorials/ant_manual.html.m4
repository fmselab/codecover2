<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: ant_manual.html.m4 28 2008-05-28 20:31:17Z ahija $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`HOWTO use CodeCover with ANT')

    <h2>HOWTO use CodeCover with ANT</h2>
    <p> This tutorial is about using CodeCover with an <a href="http://ant.apache.org/">Apache ANT</a> script file.
      The templates for ANT are the files <a href="ant-build-codecover.xml"><code>ant-build-codecover.xml</code></a> and <a href="ant-build-codecover2.xml"><code>ant-build-codecover2.xml</code></a>. <code>ant-build-codecover.xml</code> is a simple example for using CodeCover which instruments, compiles and executes the code and then creates a report.
      With <code>ant-build-codecover2.xml</code>, the compiled code will be executed twice, so two test sessions will be created. A report will be created after the second execution. The application that is used as an example here is called SimpleJavaApp. Its sources are available here, as <a href="SimpleJavaApp.zip">.zip</a> or <a href="SimpleJavaApp.tar.bz2">.tar.bz2</a>.</p>
    <p> In order to use the template files you must adapt their heads to your system.</p>
    <pre><code>&lt;property name=&quot;codecoverDir&quot; value=&quot;codecover&quot;/&gt;
&lt;property name=&quot;sourceDir&quot; value=&quot;src&quot;/&gt;
&lt;property name=&quot;instrumentedSourceDir&quot; value=&quot;instrumented&quot;/&gt;
&lt;property name=&quot;mainClassName&quot; value=&quot;Test&quot;/&gt;</code></pre>
    <ul>
      <li> The property <code>codecoverDir</code> must be set to the CodeCover folder
        (the folder with the subfolder <code>plugins</code> and the <code>codecover-core.jar</code>). </li>
      <li>The property <code>sourceDir</code> must be set to the folder that contains the java source files. </li>
      <li>The property <code>instrumentedSourceDir</code> must be set to the folder which contains the instrumented source code files and the compiled files.
        The default value for this property should work in most cases. </li>
      <li>The value of the property <code>mainClassName</code> must be set to the name of the class that will be executed.</li>
    </ul>
    <p>Example: CodeCover is located in the folder <code>/usr/local/share/java/codecover</code>. The ANT file is called <code>build-codecover.xml</code> and lies in the SimpleJavaApp folder. The SimpleJavaApp folder contains the source code files in a subfolder called <code>src</code>. The main class is called <code>SimpleJavaApp.java</code>.</p>
    <p>The head of the ANT file should be modified to look like this:</p>
    <pre><code>&lt;property name=&quot;codecoverDir&quot; value=&quot;/usr/local/share/java/codecover&quot;/&gt;
&lt;property name=&quot;sourceDir&quot; value=&quot;src&quot;/&gt;
&lt;property name=&quot;instrumentedSourceDir&quot; value=&quot;instrumented&quot;/&gt;
&lt;property name=&quot;mainClassName&quot; value=&quot;SimpleJavaApp.java&quot;/&gt;</code></pre>
    <p> After these preparations, the script can be used with the command <code>ant -f build-codecover.xml</code>. </p>

m4_web_create_footer
