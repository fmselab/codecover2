<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: install.html.m4 32 2009-03-09 12:33:36Z schmidberger $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`Installation Guide')

    <h2>Installation Guide</h2>
    This section contains installation instructions for both CodeCover Plugin for Eclipse and standalone CodeCover for use from Command Line and Ant 
    <h3>Installation requirements</h3>
    The following software packages must be installed before you can install CodeCover:
    <ul>
      <li>Java Runtime Environment or Java Development Kit 5.0 (also known as 1.5) or newer</li>
      <li>Eclipse 3.3 or higher (for Eclipse Plugin only)</li>
    </ul>
    <h3>CodeCover Standalone (Batch and Ant support)</h3>
<p>You can download the standalone CodeCover version <a href="http://downloads.sourceforge.net/codecover/codecover-batch-1.0.tar.bz2">here</a>. The releases are distributed as a tarball which you need to extract to a folder on your computer, after that you're ready to go.</p>
<p>For <a href="tutorials/how_to_batch.html">command line usage</a>, <code>codecover.sh</code> (Un*x) and <code>codecover.bat</code> (Windows) wrapper scripts are provided. You may want to set your PATH environment variable to include the folder to which you extracted CodeCover, so you don't need to type in the full path to CodeCover every time.</p>
<p>For usage with Apache Ant, refer to the <a href="tutorials/ant_manual.html">CodeCover Ant HOWTO</a>.</p>
<h3>CodeCover Eclipse Plugin</h3>
<p>The CodeCover Eclipse Plugin is installed using the standard Eclipse update mechanism:</p>
<ol>
<li>Start Eclipse.</li>
<li>From the main menu, select &quot;Help&quot;, &quot;Software updates&quot;, &quot;Find and install...&quot;.</li>
<li>In the upcoming dialog, select &quot;Search for new features to install&quot; and click &quot; "Next".</li>
<li>The list containing known update sites will be displayed. Create a new update site by clicking on &quot;New Remote Site...&quot; and enter the following information:
<pre>
   Name: <strong>CodeCover Update Site</strong>
   URL:  <strong><a href="http://update.codecover.org/">http://update.codecover.org/</a></strong>
</pre>
Select the newly created site, if not already selected, and click &quot;Finish&quot;. Eclipse will now contact the CodeCover update site; this might take a moment depending on your network connection.
</li>
<li>In the next dialog, you will see the list of features found on the update site; for now, CodeCover consists of only one feature. Select it for installation and click &quot;Next&quot;.
</li>
<li>After the download has finished, Eclipse will ask you some questions about the license and installation, and prompt to restart the workbench. You should confirm the restart as not all CodeCover features are guaranteed to work if you simply click &quot;Apply Changes&quot;.</li>
</ol>
Now CodeCover for Eclipse is installed! You can proceed to the <a href="tutorials/how_to_complete.html">CodeCover Eclipse walkthrough</a> to learn how you can use it.

m4_web_create_footer
