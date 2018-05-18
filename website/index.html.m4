<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: index.html.m4 32 2009-03-09 12:33:36Z schmidberger $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`CodeCover - an open-source glass-box testing tool')

    <div id="downloadAreaDiv">
      <img src ="images/cc-Logo_400px.png" alt="CodeCover" />
      <a href="documentation/install.html"><img src="images/dlButton.png" alt="Download CodeCover"/> </a> </div>
    <div id="screenShotDiv"> <a href="images/overview_plugin.png"><img src="images/overview_plugin_thumb.png" alt="Overview"/></a> </div>


<div id="content">
<table>
<tr>
<td>
<p>News</p>
</td>
<tr>
<td valign="top">
<ul>
<li><b>02.03.2009: (German) CodeCover auf den Test-Tagen 2009 in Stuttgart</b><br />
Auf den Test-Tagen 2009 der Java User Group Stuttgart vom 07.05.-08.05.2009 wird CodeCover pr&auml;sentiert <a href="http://www.iste.uni-stuttgart.de/se/events/testtage2009/">mehr ...</a>
<li><b>09.03.2009: New CodeCover 1.0.0.1 released</b><br />
The instrumentation is changed: return-statements are now instrumented too.
</td>
</tr>
</table>
</div>

<div id="content">
<table>
<tr>
<td>
<p>The benefits of glass-box testing</p>
</td>
<td>
<p>Codecover capabilities</p>
</td>
</tr>

<tr>
<td valign="top">

<ul>
<li><b>Testing adequacy metric</b><br />
Coverage is an objective adequacy metric which can be used for
example as a test completion criterion.
</li>

<li><b>Test suite extension</b><br />
The glass-box test denotes the program elements which were not
executed.
</li>

<li><b>Test suite reduction</b><br />
Removing (redundant) test cases from a test suite to reduce
regression testing effort without (significantly) decreasing testing
effectiveness
</li>

<li><b>Basis for selective regression testing</b><br />
Instead of "rerun-all" in regression testing, only those test cases
are selected that were "involved" in the code modification.
</li>

<li><b>Support for program comprehension</b><br />
The glass-box test denotes which program code is executed by
which test case (traceability).
</li>
</ul>

</td>
<td valign="top">
<ul>
<li><b>General</b><br />
CodeCover is a free glass-box testing tool developed in 2007 at the University of Stuttgart (<a href="http://www.iste.uni-stuttgart.de/se/einstieg_se.html">ISTE</a>).
</li>

<li><b>Coverage metrics</b><br />
CodeCover measures statement, branch, loop, and MC/DC coverage
</li>

<li><b>Reports</b><br />
CodeCover uses the template engine Velocity. 
</li>

<li><b>Platform</b><br />
Command line (Linux, Windows, Mac OS) and Eclipse and Ant integration
</li>

<li><b>Programming Languages</b><br />
Open language interface, available languages: Java and COBOL
</li>

<li><b>Licence</b><br />
Eclipse Public Licence (EPL)
</li>

<li><b>Quick Overview</b><br />
See presentation slides <a href="http://www.iste.uni-stuttgart.de/se/publications/download/SE2008_Rainer_Schmidberger_Folien.pdf">[pdf]</a>.
</li>

</ul>

</td>
</tr>
</table>

</div>



m4_web_create_footer
