<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: eclManual.html.m4 28 2008-05-28 20:31:17Z ahija $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`Eclipse Plugin Reference')

    <h2>Eclipse Plugin Reference</h2>
    <ul>
      <li><a href="#Build">Build</a></li>
      <li><a href="#CorrelationMatrix">Correlation Matrix</a></li>
      <li><a href="#TestSessionsView">Test Session View</a></li>
      <li><a href="#CoverageView">Coverage View</a></li>
      <li><a href="#BooleanAnalyzer">Boolean Analyzer</a></li>
      <li><a href="#Highlighting">Highlighting</a></li>
      <li><a href="#ImportWizards">Import Wizards</a>
        <ul>
          <li><a href="#CoverageLogImportWizard">Coverage Log Import Wizard</a></li>
          <li><a href="#TestSessionContainerImportWizard">Test Session Container Import Wizard</a></li>
        </ul>
      </li>
      <li><a href="#ExportWizards">Export Wizards</a>
        <ul>
          <li><a href="#CoverageResultExportWizard">Coverage Result Export Wizard</a></li>
        </ul>
      </li>
      <li><a href="#JUnit">CodeCover Measurement for JUnit</a></li>
      <li><a href="#LiveNotification">Live Notification</a></li>
      <li><a href="#Preferences">Preferences</a></li>
    </ul>
    <h3> <a name="Build" id="Build"></a>Build</h3>
    <h4> Selecting the files to instrument:</h4>
    <p> <img alt="PackageExplorerScreenshot" src="../../images/ecl-img/PackageExplorerScreenshot.png" />&nbsp;</p>
    <p> Open the Package Explorer view. Choose the Java Classes and Packages you want to
      instrument and open the context menu by right-clicking on the selected items. The
      menu item &quot;Use For Coverage Measurement&quot; allows you to toggle the instrumentation
      state of the selected items.</p>
    <h4> Enabling CodeCover for a Java project:</h4>
    <p> <img alt="ProjectPropertiesScreenshot" src="../../images/ecl-img/ProjectPropertiesScreenshot.png" />&nbsp;</p>
    <p>Open the Properties of your project, e.g. by selecting the project in the Package
      Explorer and choosing the Properties menu item of the Project menu. Once the Properties
      are visible, select the CodeCover tab. This tab contains a checkbox which you use
      to enable/disable CodeCover for the project.</p>
    <h4> Running a Java project with CodeCover:</h4>
    <p> <a href="../../images/ecl-img/LaunchConfigurationScreenshot.png"><img src="../../images/ecl-img/LaunchConfigurationScreenshot_small.png" alt="LaunchConfigurationScreenshot" width="700" height="358" /></a>&nbsp;</p>
    <p> Make sure CodeCover is activated for the Java project you want to run. Open the Launch
      Configuration you want to use, e.g. by selecting the &quot;Open Run Dialog&hellip;&quot; menu item
      of the Run menu and selecting it in the dialog which is displayed. You'll notice
      an additional CodeCover tab there. Select the CodeCover tab and click on the &quot;Run
      with CodeCover&quot; checkbox. Click the Run button to start the execution. In case you
      want to run your application without CodeCover later, simply go back to the CodeCover
      tab and uncheck the &quot;Run with CodeCover&quot; checkbox.</p>
    <p> Once the execution of the application is finished, the gathered coverage data will
      be shown in the Coverage View.</p>
    <h3> <a name="CorrelationMatrix" id="CorrelationMatrix"></a>Correlation Matrix</h3>
    <p> The correlation matrix is used to compare test cases of a test session container
      with each other and to display the result of this comparison. There are two different
      visualizations to choose from:</p>
    <ul>
      <li><span style="font-size: 12pt">&quot;TreeViewer&quot;</span></li>
      <li><span style="font-size: 12pt">Matrix</span></li>
    </ul>
    <p><a href="../../images/ecl-img/Matrix_Screenshot.png"> <img src="../../images/ecl-img/Matrix_Screenshot_small.png" alt="Matrix_Screenshot" height="299" width="600"/></a></p>
    <p> The &quot;TreeViewer&quot; displays those test cases, that are completely covered by other
      test cases. As can be seen in the screenshot, test case &quot;a23&quot; has no children, meaning
      it does not cover any other test case completely. On the other hand, test case &quot;b1234&quot;
      has multiple children, meaning that it, e.g., covers &quot;a23&quot; completely. If two test
      cases cover each other completely, they are displayed in the same tree node, separated
      by a &quot;,&quot;, as they are essentially equal to each other.</p>
    <p> The matrix displays not only those test case, that are completely covered, but also
      every degree of coverage. Those degrees are visualized with colors and detailed
      in the legend, that can be seen in the right-hand side of the view. The colors,
      and the ranges they represent, can be adjusted in the preferences of the plugin <a href="#Preferences">Preferences Page</a>. The matrix is to be read from the left,
      e.g. test case &quot;d123&quot; covers test case &quot;b1234&quot; to a degree in the range of 66% -
      100%, since the color of the square is purple. A tooltip is displayed for every
      square of the matrix, which, amongst other things, contains the exact degree of
      coverage and for every test case labels, such as &quot;a23&quot;, which contains the full name
      of the test case and the name of the test session the test case belongs to.</p>
    <p> The toolbar contains five items:</p>
    <ul>
      <li>Export the currently displayed matrix data into a .csv file </li>
      <li>Hide top-level tree items</li>
      <li>Choose correlation metric and calculate correlation </li>
      <li>Automatically calculate correlation</li>
      <li>Show legend</li>
    </ul>
    <p>The &quot;Export to csv&quot; toolbar item (<img alt="" src="../../images/ecl-img/csv_export.gif" width="16" height="16" />) can be used to save the currently displayed correlation percentages into a file, using the <a href="http://www.rfc-editor.org/rfc/rfc4180.txt">&quot;comma seperated value&quot; format</a>. The user can specify the location and the name of the file.</p>
    <p> The &quot;Hide top-level tree items&quot; toolbar item (<img alt="" src="../../images/ecl-img/hide_top_level.gif" width="16" height="16" />) can be used to hide all those tree
      nodes in the top-most level, that have no children &ndash; those test cases that do not
      cover any other test cases completely &ndash; since they are not as interesting to the
      user, as those that have children, and could point to redundancies in the user test
      suite.</p>
    <p> The correlation criteria, that are used in the comparison, can be chosen with the
      &quot;Choose and calculate correlation&quot; toolbar item (<img alt="" src="../../images/ecl-img/calculate_correlation.gif" width="16" height="16" />). A popup menu appears, which contains
      an entry for every one of the correlation criteria, and an additional entry for
      all available criteria. This means, that the user can either choose to use one criteria
      or all of them in the comparison. With a push of the button itself, the comparison
      can be started manually, which recalculates the correlation of the test cases and
      refreshes the &quot;TreeViewer&quot; and the matrix.</p>
    <p> The test cases, that are part of the comparison can be chosen in the <a href="#TestSessionsView"> Test Session View</a>. It is possible, with the &quot;Automatically calculate correlation&quot;
      toolbar item (<img alt="" src="../../images/ecl-img/auto_calculate.gif" width="16" height="16" />) to specify, whether the correlation should be calculated every time
      the selection in the &quot;test session view&quot; is changed, or only on the users request.
      Since the calculation can be potentially time-consuming, given enough test cases,
      this can save system resources and improve performance.</p>
    <p> The legend, that is located to the right-hand side of the matrix can be hidden,
      or shown again, with the &quot;Show legend&quot; toolbar item (<img alt="" src="../../images/ecl-img/show_legend.gif" width="16" height="16" />).</p>
    <h3> <a name="TestSessionsView" id="TestSessionsView">Test Sessions View</a></h3>
    <p><img src="../../images/ecl-img/TestSessionViewScreenshot.png" alt="Test Session View Screenshot" width="494" height="264" /></p>
    <p> The <em>Test Sessions</em> view lists the test sessions (<img alt="test session icon"
                    src="../../images/ecl-img/test_session.gif" />) and the contained test cases (<img alt="test case icon"
                        src="../../images/ecl-img/test_case.gif" />) of the active test session container (<img alt="test session container icon"
                            src="../../images/ecl-img/test_session_container.gif" />). Test sessions contain
      the coverage measurements of a test run divided into test cases.</p>
    <p> The drop-down list on the top of the <em>Test Sessions</em> view lists all test
      session containers of all opened projects. You can activate a test session container
      by choosing one of the drop-down list.</p>
    <p> You can activate test cases using the check boxes nearby the names of the test cases.
      Coverage Measurements of activated test cases are visualized by the <em>Coverage</em> view, the <em>Correlation</em> view and the code highlighting in editor windows. Bear in mind that there is a difference between activating test sessions/test cases, which is done via the check boxes, and selecting test sessions/test cases, which is done by simply clicking on the row of the relevant test session/test case.</p>
    <p> The <em>Test Sessions</em> view's toolbar offers the following actions: </p>
    <dl>
      <dt> <img alt="Save active test session container" src="../../images/ecl-img/test_session_container_save.gif" /> Save active test session container </dt>
      <dd> Saves the currently activated test session container. Test Session Containers are saved automatically if you select an other one, if you close a project or if you quit Eclipse. Thus you need this action only if you explicitly want to save the active test session container at a specific point in time. But usually you won't need this action because the plugin handles all save operations automatically.</dd>
      <dt>&nbsp;</dt>
      <dt> <img alt="Delete active test session container" src="../../images/ecl-img/test_session_container_delete.gif" /> Delete active test session container </dt>
      <dd> Delete the currently activated test session container. </dd>
      <dt>&nbsp;</dt>
      <dt><img alt="Merge selected test sessions or test cases" src="../../images/ecl-img/elements_merge.gif" /> Merge selected test sessions or test cases </dt>
      <dd> Opens a dialog which guides you in merging test sessions or test cases. </dd>
      <dt>&nbsp;</dt>
      <dt><img alt="Delete selected test sessions/test cases icon" src="../../images/ecl-img/elements_delete.gif" /> Delete selected test sessions/test cases </dt>
      <dd> Delete the selected test sessions and test cases. </dd>
      <dt>&nbsp;</dt>
    </dl>
    <p> The <em>Test Sessions</em> view's context menu offers the following actions: </p>
    <dl>
      <dt>Select all </dt>
      <dd> Select all test cases and test sessions of the currently active test session container. </dd>
      <dt>&nbsp;</dt>
      <dt>Activate all </dt>
      <dd> Activate all test cases of the currently active test session container. </dd>
      <dt>&nbsp;</dt>
      <dt>Deactivate all </dt>
      <dd> Deactivate all test cases. </dd>
      <dt>&nbsp;</dt>
      <dt>Delete </dt>
      <dd> Delete the selected test sessions and test cases. </dd>
      <dt>&nbsp;</dt>
      <dt>Properties </dt>
      <dd> View or change the properties (name and comment) of the selected test session or
        test case. </dd>
    </dl>
    <h3> <a name="CoverageView" id="CoverageView"></a>Coverage View</h3>
    <p><img src="../../images/ecl-img/CoverageViewScreenshot.png" alt="Coverage View Screenshot" width="674" height="240" /></p>
    <p> The <em>Coverage</em> view visualizes the coverage measurements of the active test
      cases. The displayed tree shows the structure of the tested software at the point
      in time the measurements were taken. Each row displays the coverage measurement
      of the corresponding element which name and type is displayed in the first column.
      Whereas elements are either java types or the project the active test cases belong
      to, in this case the corresponding coverage measurements are the combined measurements
      of the whole project. The coverage measurements themselves are displayed in the
      rest of the columns, starting from the second one. The values of the measurements
      are given in percent, whereas 0.0% means that nothing was covered by the active
      test cases and 100.0% percent means full coverage of the corresponding element.</p>
    <p> The <em>Coverage</em> view's toolbar offers four actions which control which type
      is displayed as the top-level-type in the tree. Or in other words, the actions set
      the type by which methods are grouped. All levels above the selected type are simply
      not displayed. For example if you select classes as the top-level-type, packages
      and the project won't be visible. The following actions of the toolbar (from left
      to right) control this feature: </p>
    <dl>
      <dt>Group by project </dt>
      <dd> Sets the project to the top-level type. </dd>
      <dt>Group by packages </dt>
      <dd> Sets packages to the top-level type. </dd>
      <dt>Group by classes </dt>
      <dd> Groups methods by classes, interfaces, enums and annotations. </dd>
      <dt>Group by methods </dt>
      <dd> Lists all methods. </dd>
    </dl>
    <p> Above the tree visualization a simple filter is shown which filters methods by their
      degree of coverage. You can activate or deactivate the filter using the (leftmost)
      check box.</p>
    <h3><a name="BooleanAnalyzer" id="BooleanAnalyzer"></a>Boolean Analyzer</h3>
    <p><a href="../../images/ecl-img/Boolean_Analyzer_Screenshot.png"> <img src="../../images/ecl-img/Boolean_Analyzer_Screenshot_small.png" alt="Matrix_Screenshot" height="209" width="600"/></a></p>
   <p>The Boolean Analyzer shows the boolean value of each basic boolean term, operator term and the root term according to evaluations of the condition which are recorded during the execution of the SUT. This data is presented in a table. The operators, operands and brackets define the columns and the evaluations are shown as rows in the table. In addition, a column for test cases is added. In that column one can see the test cases which covered the evaluation and the number of execution. Two table values of a column may contain green background. This represents that the basic boolean term of that column is covered according to the strict condition coverage criterion. If a column of a basic boolean term does not contain any colored values then the term is not covered. There are two ways to select a condition in the Boolean Analyzer. First, there are combo-boxes for classes and conditions. The second way is to select the keyword of the condition in the source code, right-click, and select the "Analyze Term" item in the context menu, which will automatically select the condition in the Boolean Analyzer.</p>
   
    <h3> <a name="Highlighting" id="Highlighting"></a>Highlighting</h3>
<p>CodeCover highlights the coverage produced by the active test cases in the default java editor of Eclipse. The results of all enabled metrics are shown at the same time. To show if an element is covered, the background color of the pieces of code that represent it is changed to something else than white (the default), we say it is highlighted. For example the keyword <code>for</code> is highlighted in green, if the corresponding for loop is covered fully by loop coverage, while the whole statement <code>i = i + 7;</code> is highlighted in red if it is not covered.</p>

<h4>Semantic of the colors</h4>
<!-- see specification (section 3.8.2 Java). Which is currently wrong/incomplete: try-catch -->
<p>
Each element is either not coverable by a metric and not highlighted or it has so called coverable items from exactly one metric which determine the highlighting. A coverable item is the atomic thing for which a metric decides if it is covered or not. The highlighting color of an element tells you if all, some or none of its coverable items are covered.
The default Colors are:
</p>
<ul>
<li>Green: Fully covered by the metric, i.e. all coverable items are covered</li>
<li>Yellow: Partly covered by the metric, i.e. some coverable items are covered, not all. A tool tip tells you which are not covered.</li>
<li>Red: not covered by the metric, i.e. no coverable items are covered.</li>
</ul>
<p>
Now we will go through all of Java's elements that can be covered by one of CodeCover's metrics and explain their coverable items. The elements are grouped by Metric.
</p>
<p>
<a href="HelloHighlighting.java">Here</a> is some example code with comments to give you an idea which elements can be highlighted and which coverable items they have. Some coverable items are listed explicitly after "cov:" in the comments:
</p>
<img alt="Screenshot of highlighting conditional" src="../../images/ecl-img/highlighting_conditional.png" />

<h4>Statement coverage</h4>
<p>
Statement coverage covers only so called basic statements, like <code>i++;</code>. They have exactly one coverable item, which is covered if the statement was executed successfully at least once. Throwing an exception is not considered a successful execution.
</p>
<p>Basic statements don't contain other statements and can always have statements following them. The former excludes branching statements and loops, which contain blocks of statements. This avoids highlighting the same element by two different metrics. The latter excludes <code>return</code> and <code>throw</code>, which can't be instrumented for technical reasons. Look at the statements around them to see if they were executed.</p>

<h4>Branch coverage</h4>
<p>
Branch coverage covers conditional statements (like <code>if</code>) and their individual branches (like <code>else</code>). Each branch has exactly one coverable item, which is covered if it was taken, i.e. the control flow entered its body once. A branching statement has the coverable items of all its branches. There are also branches which have no element that can be highlighted, like the 'then' branch of an if. It has no keyword in Java. And implicit branches, which are omitted in the code, like the else-branch of an if without an <code>else</code> block.</p>

<h4>Conditional coverage</h4>
<p>
Conditional coverage covers basic boolean terms that are part of the conditional expression of a loop or branching statement. A basic boolean term, sometimes called condition, is a boolean term that is not constructed of other boolean terms. Examples are boolean variables and method calls that return boolean.</p>
<p>They have one coverable item, which is covered if the term can affect the result of its encompassing decision. In other words it is covered, if the term is evaluated to both true and false and the change from true to false (or false to true) changes the result of the whole condition while every other basic boolean term of the conditional expression remains constant or is not evaluated.
</p>

<h4>Loop coverage</h4>
<img alt="Screenshot of highlighting loops" src="../../images/ecl-img/highlighting_loops.png" />

<p>
Loop coverage covers for, while and do-while represented by their keywords <code>for</code>, <code>while</code> and <code>do</code>. A loop has at most three coverable items representing the number of iterations. They are body executed in one run: </p>
<ul>
 <li>zero times (not for do-while)</li>
 <li>one time</li>
 <li>more than one time.</li>
</ul>
<p>
Each is covered it the loop has been run once with a suitable number of iterations of the loop body.
</p>

<h4>Changing colors</h4>
<p> The  highlighting is realized with Eclipse's annotations. For every coverage status (covered fully, covered partly and not covered) there is one annotation type per metric. Branch, Loop, Statement and Conditional represent the metrics included within the CodeCover release and Other is used for metrics added by plugins. You can change the layout of all of these 15 different annotations globally for your workspace in your Eclipse preferences dialog. The dialog can be found by opening Window &gt; Preferences and entering "ann" into the search field: </p>
  <p><img alt="Screenshot of Annotation Preferences" src="../../images/ecl-img/annotation_page.png" />
  </p>
    
    <h3> <a name="ImportWizards" id="ImportWizards"></a>Import Wizards </h3>
    <h4> <a name="CoverageLogImportWizard" id="CoverageLogImportWizard"></a>Coverage Log Import Wizard </h4>
    <p> <img alt="Screenshot of Coverage Log Import Wizard" src="../../images/ecl-img/ImportCoverageLogScreenshot.png" /> </p>
    <p> This import wizard imports a coverage log from a file into a selected test session
      container of an open project.</p>
    <p> A coverage log is created during the run of a coverage measurement on a software.
      It collects the measurements taken during the run. To evaluate the coverage log
      (for example, view the coverage results in the <em>Coverage</em> view), you have
      to import it into a test session container, where it will be saved as a test session.</p>
    <p> In the <em>File</em> field, type or browse to select the file which contains the
      coverage log to import. The charset of the coverage log file can be selected in
      the <em>Charset</em> field. If you're not sure about the charset, try CodeCover's
      default charset which is already selected. In the tree select the test session container
      you want to import the coverage log file into. The last step is to enter a name
      and comment for the <em>Test Session</em> which will contain the measurements of
      the imported coverage log.</p>
    <h4> <a name="TestSessionContainerImportWizard" id="TestSessionContainerImportWizard"></a>Test Session Container Import Wizard </h4>
    <p> <img alt="Screenshot of Test Session Container Import Wizard" src="../../images/ecl-img/ImportTSCScreenshot.png" /> </p>
    <p> This import wizard imports a test session container from a file into an Eclipse
      project. The process is pretty simple. In the <em>File</em> field, type or browse
      to select the file which contains the test session container to import. Then select
      the project to import into from the list beneath the <em>File</em> field and click
      on <em>Finish</em>. </p>

    <h3><a name="ExportWizards" id="ExportWizards"></a>Export Wizards</h3>
    <h4><a name="CoverageResultExportWizard" id="CoverageResultExportWizard"></a>Coverage Result Export Wizard</h4>
    <p>This export wizard can be used to export the coverage results as either a test session container, or a report.</p>
    <p><img alt="" src="../../images/ecl-img/report_export.png" width="544" height="532" /></p>
    <p>The test session container holding the coverage results can be selected in the combo box. The list of test sessions can be chosen in the list of available test sessions. The type combo box is used to select the kind of export and the destination can be specified with the &quot;Browse&quot; button. If the export type is &quot;CodeCover Test Session Container&quot;, this page can be closed with the &quot;Finish&quot; button. If it is &quot;Report&quot;, the &quot;Finish&quot; button is disabled and the &quot;Next&quot; button can be used to navigate to the next page.</p>
    <p><img alt="" src="../../images/ecl-img/report_template_wizard.png" width="613" height="550" /></p>
    <p>The template that is used in the creation of the report can be specified here. The shipped templates can be found in the <code>eclipse/plugins/org.codecover.report.*/report-templates/</code> directories. The &quot;Finish&quot; button initiates the creation of the report.</p>
    <h3><a name="JUnit" id="JUnit"></a>CodeCover Measurement for JUnit</h3>
    <p>As you know, you can use <em>Test Cases</em> to subdivide the whole coverage measurement of a coverage run. But how to do that in Eclipse? A mechanism is to use the <a href="#LiveNotification">Live Notification</a>, described in the next paragraph. But for the most projects, you have already created a JUnit test suite &ndash; why not use this? You can use this test suite to test your software and in the meanwhile, CodeCover uses the test case information to subdivide the coverage measurement.</p>
    <p>As a precondition, you have to activate CodeCover for the project you want to use CodeCover with. This is explained in the section <a href="#Build">Build</a>.</p>
    <p>Then you have to create a <em>run</em> for your test suite&mdash;respectively test case. Therefore, open your JUnit <code>TestSuite</code> or <code>TestCase</code> class, use the run button and select <em>Run As&nbsp;&nbsp;<img alt="" src="../../images/ecl-img/coverage_bar.gif" /> CodeCover Measurement for JUnit</em>. This run entry is also available in the context menu <em>Run As</em> of the editor or <em>Package Explorer</em>.</p>
    <p>This run will automatically start the Eclipse JUnit test runner and when it is finished, the coverage results appear in the <a href="#TestSessionsView">Test Sessions View</a>. Per default this session has the name &quot;eclipse run&quot;. You can change this name by editing its <em>Properties</em>. If you need to, you can inspect the test cases' properties. If there occurred any errors or failures during the run, it is logged to the comment of a test case.</p>
    <p>For more advanced features, you can use the run dialog to adapt the run configuration. For example, you can chose between JUnit 3 and JUnit 4:</p>
    <a href="../../images/ecl-img/JUnit_Run_Dialog.png"><img alt="" src="../../images/ecl-img/JUnit_Run_Dialog_small.png" width="700" height="463" /></a>
    <h3> <a name="LiveNotification" id="LiveNotification"></a>Live Notification </h3>
    <h4><img alt="" src="../../images/ecl-img/LiveNotificationScreenshot.png" width="236" height="424" />&nbsp;</h4>
    <h4>Connecting to the running program:</h4>
    <p>Live Notification is a mechanism to interact with the coverage logging inside the
      instrumented SUT. Its main purpose is to manually define test case boundaries
      from within eclipse and send them to the SUT.</p>
    <p> In the following examples replace 1234 with the port to use for Live Notification.
      If your SUT already uses JMX, make sure that neither SSL nor password authentication
      is required and use the port you defined. </p>
    <h4> Enabling Live Notification in the SUT: </h4>
    <p> If your Application does not support JMX you have to enable it.</p>
    <p> To enable JMX: Open the Run Dialog. Select your launch configuration and open its
      Arguments tab. In the text field VM Arguments add the String &quot;-Dcom.sun.management.jmxremote
      -Dcom.sun.management.jmxremote.port=1234 -Dcom.sun.management.jmxremote.ssl=false
      -Dcom.sun.management.jmxremote.authenticate=false&quot; (without the quotes). Now apply
      the changes to finish the configuration.</p>
    <p> If you start this launch configuration the SUT listens on port 1234 for JMX clients. </p>
    <h4> Using the Live Notification view: </h4>
    <p> First open the Live Notification view that comes with CodeCover. Then connect to
      a running SUT: Enter the hostname of the host the SUT runs on and the port to connect
      to. Then click on the connect button.</p>
    <p> Now you may use the Live Notification features (see below). </p>
    <h4> Defining test case boundaries: </h4>
    <p> To start a new testcase first enter its name into the text field then click the
      start button. To end the test case click the end button.</p>
    <p> If you don't start a testcase, the whole program run will be recorded as one testcase
      with the name &quot;UNNAMED&quot;. </p>
    <h4> Finish testing and downloading coverage log file: </h4>
    <p> To finish the test, click the button &quot;Finish Test Session&quot; and then the button &quot;Download
      Coverage Log File&quot; to get the coverage data of the finished test session.</p>
    <h3><a name="Preferences" id="Preferences"></a>Preferences</h3>
    <p><img src="../../images/ecl-img/Preferences_Correlation_Matrix_Screenshot.png" alt="Correlation Matrix Preferences" width="656" height="554" /></p>
    <p>In the preferences dialog, you can specify in the CodeCover-Tab the colors for the
      hotpath, the ranges and colors for the correlation matrix and the path for further CodeCover-Plugins.</p>
    <p><img src="../../images/ecl-img/Preferences_Hot_Path_Screenshot.png" alt="Hot Path Preferences" width="656" height="554" /></p>

m4_web_create_footer
