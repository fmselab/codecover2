<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: how_to_complete.html.m4 28 2008-05-28 20:31:17Z ahija $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`HOWTO Use CodeCover: From code to report, a complete walkthrough')

    <h2>HOWTO Use CodeCover: From code to report, a complete walkthrough</h2>
    <a name="Intro" id="Intro"></a>
    <div class="commandDiv">
      <h3>Introduction &amp; General Preconditions</h3>
      <p>This document describes the usage of the CodeCover Eclipse plugin, using the sample Java application &quot;SimpleJavaApp&quot;. This application is the &quot;system under test&quot; (SUT). In this document we describe how CodeCover instruments and executes the SUT, and how the test results can be visualized and reported. We will <em>not</em> discuss the installation of the Eclipse plugin, which is covered in the <a href="../install.html">installation guide.</a></p>
      The prerequisites for using this tutorial are:
      <ul>
        <li>Basic knowledge of Eclipse usage, such as creating a run configuration and opening views.</li>
        <li>The CodeCover Eclipse plugin must be installed.</li>
      </ul>
    </div>
    <a name="SimpleJavaApp" id="SimpleJavaApp"></a>
    <div class="commandDiv">
      <h3>SimpleJavaApp</h3>
      <p>&quot;SimpleJavaApp&quot; is a small swing application. It is used to display and edit lists of books. It can save and load these lists as XML files. Below is a screenshot of the application.</p>
      <p> Open your Eclipse installation and import &quot;SimpleJavaApp&quot; into your workspace. Navigate to the &quot;Import&quot; entry in the &quot;File&quot; menu in Eclipse and choose &quot;Existing Projects into Workspace&quot; under the &quot;General&quot; category. Browse to the folder you placed the sources in. You can get these sources here, as <a href="SimpleJavaApp.zip">.zip</a> or <a href="SimpleJavaApp.tar.bz2">.tar.bz2</a>.</p>
      <p><img src="../../images/howto_images/sja_screen.png" alt="" width="597" height="193" /></p>
    </div>
    <a name="EnableCC" id="EnableCC"></a>
    <div class="commandDiv">
      <h3>Enable CodeCover</h3>
      <p>You need to enable CodeCover for the imported project to  measure its coverage. Open the project properties dialog of the project and navigate to the CodeCover category. Select the checkbox as shown in the screenshot below. You have now activated CodeCover for the &quot;SimpleJavaApp&quot; project. You also need to select the coverage criteria to be used in the instrumentation. In this case all available criteria were selected.</p>
      <p><img alt="" src="../../images/howto_images/project_properties_enable_CC.png" width="508" height="397" /></p>
    </div>
    <a name="UseForCoverageMeasurement" id="UseForCoverageMeasurement"></a>
    <div class="commandDiv">
      <h3>Use for Coverage Measurement</h3>
      <p>Select the classes you want to instrument. Open the package explorer view, navigate to the source folder of the &quot;SimpleJavaApp&quot; project and select those classes you want to be instrumented. Open the context menu of this selection and select the &quot;Use For Coverage Measurement&quot; entry. This will mark the selected items &ndash; as shown in the screenshot below &ndash; with an icon. In this tutorial, all classes of the &quot;SimpleJavaApp&quot; are selected for instrumentation.</p>
      <p><img alt="" src="../../images/howto_images/choose_instrumentable_items.png" width="378" height="353" /></p>
    </div>
    <a name="RunWithCC" id="RunWithCC"></a>
    <div class="commandDiv">
      <h3>Run with CodeCover</h3>
      <p>You need to tell Eclipse to use CodeCover. This is done in the &quot;run configuration&quot; dialog. You can reuse existing configurations or create new ones. Navigate to the CodeCover tab in the configuration and select the &quot;Run with CodeCover&quot; checkbox.</p>
      <p><a href="../../images/howto_images/run_dialog_run_with_CC.png"> <img src="../../images/howto_images/run_dialog_run_with_CC_small.png" alt="Run with CodeCover" width="700" height="358" /></a> </p>
    </div>
    <a name="Execution" id="Execution"></a>
    <div class="commandDiv">
      <h3>Execution</h3>
      <p>There are several ways to execute the SUT. You can choose to start the application as usually, in which case all the measured data is collected into a single test case. Or you can use the &quot;Live Notification&quot; feature and record individual test cases. Or you can use your existing JUnit test suite to define the test cases for you. To perform a normal execution just read on, to proceed with the Live Notification, skip ahead to <a href="#LiveNotificationExecution">this section</a>, to proceed with the JUnit execution, skip ahead <a href="#JUnit">here</a>.</p>
      <h4><a name="NormalExecution" id="NormalExecution"></a>Normal Execution</h4>
      <p>You simply need to run the previously created run configuration. CodeCover measures the SUT in the background. Perform your test activities. After terminating the SUT, the measurements are automatically stored as a test case in a test session called &quot;eclipserun&quot;. You can <a href="#ViewEclipse">view the measurements</a> with the views in Eclipse, or <a href="#ExportReport">create an HTML report</a> from the measured data.</p>
      <h4><a name="LiveNotificationExecution" id="LiveNotificationExecution"></a>Live Notification execution</h4>
      <p>One additional step needs to be taken to use live notification. Open the arguments tab of your run configuration and enter the following parameters into the VM arguments:</p>
      <pre><code></code>-Dcom.sun.management.jmxremote 
-Dcom.sun.management.jmxremote.port=1234 
-Dcom.sun.management.jmxremote.ssl=false 
-Dcom.sun.management.jmxremote.authenticate=false
</pre>
      <p>NOTE: You need to remove any line breaks in the VM arguments text field, since they can lead to malfunctions.</p>
      <p>Start the SUT with the run configuration. Open the &quot;Live Notification&quot; view in Eclipse. Enter &quot;localhost&quot; as the hostname and &quot;1234&quot; as the port. Press the &quot;Connect&quot; button. Live notification is now activated.</p>
      <p>Enter a name for a test case and start it with the &quot;Start Test Case&quot; button. Perform your test activities, e.g. delete a book. End the test case with the &quot;End Test Case&quot; button. You can record more test cases by repeating the above. To finish recording, use the &quot;Finish Test Session&quot; button. You do not need to use the &quot;Download Coverage Log File&quot; button, since SimpleJavaApp is not a web application (see <a href="../../support/faq.html#LiveNotificationDuplicate">FAQ</a>). After terminating the SUT, the measurements are automatically stored in a test session called &quot;eclipserun&quot;.</p>
      <p>You can <a href="#ViewEclipse">view the measurements</a> with the views in Eclipse, or <a href="#ExportReport">create an HTML report</a> from the measured data.</p>
      <p><img alt="" src="../../images/howto_images/live_notification_view.png" width="261" height="439" /></p>
        <h4><a name="JUnit" id="JUnit"></a>JUnit execution</h4>
        <p>To use your existing test suite you need to create a new &quot;CodeCover Measurement For JUnit&quot; run configuration. As show in the screenshot below it is required to select the class which contains your JUnit test cases or test suite. You can choose to use either JUnit 3 or JUnit 4 as your test runner, depending on which your existing test suite is based on. Start the SUT with the run configuration to start the measurement. After terminating, the measurements are automatically stored in a test session called &quot;eclipserun&quot;, which holds all the test cases your test suite defined.</p>
    <p><a href="../../images/ecl-img/JUnit_Run_Dialog.png"><img alt="" src="../../images/ecl-img/JUnit_Run_Dialog_small.png" width="700" height="463" /></a>    </p>
    <p>You can <a href="#ViewEclipse">view the measurements</a> with the views in Eclipse, or <a href="#ExportReport">create an HTML report</a> from the measured data.</p>
    </div>
    <a name="ViewEclipse" id="ViewEclipse"></a>
    <div class="commandDiv">
      <h3>View measured data in Eclipse</h3>
      <p>This section describes the basic uses of each view CodeCover provides. You can skip ahead to the <a href="#ExportReport">report section</a> to immediately create a report.</p>
      <h4><a name="TestSessionView" id="TestSessionView"></a>Test Sessions View</h4>
      <p>This view displays the test sessions and test cases in a test session container. You can select, deselect, rename, delete and merge them. Other views use only the selected test cases for their visualization.</p>
      <p><img alt="" src="../../images/howto_images/test_session_view.png" width="553" height="282" /></p>
      <h4><a name="CoverageView" id="CoverageView"></a>Coverage View</h4>
      <p>In this view you can see the coverage of individual parts of the SUT. As shown in the screenshot below, every metric has its own column.</p>
      <p><img alt="" src="../../images/howto_images/coverage_view.png" width="687" height="594" /></p>
      <h4><a name="CorrelationView" id="CorrelationView"></a>Correlation View</h4>
      <p>This view is used to compare test cases with each other. You can hover over each block in the matrix and see by how much one test case covers the same parts of the code as another test case. The tree displays test cases, that completely cover the pars of the code that other test cases cover, in a hierarchy. You can choose to export the data of the matrix into a &quot;comma separated value&quot; file which is compatible with most spreadsheet applications.</p>
      <p><a href="../../images/howto_images/correlation_view.png"> <img src="../../images/howto_images/correlation_view_small.png" alt="Correlation View" width="700" height="372" /></a></p>
      <h4><a name="BooleanAnalyzer" id="BooleanAnalyzer"></a>Boolean Analyzer</h4>
      <p>With this view, you can display the measured assignments of conditions and see the coverage of the condition. You can select the keyword of the condition in the source code, right-click, and select the &quot;Analyze Term&quot; item in the context menu, which will automatically select the condition in the Boolean Analyzer.</p>
      <p><img alt="" src="../../images/howto_images/boolean_analyzer.png" width="588" height="227" /></p>
      <h4><a name="HighlightingHotPath" id="HighlightingHotPath"></a>Code Highlighting &amp; Hot Path</h4>
      <p>The source code is highlighted according to the measured data. This is only available if the source file hasn't changed since the measurement was done. As you can see in the screenshot below, the <code>onQuit(...)</code> and <code>onSaveAs(...)</code> methods were not executed during the measurement of the SUT.</p>
      <p>The hot path of the source file is shown on the left side of the editor. The more often a statement was executed, the more red the color becomes. The <code>onNewBook(...)</code> method was executed more often than other methods, so the color is red.</p>
      <p><a href="../../images/howto_images/hightlighting_hot_path.png"><img alt="" src="../../images/howto_images/hightlighting_hot_path_small.png" width="700" height="533" /></a></p>
      <h4><a name="PickTestCaseView" id="PickTestCaseView"></a>Pick Test Case View</h4>
      <p>This view shows which test cases cover a certain part of the code, e.g. which test cases executed the <code>onNewWindow()</code> method. It tracks the selection in the currently opened editor and displays a (possibly empty) list of test cases. The test session of the test case is also displayed.</p>
      <p><img src="../../images/howto_images/PickTestCaseViewScreenshot.png" alt="Pick Test Case View" width="393" height="199" /></p>
    </div>
    <a name="ExportReport" id="ExportReport"></a>
    <div class="commandDiv">
      <h3>Export Report</h3>
      <p>You can export the measured data as an HTML report. Navigate to the &quot;Export&quot; entry in the &quot;File&quot; menu in Eclipse and choose &quot;Coverage Result Export&quot; under the &quot;CodeCover&quot; category. Select the test session container which contains the test sessions you wish to export. Set the export type to &quot;Report&quot;. You  need to choose a destination for the report files. The screenshot displays a possible configuration.</p>
      <p><img alt="" src="../../images/howto_images/report_export.png" width="544" height="532" /></p>
      <p>Press the &quot;Next&quot; button to choose the template for the report.</p>
      <p><img alt="" src="../../images/howto_images/report_template_wizard.png" width="613" height="550" /></p>
      <p>The report will be created at the specified location when you click the &quot;Finish&quot; button.</p>
    </div>
    <a name="ExampleReport" id="ExampleReport"></a>
    <div class="commandDiv">
      <h3>Example Report</h3>
      <p>The created report is available <a href="SimpleJavaAppReport.html">here</a>.</p>
    </div>

m4_web_create_footer
