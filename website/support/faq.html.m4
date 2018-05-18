<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: faq.html.m4 32 2009-03-09 12:33:36Z schmidberger $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`FAQ')

    <h2>Frequently Asked Questions</h2>
    <ul>
      <li><a href="#HeapSpace">Eclipse terminates unexpectedly or an Out of memory error is shown in Ant/Batch mode</a></li>
      <li><a href="#LiveNotificationDuplicate">Two identical test sessions after using the Live Notification feature</a></li>
      <li><a href="#ReportFailureNoDirectory">Report Generation: &quot;[ERROR] Index file could not be created&quot; using CLI.</a></li>
    </ul>
    <a name="HeapSpace" id="HeapSpace"></a>
    <h3>Eclipse terminates unexpectedly or an Out of memory error is shown in Ant/Batch mode</h3>
    <p>This usually happens when the Java VM heap is full but additional memory still needs to be allocated. You need to increase the maximum heap size of the Java VM (e.g. launch your Java VM with the commandline option to set the maximum heap size to 512 MB: -Xmx512m). To change the maximum heap size when using Eclipse, open the file eclipse.ini in your Eclipse installation directory and look for an entry which starts with -Xmx... and increase it's value to e.g. 512MB by changing it to -Xmx512m. </p>
    
    <a name="LiveNotificationDuplicate" id="LiveNotificationDuplicate"></a>
    <h3>Two identical test sessions after using the Live Notification feature</h3>
    <p>This most likely occurred after using the Live Notification feature on your local machine and pressing the &quot;Download Coverage Log File&quot; button. The download of the generated coverage log file is only necessary when the system you are testing with CodeCover is executed on another machine, e.g. in the case of a web application. If the tested system is executed on your local machine, it is not necessary to download the coverage log file, since it's location is known and entered into the test-session container automatically.</p>
        <a name="ReportFailureNoDirectory" id="ReportFailureNoDirectory"></a>
    <h3>Report Generation: &quot;[ERROR] Index file could not be created&quot; using CLI.</h3>
    <p>This usually happens when some of the folders given in the destination parameter of the report command do not exist. This failure behavior is intended, since the user could have made a typo.</p>

m4_web_create_footer
