<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: coverage.html.m4 28 2008-05-28 20:31:17Z ahija $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`Features: Code Coverage')

    <h2>Features: Code Coverage</h2>
    <h3>Benefits</h3>
    <p>CodeCover supports you in several ways to increase your test quality. It shows the quality of your test suite and helps you to develop new test cases and rearrange test cases to save some of them. So you get a higher quality and a better test productivity.</p>
    <h3><a name="Quality_measurement" id="Quality_measurement"></a>Quality measurement</h3>
    <p>CodeCover instruments your code to measure several coverage metrics while it is executed during your test. Coverage metrics show you which parts of the code were executed and which were not. Currently, CodeCover supports four coverage metrics, but you can add more through a convenient plugin mechanism. The shipped metrics are:</p>
    <h4>Statement Coverage</h4>
    <p>To reach full statement coverage, each statement of the code has to be executed. It is obvious that not the whole code was tested as long as there are uncovered statements. This is the weakest of all coverage metrics.</p>
    <h4>Branch Coverage</h4>
    <p>Branch coverage - as the name tells - doesn't look at the statements but at branches. Each statement is part of a branch. New branches begin at each position in the code, where it's not sure which is the next statement (e.g. at an if-statement). To reach 100% branch coverage, you have to execute each statement and every so called empty branch, e.g. the implicit <code>else</code>-branch of an if only consisting of a <code>then</code> part.</p>
    <h4>Condition Coverage</h4>
    <p>Again, this coverage metric contains the two above, is harder to reach and thus helps to discover more bugs as you set up new test cases to reach a higher coverage. Because each branch starts with a decision, it's interesting testing the conditions leading to this decision. To reach full condition coverage the way we defined it, each basic boolean term (= something not containing a boolean operator) must have changed the decision. In Eclipse, we provide a view (the so called boolean analyzer), helping to understand which basic boolean terms of your decisions have already been tested, which values they had and which combinations are missing to reach full condition coverage.</p>
    <h4>Loop Coverage</h4>
    <p>Loop coverage is independent from the other three coverage metrics. Loops are the constructs where many of the off-by-one errors happen. To reach full loop coverage, you have to execute each loop zero (if possible), one and more than one times. Code cover names the missing ones, so you can develop new test cases increasing the coverage.</p>
  <h3>Test productivity</h3>
  <h4>Testcase correlation</h4>
  <p>Often, you have several test cases, but are not sure how different they really are. CodeCover shows you in a correlation matrix how similar your test cases are. This helps you finding unnecessary test cases. You may drop them saving the time of their execution (which can be quite a lot if it's a manual test case) without loosing code coverage or refine them taking another path through your code.</p>
  <h4>Testcase development</h4>
  <p>Developing further testcases for a test suite is often a difficult, time-killing task. How should you know which test data you need to search for the bugs in places nobody has looked before? The simple answer: with CodeCover! After you have executed you testsuite, you can see which classes, methods, packages,... miss coverage. Unexecuted parts of the code are colored red, so the only thing to do is: find the branches leading to that code. The mentioned boolean analyzer shows you, which values of the boolean terms were tested and which are missing. That are the ones for your new testcases.</p>

m4_web_create_footer
