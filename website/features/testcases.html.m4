<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: testcases.html.m4 28 2008-05-28 20:31:17Z ahija $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`Features: Testcases')

    <h2>Features: Testcases</h2>
    <p>CodeCover gives you several ways defining testcases so you can see the coverage for each individual testcase helping you to improve it.</p>
    <h3>One testcase per run</h3>
    <p>If you don't do anything, each run of the SUT (system under test) produces one test case. And each test case has a timestamp and an editable name as well as an optional description. So you can differ the runs and relate them to your defined test cases</p>
    <h3>One testcase per JUnit testMethod</h3>
    <p>For java, you surely have a whole bunch of JUnit testcases. CodeCover automatically creates a new testcase for each JUnit testMethod using the name of that method. So you can compare the coverage of the testMethods finding missing or unnecessary tests.</p>
    <h3>By comments in the code</h3>
    <p>The most flexible way creating test cases may be putting comments at the places where a testcase with a given name should start or end.</p>
    <h3>Live while executing the SUT</h3>
    <p>While you execute the SUT, you can start or stop testcases with only one click. CodeCover gives you a remote control with a start and stop button, that in fact not only works with code executed on your machine, but also (the name tells it) remotely. So you can execute the SUT on another machine, a server, e.g., but control the test and create the report on your desktop PC.</p>

m4_web_create_footer
