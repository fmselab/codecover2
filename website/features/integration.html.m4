<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: integration.html.m4 28 2008-05-28 20:31:17Z ahija $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`Features: Integration')

    <h2>Features: Integration</h2>
    <p>However you work, we are sure there is a way to integrate CodeCover in your development process.</p>
    <h3>Batch</h3>
    <p>Codecover can be used via a simple, well documented batch interface. This enables you to use it nearly everywhere and integrate it into your build scripts. Each step, from instrumentation over testcase administration to the report can be made via that batch interface.</p>
    <h3>Ant</h3>
    <p>To integrate CodeCover into your ant scripts is not only easy but powerful. And a detailed HowTo helps you integrating.</p>
    <h3>Eclipse</h3>
    <p>Most powerful is CodeCover if you use it in Eclipse, mainly because all the interesting views and the convenient star/stop testcase buttons can't be handled in a console. Some of these views are: the testcase correlation matrix showing you how similar pairwise two of your testcases are, the boolean analyzer helping you to understand condition coverage and the coverage view with the ability to filter the classes/methods with a coverage smaller (or bigger) than a given value.</p>

m4_web_create_footer
