<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: expandability.html.m4 28 2008-05-28 20:31:17Z ahija $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`Features: Expandability')

    <h2>Features: Expandability</h2>
    <p>CodeCover is highly expandable. And because it's OpenSource, everybody benefits from enhancements. To expand CodeCover, you just need to write a plugin. The following three kinds of plugins exist:</p>
    <h3>Instrumenter</h3>
    <p>CodeCover is shipped with an instrumenter for java and cobol, two languages with very little in common. So you see, there is nothing that prevents you from writing a plugin for your favorite programming language.</p>
    <h3>Metric</h3>
    <p>CodeCover is shipped with four coverage metrics. But what if you need another one? Or are interested in a static metric as McCabe? You just expand CodeCover et voil�: you have what you want.</p>
    <h3>Report</h3>
    <p>Bored of these text-based reports? Need PDFs, DOCs, want to write to a database? Write a new report generator and plug it into CodeCover!</p>

m4_web_create_footer
