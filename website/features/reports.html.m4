<?xml version="1.0" encoding="UTF-8" ?>
<!-- $Id: reports.html.m4 28 2008-05-28 20:31:17Z ahija $ -->
m4_include(`website.inc.m4')

m4_web_create_page_header(`Features: Reports')

    <h2>Features: Reports</h2>
    <p><a href="../documentation/tutorials/SimpleJavaAppReport.html">Sample Report</a> (also shipped: HTML report in one single file and CSV export)</p>
    <h3>Customizable</h3>
    <p>CodeCover summarizes and outputs its data the way you want. You can adapt the report template to create text-based output looking as you wish, e.g. make a html-report the way you need it, add your companies stylesheets, highlight results of your interest. Or you write your own report generator for every format you can imagine.</p>
    <h3>On-hand</h3>
    <p>CodeCover saves every data of interest. You can create reports of past test sessions even containing the sourcecode it's belonging to. So you can still see where the uncovered parts were.</p>
    <h3>Flexible</h3>
    <p>CodeCover outputs CSV and HTML files. But that's not enough: If you know Java, it's quite easy to implement a new report generator, so you get the (binary?) file format you need. Or just write to a database, or send the results via mail, ... you have the power.</p>

m4_web_create_footer
