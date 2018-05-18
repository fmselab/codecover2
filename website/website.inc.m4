m4_define(`m4_web_create_page_header', `
  m4_define(`m4_web_notinindex', m4_ifelse(m4_web_pagename, `index.html', `', `$`'1'))
  
  
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
          "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<!-- $Id: website.inc.m4 2 2007-12-12 19:52:06Z kiess $ -->
<head>
  <title>m4_web_notinindex(`CodeCover | ')$1</title>
  <link rel="stylesheet" href="m4_web_rootdir/css/main.css" type="text/css" />
  $2
</head>
<body>
<div id="page">
  <h1>CodeCover</h1>
  <div id="navigation"> <b class="c"> <b class="c1"><b></b></b> <b class="c2"><b></b></b> <b class="c3"></b> </b>
    <ul>
      m4_web_link(`index.html', `Home', `CodeCover Home', `', `class="first"')
      m4_web_link(`features/index.html', `Features', `CodeCover Features')
      m4_web_link(`documentation/index.html', `Documentation', `CodeCover References and HOWTOs', `
        m4_web_link(`documentation/install.html', `Installation Guide', `Installation Guide')
        m4_web_link(`documentation/references/index.html', `Reference', `CodeCover Reference')
        m4_web_link(`documentation/tutorials/index.html', `Tutorials', `CodeCover HOWTOs')
      ', `id="documentation"')
      m4_web_link(`documentation/install.html', `Download', `Downloads for CodeCover')
      m4_web_link(`support/index.html', `Support', `Support for CodeCover', `
        m4_web_link(`support/faq.html', `FAQ', `Frequently Asked Questions')
        m4_web_link_external(`http://sourceforge.net/forum/?group_id=206367', `Forum', `CodeCover Forum')
      ', `id="support"')
      m4_web_link(`development/index.html', `Development', `CodeCover Development')
      m4_web_link_external(`http://sourceforge.net/projects/codecover/', `SF Project', `SourceForge Project Site')
      m4_web_link(`about/index.html', `Contact', `Contact')
    </ul>
    <div class="clear"></div>
    <b class="c"> <b class="c3"></b> <b class="c2"><b></b></b> <b class="c1"><b></b></b> </b> </div>
  <div class="content">
  
')

m4_define(`m4_web_link', `m4_web_link_external(`m4_web_rootdir/$1', `$2', `$3', `$4', `$5')')

m4_define(`m4_web_link_external', `
  <li m4_ifelse($4, `', `', `class="dropdownmenu"') $5><a title="$3" href="$1">$2</a>
    m4_ifelse($4, `', `', `
      <ul>
        $4
      </ul>
    ')
  </li>
')

m4_define(`m4_web_create_footer', `

  </div>
  <div class="footer">
    <a href="http://sourceforge.net"><img src="http://sflogo.sourceforge.net/sflogo.php?group_id=206367&amp;type=1" width="88" height="31" alt="SourceForge.net Logo" /></a>
    <a href="http://validator.w3.org/check?uri=http://codecover.sourceforge.net/m4_web_pagename">
        <img src="m4_web_rootdir/images/valid-xhtml10.png"
             alt="Valid XHTML 1.0 Strict" height="31" width="88" />
    </a>
    m4_dnl TODO: Do we want this?
    <a href="http://www.anybrowser.org/campaign/">
        <img src="m4_web_rootdir/images/anybrowser.gif" width="102" height="29"
             alt="Viewable With Any Browser"/>
    </a>
  </div>
</div>
</body>
</html>
')
