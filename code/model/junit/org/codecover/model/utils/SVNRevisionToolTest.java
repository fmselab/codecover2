/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

/*
 * Packet: org.codecover.model.utils
 * Datei:  SVNRevisionToolTest.java
 */
package org.codecover.model.utils;

import java.io.File;
import java.io.IOException;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.model.utils.file.FileTool;
import org.tmatesoft.svn.core.SVNException;

/**
 * @author Christoph Müller
 * @version 1.0 - 14.06.2007
 *
 */
public class SVNRevisionToolTest
  extends TestCase
{
  /**
   * Test method for {@link org.codecover.model.utils.SVNRevisionTool#main(java.lang.String[])}.
   */
  public void testMain1() throws Exception
  {
    try
    {
      SVNRevisionTool.main(new String[]{});
      Assert.fail();
    }
    catch (IllegalArgumentException e)
    {
      // expected
    }
  }

  /**
   * Test method for {@link org.codecover.model.utils.SVNRevisionTool#main(java.lang.String[])}.
   */
  public void testMain2() throws Exception
  {
    try
    {
      SVNRevisionTool.main(new String[]{"hello"});
      Assert.fail();
    }
    catch (IllegalArgumentException e)
    {
      // expected
    }
  }
  
  /**
   * Test method for {@link org.codecover.model.utils.SVNRevisionTool#main(java.lang.String[])}.
   */
  public void testMain3() throws Exception
  {
    try
    {
      SVNRevisionTool.main(new String[]{"hello", "world", "!"});
      Assert.fail();
    }
    catch (IllegalArgumentException e)
    {
      // expected
    }
  }

  /**
   * Test method for {@link org.codecover.model.utils.SVNRevisionTool#main(java.lang.String[])}.
   */
  public void testMain4() throws Exception
  {
      // TODO: is caught now
      /*
    try
    {
      SVNRevisionTool.main(new String[]{"hello", "world"});
      Assert.fail();
    }
    catch (SVNException e)
    {
      // expected
    }
      */
  }

  /**
   * Test method for {@link org.codecover.model.utils.SVNRevisionTool#main(java.lang.String[])}.
   */
  public void testMain6() throws Exception
  {
    try
    {
      String root = new File(".." + File.separatorChar).getAbsolutePath();
      String codeCoverInfo = new File("testsource" + File.separatorChar + "CodeCoverInfo.java").getAbsolutePath();
      new File(codeCoverInfo).getAbsoluteFile().delete();
      SVNRevisionTool.main(new String[]{root, codeCoverInfo});
      Assert.fail();
    }
    catch (IOException e)
    {
      // expected
    }
  }

  /**
   * Test method for {@link org.codecover.model.utils.SVNRevisionTool#main(java.lang.String[])}.
   */
  public void testMain() throws Exception
  {
    String root = new File(".." + File.separatorChar).getAbsolutePath();
    String codeCoverInfo = new File("testsource" + File.separatorChar + "CodeCoverInfo.java").getAbsolutePath();
    String baseDir = System.getProperty("base.dir");
    if (baseDir == null) {
      baseDir = (new File("")).getAbsolutePath() + "/";
    } else {
      baseDir += "/";
    }
    FileTool.copy(new File(baseDir + "src/org/codecover/CodeCoverInfo.java"), new File(codeCoverInfo));
    SVNRevisionTool.main(new String[]{root, codeCoverInfo});
  }
}
