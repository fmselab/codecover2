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
 * Datei:  BatchLoggerTest.java
 */
package org.codecover.batch;

import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;

import junit.framework.TestCase;

import org.codecover.model.utils.*;

/**
 * @author Christoph Müller
 * @version 1.0 - 18.05.2007
 */
public class BatchLoggerTest extends TestCase
{
  BatchLogger logger;

  protected void setUp()
  {
    this.logger = new BatchLogger();
  }

  protected void tearDown()
  {
    this.logger = null;
  }

  public void test1()
  {
    this.logger.debug("Debug");
    this.logger.info("Info");
    this.logger.warning("Warning");
    this.logger.error("Error");
    try
    {
      this.logger.fatal("Fatal");
      throw new RuntimeException();
    }
    catch (FatalException e)
    {
    }

    Exception e = null;
    try
    {
      throw new Exception();
    }
    catch (Exception exc)
    {
      e = exc;
    }

    this.logger.debug("Debug", e);
    this.logger.info("Info", e);
    this.logger.warning("Warning", e);
    this.logger.error("Error", e);
    try
    {
      this.logger.fatal("Fatal", e);
      throw new RuntimeException();
    }
    catch (FatalException ex)
    {
    }
  }
  
  public void test2()
  {
    this.logger.setLogLevel(LogLevel.DEBUG);

    this.logger.debug("Debug");
    this.logger.info("Info");
    this.logger.warning("Warning");
    this.logger.error("Error");
    try
    {
      this.logger.fatal("Fatal");
      throw new RuntimeException();
    }
    catch (FatalException e)
    {
    }
    
    Exception e = null;
    try
    {
      throw new Exception();
    }
    catch (Exception exc)
    {
      e = exc;
    }

    this.logger.debug("Debug", e);
    this.logger.info("Info", e);
    this.logger.warning("Warning", e);
    this.logger.error("Error", e);
    try
    {
      this.logger.fatal("Fatal", e);
      throw new RuntimeException();
    }
    catch (FatalException ex)
    {
    }

    this.logger.debug("Ökologische Äcker sind übermütig und weiß.");
    this.logger.debug("ÄÖÜäöüß !\"§$%&/()=?`´@€");
  }
}
