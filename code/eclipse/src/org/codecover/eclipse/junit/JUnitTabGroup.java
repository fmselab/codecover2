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
package org.codecover.eclipse.junit;

import org.codecover.eclipse.builder.CodeCoverClasspathProvider;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;

/**
 * A TabGroup that is shown in the LaunchManager for the
 * LaunchConfigurationType <b>CodeCover Measurement For JUnit</b>.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: JUnitTabGroup.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class JUnitTabGroup
  extends org.eclipse.jdt.internal.junit.launcher.JUnitTabGroup
{
  @Override
  public void setDefaults(
    ILaunchConfigurationWorkingCopy config)
  {
    super.setDefaults(config);
    CodeCoverClasspathProvider.setRunWithCodeCover(config, true);
  }
}
