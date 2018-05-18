/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This file may be used, modifies and redistributed     *
 * under the terms of either the Eclipse Public License v1.0 which            *
 * accompanies this distribution and is available at                          *
 * http://www.eclipse.org/legal/epl-v10.html or the MIT license, available at *
 * http://www.opensource.org/licenses/mit-license.php                         *
 ******************************************************************************/

package org.codecover.model.utils;

import java.util.Comparator;

/**
 * A helper to compare basic int, long, small, byte and short.
 *
 * @author Christoph Müller
 *
 * @version 27.06.2009 - $Id:: IntComparator.java 875 2009-07-10 10:20:49Z cm                                     $:
 *
 * @see Comparator
 */
public class IntComparator
{
  public static int compare(byte first, byte second)
  {
    return first < second ? -1 : (first == second ? 0 : 1);
  }

  public static int compare(short first, short second)
  {
    return first < second ? -1 : (first == second ? 0 : 1);
  }

  public static int compare(char first, char second)
  {
    return first < second ? -1 : (first == second ? 0 : 1);
  }

  public static int compare(int first, int second)
  {
    return first < second ? -1 : (first == second ? 0 : 1);
  }

  public static int compare(long first, long second)
  {
    return first < second ? -1 : (first == second ? 0 : 1);
  }
}
