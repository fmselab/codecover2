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

package org.codecover.instrumentation.java15.test.test2;

import java.util.Iterator;

/**
 * Ein Iterator, der keine Elemente enthält.
 * 
 * @author cm
 * @version 1.0 17.06.2006 erstellt<br>
 * @param <E> Der generische Typ des Iterators.
 */
public class CNullIterator<E>
  implements Iterator<E>
{

  public boolean hasNext()
  {
    return false;
  }

  public E next()
  {
    return null;
  }

  public void remove()
  {
    // macht nix
  }
}
