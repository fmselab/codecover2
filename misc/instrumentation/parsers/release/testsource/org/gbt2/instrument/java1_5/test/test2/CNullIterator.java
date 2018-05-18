package org.gbt2.instrument.java1_5.test.test2;

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
