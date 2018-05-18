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

class AVLNode<E extends Comparable<? super E>>
  implements Comparable<E>
{
  E m_Element;

  AVLNode<E> m_NodeLeft = null;

  AVLNode<E> m_NodeRight = null;

  AVLNode<E> m_NodeParent = null;

  byte m_byBalance = 0;

  AVLNode(E element)
  {
    this.m_Element = element;
  }

  /**
   * Gibt zurück, ob das übergebene Element in der Reihenfolge nach dem Element dieses Knoten kommt. Der Aufruf wird an
   * E#compareTo(E other) delegiert.
   * 
   * @param elementOther Das andere Element.
   * @return -1 => das Element dieses Knotens kommt vor dem anderen Element<br>
   *         0 => beide Elemente sind gleich<br>
   *         1 => das andere Element kommt vor dem Element dieses Knotens
   */
  public int compareTo(
    E elementOther)
  {
    return this.m_Element.compareTo(elementOther);
  }

  /**
   * @return
   * 
   * <pre>
   * return m_Element.toString();
   * </pre>
   */
  @Override
  public String toString()
  {
    return this.m_Element.toString();
  }
}
