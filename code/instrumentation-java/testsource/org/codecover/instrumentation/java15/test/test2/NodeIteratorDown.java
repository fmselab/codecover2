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
 * Ein Iterator zum <b>aufsteigenden</b> Durchlaufen des Baumes.<br>
 * <br>
 * 
 * @author cm
 * @version 1.0 26.05.2006 erstellt<br>
 * @param <E> Der generische Typ des Iterators.
 */
class NodeIteratorDown<E extends Comparable<? super E>>
  implements Iterator<E>
{
  private AVLTree<E> m_AVLTree = null;

  private AVLNode<E> m_NodeLastVisited = null;

  private AVLNode<E> m_NodeNext = null;

  private E m_ElementLeftBoundary = null;

  /**
   * Erstellt einen neuen Iterator, welcher mit dem angegebenen Knoten beginnt.
   * 
   * @param avlTree Der zugehörige Baum.
   * @param node Der Startknoten.
   * @param elementLowerBoundary Das Element für die linke Grenze.
   */
  public NodeIteratorDown(AVLTree<E> avlTree, AVLNode<E> node, E elementLowerBoundary)
  {
    this.m_AVLTree = avlTree;
    this.m_ElementLeftBoundary = elementLowerBoundary;
    if (node != null && node.compareTo(this.m_ElementLeftBoundary) >= 0)
    {
      this.m_NodeNext = node;
    }
  }

  /**
   * überprüft, ob noch ein Element nach dem Iterator existiert.
   * 
   * @return true &rarr; ja<br>
   *         false &rarr; nein
   */
  public boolean hasNext()
  {
    return this.m_NodeNext != null;
  }

  /**
   * Gibt das nächste Element zurück.
   * 
   * @return Das kommende Element des Iterators.
   * @see #hasNext() überprüft, ob das Element existiert.
   */
  public E next()
  {
    this.m_NodeLastVisited = this.m_NodeNext;
    this.m_NodeNext = this.m_AVLTree.getPredecessor(this.m_NodeNext);
    if (this.m_NodeNext != null && this.m_NodeNext.compareTo(this.m_ElementLeftBoundary) < 0)
    {
      this.m_NodeNext = null;
    }
    return this.m_NodeLastVisited.m_Element;
  }

  /**
   * Entfernt den Knoten, auf den der aktuelle Pointer zeigt, aus dem Baum.
   * 
   * @pre
   * 
   * <pre>
   * if (it.hasNext())
   * {
   *   element = it.next();
   *   it.remove();
   * }
   * </pre>
   */
  public void remove()
  {
    if (this.m_NodeLastVisited != null)
    {
      this.m_AVLTree.remove(this.m_NodeLastVisited);
      this.m_NodeLastVisited = null;
    }
  }
}