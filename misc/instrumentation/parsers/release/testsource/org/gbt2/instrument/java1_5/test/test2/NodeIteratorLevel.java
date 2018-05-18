package org.gbt2.instrument.java1_5.test.test2;

import java.util.Iterator;
import java.util.concurrent.ConcurrentLinkedQueue;

/**
 * Ein Iterator zum <b>levelweisen</b> Durchlaufen des Baumes.<br>
 * <br>
 * 
 * @author cm
 * @version 1.0 26.05.2006 erstellt<br>
 * @param <E> Der generische Typ des Iterators.
 */
class NodeIteratorLevel<E extends Comparable<? super E>>
  implements Iterator<E>
{
  private AVLTree<E> m_AVLTree = null;

  private AVLNode<E> m_NodeLastVisited = null;

  private ConcurrentLinkedQueue<AVLNode<E>> m_Queue;

  /**
   * Erstellt einen neuen Iterator, welcher mit dem angegebenen Knoten beginnt.
   * 
   * @param avlTree Der zugehörige Baum.
   * @param node Der Startknoten.
   */
  public NodeIteratorLevel(AVLTree<E> avlTree, AVLNode<E> node)
  {
    this.m_AVLTree = avlTree;
    this.m_Queue = new ConcurrentLinkedQueue<AVLNode<E>>();
    this.m_Queue.add(node);
  }

  /**
   * überprüft, ob noch ein Element nach dem Iterator existiert.
   * 
   * @return true &rarr; ja<br>
   *         false &rarr; nein
   */
  public boolean hasNext()
  {
    return !this.m_Queue.isEmpty();
  }

  /**
   * Gibt das nächste Element zurück.
   * 
   * @return Das kommende Element des Iterators.
   * @see #hasNext() überprüft, ob das Element existiert.
   */
  public E next()
  {
    this.m_NodeLastVisited = this.m_Queue.poll();

    if (this.m_NodeLastVisited != null)
    {
      if (this.m_NodeLastVisited.m_NodeLeft != null)
      {
        this.m_Queue.add(this.m_NodeLastVisited.m_NodeLeft);
      }
      if (this.m_NodeLastVisited.m_NodeRight != null)
      {
        this.m_Queue.add(this.m_NodeLastVisited.m_NodeRight);
      }
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