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

import java.io.PrintStream;
import java.lang.reflect.Array;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.SortedSet;

/**
 * Implementiert einen AVLBaum. Alle Elemente werden geordnert und eine Laufzeit in O(log(n)) ist garantiert
 * für add, remove und contains.<br>
 * Der AVLBaum speichert Elemente, welche das Compareable-Interface implementieren. Es muss gelten:<br>
 * <code>(e1.compareTo(e2) == 0) == e1.equals(e2)</code><br>
 * Das bedeutet, dass zwei Elemente, welche als compareTo 0 ergeben als gleich angesehen werden und nicht
 * doppelt gespeichert werden.
 * 
 * @author cm
 * @version 1.0 - 10.05.2006
 * @param <E> Der generische Typ des Baumes.
 */
public class AVLTree<E extends Comparable<? super E>>
  implements SortedSet<E>, java.io.Serializable
{
  private static final long serialVersionUID = 1L;

  private final Class<E> m_GenericClass;

  private AVLNode<E> m_RootNode = null;

  private AVLNode<E> m_FirstNode = null;

  private AVLNode<E> m_LastNode = null;

  private int mi_Size = 0;

  private long ml_OperationCount;

  private boolean m_bUseSuccesor = true;

  private int m_iChecksum;

  private AVLNode<E> m_CheckSmallest;

  private AVLNode<E> m_CheckLargest;

  /**
   * Erzeugt einen neuen leeren AVL-Baum.
   * 
   * @param GenericClass Die Klasse der Elemente, welche eingefügt werden sollen. Wird zur Typüberprüfung
   *        benötigt.
   */
  public AVLTree(Class<E> GenericClass)
  {
    this.m_GenericClass = GenericClass;
  }

  /**
   * Gibt die Anzahl der Elmente des Baumes zurück.
   * 
   * @return Die Anzahl der Elemente.
   */
  public synchronized int size()
  {
    return this.mi_Size;
  }

  /**
   * Erzeugt einen neuen Iterator zum aufsteigenden Durchlaufen des Baumes.<br>
   * <br>
   * Der Iterator zeigt zu Beginn auf das erste Element. Es wird der ganze Baum bis zum letzten Knoten
   * durchlaufen.
   * 
   * @return Ein neuer Iterator
   * @see Iterator
   * @see #iteratorUpAll() wird verwendet
   */
  public synchronized Iterator<E> iterator()
  {
    return iteratorUpAll();
  }

  /**
   * Erzeugt einen neuen Iterator zum aufsteigenden Durchlaufen des Baumes.<br>
   * <br>
   * Der Iterator zeigt zu Beginn auf das erste Element. Es wird der ganze Baum bis zum letzten Knoten
   * durchlaufen.
   * 
   * @return Ein neuer Iterator
   * @see Iterator
   */
  public synchronized Iterator<E> iteratorUpAll()
  {
    if (isEmpty())
      return new CNullIterator<E>();

    return new NodeIteratorUp<E>(this, this.m_FirstNode, this.m_LastNode.m_Element);
  }

  /**
   * Erzeugt einen neuen Iterator zum aufsteigenden Durchlaufen des Baumes.<br>
   * <br>
   * Der Iterator zeigt zu Beginn auf das übergebene Element, falls es enthalten ist. Falls nicht, dann zeigt
   * der Iterator auf das nächstgrößere Element. Es wird der ganze Baum bis zum letzten Knoten durchlaufen.
   * 
   * @param elementStart
   * @return Ein neuer Iterator
   * @see Iterator
   */
  public synchronized Iterator<E> iteratorUpLarger(
    E elementStart)
  {
    if (isEmpty())
      return new CNullIterator<E>();

    return iteratorUpBetween(elementStart, this.m_LastNode.m_Element);
  }

  /**
   * Erzeugt einen neuen Iterator zum aufsteigenden Durchlaufen des Baumes.<br>
   * <br>
   * Der Iterator zeigt zu Beginn auf das erste Element des Baumes. Es wird der ganze Baum bis einschließlich
   * zum übergebenen Element durchlaufen.
   * 
   * @param elementEnd
   * @return Ein neuer Iterator
   * @see Iterator
   */
  public synchronized Iterator<E> iteratorUpSmaller(
    E elementEnd)
  {
    if (isEmpty())
      return new CNullIterator<E>();

    return iteratorUpBetween(this.m_FirstNode.m_Element, elementEnd);
  }

  /**
   * Erzeugt einen neuen Iterator zum aufsteigenden Durchlaufen des Baumes.<br>
   * <br>
   * Der Iterator zeigt zu Beginn auf das übergebene Element, falls es enthalten ist. Falls nicht, dann zeigt
   * der Iterator auf das nächstgrößere Element. Es wird der ganze Baum solange durchlaufen, wie die Elemente
   * des Iterators <= der oberen Schranke sind.
   * 
   * @param elementStart
   * @param elementUpperBoundary Das Element für die obere Schranke.
   * @return Ein neuer Iterator
   * @see Iterator
   */
  public synchronized Iterator<E> iteratorUpBetween(
    E elementStart,
    E elementUpperBoundary)
  {
    if (isEmpty())
      return new CNullIterator<E>();

    AVLSearchResult searchResult = findNode(elementStart);
    AVLNode<E> nodePointer = null;
    if (searchResult.m_NodePointer != null)
    {
      nodePointer = searchResult.m_NodePointer;
    }
    else if (searchResult.m_NodeParent != null)
    {
      if (searchResult.mb_PointsLeft)
      {
        nodePointer = searchResult.m_NodeParent;
      }
      else
      {
        nodePointer = getSuccessor(searchResult.m_NodeParent);
      }
    }

    return new NodeIteratorUp<E>(this, nodePointer, elementUpperBoundary);
  }

  /**
   * Erzeugt einen neuen Iterator zum absteigenden Durchlaufen des Baumes.<br>
   * <br>
   * Der Iterator zeigt zu Beginn auf das letzte Element. Es wird der ganze Baum bis zum ersten Knoten
   * durchlaufen.
   * 
   * @return Ein neuer Iterator
   * @see Iterator
   */
  public synchronized Iterator<E> iteratorDownAll()
  {
    if (isEmpty())
      return new CNullIterator<E>();

    return new NodeIteratorDown<E>(this, this.m_LastNode, this.m_FirstNode.m_Element);
  }

  /**
   * Erzeugt einen neuen Iterator zum absteigenden Durchlaufen des Baumes.<br>
   * <br>
   * Der Iterator zeigt zu Beginn auf das übergebene Element, falls es enthalten ist. Falls nicht, dann zeigt
   * der Iterator auf das nächstkleinere Element. Es wird der ganze Baum bis zum ersten Knoten durchlaufen.
   * 
   * @param elementStart
   * @return Ein neuer Iterator
   * @see Iterator
   */
  public synchronized Iterator<E> iteratorDown(
    E elementStart)
  {
    if (isEmpty())
      return new CNullIterator<E>();

    return iteratorDown(elementStart, this.m_FirstNode.m_Element);
  }

  /**
   * Erzeugt einen neuen Iterator zum absteigenden Durchlaufen des Baumes.<br>
   * <br>
   * Der Iterator zeigt zu Beginn auf das übergebene Element, falls es enthalten ist. Falls nicht, dann zeigt
   * der Iterator auf das nächstkleinere Element. Es wird der ganze Baum solange durchlaufen, wie die Elemente
   * des Iterators >= der unteren Schranke sind.
   * 
   * @param elementStart
   * @param elementLowerBoundary Das Element für die untere Schranke.
   * @return Ein neuer Iterator
   * @see Iterator
   */
  public synchronized Iterator<E> iteratorDown(
    E elementStart,
    E elementLowerBoundary)
  {
    if (isEmpty())
      return new CNullIterator<E>();

    AVLSearchResult searchResult = findNode(elementStart);
    AVLNode<E> nodePointer = null;
    if (searchResult.m_NodePointer != null)
    {
      nodePointer = searchResult.m_NodePointer;
    }
    else if (searchResult.m_NodeParent != null)
    {
      if (!searchResult.mb_PointsLeft)
      {
        nodePointer = searchResult.m_NodeParent;
      }
      else
      {
        nodePointer = getPredecessor(searchResult.m_NodeParent);
      }
    }

    return new NodeIteratorDown<E>(this, nodePointer, elementLowerBoundary);
  }

  /**
   * Erzeugt einen neuen Iterator zum levelweisen Durchlaufen des Baumes.
   * 
   * @return Den oben beschriebenen Iterator
   * @see Iterator
   */
  public synchronized Iterator<E> iteratorLevel()
  {
    if (isEmpty())
      return new CNullIterator<E>();

    return new NodeIteratorLevel<E>(this, this.m_RootNode);
  }

  /**
   * Gibt einen Comparator des Typs des Baumes zurück.<br>
   * <br>
   * Der Comparator benutzt die compareTo-Funktion des Typs des Baumes.
   * 
   * @return Ein Comparator.
   */
  public Comparator<? super E> comparator()
  {
    return new AVLElementComparator<E>();
  }

  /**
   * Gibt einen neuen Baum mit einer Teilmenge dieses Baumes zurück.<br>
   * <br>
   * Es werden alle Elemente übertragen, die größer oder gleich fromElement sind und kleiner oder gleich wie
   * toElement.<br>
   * <b>MERKE:</b> Es werden nur die Referenzen auf die Elemente in den neuen Baum kopiert. Es findet keine
   * tiefe Kopie der Elemente statt.
   * 
   * @param fromElement Die untere Grenze.
   * @param toElement Die obere Grenze.
   * @return Ein neuer AVLTree mit den oben beschriebenen Elementen.
   */
  public synchronized SortedSet<E> subSet(
    E fromElement,
    E toElement)
  {
    AVLTree<E> oAVLTreeReturn = new AVLTree<E>(this.m_GenericClass);

    Iterator<E> oIterator = iteratorUpBetween(fromElement, toElement);
    while (oIterator.hasNext())
    {
      oAVLTreeReturn.add(oIterator.next());
    }

    return oAVLTreeReturn;
  }

  /**
   * Gibt einen neuen Baum mit einer Teilmenge dieses Baumes zurück.<br>
   * <br>
   * Es werden alle Elemente übertragen, die kleiner oder gleich wie toElement sind.<br>
   * <b>MERKE:</b> Es werden nur die Referenzen auf die Elemente in den neuen Baum kopiert. Es findet keine
   * tiefe Kopie der Elemente statt.
   * 
   * @param toElement Die obere Grenze.
   * @return Ein neuer AVLTree mit den oben beschriebenen Elementen.
   */
  public synchronized SortedSet<E> headSet(
    E toElement)
  {
    return subSet(this.m_FirstNode.m_Element, toElement);
  }

  /**
   * Gibt einen neuen Baum mit einer Teilmenge dieses Baumes zurück.<br>
   * <br>
   * Es werden alle Elemente übertragen, die größer oder gleich fromElement.<br>
   * <b>MERKE:</b> Es werden nur die Referenzen auf die Elemente in den neuen Baum kopiert. Es findet keine
   * tiefe Kopie der Elemente statt.
   * 
   * @param fromElement Die untere Grenze.
   * @return Ein neuer AVLTree mit den oben beschriebenen Elementen.
   */
  public synchronized SortedSet<E> tailSet(
    E fromElement)
  {
    return subSet(fromElement, this.m_LastNode.m_Element);
  }

  /**
   * Gibt das kleinste Element zurück.
   * 
   * @return Das kleinste Element.
   */
  public synchronized E first()
  {
    return (this.m_FirstNode != null ? this.m_FirstNode.m_Element : null);
  }

  /**
   * Gibt das größte Element zurück.
   * 
   * @return Das größte Element.
   */
  public synchronized E last()
  {
    return (this.m_LastNode != null ? this.m_LastNode.m_Element : null);
  }

  /**
   * Gibt zurück, ob der Baum leer ist, also keine Elemente enthält.
   * 
   * @return Ist der baum leer?
   */
  public synchronized boolean isEmpty()
  {
    if (!((this.mi_Size == 0) == (this.m_RootNode == null)))
    {
      throw new AVLTreeException("isEmpty: !((this.mi_Size == 0) == (this.m_RootNode == null))");
    }
    return this.mi_Size == 0;
  }

  /**
   * Ist das Element enthalten?
   * 
   * @param oObject Das gesuchte Objekt.
   * @return true &rarr; das Objekt ist enthalten; false &rarr; das Objekt ist nicht enthalten
   */
  public synchronized boolean contains(
    Object oObject)
  {
    if (oObject == null || !this.m_GenericClass.isInstance(oObject))
    {
      return false;
    }

    // Assert: object != null && object instanceOf E
    E element = this.m_GenericClass.cast(oObject);

    return findNode(element).m_NodePointer != null;
  }

  /**
   * Sucht in dem Baum nach einem Element, welches mit dem übergebenen übereinstimmt.<br>
   * <br>
   * Es gilt foundElement.compareTo(otherElement) == 0.
   * 
   * @param otherElement Das andere Element.
   * @return Das gefundenen Element, welches mit dem anderen übereinstimmt; null &rarr; es wurde keine
   *         Entsprechung gefunden.
   */
  public synchronized E getEqualElement(
    E otherElement)
  {
    AVLNode<E> oFoundNode = findNode(otherElement).m_NodePointer;

    if (oFoundNode == null)
    {
      return null;
    }

    return oFoundNode.m_Element;
  }

  public synchronized Object[] toArray()
  {
    Object[] aoResult = new Object[size()];

    if (aoResult.length == 0)
    {
      return aoResult;
    }

    Iterator<E> oIterator = iterator();

    for (int i = 0; oIterator.hasNext(); i++)
    {
      aoResult[i] = oIterator.next();
    }

    return aoResult;
  }

  @SuppressWarnings("unchecked")
  public synchronized <T> T[] toArray(
    T[] aoStorage)
  {
    if (aoStorage == null)
    {
      throw new NullPointerException("aoStorage == null");
    }

    Class<T> oTClass = (Class<T>) aoStorage.getClass().getComponentType();

    if (!oTClass.isAssignableFrom(this.m_GenericClass))
    {
      throw new ArrayStoreException(oTClass.getName() + " is no superclass of "
        + this.m_GenericClass.getName());
    }

    if (aoStorage.length < size())
    {
      aoStorage = (T[]) Array.newInstance(oTClass, size());
    }

    Iterator<E> oIterator = iterator();

    int i;
    for (i = 0; oIterator.hasNext(); i++)
    {
      aoStorage[i] = oTClass.cast(oIterator.next());
    }

    for (; i < aoStorage.length; i++)
    {
      aoStorage[i] = null;
    }

    return aoStorage;
  }

  public synchronized boolean add(
    E oElement)
  {
    return add(oElement, false) == null;
  }
  
  /**
   * Fügt ein Element dem Baum hinzu.<br>
   * <br>
   * Ist das Element schon enthalten, entscheidet der Parameter bReplace über das Verhalten.
   * 
   * @param oElement Das neue Element.
   * @param bReplace Soll das Element ersetzt werden, falls es schon enthalten ist?
   * @return
   *        <ul>
   *        <li>null &rarr; Das Element oElement wurde hinzugefügt.
   *        <li>oOld &rarr; Das Element oOld wurde gefunden und je nach Parameter ersetzt oder nicht.
   *        </ul>
   */
  public synchronized E add(
    E oElement,
    boolean bReplace)
  {
    if (oElement == null)
    {
      throw new NullPointerException("oElement == null");
    }

    if (isEmpty())
    {
      this.m_RootNode = new AVLNode<E>(oElement);
      this.m_FirstNode = this.m_RootNode;
      this.m_LastNode = this.m_RootNode;
      notifyNodeAdded();
      return null;
    }

    // Assert: !isEmpty() && this.m_RootNode != null
    AVLSearchResult searchResult = findNode(oElement);
    if (searchResult.m_NodePointer != null)
    {
      E oOldElement = searchResult.m_NodePointer.m_Element;
      
      // Das Element ist schon vorhanden
      if (bReplace)
      {
        searchResult.m_NodePointer.m_Element = oElement;
      }

      return oOldElement;
    }

    // Assert: searchResult.m_NodeParent != null
    if (searchResult.m_NodeParent == null)
    {
      throw new AVLTreeException("add: searchResult.m_NodeParent == null");
    }
    else if (searchResult.mb_PointsLeft)
    {
      // Ort gefunden --> füge Links ein
      AVLNode<E> nodeNew = new AVLNode<E>(oElement);
      nodeNew.m_NodeParent = searchResult.m_NodeParent;
      searchResult.m_NodeParent.m_NodeLeft = nodeNew;
      setBalancesAfterAdd(nodeNew);
      notifyNodeAdded();
      if (this.m_FirstNode.compareTo(oElement) > 0)
      {
        this.m_FirstNode = nodeNew;
      }
      return null;
    }
    else if (!searchResult.mb_PointsLeft)
    {
      // Ort gefunden --> füge Rechts ein
      AVLNode<E> nodeNew = new AVLNode<E>(oElement);
      nodeNew.m_NodeParent = searchResult.m_NodeParent;
      searchResult.m_NodeParent.m_NodeRight = nodeNew;
      setBalancesAfterAdd(nodeNew);
      notifyNodeAdded();
      if (this.m_LastNode.compareTo(oElement) < 0)
      {
        this.m_LastNode = nodeNew;
      }
      return null;
    }
    else
    {
      throw new AVLTreeException("add: unreachable Code");
    }
  }

  private void setBalancesAfterAdd(
    AVLNode<E> nodeChild)
  {
    if (nodeChild != null)
    {
      AVLNode<E> pointerChild = nodeChild;
      AVLNode<E> pointerParent = nodeChild.m_NodeParent;
      while (pointerParent != null)
      {
        if (pointerParent.m_NodeLeft == pointerChild)
        {
          // Kind wurde links hinzugefügt
          pointerParent.m_byBalance--;
        }
        else if (pointerParent.m_NodeRight == pointerChild)
        {
          // Kind wurde rechts hinzugefügt
          pointerParent.m_byBalance++;
        }
        else
        {
          throw new AVLTreeException("setBalancesAfterAdd: pointerParent.m_NodeLeft != pointerChild "
            + "&& pointerParent.m_NodeRight != pointerChild");
        }

        if (pointerParent.m_byBalance == 0)
        {
          return;
        }
        else if (pointerParent.m_byBalance == -2 || pointerParent.m_byBalance == 2)
        {
          rotate(pointerParent);
          return;
        }
        else
        {
          pointerChild = pointerParent;
          pointerParent = pointerParent.m_NodeParent;
        }
      }
    }
  }

  private AVLNode<E> rotate(
    AVLNode<E> pointerParent)
  {
    if (pointerParent != null)
    {
      if (pointerParent.m_byBalance == 2)
      {
        AVLNode<E> pointerChild = pointerParent.m_NodeRight;
        if (pointerChild == null)
        {
          throw new AVLTreeException("rotate (2|?): pointerChild == null");
        }
        else if (pointerChild.m_byBalance == -1)
        {
          AVLNode<E> pointerGrandChild = pointerChild.m_NodeLeft;

          if (pointerGrandChild == null)
          {
            throw new AVLTreeException("rotate (2|-1): pointerGrandChild == null");
          }

          // (2|-1) -> Rechts-Links-Rotation
          AVLNode<E> pointerGrandFather = pointerParent.m_NodeParent;
          if (pointerGrandFather == null)
          {
            if (pointerParent == this.m_RootNode)
            {
              this.ml_OperationCount++;
              this.m_RootNode = pointerGrandChild;
              pointerGrandChild.m_NodeParent = null;
            }
            else
            {
              throw new AVLTreeException(
                "rotate (2|-1): pointerParent != this.m_RootNode && pointerGrandFather == null");
            }
          }
          else
          {
            if (pointerGrandFather.m_NodeLeft == pointerParent)
            {
              this.ml_OperationCount++;
              pointerGrandFather.m_NodeLeft = pointerGrandChild;
              pointerGrandChild.m_NodeParent = pointerGrandFather;
            }
            else if (pointerGrandFather.m_NodeRight == pointerParent)
            {
              this.ml_OperationCount++;
              pointerGrandFather.m_NodeRight = pointerGrandChild;
              pointerGrandChild.m_NodeParent = pointerGrandFather;
            }
            else
            {
              throw new AVLTreeException("rotate (2|-1): pointerGrandFather.m_NodeLeft != pointerParent "
                + "&& pointerGrandFather.m_NodeRight != pointerParent");
            }
          }

          this.ml_OperationCount += 4;
          pointerParent.m_NodeRight = pointerGrandChild.m_NodeLeft;
          if (pointerParent.m_NodeRight != null)
          {
            pointerParent.m_NodeRight.m_NodeParent = pointerParent;
          }
          pointerChild.m_NodeLeft = pointerGrandChild.m_NodeRight;
          if (pointerChild.m_NodeLeft != null)
          {
            pointerChild.m_NodeLeft.m_NodeParent = pointerChild;
          }
          pointerGrandChild.m_NodeLeft = pointerParent;
          pointerParent.m_NodeParent = pointerGrandChild;
          pointerGrandChild.m_NodeRight = pointerChild;
          pointerChild.m_NodeParent = pointerGrandChild;

          if (pointerGrandChild.m_byBalance == -1)
          {
            pointerParent.m_byBalance = (byte) 0;
            pointerChild.m_byBalance = (byte) 1;
          }
          else if (pointerGrandChild.m_byBalance == 0)
          {
            pointerParent.m_byBalance = (byte) 0;
            pointerChild.m_byBalance = (byte) 0;
          }
          else if (pointerGrandChild.m_byBalance == 1)
          {
            pointerParent.m_byBalance = (byte) -1;
            pointerChild.m_byBalance = (byte) 0;
          }

          pointerGrandChild.m_byBalance = (byte) 0;
          return pointerGrandChild;
        }
        else if (pointerChild.m_byBalance == 1 || pointerChild.m_byBalance == 0)
        {
          // (2|1/0) -> Links-Rotation
          AVLNode<E> pointerGrandFather = pointerParent.m_NodeParent;
          if (pointerGrandFather == null)
          {
            if (pointerParent == this.m_RootNode)
            {
              this.ml_OperationCount++;
              this.m_RootNode = pointerChild;
              pointerChild.m_NodeParent = null;
            }
            else
            {
              throw new AVLTreeException(
                "rotate (2|1/0): pointerParent != this.m_RootNode && pointerGrandFather == null");
            }
          }
          else
          {
            if (pointerGrandFather.m_NodeLeft == pointerParent)
            {
              this.ml_OperationCount++;
              pointerGrandFather.m_NodeLeft = pointerChild;
              pointerChild.m_NodeParent = pointerGrandFather;
            }
            else if (pointerGrandFather.m_NodeRight == pointerParent)
            {
              this.ml_OperationCount++;
              pointerGrandFather.m_NodeRight = pointerChild;
              pointerChild.m_NodeParent = pointerGrandFather;
            }
            else
            {
              throw new AVLTreeException("rotate (2|1/0): pointerGrandFather.m_NodeLeft != pointerParent "
                + "&& pointerGrandFather.m_NodeRight != pointerParent");
            }
          }

          this.ml_OperationCount += 2;
          pointerParent.m_NodeRight = pointerChild.m_NodeLeft;
          if (pointerParent.m_NodeRight != null)
          {
            pointerParent.m_NodeRight.m_NodeParent = pointerParent;
          }
          pointerChild.m_NodeLeft = pointerParent;
          pointerParent.m_NodeParent = pointerChild;

          if (pointerChild.m_byBalance == 1)
          {
            pointerParent.m_byBalance = (byte) 0;
            pointerChild.m_byBalance = (byte) 0;
          }
          else if (pointerChild.m_byBalance == 0)
          {
            pointerParent.m_byBalance = (byte) 1;
            pointerChild.m_byBalance = (byte) -1;
          }

          return pointerChild;
        }
        else
        {
          throw new AVLTreeException("rotate (2|?): pointerChild.m_byBalance != -1 && "
            + "pointerChild.m_byBalance != 1 && pointerChild.m_byBalance != 0");
        }
      }
      else if (pointerParent.m_byBalance == -2)
      {
        AVLNode<E> pointerChild = pointerParent.m_NodeLeft;
        if (pointerChild == null)
        {
          throw new AVLTreeException("rotate (-2|?): pointerChild == null");
        }
        else if (pointerChild.m_byBalance == 1)
        {
          AVLNode<E> pointerGrandChild = pointerChild.m_NodeRight;

          if (pointerGrandChild == null)
          {
            throw new AVLTreeException("rotate (-2|1): pointerGrandChild == null");
          }

          // (-2|1) -> Links-Rechts-Rotation
          AVLNode<E> pointerGrandFather = pointerParent.m_NodeParent;
          if (pointerGrandFather == null)
          {
            if (pointerParent == this.m_RootNode)
            {
              this.ml_OperationCount++;
              this.m_RootNode = pointerGrandChild;
              pointerGrandChild.m_NodeParent = null;
            }
            else
            {
              throw new AVLTreeException(
                "rotate (-2|1): pointerParent != this.m_RootNode && pointerGrandFather == null");
            }
          }
          else
          {
            if (pointerGrandFather.m_NodeLeft == pointerParent)
            {
              this.ml_OperationCount++;
              pointerGrandFather.m_NodeLeft = pointerGrandChild;
              pointerGrandChild.m_NodeParent = pointerGrandFather;
            }
            else if (pointerGrandFather.m_NodeRight == pointerParent)
            {
              this.ml_OperationCount++;
              pointerGrandFather.m_NodeRight = pointerGrandChild;
              pointerGrandChild.m_NodeParent = pointerGrandFather;
            }
            else
            {
              throw new AVLTreeException("rotate (-2|1): pointerGrandFather.m_NodeLeft != pointerParent"
                + "&& pointerGrandFather.m_NodeRight != pointerParent");
            }
          }

          this.ml_OperationCount += 4;
          pointerParent.m_NodeLeft = pointerGrandChild.m_NodeRight;
          if (pointerParent.m_NodeLeft != null)
          {
            pointerParent.m_NodeLeft.m_NodeParent = pointerParent;
          }
          pointerChild.m_NodeRight = pointerGrandChild.m_NodeLeft;
          if (pointerChild.m_NodeRight != null)
          {
            pointerChild.m_NodeRight.m_NodeParent = pointerChild;
          }
          pointerGrandChild.m_NodeLeft = pointerChild;
          pointerChild.m_NodeParent = pointerGrandChild;
          pointerGrandChild.m_NodeRight = pointerParent;
          pointerParent.m_NodeParent = pointerGrandChild;

          if (pointerGrandChild.m_byBalance == 1)
          {
            pointerParent.m_byBalance = (byte) 0;
            pointerChild.m_byBalance = (byte) -1;
          }
          else if (pointerGrandChild.m_byBalance == 0)
          {
            pointerParent.m_byBalance = (byte) 0;
            pointerChild.m_byBalance = (byte) 0;
          }
          else if (pointerGrandChild.m_byBalance == -1)
          {
            pointerParent.m_byBalance = (byte) 1;
            pointerChild.m_byBalance = (byte) 0;
          }

          pointerGrandChild.m_byBalance = (byte) 0;
          return pointerGrandChild;
        }
        else if (pointerChild.m_byBalance == -1 || pointerChild.m_byBalance == 0)
        {
          // (-2|-1/0) -> Rechts
          AVLNode<E> pointerGrandFather = pointerParent.m_NodeParent;
          if (pointerGrandFather == null)
          {
            if (pointerParent == this.m_RootNode)
            {
              this.ml_OperationCount++;
              this.m_RootNode = pointerChild;
              pointerChild.m_NodeParent = null;
            }
            else
            {
              throw new AVLTreeException(
                "rotate (-2|-1/0): pointerParent != this.m_RootNode && pointerGrandFather == null");
            }
          }
          else
          {
            if (pointerGrandFather.m_NodeLeft == pointerParent)
            {
              this.ml_OperationCount++;
              pointerGrandFather.m_NodeLeft = pointerChild;
              pointerChild.m_NodeParent = pointerGrandFather;
            }
            else if (pointerGrandFather.m_NodeRight == pointerParent)
            {
              this.ml_OperationCount++;
              pointerGrandFather.m_NodeRight = pointerChild;
              pointerChild.m_NodeParent = pointerGrandFather;
            }
            else
            {
              throw new AVLTreeException("rotate (-2|-1/0): pointerGrandFather.m_NodeLeft != pointerParent "
                + "&& pointerGrandFather.m_NodeRight != pointerParent");
            }
          }

          this.ml_OperationCount += 2;
          pointerParent.m_NodeLeft = pointerChild.m_NodeRight;
          if (pointerParent.m_NodeLeft != null)
          {
            pointerParent.m_NodeLeft.m_NodeParent = pointerParent;
          }
          pointerChild.m_NodeRight = pointerParent;
          pointerParent.m_NodeParent = pointerChild;

          if (pointerChild.m_byBalance == -1)
          {
            pointerParent.m_byBalance = (byte) 0;
            pointerChild.m_byBalance = (byte) 0;
          }
          else if (pointerChild.m_byBalance == 0)
          {
            pointerParent.m_byBalance = (byte) -1;
            pointerChild.m_byBalance = (byte) 1;
          }

          return pointerChild;
        }
        else
        {
          throw new AVLTreeException("rotate (-2|?): pointerChild.m_byBalance != -1 && "
            + "pointerChild.m_byBalance != 1 && pointerChild.m_byBalance != 0");
        }
      }
      else if (pointerParent.m_byBalance > 2 || pointerParent.m_byBalance < -2)
      {
        throw new AVLTreeException("pointerParent.m_byBalance > 2 " + "|| pointerParent.m_byBalance < -2");
      }
    }

    return null;
  }

  /**
   * Entfernt ein Element aus dem Baum.
   * 
   * @param oObject Das zu löschende Element.
   * @return true &rarr; das Element wurde gelöscht<br>
   *         false &rarr; das Element war nicht enthalten oder null
   */
  public synchronized boolean remove(
    Object oObject)
  {
    if (oObject == null || !this.m_GenericClass.isInstance(oObject))
    {
      return false;
    }

    // Assert: oObject != null && oObject instanceOf E
    E oElement = this.m_GenericClass.cast(oObject);

    AVLSearchResult searchResult = findNode(oElement);

    if (searchResult.m_NodePointer == null)
    {
      // Das Element ist nicht vorhanden
      return false;
    }

    AVLNode<E> nodeToDelete = searchResult.m_NodePointer;
    AVLNode<E> nodeParent = nodeToDelete.m_NodeParent;

    if (nodeToDelete == this.m_FirstNode)
    {
      this.m_FirstNode = getSuccessor(this.m_FirstNode);
    }
    if (nodeToDelete == this.m_LastNode)
    {
      this.m_LastNode = getPredecessor(this.m_LastNode);
    }

    if (nodeToDelete.m_NodeLeft == null)
    {
      if (nodeParent == null)
      {
        if (nodeToDelete == this.m_RootNode)
        {
          // Die Wurzel soll gelöscht werden und der linke Baum ist null
          // evt. ist auch der rechte Baum null
          this.m_RootNode = nodeToDelete.m_NodeRight;
          if (this.m_RootNode != null)
          {
            this.m_RootNode.m_NodeParent = null;
          }
          notifyNodeRemoved();
          return true;
        }

        throw new AVLTreeException("remove: nodeToDelete.m_NodeLeft == null && nodeParent == null"
          + "&& nodeToDelete != this.m_RootNode");
      }
      else if (nodeParent.m_NodeLeft == nodeToDelete)
      {
        nodeParent.m_NodeLeft = nodeToDelete.m_NodeRight;
        if (nodeParent.m_NodeLeft != null)
        {
          nodeParent.m_NodeLeft.m_NodeParent = nodeParent;
        }
        setBalancesAfterDeletion(nodeParent, nodeParent.m_NodeLeft);
        notifyNodeRemoved();
        return true;
      }
      else if (nodeParent.m_NodeRight == nodeToDelete)
      {
        nodeParent.m_NodeRight = nodeToDelete.m_NodeRight;
        if (nodeParent.m_NodeRight != null)
        {
          nodeParent.m_NodeRight.m_NodeParent = nodeParent;
        }
        setBalancesAfterDeletion(nodeParent, nodeParent.m_NodeRight);
        notifyNodeRemoved();
        return true;
      }
      else
      {
        throw new AVLTreeException("remove: nodeToDelete.m_NodeLeft == null && nodeParent != null"
          + "&& nodeParent.m_NodeLeft != nodeToDelete && nodeParent.m_NodeRight != nodeToDelete");
      }
    }
    else if (nodeToDelete.m_NodeRight == null)
    {
      if (nodeParent == null)
      {
        if (nodeToDelete == this.m_RootNode)
        {
          this.m_RootNode = nodeToDelete.m_NodeLeft;
          this.m_RootNode.m_NodeParent = null;
          notifyNodeRemoved();
          return true;
        }

        throw new AVLTreeException("remove: nodeToDelete.m_NodeRight == null && nodeParent == null"
          + "&& nodeToDelete != this.m_RootNode");
      }
      else if (nodeParent.m_NodeLeft == nodeToDelete)
      {
        nodeParent.m_NodeLeft = nodeToDelete.m_NodeLeft;
        nodeParent.m_NodeLeft.m_NodeParent = nodeParent;
        setBalancesAfterDeletion(nodeParent, nodeParent.m_NodeLeft);
        notifyNodeRemoved();
        return true;
      }
      else if (nodeParent.m_NodeRight == nodeToDelete)
      {
        nodeParent.m_NodeRight = nodeToDelete.m_NodeLeft;
        nodeParent.m_NodeRight.m_NodeParent = nodeParent;
        setBalancesAfterDeletion(nodeParent, nodeParent.m_NodeRight);
        notifyNodeRemoved();
        return true;
      }
      else
      {
        throw new AVLTreeException("remove: nodeToDelete.m_NodeRight == null && nodeParent != null"
          + "&& nodeParent.m_NodeLeft != nodeToDelete && nodeParent.m_NodeRight != nodeToDelete");
      }
    }
    else
    {
      // der Nachfolger oder Vorgänger muss ein Kind des zu löschenden Knotens
      // sein, da der linke und der rechte Kindknoten nicht leer ist
      if (this.m_bUseSuccesor)
      {
        AVLNode<E> pointerSuccessor = getSuccessor(nodeToDelete);
        if (pointerSuccessor == null || pointerSuccessor.m_NodeLeft != null)
        {
          throw new AVLTreeException("remove: pointerSuccessor == null "
            + "|| pointerSuccessor.m_NodeLeft != null");
        }
        if (pointerSuccessor == this.m_LastNode)
        {
          this.m_LastNode = nodeToDelete;
        }
        remove(pointerSuccessor.m_Element);
        nodeToDelete.m_Element = pointerSuccessor.m_Element;
      }
      else
      {
        AVLNode<E> pointerPredecessor = getPredecessor(nodeToDelete);
        if (pointerPredecessor == null || pointerPredecessor.m_NodeRight != null)
        {
          throw new AVLTreeException("remove: pointerPredecessor == null "
            + "|| pointerPredecessor.m_NodeRight != null");
        }
        if (pointerPredecessor == this.m_FirstNode)
        {
          this.m_FirstNode = nodeToDelete;
        }
        remove(pointerPredecessor.m_Element);
        nodeToDelete.m_Element = pointerPredecessor.m_Element;
      }
      this.m_bUseSuccesor = !this.m_bUseSuccesor;
      return true;
    }
  }

  private void setBalancesAfterDeletion(
    AVLNode<E> nodeParent,
    AVLNode<E> nodeChild)
  {
    AVLNode<E> pointerParent = nodeParent;
    AVLNode<E> pointerChild = nodeChild;

    while (pointerParent != null)
    {
      if (pointerChild == null)
      {
        if (pointerParent.m_NodeLeft == null)
        {
          if (pointerParent.m_NodeRight == null)
          {
            pointerParent.m_byBalance = 0;
          }
          else
          {
            pointerParent.m_byBalance++;
          }
        }
        else if (pointerParent.m_NodeRight == null)
        {
          pointerParent.m_byBalance--;
        }
        else
        {
          throw new AVLTreeException("setBalancesAfterDeletion: "
            + "pointerChild == null && pointerParent.m_NodeLeft != null && pointerParent.m_NodeRight != null");
        }
      }
      else if (pointerParent.m_NodeLeft == pointerChild)
      {
        // Kind wurde links entfernt
        pointerParent.m_byBalance++;
      }
      else if (pointerParent.m_NodeRight == pointerChild)
      {
        // Kind wurde rechts entfernt
        pointerParent.m_byBalance--;
      }
      else
      {
        throw new AVLTreeException("setBalancesAfterDeletion: " + "pointerParent.m_NodeLeft != pointerChild "
          + "&& pointerParent.m_NodeRight != pointerChild");
      }

      if (pointerParent.m_byBalance == -2 || pointerParent.m_byBalance == 2)
      {
        pointerParent = rotate(pointerParent);
      }

      if (pointerParent == null)
      {
        throw new AVLTreeException("setBalancesAfterDeletion: pointerParent == null");
      }
      else if (pointerParent.m_byBalance == -1 || pointerParent.m_byBalance == 1)
      {
        return;
      }
      else
      {
        pointerChild = pointerParent;
        pointerParent = pointerParent.m_NodeParent;
      }
    }
  }

  /**
   * überprüft, ob alle Elemente einer anderen Collection in diesem Baum enthalten sind.
   * 
   * @param oCollection Die andere Collection
   * @return true &rarr; ALLE Elemente der anderen Collection sind in dem Baum enthalten; false &rarr; es gibt
   *         mindestens ein Element, das in der anderen Collection aber nicht in dem Baum enthalten ist
   */
  public synchronized boolean containsAll(
    Collection<?> oCollection)
  {
    Iterator<?> oIteratorOther = oCollection.iterator();
    while (oIteratorOther.hasNext())
    {
      if (!contains(oIteratorOther.next()))
      {
        return false;
      }
    }
    return true;
  }

  /**
   * Fügt alle Elemente einer anderen Collection in den Baum ein.
   * 
   * @param oCollection
   * @return true &rarr; es wurden neue Elemente eingefügt; false &rarr; es gab kein neues Element, das
   *         eingefügt wurde
   */
  public synchronized boolean addAll(
    Collection<? extends E> oCollection)
  {
    boolean bAddedNew = false;
    Iterator<? extends E> oIteratorOther = oCollection.iterator();
    while (oIteratorOther.hasNext())
    {
      bAddedNew |= add(oIteratorOther.next());
    }
    return bAddedNew;
  }

  /**
   * Behält in diesem Baum nur die Elemente, welche in der anderen Collection enthalten sind. <br>
   * <br>
   * D.h. es werden in diesem Baum alle Elemente gelöscht, welche in der anderen Collection nicht enthalten
   * sind.
   * 
   * @param oCollection Die andere Collection
   * @return true &rarr; es wurde mindestens ein Element aus diesem Baum entfernt; false &rarr; es wurden
   *         keine Veränderungen an diesem Baum durchgeführt
   */
  public synchronized boolean retainAll(
    Collection<?> oCollection)
  {
    boolean bModified = false;
    Iterator<E> oIteratorThis = iterator();
    while (oIteratorThis.hasNext())
    {
      if (!oCollection.contains(oIteratorThis.next()))
      {
        oIteratorThis.remove();
        bModified = true;
      }
    }
    return bModified;
  }

  /**
   * Entfernt alle Elemente der anderen Collection aus dieser.
   * 
   * @param oCollection Die andere Collection.
   * @return true &rarr; es wurde mindestens ein Element aus diesem Baum entfernt; false &rarr; es wurden
   *         keine Veränderungen an diesem Baum durchgeführt
   */
  public synchronized boolean removeAll(
    Collection<?> oCollection)
  {
    boolean bRemovedItems = false;
    Iterator<?> oIteratorOther = oCollection.iterator();
    while (oIteratorOther.hasNext())
    {
      bRemovedItems |= remove(oIteratorOther.next());
    }
    return bRemovedItems;
  }

  /**
   * Leert alle Elemente aus dem Baum.
   */
  public void clear()
  {
    this.mi_Size = 0;
    this.m_RootNode = null;
    this.m_FirstNode = null;
    this.m_LastNode = null;
  }

  @Override
  public String toString()
  {
    return "Empty: " + (isEmpty() ? "Yes" : "No") + "; Elementcount: " + this.mi_Size
      + "; Modificationcount: " + this.ml_OperationCount;
  }

  /**
   * Gibt den Baum aufsteigend an den übergebenen PrintWriter aus. <br>
   * <br>
   * Der Aufruf geschieht rekursiv
   * 
   * @param stream Der PrintStream für die Ausgabe.
   * @param strSeparator Das Trennzeichen zwischen den Elementen
   */
  public synchronized void printInorderRekursive(
    PrintStream stream,
    String strSeparator)
  {
    if (stream == null || isEmpty())
    {
      return;
    }

    // Assert: this.m_RootNode != null
    printInorderRekursive(stream, this.m_RootNode, strSeparator);
  }

  /**
   * Gibt den Baum aufsteigend an den übergebenen PrintWriter aus. Der Aufruf der Unterknoten erfolgt
   * rekursiv.
   * 
   * @param stream Der PrintStream für die Ausgabe.
   * @param node Der Knoten, der ausgegeben werden soll.
   * @param strSeparator Das Trennzeichen zwischen den Elementen
   */
  private synchronized void printInorderRekursive(
    PrintStream stream,
    AVLNode<E> node,
    String strSeparator)
  {
    // Assert: stream != null && node != null
    if (node.m_NodeLeft != null)
    {
      printInorderRekursive(stream, node.m_NodeLeft, strSeparator);
    }
    stream.print(node + strSeparator);
    if (node.m_NodeRight != null)
    {
      printInorderRekursive(stream, node.m_NodeRight, strSeparator);
    }
  }

  /**
   * Gibt den Baum aufsteigend an den übergebenen PrintWriter aus. <br>
   * <br>
   * Der Aufruf geschieht iterativ durch Iterator.
   * 
   * @param stream Der PrintStream für die Ausgabe.
   * @param strSeparator Das Trennzeichen zwischen den Elementen
   * @see #iterator()
   */
  public synchronized void printInorderIterative(
    PrintStream stream,
    String strSeparator)
  {
    if (stream == null || isEmpty())
    {
      return;
    }

    // Assert: this.m_RootNode != null

    Iterator<E> oIterator = iterator();
    while (oIterator.hasNext())
    {
      stream.print(oIterator.next() + strSeparator);
    }
  }

  /**
   * Gibt den Baum absteigend an den übergebenen PrintWriter aus.<br>
   * <br>
   * Der Aufruf geschieht iterativ durch Iterator.
   * 
   * @param stream Der PrintStream für die Ausgabe.
   * @param strSeparator Das Trennzeichen zwischen den Elementen
   * @see #iterator()
   */
  public synchronized void printInorderIterativeDown(
    PrintStream stream,
    String strSeparator)
  {
    if (stream == null || isEmpty())
    {
      return;
    }

    // Assert: this.m_RootNode != null

    Iterator<E> oIterator = iteratorDownAll();
    while (oIterator.hasNext())
    {
      stream.print(oIterator.next() + strSeparator);
    }
  }

  /**
   * Gibt den Baum levelweise an den übergebenen PrintWriter aus.<br>
   * <br>
   * Der Aufruf geschieht iterativ durch Iterator.
   * 
   * @param stream Der PrintStream für die Ausgabe.
   * @param strSeparator Das Trennzeichen zwischen den Elementen
   * @see #iterator()
   */
  public synchronized void printLevelOrderIterative(
    PrintStream stream,
    String strSeparator)
  {
    if (stream == null || isEmpty())
    {
      return;
    }

    // Assert: this.m_RootNode != null

    Iterator<E> oIterator = iteratorLevel();
    while (oIterator.hasNext())
    {
      stream.print(oIterator.next() + strSeparator);
    }
  }

  /**
   * Sucht in dem Baum nach dem übergebenen Element.
   * 
   * @param element Das gesuchte Element.
   * @return Ein Suchresultat:<br>
   * @see AVLSearchResult#m_NodePointer Gibt den Pointer auf den gefundenen Knoten an. Null, falls nicht
   *      gefunden.
   * @see AVLSearchResult#m_NodeParent Gibt den Vater des gefundenen Knotens an.
   * @see AVLSearchResult#mb_PointsLeft true &rarr; Der Vater-Pointer zeigt nach links; false &rarr; Der
   *      Vater-Pointer zeigt nach rechts
   */
  protected AVLSearchResult findNode(
    E element)
  {
    AVLSearchResult returnResult = new AVLSearchResult();

    if (element == null || isEmpty())
    {
      return returnResult;
    }

    // Assert: !isEmpty() && this.m_RootNode != null
    AVLNode<E> nodePointer = this.m_RootNode;
    AVLNode<E> nodeTmp = null;

    while (true)
    {
      // Assert nodePointer != null
      int iCompare = nodePointer.compareTo(element);
      if (iCompare > 0)
      {
        nodeTmp = nodePointer.m_NodeLeft;
        if (nodeTmp != null)
        {
          nodePointer = nodeTmp;
        }
        else
        {
          // leeren Pointer nach Links gefunden
          returnResult.m_NodePointer = null;
          returnResult.m_NodeParent = nodePointer;
          returnResult.mb_PointsLeft = true;
          return returnResult;
        }
      }
      else if (iCompare < 0)
      {
        nodeTmp = nodePointer.m_NodeRight;
        if (nodeTmp != null)
        {
          nodePointer = nodeTmp;
        }
        else
        {
          // leeren Pointer nach Rechts gefunden
          returnResult.m_NodePointer = null;
          returnResult.m_NodeParent = nodePointer;
          returnResult.mb_PointsLeft = false;
          return returnResult;
        }
      }
      else
      {
        // Knoten gefunden
        returnResult.m_NodePointer = nodePointer;
        returnResult.m_NodeParent = nodePointer.m_NodeParent;
        if (returnResult.m_NodeParent != null)
        {
          returnResult.mb_PointsLeft = returnResult.m_NodeParent.m_NodeLeft == returnResult.m_NodePointer;
        }
        return returnResult;
      }
    }
  }

  protected void notifyNodeAdded()
  {
    this.mi_Size++;
    this.ml_OperationCount++;
  }

  protected void notifyNodeRemoved()
  {
    if (this.mi_Size <= 0)
    {
      throw new AVLTreeException("notifyNodeRemoved: this.mi_Size <= 0");
    }
    this.mi_Size--;
    this.ml_OperationCount++;
  }

  /**
   * Gibt den Nachfolger eines Knotens im Baum zurück. Also den Knoten, der in der Reihenfolge als nächstes
   * kommt.
   * 
   * @param node Der betrachtete Knoten.
   * @return Den Nachfolger des Knotens.
   */
  AVLNode<E> getSuccessor(
    AVLNode<E> node)
  {
    if (node == null)
    {
      return null;
    }
    if (node.m_NodeRight != null)
    {
      AVLNode<E> nodePointer = node.m_NodeRight;
      while (true)
      {
        if (nodePointer.m_NodeLeft == null)
        {
          return nodePointer;
        }

        nodePointer = nodePointer.m_NodeLeft;
      }
    }

    // Assert: Der Knoten hat kein größeres Kind. Suche nächsten Knoten bei den
    // Vorgängern.
    AVLNode<E> nodePointer = node;
    AVLNode<E> nodeParent = node.m_NodeParent;

    while (true)
    {
      if (nodeParent == null)
      {
        return null;
      }
      else if (nodePointer == nodeParent.m_NodeLeft)
      {
        return nodeParent;
      }
      nodePointer = nodeParent;
      nodeParent = nodePointer.m_NodeParent;
    }
  }

  /**
   * Gibt den Vorgänger eines Knotens im Baum zurück. Also den Knoten, der in der Reihenfolge als nächstes
   * kommt.
   * 
   * @param node Der betrachtete Knoten.
   * @return Den Nachfolger des Knotens.
   */
  AVLNode<E> getPredecessor(
    AVLNode<E> node)
  {
    if (node == null)
    {
      return null;
    }
    if (node.m_NodeLeft != null)
    {
      AVLNode<E> nodePointer = node.m_NodeLeft;
      while (true)
      {
        if (nodePointer.m_NodeRight == null)
        {
          return nodePointer;
        }

        nodePointer = nodePointer.m_NodeRight;
      }
    }

    // Assert: Der Knoten hat kein kleineres Kind.
    AVLNode<E> nodePointer = node;
    AVLNode<E> nodeParent = node.m_NodeParent;

    while (true)
    {
      if (nodeParent == null)
      {
        return null;
      }
      else if (nodePointer == nodeParent.m_NodeRight)
      {
        return nodeParent;
      }
      nodePointer = nodeParent;
      nodeParent = nodePointer.m_NodeParent;
    }
  }

  /**
   * Testroutine zum Validieren der AVL-Eigenschaften.<br>
   * <br>
   * überprüft, ob der Baum ein AVL-Baum ist und alle Knoten korrekt vernetzt sind.
   * 
   * @param stream In des Ausgabestrom werden Fehler-Nachrichten geschrieben.
   * @throws AVLCheckException Wird geworfen, falls ein Fehler gefunden wurde.
   */
  public synchronized void checkTree(
    PrintStream stream)
    throws AVLCheckException
  {
    if (stream == null || isEmpty())
    {
      return;
    }

    this.m_iChecksum = 0;
    this.m_CheckSmallest = null;
    this.m_CheckLargest = null;

    checkTree(stream, this.m_RootNode);

    if (this.m_iChecksum != this.mi_Size)
    {
      stream.println("checkTree: m_iChecksum != this.mi_Size " + this.m_iChecksum + " vs. " + this.mi_Size);
      throw new AVLCheckException();
    }
    if (this.m_CheckSmallest != this.m_FirstNode)
    {
      stream.println("checkTree: this.m_CheckSmallest != this.m_FirstNode");
      throw new AVLCheckException();
    }
    if (this.m_CheckLargest != this.m_LastNode)
    {
      stream.println("checkTree: this.m_CheckLargest != this.m_LastNode");
      throw new AVLCheckException();
    }
  }

  private int checkTree(
    PrintStream stream,
    AVLNode<E> nodeCurrent)
  {
    this.m_iChecksum++;

    int iDepthLeft = 0;
    int iDepthRight = 0;
    if (nodeCurrent.m_NodeLeft != null)
    {
      iDepthLeft = checkTree(stream, nodeCurrent.m_NodeLeft);
      if (nodeCurrent.m_NodeLeft.m_NodeParent != nodeCurrent)
      {
        stream.println("checkTree: leftNode |" + nodeCurrent.m_NodeLeft.toString() + ".parent != |"
          + nodeCurrent.toString() + "|");
        throw new AVLCheckException();
      }
      if (nodeCurrent.compareTo(nodeCurrent.m_NodeLeft.m_Element) <= 0)
      {
        stream.println("checkTree: leftNode |" + nodeCurrent.m_NodeLeft.toString() + "| >= |"
          + nodeCurrent.toString() + "|");
        throw new AVLCheckException();
      }
    }
    if (nodeCurrent.m_NodeRight != null)
    {
      iDepthRight = checkTree(stream, nodeCurrent.m_NodeRight);
      if (nodeCurrent.m_NodeRight.m_NodeParent != nodeCurrent)
      {
        stream.println("checkTree: rightNode |" + nodeCurrent.m_NodeRight.toString() + ".parent != |"
          + nodeCurrent.toString() + "|");
        throw new AVLCheckException();
      }
      if (nodeCurrent.compareTo(nodeCurrent.m_NodeRight.m_Element) >= 0)
      {
        stream.println("checkTree: leftNode |" + nodeCurrent.m_NodeRight.toString() + "| <= |"
          + nodeCurrent.toString() + "|");
        throw new AVLCheckException();
      }
    }

    int iBalance = iDepthRight - iDepthLeft;
    if (iBalance != nodeCurrent.m_byBalance)
    {
      stream.println("checkTree: |" + nodeCurrent.toString() + "| balance " + iBalance + " vs. "
        + nodeCurrent.m_byBalance);
      throw new AVLCheckException();
    }
    else if (iBalance < -2 || iBalance > 2)
    {
      stream.println("checkTree: |" + nodeCurrent.toString() + "| balance " + iBalance);
      throw new AVLCheckException();
    }

    if (this.m_CheckSmallest == null || this.m_CheckSmallest.compareTo(nodeCurrent.m_Element) > 0)
    {
      this.m_CheckSmallest = nodeCurrent;
    }
    if (this.m_CheckLargest == null || this.m_CheckLargest.compareTo(nodeCurrent.m_Element) < 0)
    {
      this.m_CheckLargest = nodeCurrent;
    }

    return Math.max(iDepthLeft, iDepthRight) + 1;
  }

  class AVLSearchResult
  {
    AVLNode<E> m_NodePointer = null;

    AVLNode<E> m_NodeParent = null;

    boolean mb_PointsLeft = true;
  }

  /**
   * Comparator zum Vergleichen von Elementen des Baumes.<br>
   * <br>
   * Wird nie im Baum benötigt. Nur für die Funktion AVLTree#comparator() von Bedeutung.
   * 
   * @author cm
   * @version 1.0 27.05.2006 erstellt<br>
   * @param <E> Der generische Typ des Comparators.
   * @see AVLTree#comparator()
   */
  static class AVLElementComparator<E extends Comparable<? super E>>
    implements Comparator<E>
  {
    /**
     * Vergleicht die beiden Elemente.
     * 
     * @param oElementFirst Das erste Element.
     * @param oElementSecond Das zweite Element.
     * @return -1 => das erste Element kommt vor dem zweiten Element<br>
     *         0 => beide Elemente sind gleich<br>
     *         1 => das zweite Element kommt vor dem ersten Element
     */
    public int compare(
      E oElementFirst,
      E oElementSecond)
    {
      return oElementFirst.compareTo(oElementSecond);
    }
  }

  /**
   * Fehlerexception für die checkTreeFunktion<br>
   * 
   * @author cm
   * @version 1.0 27.05.2006 erstellt<br>
   */
  @SuppressWarnings("serial")
  public static class AVLCheckException
    extends RuntimeException
  {
    // nix besonderes
  }
}