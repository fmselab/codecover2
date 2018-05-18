package de.ahija;

import java.util.GregorianCalendar;

/**
 * Ermöglicht es im Programmablauf zu erfahren, wie lange bestimmte Bereiche benötigt haben. Beim der Konstruktion eines
 * Timers wird die Uhrzeit gespeichert. über printTimer kann die vollständig verstrichene Zeit und die Zeit seit dem
 * letzten Aufruf angezeigt werrden.
 * 
 * @author ahija
 */
public class ATTimer
{
  GregorianCalendar dStartTime;

  GregorianCalendar dLastView;

  /**
   * Erstellt einen neuen ATTimer und speichert sich den aktuellen Zeitstempel.
   */
  public ATTimer()
  {
    this.dStartTime = new GregorianCalendar();
    this.dLastView = new GregorianCalendar();
  }

  private GregorianCalendar getLastView()
  {
    GregorianCalendar dCopyLastView = this.dLastView;
    this.dLastView = new GregorianCalendar();
    return dCopyLastView;
  }

  /**
   * @return Die Zeitdifferenz in Millisekunden seit dem Start des Timers
   */
  public long getDifference()
  {
    return new GregorianCalendar().getTimeInMillis() - this.dStartTime.getTimeInMillis();
  }

  /**
   * @return Die Zeitdifferenz in Millisekunden seit dem Letzten Aufruf der Funktion getLastDifference()
   */
  public long getLastDifference()
  {
    return new GregorianCalendar().getTimeInMillis() - this.getLastView().getTimeInMillis();
  }

  /**
   * Rückgabe der vollständig verstrichenen Zeit und der Zeit seit dem letzten Aufruf.
   * 
   * @return Der beschriebene String.
   */
  @Override
  public String toString()
  {
    return "running Time: " + this.getDifference() + " | period Time: " + this.getLastDifference();
  }

  /**
   * Ausgabe der vollständig verstrichenen Zeit und der Zeit seit dem letzten Aufruf.
   */
  public void printTimer()
  {
    System.out.println(this);
  }
}
