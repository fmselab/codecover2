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

///////////////////////////////////////////////////////////////////////////////
//
// $URL: https://svn.mayastudios.de/sopra/repos/sources/fred/trunk/src/de/sopra06/view/dialogs/project/CDlgProjectSubscribeProjectList.java $
// $Rev: 474 $
// $Date: 2007-02-19 23:46:29 +0100 (Mo, 19 Feb 2007) $
//
///////////////////////////////////////////////////////////////////////////////
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You have received a copy of the GNU General Public License
//  along with this program (license.txt) or you can download it from
//  http://www.gnu.org/licenses/gpl.txt
//
///////////////////////////////////////////////////////////////////////////////

package de.sopra06.view.dialogs.project;

import java.awt.Dimension;
import java.util.Vector;

import javax.swing.AbstractListModel;
import javax.swing.JCheckBox;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;

import de.sopra06.controller.MainController;
import de.sopra06.model.CProjectImpl;
import de.sopra06.model.ProjectList;
import de.sopra06.resources.Labels;
import de.sopra06.view.dialogs.ADialogEx;
import de.wb.view.CSelectedList;

/**
 * Dieser Dialog zeigt eine Liste der auf dem Server befindlichen Projekte an.
 * <p>
 * Dabei werden nur die Projekte angezeigt, die allen folgenden Anforderungen
 * genügen:
 * <ul>
 *   <li>Das Projekt darf noch nicht abboniert wurden sein.</li>
 *   <li>Das Projekt darf noch nicht abgeschlossen sein.</li>
 *   <li>Der Benutzer darf nicht Projektleiter des Projekts sein.</li>
 * </ul><p>
 * 
 * Sind alle Anforderungen erfüllt, wird das Projekt angezeigt. Ansonsten wird
 * es einfach ausgeblendet.
 * 
 * @remarks Um diesen Dialog anzuzeigen, wurde vorher die Dialog 
 *   {@link de.sopra06.view.dialogs.project.CDlgProjectSubscribeProjectURL} 
 *   aufgerufen, in dem der Benutzer die Server-URL angeben musste.
 * 
 * @version 1.0 07.06.2006 erstellt<br>
 */
// TODO Projekt-Liste filterbar und sortierbar machen
@SuppressWarnings("serial")
public class CDlgProjectSubscribeProjectList extends ADialogEx
{
  private CProjectImpl m_oSelectedProject = null;
  private boolean m_bStartSubscribeTasks = false;
  
  private JCheckBox m_oStartSubTasksCheckBox = null;
  private CSelectedList m_oProjectList = null;
  
  /**
   * {Kurzbeschreibung}
   * <br><br>
   * {Beschreibung}
   * 
   * @param p_oProjects Dies ist der Vector der Projekte, die
   * vom Server geschickt wurden. Sie enthält eventuel Projekte,
   * die bereits vom Benutzer abonniert worden sind.
   * @remarks {Anmerkungen}
   * @pre
   * @post
   */
  public CDlgProjectSubscribeProjectList(Vector<CProjectImpl> p_oProjects)
  {
    super(MainController.getInstance().getMainWindow(),
          Labels.get("CDLGPROJECTSUBSCRIBE"),
          Labels.get("CDLGPROJECTSUBSCRIBE_SUBTITLE"),
          "images/dialog-icon-project-server.png", true,
          Labels.get("BUTTON_SUBSCRIBE"),
          ADialogEx.OKCANCEL_BUTTON_FLAG,
          new Dimension(380, 370));
    
    this.getContentPanel().setLayout(null);
    
    this.addLabel(Labels.get("AVAILABLE_PROJECTS") + Labels.get("LABEL_NEWEST_FIRST"), 
                  this.getContentPanel(), true,
                  0, 0, 380, 20);
    
    // Das Model des CSelectedList erzeugen
    CProjectSelectListModel oModel = new CProjectSelectListModel(p_oProjects);
    
    this.m_oProjectList = this.addSelectedList(oModel);
    this.m_oProjectList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    JScrollPane oScrollPane = new JScrollPane(this.m_oProjectList);
    oScrollPane.setBounds(10,30,370,300);
    this.getContentPanel().add(oScrollPane);
    
    this.m_oStartSubTasksCheckBox = new JCheckBox(Labels.get("CHECKBOX_SELECT_TASKS_AFTERWARDS"));
    this.m_oStartSubTasksCheckBox.setBounds(0, 350, 370, 20);
    this.getContentPanel().add(this.m_oStartSubTasksCheckBox);
  }
  

  /**
   * Diese Methode gibt das vom Benutzer ausgewählte Projekt zurück.
   * <br><br>
   * {Beschreibung}
   * 
   * @return null falls der Dialog vom Benutzer abgebrochen wurde.
   * @remarks {Anmerkungen}
   * @pre
   * @post
   */
  public CProjectImpl getProject()
  {
    return this.m_oSelectedProject;
  }

  /**
   * Diese Methode zeigt an, ob der Benutzer direkt im Anschluß an 
   * diesen Use-Case noch Arbeitspakete des Projektes abonnieren möchte.
   * <br><br>
   * {Beschreibung}
   * 
   * @return true = ja Arbeitspakete abonnieren. false = keine 
   * Arbeitspakete abonnieren.
   * @remarks {Anmerkungen}
   * @pre
   * @post
   */
  public boolean startSubscribeTasks()
  {
    return this.m_bStartSubscribeTasks;
  }

  @Override
  public boolean onOK()
  {
    // Gibt es irgendeine Auswahl in der Liste oder hat der Benutzer
    // nichts ausgewählt.
    if (this.evaluateValidationList(true, true) != null)
      return false;

    this.m_oSelectedProject = (CProjectImpl) this.m_oProjectList.getSelectedValue();

    // Ist ein Projekt mit gleichem Namen in der ProjectListe enthalten
    // so soll laut Spezifikation Seite 71 (Alternative Abläufe)
    // ein Nachrichtenfenster angezeigt werden mit der Nachricht
    // "Ein gleichnamiges Projekt ist bereits lokal vorhanden".
    // dies wird vom MainController gemacht.

    this.m_bStartSubscribeTasks = this.m_oStartSubTasksCheckBox.isSelected();
    
    return true;
  }

  /**
   * Logisch gesehen ist das Model einer JList ein Vector der
   * Länge CProjectSelectListModel.getSize(), dessen erstes Element
   * den Index 0 hat.
   *
   * @version 1.0 07.06.2006 erstellt<br>
   */
  private class CProjectSelectListModel extends AbstractListModel
  {
    private Vector<CProjectImpl> m_oProjects = null;
    
    /**
     * Diese Methode nimmt einen Projekt Vektor entgegen, fügt daraus
     * aber nur die Projekte in das Model ein, die nicht bereits
     * vom Benutzer abonniert sind, die nicht bereits abgeschlossen sind
     * oder von denen der Benutzer selbst der Teamleiter ist.
     */
    public CProjectSelectListModel(Vector p_oProjects)
    {
      if (p_oProjects == null || p_oProjects.size() == 0)
        return;

      CProjectImpl  oTempProject  = null;
      int           iSize         = p_oProjects.size();
      
      this.m_oProjects = new Vector<CProjectImpl>(iSize);      
      
      for (int i = 0; i < iSize; i++)
      {
        oTempProject = (CProjectImpl)p_oProjects.elementAt(i);
        
        // Ist das Projekt geschlossen, wird es nicht angezeigt.
        if (oTempProject.isClosed() == true)
          continue;
        
        // Ist der Benutzer Projektleiter dieses Projektes, hat er
        // das Projekt also schon lokal auf der Arbeitsdatei.
        if (oTempProject.isUserProjectLeader() == true)
          continue;
        
        // Wenn das Projekt bereits abboniert ist, dann wird es nicht
        // mehr angezeigt.
        if (ProjectList.getInstance().contains(oTempProject))
          continue;
        
        this.m_oProjects.add(oTempProject);
      }
    }
    
    /**
     * @inheritDoc
     * 
     * @remarks Gefordert von der Klasse {@link AbstractListModel}.
     */
    public Object getElementAt(int p_iIndex)
    {
      // Wir drehen die Reihenfolge der Einträge um. D.h. die neuesten Projekte
      // stehen jetzt oben.
      return this.m_oProjects.elementAt(this.m_oProjects.size() - p_iIndex - 1);
    }
    
    /**
     * @inheritDoc
     * 
     * @remarks Gefordert von der Klasse {@link AbstractListModel}.
     */
    public int getSize()
    {
      if (this.m_oProjects == null)
        return 0;
      
      return this.m_oProjects.size();
    }
    
  } // CProjectSelectListModel
  
} // CDlgProjectSubscribeProjectList
