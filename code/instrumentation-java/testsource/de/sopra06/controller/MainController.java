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
// $URL: https://svn.mayastudios.de/sopra/repos/sources/fred/trunk/src/de/sopra06/controller/MainController.java $
// $Rev: 536 $
// $Date: 2007-05-01 15:57:16 +0200 (Di, 01 Mai 2007) $
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

package de.sopra06.controller;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collection;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.Observable;
import java.util.TreeMap;
import java.util.Vector;
import java.util.Map.Entry;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

import net.mayastudios.mscl.CExecutionTime;
import net.mayastudios.mscl.CResourceLoaderEx;
import net.mayastudios.mscl.mwt.MessageBox;
import net.mayastudios.mscl.mwt.MessageBox.EButton;
import net.mayastudios.mscl.mwt.MessageBox.EIcon;
import net.mayastudios.mscl.mwt.MessageBox.EOptionButtons;
import net.mayastudios.mscl.mwt.dockpane.ITabStateListener.EState;
import net.mayastudios.mscl.mwt.widgets.SplashScreen;
import net.mayastudios.mscl.mwt.widgets.CFloatingWindow.EDockBorder;
import net.mayastudios.mscl.mwt.windowcloser.CWindowCloser;
import net.mayastudios.mscl.mwt.windowcloser.IWindowCloseListener;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import de.sopra06.controller.CServerController.EReplicationJob;
import de.sopra06.controller.CViewController.StatusMask;
import de.sopra06.exceptions.CFredRuntimeException;
import de.sopra06.exceptions.CWorkfileVersionException;
import de.sopra06.exceptions.CXMLException;
import de.sopra06.filehandling.CWorkfile;
import de.sopra06.filehandling.Export;
import de.sopra06.filehandling.FolderUtil;
import de.sopra06.filehandling.RecentFile;
import de.sopra06.filehandling.objectcontainer.AChildlessXMLObjectContainer;
import de.sopra06.filehandling.objectcontainer.AXMLObjectContainer;
import de.sopra06.filehandling.xml.CSimpleXMLStream;
import de.sopra06.filehandling.xml.IXMLizable;
import de.sopra06.model.AFilter;
import de.sopra06.model.CColumnModel;
import de.sopra06.model.CEffortImpl;
import de.sopra06.model.CEffortTree;
import de.sopra06.model.CPersonImpl;
import de.sopra06.model.CProjectImpl;
import de.sopra06.model.CSingleCol;
import de.sopra06.model.CTaskImpl;
import de.sopra06.model.CURLList;
import de.sopra06.model.ObjectFactoryImpl;
import de.sopra06.model.ProjectList;
import de.sopra06.model.AFilter.CCategoryProjectMapper;
import de.sopra06.model.AFilter.CTasksInTreeFilter;
import de.sopra06.model.CColumnModel.EColumnType;
import de.sopra06.model.CEffortImpl.EAccepted;
import de.sopra06.replication.IReplicationService;
import de.sopra06.resources.GlobalSettings;
import de.sopra06.resources.ManualHelper;
import de.sopra06.resources.CategoryList;
import de.sopra06.resources.Configuration;
import de.sopra06.resources.DataMask;
import de.sopra06.resources.DateTime;
import de.sopra06.resources.ID;
import de.sopra06.resources.Labels;
import de.sopra06.resources.Shout;
import de.sopra06.resources.CategoryList.CCategory;
import de.sopra06.resources.Configuration.EConf;
import de.sopra06.resources.configfile.AXMLConfigFile;
import de.sopra06.servertuning.CReplicationServiceEx;
import de.sopra06.view.CMainWindow;
import de.sopra06.view.CQuickBar;
import de.sopra06.view.CView;
import de.sopra06.view.FredStatusBar;
import de.sopra06.view.CMainWindow.EWindowContent;
import de.sopra06.view.dialogs.ADialogEx;
import de.sopra06.view.dialogs.CDlgExtrasGlobalSettings;
import de.sopra06.view.dialogs.CDlgExtrasOptions;
import de.sopra06.view.dialogs.FredMessageBox;
import de.sopra06.view.dialogs.ADialogEx.EReturnValue;
import de.sopra06.view.dialogs.effort.CDlgEffortCollectEffortsProjects;
import de.sopra06.view.dialogs.effort.CDlgEffortCollectNotification;
import de.sopra06.view.dialogs.effort.CDlgEffortEditEffort;
import de.sopra06.view.dialogs.effort.CDlgEffortExportEfforts;
import de.sopra06.view.dialogs.effort.CDlgEffortReplicateNotification;
import de.sopra06.view.dialogs.effort.CDlgEffortExportEfforts.EExportFormat;
import de.sopra06.view.dialogs.file.CDlgFileCreateWorkfile;
import de.sopra06.view.dialogs.file.CDlgFileOpenWorkfile;
import de.sopra06.view.dialogs.file.CDlgFileSaveWorkfileAs;
import de.sopra06.view.dialogs.file.CDlgFileShowProperties;
import de.sopra06.view.dialogs.help.CDlgHelpQuickStart;
import de.sopra06.view.dialogs.help.CDlgHelpShowAbout;
import de.sopra06.view.dialogs.project.CDlgProjectBroadcastProject;
import de.sopra06.view.dialogs.project.CDlgProjectCreate;
import de.sopra06.view.dialogs.project.CDlgProjectProperties;
import de.sopra06.view.dialogs.project.CDlgProjectSubscribeProjectList;
import de.sopra06.view.dialogs.project.CDlgProjectSubscribeProjectURL;
import de.sopra06.view.dialogs.task.CDlgTaskCreateTask;
import de.sopra06.view.dialogs.task.CDlgTaskProperties;
import de.sopra06.view.dialogs.task.CDlgTaskSubscribeTaskList;
import de.sopra06.view.dialogs.task.CDlgTaskSubscribeTaskProject;

/**
 * Die Hauptkontrolle des Programms.<br>
 * <br>
 * Der MainController ist ein Singleton und übernimmt folgende Funktionen:
 * <ul>
 * <li>er initialisiert diverse statische Klassen
 * <li>er stellt einen ResourceLoader bereit {@link #getResLoader()} um Daten
 * aus der jar-Datei laden zu können.
 * <li>er dient als Anlaufstelle für alle Funktionen des Hauptmenüs und der
 * Toolbar
 * <li>er steuert alle Änderungen am Model
 * <li>er lädt das Model aus einer XML-Datei, kann es auch speichern oder eine
 * neue XML-Datei anlegen
 * <li>er enthält Referenzen auf:
 * <ul>
 * <li>den Benutzer des Programms {@link #getUser()}
 * <li>die URLList {@link #getURLList()}
 * <li>den UserMode {@link #m_oUserModeBundle}
 * <li>den Pfad zur letzten Arbeitsdatei {@link #m_oRecentBundle}
 * <li>die Arbeitsdatei {@link #m_oWorkfile}
 * </ul>
 * </ul>
 * 
 * Änderungen: wb
 * Zeile: 493
 * Änderung: 
 * if (p_strPathToFile != null)
 * abgeändert zu
 * if (p_strPathToFile != null && p_strPathToFile != "")
 * Motivation:
 * Eine Recent-Datei mit einem "" Pfad zur Arbeitsdatei zwingt
 * das Programm nicht mehr zum Absturz.
 * 
 * @author sk, wb, cm
 * @version 1.0 20.05.2006 erstellt<br>
 */
public class MainController extends Observable
{
  /** Der Name des Programms - Fred */
  public static final String PROGRAMM_NAME = "• Fred 2007 \u00a9 by S.K, C.M., W.B.";

  /** Das copyright-Zeichen \u00a9 */
  public static final char COPYRIGHT_SIGN = '\u00a9';

  /** Der RAW-Title des Programms für die Titelleiste */
  public static final String PROGRAMM_TITLE_BAR = PROGRAMM_NAME;

  private static MainController s_oInstance = null;

  private CResourceLoaderEx m_oResourceLoader;

  private EWindowContent m_eWindowContent;

  /** Enthält alle ViewController, welche angezeigt werden. */
  private Vector<CViewController> m_oViewControllers;
  
  /** Enthält den ViewController, der gerade sichtbar ist */
  private CViewController m_oCurrentViewController;

  private CMainWindow m_oMainWindow;

  private CURLList m_oURLList;

  private CPersonImpl m_oUser;

  private CTaskImpl m_oDefaultTask;

  private CWorkfile m_oWorkfile;
  
  private String m_strPathToLastDirectory;

  private int m_iCurrentStatusMask;

  private String strLabelNoFileLoaded = "";
  
  @SuppressWarnings("unused")
  private TrayIconController m_oTrayIcon;
  
  /**
   * Konstruktor für MainController (Singleton).
   * <p>
   * 
   * Der Konstruktor tut nichts. Der MainControler wird mit der Methode
   * {@link #initMainController(CResourceLoaderEx)} initialisiert.
   */
  private MainController()
  {
    // no op
  }

  /**
   * Liefert die Instanz des MainControlers zurück.
   * 
   * @return Die Instanz des MainControlers. (Nie <code>null</code>)
   * 
   * @remarks Diese Methode liefert <b>immer</b> die Instanz des MainControllers
   * zurück. Es wird nie <code>null</code> zurückgegeben, solange der Code der
   * Klasse Main und der Code der Methode
   * {@link #initMainController(CResourceLoaderEx)} nicht durcheinander gebracht
   * wird.
   * 
   * @pre Der MainControler wurde initialisiert. (Geschieht durch
   * <code>MainControler.initMainControler(CResourceLoaderEx)</code>)
   */
  public static MainController getInstance()
  {
    return s_oInstance;
  }

  /**
   * Initialisiert den MainControler. Diese Funktion wird von der Funktion
   * Main#createInstance() aufgerufen.
   * 
   * @param p_oResLoader [in] Der Resourcen-Loader, der für das Laden der
   * Resourcen von Fred zuständig ist.
   * 
   * @remarks Zum Initialisieren des MainControlers wird statt dem Konstruktor
   * diese Methode hier verwendet. Der Grund liegt darin, dass bei der
   * Initialisierung bereits auf die Methode {@link #getInstance()}
   * zurückgegriffen wird. Diese ist aber noch nicht verfügbar während der
   * Konstruktor ausgeführt wird, da die Variable, die die Instanz des
   * MainControlers speichert (<code>s_oInstance</code>), erst nach dem
   * Beenden des Konstruktors gefüllt wird. Davor (d.h. insbesondere während der
   * Konstruktor läuft) ist sie <code>null</code>.
   * <p>
   * 
   * (In diesem Fall würde ein Aufruf der Methode <code>getInstance()</code> -
   * je nach Implementierung - entweder zu einer Endlosschleife führen (wenn die
   * Instanz direkt in der Methode <code>getInstance()</code> erzeugt wird)
   * oder (wenn die Instanz direkt bei der Deklaration des Variable erzeugt
   * wird) eine {@link NullPointerException} werfen.)
   * 
   * @post Der MainControler ist initialisiert.
   */
  @SuppressWarnings("synthetic-access")
  public static void initMainController(CResourceLoaderEx p_oResLoader)
  {
    if (s_oInstance != null)
      throw new Error("MainControler has been already initialized!");

    // Als erstes (logischerweise) den MainControler erzeugen
    s_oInstance = new MainController();
    // Dann als zweites unbedingt gleich den Resourcen-Loader erzeugen,
    // da dieser evtl. von den folgenden Initialisierungen benötigt wird.
    s_oInstance.m_oResourceLoader = p_oResLoader;

    // Lege Instanzen von statischen Klassen an
    AXMLConfigFile.initializeSchemaPath();
    Labels.initialize();
    CategoryList.loadFromXML();
    Configuration.initialize();

    //
    // Die Initialisierungen machen
    //
    s_oInstance.m_oMainWindow = new CMainWindow();
    CWindowCloser.initObservation(s_oInstance.m_oMainWindow, 0, new CExitListener());
    
    s_oInstance.setContent(EWindowContent.UnknownStatus);
    TrayIconController.init();
    
    s_oInstance.strLabelNoFileLoaded = Labels.get("NO_FILE_LOADED");
  }

  /**
   * Gibt den ResourceLoader zurück, welcher für URLs, InputStreams und Icons
   * innerhalb der jar-Datei verwendet werden soll.
   * 
   * @return Der ResourceLoader.
   * @pre erst nach {@link #initMainController(CResourceLoaderEx) verfügbar}
   */
  public static CResourceLoaderEx getResLoader()
  {
    return getInstance().m_oResourceLoader;
  }

  /**
   * Lädt die UserMode-Datei, Die Recent-Datei und startet die Verarbeitung der
   * Arbeitsdatei.
   */
  public void start()
  {
    this.m_iCurrentStatusMask = 0;
    
    _initializeRecentPath();

    _initializeWorkfile(RecentFile.getRecentWorkfilePath());

    // assert this.m_oMainWindow.getContent() != EWindowContent.UnknownStatus
    // => das Hauptfenster zeigt den Startbildschirm oder die Views
    if (this.m_oMainWindow.getContent() == EWindowContent.UnknownStatus)
      throw new CFredRuntimeException("Das Hauptfenster hat sich noch nicht angepasst.");
  }

  /**
   * @pre this.m_eUserMode == EUserMode.Undefined
   */
  private void _initializeRecentPath()
  {
    String strPathToWorkFile = RecentFile.getRecentWorkfilePath();

    // Aktualisiere den LastDirectoryPath oder setzte ihn auf das
    // Home-Verzeichnis. Geht auch, wenn strPathToWorkFile "null" ist.
    // TODO Wozu? Wird das nicht gespeichert?
    _setPathToLastDirectory(strPathToWorkFile);

    if (strPathToWorkFile != null && new File(strPathToWorkFile).exists() == false)
    {
      // Letzte Arbeitsdatei existiert scheinbar nicht mehr.
      RecentFile.setRecentWorkfilePath(null);
    }

    Shout.println("m_strPathToWorkFile: " + strPathToWorkFile);
  }

  /**
   * @param p_strPathToWorkFile DEr Pfad zur Arbeitsdatei.
   * @return true &rarr; Initialisieren war erfolgreich; false &rarr;
   * Initialisieren brachte Fehler
   * @pre m_strPathToWorkFile enthält den Pfad der workfile oder ist null
   * @post this.m_oMainWindow.getContent != UnknownStatus
   */
  private boolean _initializeWorkfile(String p_strPathToWorkfile)
  {
    if (p_strPathToWorkfile == null)
    {
      // es gab keine Speicherung der ZuletztVerwendetDatei
      this.setContent(EWindowContent.StartScreen);
      return false;
    }
    else
    {
      // Model initialisieren und Arbeitsdatei laden
      return _loadWorkfile(p_strPathToWorkfile);
    }
  }

  /**
   * Versucht die Arbeitdatei zu laden.<p>
   * 
   * Im Erfolgfall wird das Hauptfenster angepasst, ansonsten wird es auf den 
   * StartScreen zurückgesetzt.
   * 
   * @param p_strPathToWorkFile Der Pfad zur Arbeitsdatei.
   * 
   * @return Liefert:
   * <ul>
   *   <li><code>true</code>: Laden war erfolgreich</li>
   *   <li><code>false<code>: Beim Laden traten Fehler auf</li>
   * </ul>
   * 
   * @pre
   * <ul> 
   *   <li><code>this.m_strPathToWorkFile != null</code></li>
   *   <li><code>this.m_strPathToWorkFile.exists()</code></li>
   * </ul>
   * 
   * @post 
   * <ul>
   *   <li><code>ProjectList.getInstance() != null</code></li>
   *   <li><code>ObjectFactoryImpl.getInstance() != null</code></li>
   *   <li><code>m_oURLList != null</code></li>
   *   <li><code>m_oUser != null</code></li>
   * </ul>
   */
  private boolean _loadWorkfile(String p_strPathToWorkFile)
  {
    CExecutionTime  oTimer            = new CExecutionTime();
    Exception       oCatchedException = null;
    String          strError          = null;
    
    try
    {
      this.m_oWorkfile = null;
      
      oTimer.startTimer();
      this.m_oWorkfile = CWorkfile.load(p_strPathToWorkFile);
      oTimer.stopTimer();
    }
    catch (FileNotFoundException oException)
    {
      strError = Labels.get("INFORMATION_LOADING_ERROR_NOTFOUND");
      oCatchedException = oException;
    }
    catch (CXMLException oException)
    {
      strError = Labels.get("INFORMATION_LOADING_ERROR_PARSING");
      
      if (oException.getContainedException() != null)
        oCatchedException = oException.getContainedException();
      else
        oCatchedException = oException;
    }
    catch (IOException oException)
    {
      strError = Labels.get("INFORMATION_LOADING_ERROR_IO");
      oCatchedException = oException;
    }  
    catch (CWorkfileVersionException oException)
    {
      strError = oException.getMessage();
      oCatchedException = oException;
    }
    
    if (oCatchedException != null)
    {
      //
      // Es gab einen Fehler! Raus hier!
      //
      Shout.printException(oCatchedException);
      FredMessageBox.showInfoDlg(String.format(Labels.get("INFORMATION_LOADING_ERROR"),
                                               p_strPathToWorkFile,
                                               strError), 
                                 Labels.get("TITLE_OPENING_WORKFILE"), 
                                 EIcon.ERROR,
                                 oCatchedException);

      _clearEverything();
      return false;
    }
    
    FredStatusBar.setTextForInterval(String.format(Labels.get("STATUSBAR_WORKFILE_LOADED"),
                                                   DateTime.getExactDurationString(oTimer.getTimeDiffInMillis())));
    

    if (this.m_oViewControllers == null)
      MainController.getInstance().m_oViewControllers = new Vector<CViewController>();

    setContent(EWindowContent.Views);

    if (this.m_oViewControllers.size() == 0)
      incomingViewNewView();

    return true;
  }

  /**
   * Folgendes passiert:
   * <ul>
   * <li>es wird das Modell geleert
   * <ul>
   * <li>ProjectList.clearInstance();
   * <li>ObjectFactoryImpl.clearInstance();
   * <li>this.m_oURLList = null;
   * <li>this.m_oUser = null;
   * <li>this.m_oDefaultTask = null;
   * <li>this.m_Workfile = null;
   * </ul>
   * <li>alle Views beendet
   * <ul>
   * <li>this.m_oViewControllers = null;
   * <li>this.m_oCurrentViewController = null;
   * </ul>
   * <li>der WindowContent auf "StartScreen" gesetzt
   * <li>m_bMadeChanges auf false gesetzt
   * <li>der "garbage collector" aufgerufen
   * </ul>
   */
  private void _clearEverything()
  {
    ProjectList.clearInstance();
    ObjectFactoryImpl.clearInstance();
    this.m_oURLList = null;
    this.m_oUser = null;
    this.m_oDefaultTask = null;
    this.m_oWorkfile = null;

    this.m_oViewControllers = null;
    this.m_oCurrentViewController = null;

    setContent(EWindowContent.StartScreen);

    Runtime.getRuntime().runFinalization();
    System.gc();
  }

  /**
   * speichert sich den Pfad zum Ordner der letzten verarbeiteten Arbeitsdatei
   * 
   * @param p_strPathToFile Die Datei, oder der Ordner, der verarbeitet wurde.
   */
  private void _setPathToLastDirectory(String p_strPathToFile)
  {
    if (p_strPathToFile != null && p_strPathToFile != "")
    {
      File oRecentFile = new File(p_strPathToFile);
      if (oRecentFile.isDirectory())
      {
        this.m_strPathToLastDirectory = oRecentFile.getAbsolutePath();
        return;
      }

      File oRecentDirectory = oRecentFile.getParentFile();
      if (oRecentDirectory.isDirectory() && oRecentDirectory.exists())
      {
        this.m_strPathToLastDirectory = oRecentDirectory.getAbsolutePath();
        return;
      }
    }

    // wird nur erreicht, wenn kein neuer Pfad gesetzt werden konnte
    if (this.m_strPathToLastDirectory == null)
      this.m_strPathToLastDirectory = FolderUtil.getHomeFolder();
  }

  /**
   * Setzt einen ViewController an die Spitze der angezeigten ViewControllers<br>
   * <br>
   * Existiert der ViewController bereits in der Liste, so wird er erst gelöscht
   * und dann an der Spitze neu eingefügt.
   * 
   * @param p_oVisibleController Der jetzt sichtbare ViewController.
   */
  private void _addViewControllerToTop(CViewController p_oVisibleController)
  {
    if (this.m_oCurrentViewController != p_oVisibleController)
    {
      if (!this.m_oViewControllers.contains(p_oVisibleController))
        this.m_oViewControllers.addElement(p_oVisibleController);

      this.m_oCurrentViewController = p_oVisibleController;
      
      // Benachrichtige Menü wegen neuer Ansicht
      p_oVisibleController.updateNotification(EUpdateMessage.BecomesVisible, null, true);
    }
  }
  
  private void _removeViewController(CViewController p_oController)
  {
    this.m_oViewControllers.remove(p_oController);
    
    if (this.m_oCurrentViewController == p_oController)
      this.m_oCurrentViewController = null;
  }

  /**
   * Gibt den gerade angezeigten ViewController zuück.<br>
   * <br>
   * Wird gerade keine CView angezeigt oder ist der {@link #m_eWindowContent}
   * nicht auf Views, dann wird null zurückgegeben.
   * 
   * @return Der aktuell sichtbare ViewController.
   */
  private CViewController _getCurrentViewController()
  {
    if (this.m_eWindowContent == EWindowContent.Views)
    {
      return this.m_oCurrentViewController;
    }
    
    return null;
  }
  
  private boolean _shutDown()
  {
    return _closeWorkfile();
  }
  
  private String _getErrorMessageByStatusShort(int iState)
  {
    switch (iState)
    {
      case IReplicationService.STATUS_ERROR:
        return Labels.get("INFORMATION_SERVER_ERROR_SHORT");
      case IReplicationService.STATUS_PROJECT_CLOSED:
        return Labels.get("INFORMATION_SERVER_PROJECT_CLOSED_SHORT");
      case IReplicationService.STATUS_INIT:
        return Labels.get("INFORMATION_SERVER_INIT_SHORT");
      case CServerController.STATUS_REPLICATION_EXCEPTION:
        return Labels.get("INFORMATION_SERVER_EXCEPTION_SHORT");
      case CServerController.STATUS_USER_HAS_ABORTED:
        return Labels.get("INFORMATION_SERVER_USER_HAS_ABORTED_SHORT");
    }
    
    return "";
  }
  
  private void _refreshTitleBar()
  {
    String strTitleBar; 
    
    if (this.m_eWindowContent == EWindowContent.Views)
    {
      if (this.m_oWorkfile.hasChanged())
      {
        strTitleBar = String.format("%s - %s an %s *",
          PROGRAMM_TITLE_BAR,
          getUser().toString(),
          new File(getPathToWorkFile()).getName()
        );
      }
      else
      {
        strTitleBar = String.format("%s - %s an %s",
            PROGRAMM_TITLE_BAR,
            getUser().toString(),
            new File(getPathToWorkFile()).getName()
          );
      }
    }
    else
    {
      strTitleBar = String.format("%s - %s",
        PROGRAMM_TITLE_BAR,
        this.strLabelNoFileLoaded
      );
    }
    
    getMainWindow().setTitle(strTitleBar);
  }
  
  void publishStatusMask()
  {
    this.m_iCurrentStatusMask = 0;
    
    if (this.m_eWindowContent == EWindowContent.Views)
    {
      CViewController oCurrentViewController = _getCurrentViewController();
      ProjectList oProjectList = ProjectList.getInstance();
      
      if (oCurrentViewController != null)
        this.m_iCurrentStatusMask = oCurrentViewController.getStatusMask();
      
      // Werden gerade Views angezeigt
      this.m_iCurrentStatusMask = DataMask.setBit(this.m_iCurrentStatusMask, StatusMask.BIT_SHOW_VIEWS,
        true);
  
      // Ist mindestens eine View geöffnet?
      this.m_iCurrentStatusMask = DataMask.setBit(this.m_iCurrentStatusMask, StatusMask.BIT_VIEW_AVAILABLE,
        this.m_oViewControllers != null && this.m_oViewControllers.size() > 0);
      
      // Ist ein lokales, nicht abgeschlossenes Projekt vorhanden?
      this.m_iCurrentStatusMask = DataMask.setBit(this.m_iCurrentStatusMask,
        StatusMask.BIT_LOCAL_PROJECT_AVAILABLE,
        oProjectList.existsBroadcastableProject());
      
      // Ist ein bereitgestelltest nicht abgeschlossenes Projekt vorhanden?
      this.m_iCurrentStatusMask = DataMask.setBit(this.m_iCurrentStatusMask,
        StatusMask.BIT_BROADCASTED_PROJECT_AVAILABLE,
        oProjectList.existsBroadcastedUnclosedProject());
      
      // Ist ein bereitgestelltes, nicht abgeschlossenes AP vorhanden?
      this.m_iCurrentStatusMask = DataMask.setBit(this.m_iCurrentStatusMask,
        StatusMask.BIT_BROADCASTED_TASK_AVAILABLE,
        oProjectList.existsBroadcastedUnclosedTask());
      
      //  Existiert mindestens 1 noch nicht abgeschlossenes Projekt,
      //  von dem ein Task erstellt werden könnte
      this.m_iCurrentStatusMask = DataMask.setBit(this.m_iCurrentStatusMask, StatusMask.BIT_PROJEKT_AVAILABLE,
        oProjectList.getProjectForTaskCreation() != null);
      
      // existiert mindestens ein Task, auf den ein Aufwand gebucht werden könnte
      this.m_iCurrentStatusMask = DataMask.setBit(this.m_iCurrentStatusMask, StatusMask.BIT_TASK_AVAILABLE,
        this.getDefaultTask() != null);
    }
    else
    {
      this.m_iCurrentStatusMask = 0;
      this.m_iCurrentStatusMask = DataMask.setBit(this.m_iCurrentStatusMask, StatusMask.BIT_SHOW_VIEWS, false);
    }

    notifyObservers();
  }

  /**
   * Setzt das Bit für MADE_CHANGES der StatusMask und benachrichtigt alle Observer.
   */
  @Override
  public void notifyObservers()
  {
    boolean m_bChanged = (this.m_oWorkfile != null)
      && this.m_oWorkfile.hasChanged();
    
    this.m_iCurrentStatusMask = DataMask.setBit(this.m_iCurrentStatusMask, StatusMask.BIT_MADE_CHANGES,
      m_bChanged);
    
    super.setChanged();
    
    // Shout.println(">" + Integer.toBinaryString(this.m_iCurrentStatusMask));
    this.notifyObservers(new Integer(this.m_iCurrentStatusMask));
    
    super.clearChanged();
    
    _refreshTitleBar();
  }
  
  /**
   * Informiert alle ViewController darüber, dass sich Objekte geändert haben.<br>
   * <br>
   * Dies können z.B. Änderungen am Model sein. Außerdem wird das Model als
   * geändert gesetzt.
   * 
   * @param p_eMessage Was hat sich geändert?
   * @param p_oChangedArg Die geänderte(n) Objekte.
   */
  public void notifyControllers(EUpdateMessage p_eMessage, Collection<?> p_oChangedArg)
  {
    if (this.m_eWindowContent == EWindowContent.Views)
    {
      CViewController oCurrentViewController;
      CViewController oThisViewController;
      
      // erst das Update für den aktuell angezeigten.
      oCurrentViewController = _getCurrentViewController();
      if (oCurrentViewController != null)
        oCurrentViewController.updateNotification(p_eMessage, p_oChangedArg, true);
      
      // dann das Update für die anderen. 
      Iterator<CViewController> oIteratorViewControllers = this.m_oViewControllers.iterator();
      while (oIteratorViewControllers.hasNext())
      {
        oThisViewController = oIteratorViewControllers.next();
        if (oThisViewController != oCurrentViewController)
          oThisViewController.updateNotification(p_eMessage, p_oChangedArg, false);
      }
      
      if (oCurrentViewController != null)
        oCurrentViewController.updateNotification(EUpdateMessage.VisualUpdate, null, true);
      
      this.m_oWorkfile.setChanged();
      
      notifyObservers();
    }
  }

  /**
   * Gibt den aktuellen Content des Hauptfensters zurück.
   * 
   * @return Der aktuelle Content.
   */
  public EWindowContent getContent()
  {
    return this.m_eWindowContent;
  }

  /**
   * Setzt den Content des Hauptfensters.
   * 
   * @param p_eContent Der Content des Hauptfensters.
   */
  public void setContent(EWindowContent p_eContent)
  {
    if (this.m_eWindowContent == p_eContent)
      return;

    this.m_eWindowContent = p_eContent;
    this.m_oMainWindow.setContent(this.m_eWindowContent);

    // Benachrichtige Menü wegen neuer Ansicht
    publishStatusMask();
    
    // Aktualisiere die Titelleiste.
    _refreshTitleBar();
  }

  /**
   * Gibt den Benutzer des Programms zurück.
   * 
   * @return Der Benutzer des Programms.
   */
  public CPersonImpl getUser()
  {
    return this.m_oUser;
  }

  /**
   * Gibt den Benutzer des Programms in einem Vektor zurück.
   * 
   * @return Der Benutzer des Programms.
   */
  public Vector<CPersonImpl> getUserVector()
  {
    Vector<CPersonImpl> oVectorReturn = new Vector<CPersonImpl>(1);
    oVectorReturn.add(getUser());
    return oVectorReturn;
  }

  
  /**
   * Gibt den Default-Task zurück.
   * 
   * @return Der Default-Task, falls gesetzt; sonst <b>null</b>
   */
  public CTaskImpl getDefaultTask()
  {
    return this.m_oDefaultTask;
  }

  /**
   * Setzt den Default-Task.
   * 
   * @param p_oDefaultTask Der neue Default-Task.
   */
  public void setDefaultTask(CTaskImpl p_oDefaultTask)
  {
    this.m_oDefaultTask = p_oDefaultTask;
  }

  /**
   * Gibt den Pfad zur Arbeitsdatei zurück.
   * 
   * @return Der Pfad zur Arbeitsdatei.
   */
  public String getPathToWorkFile()
  {
    return this.m_oWorkfile.getPathToWorkFile();
  }

  /**
   * Gibt das Hauptfenster zurück.
   * 
   * @return Das Hauptfenster.
   */
  public CMainWindow getMainWindow()
  {
    if (this.m_oMainWindow == null)
      Shout.println("Wolfgang sach: Referenz auf MainWindow ist null");
    
    return this.m_oMainWindow;
  }

  /**
   * Gibt die URL-Liste zurück.
   * 
   * @return Die URL-Liste.
   */
  public CURLList getURLList()
  {
    return this.m_oURLList;
  }
  
  /**
   * Mit dem int den diese Methode liefert beschreibt der MainController
   * den Zustand in dem sich das Programm gerade befindet. 
   * <br><br>
   * Verwenden Sie diesen Integer um z.B. die Menüsichtbarkeiten
   * an den Zustand des Programms anzupassen. 
   * 
   * @return Die aktuelle StatusMask,
   */
  public int getCurrentStatusMask()
  {
    return this.m_iCurrentStatusMask;
  }
  
  /**
   * Gibt das Erstelldatum zurück.
   * 
   * @return Das Erstelldatum.
   */
  public GregorianCalendar getCreatedAt()
  {
    return this.m_oWorkfile.getCreatedAt();
  }
  
  /**
   * Gibt das Datum des letzten Speicherns zurück.
   * 
   * @return Das Datum des letzten Speicherns.
   */
  public GregorianCalendar getLastSaved()
  {
    return this.m_oWorkfile.getLastSaved();
  }

  /**
   * Erstellt eine Instanz der URLListe, falls sie noch nicht existiert.
   */
  public void createURLList()
  {
    if (this.m_oURLList == null)
      this.m_oURLList = new CURLList();
  }
  
  /**
   * Erstellt eine neue Arbeitsdatei.<br>
   * Erstellt das Model und öffnete eine View. <br>
   * Falls noch eine Arbeitsdatei geöffnet ist, so muss diese geschlossen
   * werden.<br>
   * Die Konfiguration wird auf die Standard-Konfiguration zurückgesetzt.
   * {@link Configuration#initialize()}
   * 
   * @post IM ERFOLGSFALL<br>
   * ProjectList.getInstance() != null <br>
   * ObjectFactoryImpl.getInstance() != null <br>
   * m_oURLList != null <br>
   * m_oUser != null <br>
   * m_oWorkfile != null <br>
   * this.m_eWindowContent == Views <br>
   * this.m_oViewControllers != null <br>
   */
  public static void incomingFileCreateWorkfile()
  {
    // Versuche, die alte Sitzung zu beenden
    if (s_oInstance._closeWorkfile() == false)
      return;

    // assert
    // ProjectList.getInstance() == null<br>
    // ObjectFactoryImpl.getInstance() == null<br>
    // this.m_oURLList == null<br>
    // this.m_oUser == null<br>
    // this.m_oDefaultTask == null<br>
    // this.m_oWorkfile == null<br>
    // <br>
    // this.m_oViewControllers == null<br>

    // Erstelle eine neue Sitzung.
    CDlgFileCreateWorkfile oCreateDialog = new CDlgFileCreateWorkfile();

    if (oCreateDialog.showDialog() == EReturnValue.OK)
    {
      ObjectFactoryImpl.createInstance();
      ProjectList.createInstance();
      Configuration.initialize();

      s_oInstance.m_oUser = CPersonImpl.create();
      s_oInstance.m_oUser.setFirstName(oCreateDialog.getUserFirstName());
      s_oInstance.m_oUser.setName(oCreateDialog.getUserName());
      s_oInstance.m_oUser.setInitials(oCreateDialog.getUserInitials());
      s_oInstance.m_oURLList = new CURLList();

      String strPathToWorkfile = oCreateDialog.getWorkFilePath();
      s_oInstance._setPathToLastDirectory(strPathToWorkfile);

      s_oInstance.m_oViewControllers = new Vector<CViewController>();
      
      s_oInstance.m_oWorkfile = CWorkfile.createWorkfile(strPathToWorkfile);
      if (s_oInstance.m_oWorkfile == null)
      {
        s_oInstance._clearEverything();
        return;
      }

      s_oInstance.setContent(EWindowContent.Views);
        
      incomingViewNewView();
        
      FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_WORKFILE_CREATED"));
    }
  }

  /**
   * Öffnet eine neue Arbeitsdatei.<br>
   * <br>
   * Falls noch eine Arbeitsdatei geöffnet ist, so muss diese geschlossen
   * werden.<br>
   * Es wird überprüft, ob m_strPathToLastDirectory auf ein Verzeichnis
   * verweist. <br>
   * Nach dem Erfragen der Datei wird {@link #_setPathToLastDirectory}
   * aufgerufen und die Workfile geladen: {@link #_initializeWorkfile(String)}
   */
  public static void incomingFileOpenWorkfile()
  {
    // Versuche, die alte Sitzung zu beenden
    if (s_oInstance._closeWorkfile() == false)
      return;

    // assert
    // ProjectList.getInstance() == null<br>
    // ObjectFactoryImpl.getInstance() == null<br>
    // this.m_oURLList == null<br>
    // this.m_oUser == null<br>
    // this.m_oDefaultTask == null<br>
    // this.m_strPathToWorkFile == null<br>
    // this.m_oDateCreated == null<br>
    // <br>
    // this.m_oViewControllers == null<br>

    CDlgFileOpenWorkfile oOpenDialog;

    if (s_oInstance.m_strPathToLastDirectory == null)
      oOpenDialog = new CDlgFileOpenWorkfile();
    else
      oOpenDialog = new CDlgFileOpenWorkfile(s_oInstance.m_strPathToLastDirectory);

    String strPathToWorkfile = oOpenDialog.getWorkFilePath();
    
    if (strPathToWorkfile == null)
      return;

    s_oInstance._setPathToLastDirectory(strPathToWorkfile);
    if (s_oInstance._initializeWorkfile(strPathToWorkfile) == true)
      RecentFile.setRecentWorkfilePath(strPathToWorkfile);
  }

  /**
   * Speichert das gesamte Model und die gesamte Konfiguration<br>
   * <br>
   * Genauer: {@link CXMLRootElementModel}
   * 
   * @return true &rarr; Das Speichern war erfolgreich; false &rarr; Das
   * Speichern führte zu Fehlern.
   * @remarks Es wird auch gespeichert wenn keine Änderungen gemacht worden sind.
   */
  public static boolean incomingFileSaveWorkfile()
  {
    CExecutionTime oTimer = new CExecutionTime();
    
    oTimer.startTimer();
    if (s_oInstance.m_oWorkfile.save() == true)
    {
      oTimer.stopTimer();
      s_oInstance.notifyObservers();

      FredStatusBar.setTextForInterval(String.format(Labels.get("STATUSBAR_WORKFILE_SAVED"),
                                                     DateTime.getExactDurationString(oTimer.getTimeDiffInMillis())));
      return true;
    }

    FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_WORKFILE_NOT_SAVED"));    
    return false;
  }

  /**
   * {Kurzbeschreibung}<br>
   * <br>
   * Dieser Menüpunkt kann auch aktiviert werden, falls die Arbeitsdatei
   * zuvor niemals mit Save Workfile gespeichert wurde, d.h. Sie muss
   * dann eventuell angelegt werden. Falsch, da der Menüpunkt Save und
   * Save as immer nur aktiviert ist, wenn zuvor eine Workfile angelegt
   * worden ist also bereits in jedem Fall existiert.
   * 
   * Situation A:
   * Bisher ist eine Workfile geöffnet und angezeigt.
   * Aktion A: 
   * Speichere diese Datei unter neuem Namen an einem neuen Ort.
   * Speichere die Änderungen nicht in der bisher geöffneten Datei.
   * Schließe die bisher geöffnete Datei.
   * Öffne die neue Datei.
   * 
   * Situation B:
   * Bisher ist keine Workfile gespeichert worden. Diese Situation 
   * taucht niemals auf.
   * 
   * @remarks {Anmerkungen}
   * @pre
   * @post
   */
  public static void incomingFileSaveWorkfileAs()
  {
    CWorkfile oOldWorkfile = s_oInstance.m_oWorkfile;
    
    // Sammle den Pfad zur neuen Datei.
    CDlgFileSaveWorkfileAs oSaveAsDialog = 
      new CDlgFileSaveWorkfileAs(s_oInstance.m_strPathToLastDirectory);

    if (oSaveAsDialog.wasSuccessful())
    {
      s_oInstance.m_oWorkfile = s_oInstance.m_oWorkfile.clone(oSaveAsDialog.getWorkFilePath());

      if (incomingFileSaveWorkfile())
      {
        s_oInstance.notifyObservers();
        s_oInstance._setPathToLastDirectory(s_oInstance.m_oWorkfile.getPathToWorkFile());
      }
      else
      {
        s_oInstance.m_oWorkfile = oOldWorkfile;
      }
    }
  }

  /**
   * Dateieigenschaften anzeigen.
   */
  public static void incomingFileShowProperties()
  {
    // Der Dialog benötigt den Benuzter und die Arbeitsdatei als File Objekt.
    CDlgFileShowProperties.showDialog(s_oInstance.getUser(), new File(s_oInstance.getPathToWorkFile()));
  }
  
  /**
   * Versucht die aktuelle Arbeitsdatei zu schließen.
   * 
   * @return true, falls:
   * <ul>
   * <li>keine Arbeitsdatei geladen ist
   * <li>an der aktuellen Datei keine Änderungen gemacht worden sind
   * <li>Änderungen erfolgreich gespeichert werden konnten  
   * </ul>
   * false, falls
   * <ul>
   * <li>es zu Fehlern beim Schließen kam
   * </ul> 
   */
  public static boolean incomingFileClose()
  {
    if (s_oInstance._closeWorkfile() == false)
      return false;
    
    // Wir setzen den Recent-Pfad auf leer, damit sich beim neuen Starten von
    // Fred nicht die gerade geschlossene Arbeitsdatei wieder öffnet.
    RecentFile.setRecentWorkfilePath("");
    
    return true;
  }
  
  private boolean _closeWorkfile()
  {
    if (this.m_eWindowContent != EWindowContent.Views)
      return true;
    
    // Wenn mit der Quickbar momentan noch ein Aufwand aufgezeichnet wird,
    // dann fragen wir den Benutzer, was damit geschehen soll.
    //
    // Da die Quickbar unabhängig von der momentanen Ansicht ist, wird diese
    // hier nicht mit "this.m_oMainWindow.getContent()" abgefragt.
    //
    if (CQuickBar.getInstance().isRecording() == true)
    {
      String astrOptions[] = {Labels.get("BUTTON_MEASURE"),
                              Labels.get("BUTTON_DISCARD"),
                              Labels.get("BUTTON_CANCEL")};
      
      int iReturn = JOptionPane.showOptionDialog(getMainWindow(),
                                                 Labels.get("OPTION_QUICKBAR_ONCLOSE"),
                                                 Labels.get("OPTION_QUICKBAR_ONCLOSE_TITLE"),
                                                 JOptionPane.YES_NO_CANCEL_OPTION,
                                                 JOptionPane.QUESTION_MESSAGE,
                                                 null,
                                                 astrOptions,
                                                 astrOptions[0]);
      
      switch (iReturn)
      {
      // Aufwand erfassen
      case JOptionPane.YES_OPTION:
        if (CQuickBar.getInstance().stopRecording(false) == false)
          return false;
        break;
        
      // Aufwand verwerfen
      case JOptionPane.NO_OPTION:
        CQuickBar.getInstance().stopRecording(true);
        break;
      
      // Abbrechen
      case JOptionPane.CANCEL_OPTION:
        return false;
      }
    }
    
    if (this.m_oWorkfile.close() == false)
      return false;

    // Datei kann geschlossen werden und das gesamte Model geleert werden.
    _clearEverything();

    return true;
  }
  
  /**
   * Soll das Programm beenden.
   */
  public static void incomingFileExit()
  {
    if (s_oInstance._closeWorkfile() == true)
      CWindowCloser.closeWindow(s_oInstance.m_oMainWindow);
  }

  /**
   * Erstellt eine neue View.
   * 
   * @pre this.m_oViewControllers != null<br>
   * this.m_eWindowContent = Views
   */
  public static void incomingViewNewView()
  {
    s_oInstance._newViewByFilter(null);
  }
  
  private void _newViewByFilter(AFilter p_oFilter)
  {
    _newViewByFilter(p_oFilter, null);
  }

  private void _newViewByFilter(AFilter p_oFilter, String p_strName)
  {
    new CViewController(p_oFilter, p_strName);
  }
  
  /**
   * Die aktuelle View duplizieren.
   */
  public static void incomingViewDuplicateView()
  {
    s_oInstance._getCurrentViewController().duplicate();
  }

  /**
   * Schließe die aktuelle View
   */
  public static void incomingViewCloseView()
  {
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
    if (oCurrentViewController != null)
      s_oInstance.m_oMainWindow.removeView(oCurrentViewController.getView());
  }

  /**
   * Das Tab wurde geschlossen.
   * @param p_oViewController 
   */
  public void eventViewClosed(CViewController p_oViewController)
  {
    _removeViewController(p_oViewController);
    
    CViewController oCurrentViewController = _getCurrentViewController();
    
    if (oCurrentViewController != null)
    {
      // Benachrichtige Menü wegen neuer Ansicht
      oCurrentViewController.updateNotification(EUpdateMessage.BecomesVisible, null, true);
      _addViewControllerToTop(oCurrentViewController);
    }
    else
    {
      FredStatusBar.resetEffortData();
      publishStatusMask();
    }
    
    Shout.println(p_oViewController.toString() + " closed >> " + this.m_oViewControllers.toString());
  }
  
  /**
   * {Kurzbeschreibung}<br>
   * <br>
   * {Beschreibung}
   * 
   * @remarks {Anmerkungen}
   * @pre
   * @post
   */
  public static void incomingViewQuickbar()
  {
    CQuickBar oQuickbar = CQuickBar.getInstance();
    
    oQuickbar.setVisible(!oQuickbar.isVisible());
    s_oInstance.getMainWindow().getViewContainer().getToolbar().updateQuickbarButton();
  }

  /**
   * Passt die aktuelle Ansicht an.
   */
  public static void incomingViewCustomizeColumns()
  {
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
    if (oCurrentViewController != null)
      oCurrentViewController.incomingViewCustomizeColumns();
  }

  /**
   * Eine neues Projekt soll erstellt werden.
   */
  public static void incomingProjectCreate()
  {
    if (s_oInstance.m_eWindowContent == EWindowContent.Views)
    {
      CDlgProjectCreate oCreateDialog = new CDlgProjectCreate();

      if (oCreateDialog.showDialog() == ADialogEx.EReturnValue.OK)
      {
        CProjectImpl oNewProject = oCreateDialog.getProject();

        ProjectList.getInstance().addProject(oNewProject, true);
        
        // informiere alle ViewController
        s_oInstance.notifyControllers(EUpdateMessage.ProjectCreated, new CSingleCol<CProjectImpl>(oNewProject));
        
        FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_PROJECT_CREATED"));
      }

      return;
    }

    Shout.newError(s_oInstance.getClass().getName() + "#incomingProjectCreate",
      "should not be usable in this configuration");
  }

  /**
   * Projekteigenschaften anzeigen.
   */
  public static void incomingProjectProperties()
  {
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
    
    if (oCurrentViewController != null)
    {
      CProjectImpl oProjectToEdit = oCurrentViewController.getSelectedProject();

      if (oProjectToEdit != null)
      {
        CDlgProjectProperties oProjectProps = new CDlgProjectProperties(oProjectToEdit);
        
        if (oProjectProps.showDialog() == EReturnValue.OK)
        {
          if (oProjectToEdit.isClosed() == false && oProjectProps.isClosed() == true)
            incomingProjectClose();
        
          // informiere alle ViewController
          s_oInstance.notifyControllers(EUpdateMessage.ProjectUpdated, 
                                        new CSingleCol<CProjectImpl>(oProjectToEdit));
          FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_PROJECT_EDITED"));
        }

        return;
      }
    }

    Shout.newError(s_oInstance.getClass().getName() + "#incomingProjectProperties",
      "should not be usable in this configuration");
  }

  /**
   * Schließt das angebene Projekt.
   */
  public static void incomingProjectClose()
  {
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
    
    if (oCurrentViewController != null)
    {
      CProjectImpl oProjectToClose = oCurrentViewController.getSelectedProject();

      if (oProjectToClose != null)
      {
        if (oProjectToClose.isClosed())
        {
          FredMessageBox.showInfoDlg(Labels.get("INFORMATION_PROJECT_ALREADY_CLOSED") + "\n" + oProjectToClose.toString(),
                                     Labels.get("TITLE_CLOSE_PROJECT"),
                                     EIcon.WARNING);
          return;
        }
        else if (!oProjectToClose.isUserProjectLeader())
        {
          FredMessageBox.showInfoDlg(Labels.get("INFORMATION_CLOSE_NOT_PROJECTLEADER") + "\n" + oProjectToClose.toString(),
                                     Labels.get("TITLE_CLOSE_PROJECT"),
                                     EIcon.ERROR);
          return;
        }
        else if (s_oInstance.m_oDefaultTask != null
          && oProjectToClose == s_oInstance.m_oDefaultTask.getProject()
          && ProjectList.getInstance().isUnclosedTaskAvailableExcProject(oProjectToClose))
        {
          FredMessageBox.showInfoDlg(Labels.get("INFORMATION_CLOSE_PROJECT_OF_DEFAULTTASK") + "\n" + oProjectToClose.toString(),
                                     Labels.get("TITLE_CLOSE_PROJECT"),
                                     EIcon.ERROR);
          return;
        }
        else
        {
          EButton oButton = FredMessageBox.showOptionDlg(Labels.get("CONFIRM_CLOSE_PROJECT") + "\n" + oProjectToClose.toString(),
                                                         Labels.get("TITLE_CLOSE_PROJECT"),
                                                         EIcon.QUESTION,
                                                         EOptionButtons.YES_NO);

          if (oButton == EButton.YES)
          {
            if (oProjectToClose.isTeamproject())
            {
              //teile es dem Server mit
              CReplicationServiceEx oRS = new CReplicationServiceEx();
              oRS.setURL(oProjectToClose.getServerURL());
              oRS.setObjectFactory(ObjectFactoryImpl.getInstance());
              oRS.setProjects(oProjectToClose.toVector());
              oRS.setPersons(s_oInstance.getUserVector());
              
              CServerController oServerController = new CServerController(oRS, EReplicationJob.closeProject);
              oServerController.start();
              
              if (oServerController.getStatus() != IReplicationService.STATUS_OK)
              {
                s_oInstance._checkAndCloseProject(oServerController.getStatus(), oProjectToClose);
              }
              else
              {
                FredMessageBox.showInfoDlg(Labels.get("INFORMATION_PROJECT_CLOSED_ON_SERVER"),
                                           Labels.get("TITLE_CLOSE_PROJECT"),
                                           EIcon.INFORMATION);
                
                oProjectToClose.close();

                if (s_oInstance.m_oDefaultTask != null && oProjectToClose == s_oInstance.m_oDefaultTask.getProject())
                  s_oInstance.m_oDefaultTask = null;
                
                s_oInstance.notifyControllers(EUpdateMessage.ProjectUpdated, new CSingleCol<CProjectImpl>(oProjectToClose));
                FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_PROJECT_CLOSED"));
              }
            }
            else
            {
              oProjectToClose.close();

              if (s_oInstance.m_oDefaultTask != null && oProjectToClose == s_oInstance.m_oDefaultTask.getProject())
                s_oInstance.m_oDefaultTask = null;

              s_oInstance.notifyControllers(EUpdateMessage.ProjectUpdated, new CSingleCol<CProjectImpl>(oProjectToClose));
              FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_PROJECT_CLOSED"));
            }
          }

          return;
        }
      }
    }

    Shout.newError(s_oInstance.getClass().getName() + "#incomingProjectProperties",
      "should not be usable in this configuration");
  }

  /**
   * Überprüft den Serverfehlerstatus und schließt evt. das Projekt.<br>
   * </br>
   * Es werden die ViewController benachrichtigt.
   * 
   * @param p_iStatus {@link CServerController#getStatus()}
   * @param p_oItsProject Das betreffende Projekt, mit dem der Repliziervorgang
   * gestartet wurde.
   */
  private void _checkAndCloseProject(int p_iStatus, CProjectImpl p_oItsProject)
  {
    if (p_iStatus == IReplicationService.STATUS_PROJECT_CLOSED
      && !p_oItsProject.isClosed())
    {
      p_oItsProject.setClosed(true);
      notifyControllers(EUpdateMessage.ProjectUpdated,
        new CSingleCol<CProjectImpl>(p_oItsProject));
    }
  }

  /**
   * Löscht das ausgewählte Project
   */
  public static void incomingProjectDeleteProject()
  {
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
    
    if (oCurrentViewController != null)
    {
      CProjectImpl oProjectToDelete = oCurrentViewController.getSelectedProject();

      if (oProjectToDelete != null)
      {
        // soll der Defaulttask gelöscht werden?
        if (   s_oInstance.m_oDefaultTask == null
            || oProjectToDelete != s_oInstance.m_oDefaultTask.getProject()
            || ProjectList.getInstance().isUnclosedTaskAvailableExcProject(oProjectToDelete) == false)
        {
          EButton eButton = FredMessageBox.showOptionDlg(Labels.get("CONFIRM_DELETE_PROJECT") + "\n" + oProjectToDelete.toString(),
                                                         Labels.get("TITLE_DELETE_PROJECT"),
                                                         EIcon.QUESTION,
                                                         EOptionButtons.YES_NO);
          if (eButton == EButton.YES)
          {
            if (s_oInstance.m_oDefaultTask != null && oProjectToDelete == s_oInstance.m_oDefaultTask.getProject())
              s_oInstance.m_oDefaultTask = null;

            ProjectList.getInstance().removeProject(oProjectToDelete);
            ObjectFactoryImpl.destroy(oProjectToDelete);
            // informiere alle ViewController
            s_oInstance.notifyControllers(EUpdateMessage.ProjectDeleted, new CSingleCol<CProjectImpl>(oProjectToDelete));
            FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_PROJECT_DELETED"));
          }
        }
        else
        {
          FredMessageBox.showInfoDlg(Labels.get("INFORMATION_DO_NOT_DELETE_DEFAULT"),
                                     Labels.get("TITLE_DELETE_PROJECT"),
                                     EIcon.WARNING);
        }

        return;
      }
    }

    Shout.newError(s_oInstance.getClass().getName() + "#incomingProjectProperties",
      "should not be usable in this configuration");
  }
  
  /**
   * Projekt bereitstellen.
   */
  public static void incomingProjectBroadcastProject()
  {
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
    
    if (oCurrentViewController != null)
    {
      CProjectImpl oProjectToBroadcast = oCurrentViewController.getSelectedProject();
      
      if (oProjectToBroadcast != null)
      {
        if (oProjectToBroadcast.isClosed()
          ||!oProjectToBroadcast.isUserProjectLeader()
          || oProjectToBroadcast.isTeamproject())
          oProjectToBroadcast = null;
      }

      CDlgProjectBroadcastProject oDialog = new CDlgProjectBroadcastProject(oProjectToBroadcast, null, false);
      if (oDialog.showDialog() == EReturnValue.OK)
      {
        oProjectToBroadcast = oDialog.getSelectedProject();
        CReplicationServiceEx oRS = new CReplicationServiceEx();
        oRS.setURL(oDialog.getSelectedURL());
        oRS.setProjects(oProjectToBroadcast.toVector());
        oRS.setPersons(s_oInstance.getUserVector());
        
        CServerController oServerController = new CServerController(oRS, EReplicationJob.sendNewProject);
        oServerController.start();
        
        if (oServerController.getStatus() == IReplicationService.STATUS_OK)
        {
          // Projekt bereitgestellt
          oProjectToBroadcast.setServerURL(oDialog.getSelectedURL());
          oProjectToBroadcast.setTeamproject(true);
          
          // In die folgenden Dialoge eingesetzt.
          
          // Die Arbeitsdatei wurde manipuliert, 
          // die Änderungen müssen später gespeichert werden.
          s_oInstance.notifyControllers(EUpdateMessage.ProjectUpdated, new CSingleCol<CProjectImpl>(oProjectToBroadcast));
          
          // Stelle nun die Arbeitspakete in diesem Projekt bereit.
          Vector<CTaskImpl> oTasksToSubmit = oDialog.getSelectedTaskList();
          if (oTasksToSubmit.size() > 0)
          {
            oRS = new CReplicationServiceEx();
            oRS.setURL(oDialog.getSelectedURL());
            oRS.setProjects(oProjectToBroadcast.toVector());
            oRS.setTasks(oTasksToSubmit);
            oRS.setPersons(s_oInstance.getUserVector());
            
            oServerController = new CServerController(oRS, EReplicationJob.sendNewTasks);
            oServerController.start();
            
            if (oServerController.getStatus() == IReplicationService.STATUS_OK)
            {
              Iterator<CTaskImpl> oIteratorBroadcasted = oTasksToSubmit.iterator();  
              while (oIteratorBroadcasted.hasNext())
              {
                oIteratorBroadcasted.next().setTeamTask(true);
              }
              
              FredMessageBox.showInfoDlg(Labels.get("INFORMATION_PROJECT_AND_TASKS_BROADCASTED"),
                                         Labels.get("TITLE_BROADCAST_PROJECT"),
                                         EIcon.INFORMATION);
              
              s_oInstance.notifyControllers(EUpdateMessage.TaskUpdated, oTasksToSubmit);
              
              FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_PROJECT_AND_TASKS_BROADCASTED"));
            }
          }
          else
          {
            // oTasksToSubmit.size() == 0
            // Projekt erfolgreich bereitgestellt
            FredMessageBox.showInfoDlg(Labels.get("INFORMATION_PROJECT_BROADCASTED"),
                                       Labels.get("TITLE_BROADCAST_PROJECT"),
                                       EIcon.INFORMATION);
            
            s_oInstance.notifyControllers(EUpdateMessage.ProjectUpdated, new CSingleCol<CProjectImpl>(oProjectToBroadcast));
            
            FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_PROJECT_BROADCASTED"));
          }
        }
      } // was Succesfull
          
      return;
    }

    Shout.newError(s_oInstance.getClass().getName() + "#incomingProjectBroadcastProject",
      "should not be usable in this configuration");
  }

  /**
   * Ein Projekt abbonieren.
   */
  @SuppressWarnings("unchecked")
  public static void incomingProjectSubscribeProject()
  {
    if (s_oInstance.m_eWindowContent == EWindowContent.Views)
    {
      CDlgProjectSubscribeProjectURL oDlgURL = 
        new CDlgProjectSubscribeProjectURL(s_oInstance.m_oURLList.getURLVector());
      
      if (oDlgURL.showDialog() == EReturnValue.OK)
      {
        CReplicationServiceEx oRS = new CReplicationServiceEx();
        oRS.setObjectFactory(ObjectFactoryImpl.getInstance());
        oRS.setURL(oDlgURL.getURL());
        
        CServerController oServerController = new CServerController(oRS, EReplicationJob.receiveProjects);
        oServerController.start();
        
        if (oServerController.getStatus() == IReplicationService.STATUS_OK)
        {
          // Es werden alle Aufwände, welche tatsächlich eingesammelt werden können
          // herausgefunden
          Vector<CProjectImpl> oReceivedProjects = (Vector<CProjectImpl>) oRS.getProjects();
          ProjectList oProjectList = ProjectList.getInstance();
          oProjectList.deleteContained(oReceivedProjects);

          // oSubscribables enthält alle Projekte, die angezeigt werden können
          
          CDlgProjectSubscribeProjectList oDlgList = new CDlgProjectSubscribeProjectList(oReceivedProjects);
          if (oDlgList.showDialog() == EReturnValue.OK)
          {
            CProjectImpl oSubscribedProject = oDlgList.getProject();
            // Doppelte Namen abfangen
            if (!oProjectList.isProjectNameValid(oSubscribedProject, oSubscribedProject.getName()))
            {
              FredMessageBox.showInfoDlg(Labels.get("PROJECT_NAME_EXISTS_LOCAL") + "\n" + oSubscribedProject.getName(),
                                         Labels.get("TITLE_SUBSCRIBE_PROJECT"),
                                         EIcon.WARNING);
            }
            else
            {
              // Projekt kann hinzugefügt werden
              oSubscribedProject.setSubscribedNow();
              oSubscribedProject.setTeamproject(true);
              oSubscribedProject.setServerURL(oDlgURL.getURL());
              oProjectList.addProject(oSubscribedProject, true);
              
              // ein Projekt wurde hinzugefügt - benachrichtige GUI
              s_oInstance.notifyControllers(EUpdateMessage.ProjectCreated, new CSingleCol<CProjectImpl>(oSubscribedProject));
              
              // das Projekt muss aus der Liste gelöscht werden, da alle diese Projekte
              // aus der ObjectFactory entfernt werden
              oReceivedProjects.removeElement(oSubscribedProject);
              
              if (oDlgList.startSubscribeTasks())
              {
                int iTasksSubscibedCount = s_oInstance._localTaskSubscribeTasks(oSubscribedProject);
                if (iTasksSubscibedCount > 0)
                {
                  FredMessageBox.showInfoDlg(  Labels.get("INFORMATION_PROJECT_AND_TASK_SUBSCRIBED") + "\n"
                                             + oSubscribedProject.getName(),
                                             Labels.get("TITLE_SUBSCRIBE_PROJECT"),
                                             EIcon.INFORMATION);
                  
                  FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_PROJECT_SUBSCRIBED"));
                }
                else if (iTasksSubscibedCount == 0)
                {
                  FredMessageBox.showInfoDlg(Labels.get("INFORMATION_PROJECT_SUBSCRIBED_NO_TASKS") + "\n" + oSubscribedProject.getName(),
                                             Labels.get("TITLE_SUBSCRIBE_PROJECT"),
                                             EIcon.INFORMATION);
                  
                  FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_PROJECT_AND_TASKS_SUBSCRIBED"));
                }
              }
              else
              {
                FredMessageBox.showInfoDlg(Labels.get("INFORMATION_PROJECT_SUBSCRIBED") + "\n" + oSubscribedProject.getName(),
                                           Labels.get("TITLE_SUBSCRIBE_PROJECT"),
                                           EIcon.INFORMATION);
                
                FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_PROJECT_SUBSCRIBED"));
              }
            }
          } // oDlgList.wasSuccessful()

          // alle nicht verwendeten Projekte wieder löschen
          ObjectFactoryImpl.destroy(oReceivedProjects);
          
          return;
        }
      }
      
      return;
    }

    Shout.newError(s_oInstance.getClass().getName() + "#incomingProjectSubscribeProject",
      "should not be usable in this configuration");

  }

  /**
   * Einen Task erstellen
   */
  public static void incomingTaskCreateTask()
  {
    if (s_oInstance.m_eWindowContent == EWindowContent.Views
      && ProjectList.getInstance().getProjectForTaskCreation() != null)
    {
      CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
      CProjectImpl oSelectedProject = null;
      CCategory oSelectedCategory = null;
      
      if (oCurrentViewController != null)
      {
        oSelectedProject = oCurrentViewController.getSelectedProject();
        
        if (oSelectedProject != null && !oSelectedProject.isUserProjectLeader())
          oSelectedProject = null;
        
        oSelectedCategory = oCurrentViewController.getSelectedCategory();
      }
    
      CDlgTaskCreateTask oCreateDialog = new CDlgTaskCreateTask(oSelectedProject, oSelectedCategory);

      if (oCreateDialog.showDialog() == EReturnValue.OK)
      {
        CTaskImpl oNewTask = oCreateDialog.getCreatedTask();
        CProjectImpl oItsProject = oNewTask.getProject();
        oItsProject.addTask(oNewTask, true);
        
        if (s_oInstance.m_oDefaultTask == null)
          s_oInstance.m_oDefaultTask = oNewTask;

        // informiere alle ViewController
        s_oInstance.notifyControllers(EUpdateMessage.TasksCreated, new CSingleCol<CTaskImpl>(oNewTask));
        FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_TASK_CREATED"));
      }

      return;
    }

    Shout.newError(s_oInstance.getClass().getName() + "#incomingProjectCreate",
      "should not be usable in this configuration");
  }

  /**
   * Einen Task bearbeiten
   */
  public static void incomingTaskProperties()
  {
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();

    if (oCurrentViewController != null)
    {
      CTaskImpl oTaskToEdit = oCurrentViewController.getSelectedTask();

      if (oTaskToEdit != null)
      {
        CDlgTaskProperties oDlg = new CDlgTaskProperties(oTaskToEdit);

        if (oDlg.showDialog() == EReturnValue.OK)
        {
          // informiere alle ViewController
          s_oInstance.notifyControllers(EUpdateMessage.TaskUpdated, new CSingleCol<CTaskImpl>(oTaskToEdit));
          FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_TASK_EDITED"));
        }
        return;
      }
    }

    Shout.newError(s_oInstance.getClass().getName() + "#incomingProjectProperties",
      "should not be usable in this configuration");
  }

  /**
   * Einen Task bearbeiten
   */
  public static void incomingTaskDefaultTask()
  {
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
    
    if (oCurrentViewController != null)
    {
      CTaskImpl oNewDefaultTask = oCurrentViewController.getSelectedTask();

      if (oNewDefaultTask != null)
      {
        if (oNewDefaultTask.getProject().isClosed())
        {
          FredMessageBox.showInfoDlg(Labels.get("INFORMATION_DEFAULT_TASK_CLOSED"),
                                     Labels.get("TITLE_DEFAULT_TASK"),
                                     EIcon.WARNING);
        }
        else
        {
          s_oInstance.m_oDefaultTask = oNewDefaultTask;
        }
        
        return;
      }
    }

    Shout.newError(s_oInstance.getClass().getName() + "#incomingProjectProperties",
      "should not be usable in this configuration");
  }
  
  /**
   * Löscht einen Task
   */
  public static void incomingTaskDeleteTask()
  {
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
    
    if (oCurrentViewController != null)
    {
      CTaskImpl oTaskToDelete = oCurrentViewController.getSelectedTask();

      if (oTaskToDelete != null)
      {
        // soll der Defaulttask gelöscht werden?
        if (   oTaskToDelete != s_oInstance.m_oDefaultTask
            || ProjectList.getInstance().isUnclosedTaskAvailableExcTask(oTaskToDelete) == false)
        {
          EButton eButton = FredMessageBox.showOptionDlg(Labels.get("CONFIRM_DELETE_TASK") + "\n" + oTaskToDelete.toString(),
                                                         Labels.get("TITLE_DELETE_TASK"),
                                                         EIcon.QUESTION,
                                                         EOptionButtons.YES_NO);
          
          if (eButton == EButton.YES)
          {
            if (oTaskToDelete == s_oInstance.m_oDefaultTask)
              s_oInstance.m_oDefaultTask = null;

            oTaskToDelete.getProject().removeTask(oTaskToDelete);
            ObjectFactoryImpl.destroy(oTaskToDelete);
  
            // informiere alle ViewController
            s_oInstance.notifyControllers(EUpdateMessage.TaskDeleted, new CSingleCol<CTaskImpl>(oTaskToDelete));
            FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_TASK_DELETED"));
          }
        }
        else
        {
          FredMessageBox.showInfoDlg(Labels.get("INFORMATION_DO_NOT_DELETE_DEFAULT"),
                                     Labels.get("TITLE_DELETE_TASK"),
                                     EIcon.WARNING);
        }
        return;
      }
    }

    Shout.newError(s_oInstance.getClass().getName() + "#incomingProjectProperties",
      "should not be usable in this configuration");
  }

  /**
   * Einen Tasks abonnieren
   */
  public static void incomingTaskSubscribeTask()
  {
    if (s_oInstance.m_eWindowContent == EWindowContent.Views)
    {
      CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
      CProjectImpl oItsProject = null;
      
      // schau, ob gerade ein Projekt markiert ist
      if (oCurrentViewController != null)
      {
        oItsProject = oCurrentViewController.getSelectedProject();

        if (oItsProject != null)
        {
          if (!oItsProject.isTeamproject()
            || oItsProject.isUserProjectLeader())
            oItsProject = null;
        }
      }
      
      CDlgTaskSubscribeTaskProject oDialog = new CDlgTaskSubscribeTaskProject(oItsProject,
        ProjectList.getInstance().getSubscribedProjectComboBoxModel());

      if (oDialog.showDialog() == EReturnValue.OK)
      {
        int iSubscribedTaskCount = s_oInstance._localTaskSubscribeTasks(oDialog.getProject());
        if (iSubscribedTaskCount == 0)
        {
          FredMessageBox.showInfoDlg(Labels.get("INFORMATION_NO_TASKS_AVAILABLE"),
                                     Labels.get("TITLE_SUBSCRIBE_TASK"),
                                     EIcon.WARNING);
        }
        else if(iSubscribedTaskCount > 0)
        {
          FredMessageBox.showInfoDlg(Labels.get("INFORMATION_TASK_SUBSCRIBED"),
                                     Labels.get("TITLE_SUBSCRIBE_TASK"),
                                     EIcon.INFORMATION);
          
          FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_TASK_SUBSCRIBED"));
        }
      }
      
      return;
    }
    
    Shout.newError(s_oInstance.getClass().getName() + "#incomingTaskSubscribeTask",
    "should not be usable in this configuration");
  }

  /**
   * Lädt zu einem Projekt alle Tasks vom Server und zeigt noch nicht
   * akzeptierte an. Die ausgewählten Tasks werden in das Model eingetragen und
   * dann das Model akutalisiert
   * {@link #notifyControllers(EUpdateMessage, Collection)}. <br>
   * Im Fehlerfall wird ein Fehler ausgegeben.
   * 
   * @param p_oItsProject Das Projekt.
   * @return -1 &rarr; es traten Fehler beim Laden auf; 0 &rarr; keine neuen
   * Tasks; > 0 wurde(n) [return] Arbeitspakete geladen;
   * @pre p_oItsProject.isTeamprojekt && !p_oItsProject.isUserProjectLeader
   */
  @SuppressWarnings("unchecked")
  private int _localTaskSubscribeTasks(CProjectImpl p_oItsProject)
  {
    int iReturn = -1;
    CReplicationServiceEx oRS = new CReplicationServiceEx();
    oRS.setObjectFactory(ObjectFactoryImpl.getInstance());
    oRS.setURL(p_oItsProject.getServerURL());
    oRS.setProjects(p_oItsProject.toVector());
    oRS.setPersons(getUserVector());
    
    CServerController oServerController = new CServerController(oRS, EReplicationJob.receiveTasks);
    oServerController.start();
    
    if (oServerController.getStatus() != IReplicationService.STATUS_OK)
    {
      _checkAndCloseProject(oServerController.getStatus(), p_oItsProject);
    }
    else
    {
      Vector<CTaskImpl> oReceivedTasks = (Vector<CTaskImpl>) oRS.getTasks();
      p_oItsProject.deleteContained(oReceivedTasks);

      if (oReceivedTasks.size() == 0)
      {
        iReturn = 0;
      }
      else
      {
        CDlgTaskSubscribeTaskList oDialog = new CDlgTaskSubscribeTaskList(oReceivedTasks);
        if (oDialog.showDialog() == EReturnValue.OK)
        {
          Vector<CTaskImpl> oTasksToSubscribe = oDialog.getTaskVector();
          Vector<CTaskImpl> oTasksSubscribed = new Vector<CTaskImpl>();
          
          StringBuffer oUnsubscribableTaskList = new StringBuffer();
          
          if (oTasksToSubscribe.size() > 0)
          {
            Iterator<CTaskImpl> oIteratorToSubscribe = oTasksToSubscribe.iterator();
            while (oIteratorToSubscribe.hasNext())
            {
              CTaskImpl oThisTask = oIteratorToSubscribe.next();
              
              if (p_oItsProject.isTaskNameValid(null, oThisTask.getName())
                && p_oItsProject.isTaskShortNameValid(null, oThisTask.getShortName()))
              {
                oTasksSubscribed.add(oThisTask);
                oThisTask.setTeamTask(true);
                oThisTask.setSubscribedNow();
                oThisTask.setProject(p_oItsProject);
                p_oItsProject.addTask(oThisTask, true);
                oReceivedTasks.removeElement(oThisTask);
                
                if (this.m_oDefaultTask == null)
                  this.m_oDefaultTask = oThisTask;
              }
              else
              {
                oUnsubscribableTaskList.append(oThisTask.toString() + "\n");
              }
            }

            if (oUnsubscribableTaskList.length() > 0)
            {
              FredMessageBox.showInfoDlg(  Labels.get("INFORMATION_TASKNAME_ALREADY_USED") + "\n"
                                         + oUnsubscribableTaskList.toString(),
                                         Labels.get("TITLE_SUBSCRIBE_TASK"),
                                         EIcon.WARNING);  
            }

            notifyControllers(EUpdateMessage.TasksCreated, oTasksSubscribed);
            iReturn = oTasksSubscribed.size();
          } // oTasksToSubscribe.size() > 0
          else
          {
            iReturn = 0;
          }
        } // oDialog.wasSuccessful()
      } // oAvailableTasks.size() != 0
      
      // alle nicht verwendeten Tasks wieder löschen
      ObjectFactoryImpl.destroy(oReceivedTasks);
    }

    return iReturn;
  }
  
  /**
   * Task bereitstellen
   */
  public static void incomingTaskBroadcastTask()
  {
    if (s_oInstance.m_eWindowContent == EWindowContent.Views)
    {
      CProjectImpl oItsProject = null;
      CTaskImpl oTaskToBroadcast = null;
      CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
      
      if (oCurrentViewController != null)
      {
        oItsProject = oCurrentViewController.getSelectedProject();
        oTaskToBroadcast = oCurrentViewController.getSelectedTask();
        if (oTaskToBroadcast != null)
        {
          oItsProject = oTaskToBroadcast.getProject();
        
          if (oItsProject.isClosed()
            || !oItsProject.isUserProjectLeader()
            || oTaskToBroadcast.isTeamTask())
          {
            oItsProject = null;
            oTaskToBroadcast = null;
          }
        }
        else if (oItsProject != null)
        {
          if (oItsProject.isClosed()
            || !oItsProject.isUserProjectLeader()
            || !oItsProject.isTeamproject())
          {
            oItsProject = null;
            oTaskToBroadcast = null;
          }
        }
      }

      CDlgProjectBroadcastProject oDialog = new CDlgProjectBroadcastProject(oItsProject, oTaskToBroadcast, true);
      if (oDialog.showDialog() == EReturnValue.OK)
      {
        Vector<CTaskImpl> oTasksToSubmit = oDialog.getSelectedTaskList();
        if (oTasksToSubmit.size() > 0)
        {
          CReplicationServiceEx oRS = new CReplicationServiceEx();
          oRS.setURL(oDialog.getSelectedURL());
          oRS.setProjects(oTasksToSubmit.get(0).getProject().toVector());
          oRS.setTasks(oTasksToSubmit);
          oRS.setPersons(s_oInstance.getUserVector());
          
          CServerController oServerController = new CServerController(oRS, EReplicationJob.sendNewTasks);
          oServerController.start();
          
          if (oServerController.getStatus() != IReplicationService.STATUS_OK)
          {
            s_oInstance._checkAndCloseProject(oServerController.getStatus(), oTasksToSubmit.get(0).getProject());
          }
          else
          {
            Iterator<CTaskImpl> oIteratorBoradcasted = oTasksToSubmit.iterator();  
            while (oIteratorBoradcasted.hasNext())
            {
              oIteratorBoradcasted.next().setTeamTask(true);
            }
            
            FredMessageBox.showInfoDlg(Labels.get("INFORMATION_TASK_BROADCASTED"),
                                       Labels.get("TITLE_BROADCAST_TASK"),
                                       EIcon.INFORMATION);
            
            s_oInstance.notifyControllers(EUpdateMessage.TaskUpdated, oTasksToSubmit);
            
            FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_TASK_BROADCASTED"));
          }
        }
      } // oDialog.wasSuccessful()
      
      return;
    }

    Shout.newError(s_oInstance.getClass().getName() + "#incomingProjectBroadcastProject",
      "should not be usable in this configuration");
  }


  
  /**
   * {Kurzbeschreibung}<br>
   * <br>
   * {Beschreibung} 
   * 1. erzeugt leeren EffortImpl<br>
   * 2. setzt Datum = jetzt(), dauer = 0, Entwickeler = m_User<br>
   * 3. benutzt DialogEffortEditEffort <br>
   * 4. wenn dialog.wasSuccessful = true then <br>
   * 5. Füge den Aufwand in das model ein.
   * 6. Überprüfe auf semantische Korrektheit<br>
   * 
   * @remarks {Anmerkungen}
   * @pre
   * @post
   */
  public static void incomingEffortCreateEffort()
  {
    if (s_oInstance.m_eWindowContent == EWindowContent.Views)
    {
      CEffortImpl oEffort = null;
      CDlgEffortEditEffort oDialog = null;
      CTaskImpl oTask = null;
      CProjectImpl oProject = null;
      CCategoryProjectMapper oMappedCategory = null; 
      CViewController oViewController = null;
      
      // 1. Erzeuge Effort mit 
      // Startzeitpunkt: dieser Augenblick
      // Endzeitpunkt: dieser Augenblick
      // Dauer: 0
      // Arbeitspaket: Standardarbeitspaket
      // Projekt: Projekt des Standardarbeitspaketes
      // Kategorie: Standardkategorie des Standardarbeitspaketes
      // Verursacher: Die Person, der die Arbeitsdatei gehört
      // Beschreibung: leere Beschreibung
      // Zustand Repliziert: false
      // Zustand Fehler: false
      // Zustand Fehler ignorieren: false
      // Zustand akzeptiert: noch nicht akzeptiert
      //Shout.println("Erzeuge einen Aufwand.");
      oEffort = CEffortImpl.create();
      
      // 2. Sind Dinge im Projektbaum markiert?
      oViewController = s_oInstance._getCurrentViewController();
      if (oViewController != null)
      {
        oTask = oViewController.getSelectedTask();
        oProject = oViewController.getSelectedProject();
        oMappedCategory = oViewController.getSelectedCategoryMapped();
        
        if (oTask != null
          && !oTask.getProject().isClosed())
        {
          oEffort.setTask(oTask);
          oEffort.setCategory(oTask.getDefaultCategory());
        }
        else if (   oProject != null
                 && oProject != s_oInstance.getDefaultTask().getProject()
                 && oProject.getTaskCount() > 0
                 && oProject.isClosed() == false)
        {
          // das liefert den ersten Tasks, da oSelectedProject.getTaskCount() > 0
          oTask = oProject.getTaskIterator().next();
          oEffort.setTask(oTask);
          oEffort.setCategory(oTask.getDefaultCategory());
        }
        else if (oMappedCategory != null)
        {
          oProject = oMappedCategory.getProject();
          if (oProject.getTaskCount() > 0
            && !oProject.isClosed())
          {
            oTask = oProject.getTaskIterator().next();
            oEffort.setTask(oTask);
            oEffort.setCategory(oMappedCategory.getCategory());            
          }
        }
      }
      
      // 3. Zeige den Aufwand im Dialog an, ohne verwerfen Button.
      oDialog = new CDlgEffortEditEffort(oEffort, false, true);
      
      // 4. Überprüfe ob der Benutzer die Aktion abgebrochen hat,
      //    oder ob er den Dialog bestätigt hat. 
      if (oDialog.showDialog() == EReturnValue.OK)
      {
        // Die kritischen Daten werden in den Aufwand eingetragen,
        // da wird einen neuen Aufwand erzeugen.
        oEffort.setTask(oDialog.getNewTask());
        oEffort.setBegin(oDialog.getBegin());
        oEffort.setDuration(oDialog.getWorkDuration());
        oEffort.setPauseDuration(oDialog.getPauseDuration());
        
        // 5. Füge den Aufwand in das Model ein.
        //Shout.println(oEffort.getDescription());
        ProjectList.getInstance().addEffort(oEffort);
        
        s_oInstance.notifyControllers(EUpdateMessage.EffortsCreated, new CSingleCol<CEffortImpl>(oEffort));
        FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_EFFORT_CREATED"));
      }
      
      return;
    }
    
    Shout.newError(s_oInstance.getClass().getName() + "#incomingTaskSubscribeTask",
    "should not be usable in this configuration");
  }

  /**
   * {Kurzbeschreibung}<br>
   * <br>
   * {Beschreibung}
   * 
   * @remarks {Anmerkungen}
   * @pre
   * @post
   */
  public static void incomingEffortEditEffort()
  {
    CEffortImpl oEffort = null;
    CDlgEffortEditEffort oDialog = null;
    CViewController oViewController = null;
    
    // 1. Welcher Aufwand ist in der Aufwandstabelle markiert ?
    // Es wird davon ausgegangen, das der Benutzer diese Aktion nur
    // durchführen kann, wenn in der Aufwandstabelle ein einziger
    // Aufwand markiert ist.
    
    oViewController = s_oInstance._getCurrentViewController();
    
    if (oViewController != null)
      oEffort = oViewController.getSelectedEffort();
    
    if (oEffort == null)
    {
      Shout.newError(s_oInstance.getClass().getName() + "#incomingEffortEditEffort",
      "should not be usable in this configuration");
      return;
    }
    
    // 2. Zeige den Aufwand im Dialog an, ohne verwerfen Button.
    oDialog = new CDlgEffortEditEffort(oEffort, false, false);
    
    // 3. Überprüfe ob der Benutzer die Aktion abgebrochen hat,
    //    oder ob er den Dialog bestätigt hat. 
    if (oDialog.showDialog() == EReturnValue.OK)
    {
      // 4. überprüfe, ob sich die kritischen Informationen geändert haben
      if (oEffort.getTask() != oDialog.getNewTask())
      {
        // Wurde das Arbeitspaket geändert, muss der Aufwand völlig neu 
        // eingegefügt werden.
        // 4.a entferne ihn aus dem Model
        ProjectList.getInstance().removeEffort(oEffort);

        // 4.b setzt die kritischen Informationen
        oEffort.setTask(oDialog.getNewTask());
        oEffort.setBegin(oDialog.getBegin());
        oEffort.setDuration(oDialog.getWorkDuration());
        oEffort.setPauseDuration(oDialog.getPauseDuration());

        // 4.c Füge den Aufwand in das Model ein
        ProjectList.getInstance().addEffort(oEffort);
      }
      else if (   oEffort.getBegin().equals(oDialog.getBegin()) == false
               || oEffort.getDuration() != oDialog.getWorkDuration()
               || oEffort.getPauseDuration() != oDialog.getPauseDuration())
      {
        // Das Arbeitspaket hat sich nicht geändert!
        // Die Zeit hat sich geändert. Verschiebe den Aufwand innerhalb des 
        // Arbeitspakets.
        CTaskImpl oItsTask = oEffort.getTask();
        oItsTask.repositionEffort(oEffort, 
                                  oDialog.getBegin(), 
                                  oDialog.getWorkDuration(),
                                  oDialog.getPauseDuration());
      }
      
      // Falls der Aufwand keine Kollision mehr verursacht, wird 
      // "Kollisionen ignorieren" auf false gesetzt
      // Spezi Seite: 63
      if (!oEffort.isColliding())
        oEffort.setIgnoreMistake(false);
      
      // 5. Verursache ein Update.
      s_oInstance.notifyControllers(EUpdateMessage.EffortsUpdated, new CSingleCol<CEffortImpl>(oEffort));
      FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_EFFORT_EDITED"));
      
      // 6. Überprüfe auf semantische Korrektheit.
      // Wird bereits in der addEffort Methode der ProjectList gemacht.
      //ProjectList.getInstance().discoverSemanticConflicts(oEffort);
      
      // Die Aufwandstabelle des aktuellen Tabs wird über eine neue 
      // Filterabfrage aktualisiert.
      //oViewController.incomingFilterRequest();
    }
  }

  /**
   * Aufwände löschen
   */
  public static void incomingEffortDeleteEfforts()
  {
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
    
    if (oCurrentViewController != null)
    {
      Collection<CEffortImpl> oCollectionDeletions = new CEffortTree();
      oCollectionDeletions.addAll(oCurrentViewController.getSelectedEfforts());
    
      if (oCollectionDeletions.size() > 0)
      {
        EButton eButton = FredMessageBox.showOptionDlg(Labels.get("CONFIRM_DELETE_EFFORTS") + "\n" + oCollectionDeletions.size() + " " + Labels.get("CONFIRM_DELETE_EFFORTS2"),
                                                       Labels.get("TITLE_DELETE_EFFORTS"),
                                                       EIcon.QUESTION,
                                                       EOptionButtons.YES_NO);

        if (eButton == EButton.YES)
        {
          Iterator<CEffortImpl> oToDelete = oCollectionDeletions.iterator();
          ProjectList oProjectList = ProjectList.getInstance();
 
          while (oToDelete.hasNext())
          {
            CEffortImpl oThisEffort = oToDelete.next();
            oProjectList.removeEffort(oThisEffort);
            ObjectFactoryImpl.destroy(oThisEffort);
          }
          
          s_oInstance.notifyControllers(EUpdateMessage.EffortsDeleted, oCollectionDeletions);
          FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_EFFORT_DELETED"));
        }
        
        return;
      }
    }
    
    Shout.newError(s_oInstance.getClass().getName() + "#incomingEffortDeleteEfforts",
    "should not be usable in this configuration");
  }

  /**
   * Zeigt zu einem Aufwand alle Kollisionen an.
   */
  public static void incomingEffortShowCollidators()
  {
    CEffortImpl oEffort = null;
    
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
    
    if (oCurrentViewController != null)
      oEffort = oCurrentViewController.getSelectedEffort();
    
    if (oEffort == null || !oEffort.isColliding())
    {
      Shout.newError(s_oInstance.getClass().getName() + "#incomingEffortShowColidings",
      "should not be usable in this configuration");
      return;
    }
    
    AFilter oNewFilter = AFilter.createEmptyFilter(CViewController.DEFAULT_TREE_TYPE);
    oNewFilter.setUseThisFilter(true);
    oNewFilter.addAllTasks();
    oNewFilter.addPerson(oEffort.getPerson());
    oNewFilter.addAllCategories();
    oNewFilter.setStart(oEffort.getEarliestColidersStart());
    oNewFilter.setEnd(oEffort.getLatestColidersEnd());
    oNewFilter.setRelevantMaskBit(AFilter.BIT_COLLIDATIONS, true);
    oNewFilter.setDataMaskBit(AFilter.BIT_COLLIDATIONS, true);
    oNewFilter.setColidingReference(oEffort);
    
    String strName = oCurrentViewController.getName() + "+" + Labels.get("COLIDINGS");
    
    s_oInstance._newViewByFilter(oNewFilter, strName);
  }
  
  /**
   * Exportiert die Aufwandstablle in CSV oder HTML<br>
   * <br>
   * Entweder es liegen in der Aufwandstabelle markierte Aufwände vor,
   * dann werden nur die markierten Aufwände exportiert, oder es liegt
   * keine Markierung vor, woraufhin dieser Sachverhalt als Aufforderung
   * interpretiert wird, die gesammte Aufwandstabelle zu markieren.
   */
  public static void incomingEffortExportEfforts()
  {
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
    
    if (oCurrentViewController != null)
    {
      // Es wird gerade eine View angezeigt
      Collection<CEffortImpl> oSelectedEfforts;
      Collection<CEffortImpl> oEffortsToExport;
      Vector<CColumnModel.EColumnType> oDisplayedColumns;
      
      // 1. Markierung der Aufwandstabelle ermitteln.
      oSelectedEfforts = oCurrentViewController.getSelectedEfforts();
      oDisplayedColumns = oCurrentViewController.getColumns().getVisibleColumTypes();

      // Wenn Aufwände selektiert sind verwende diese Auswahl
      // Ansonsten nehme alle
      if (oSelectedEfforts.size() > 0)
      {
        oEffortsToExport = new CEffortTree();
        oEffortsToExport.addAll(oSelectedEfforts);
      }
      else
      {
        oEffortsToExport = oCurrentViewController.getEffortVector();
      }

      s_oInstance._incomingEffortExportEfforts(oEffortsToExport, oDisplayedColumns);
      
      return;
    }

    Shout.newError(s_oInstance.getClass().getName() + "#incomingEffortExportEfforts",
    "should not be usable in this configuration");
  }

  /**
   * Exportieren von Aufwänden.<br><br>
   *
   * Die Aktion wurde im Kontextmenü angestoßen.
   */
  public static void incomingEffortExportEffortsContext()
  {
    CViewController oCurrentViewController;
    Vector<CColumnModel.EColumnType> oDisplayedColumns;
    CProjectImpl oSelectedProject;
    CTaskImpl oSelectedTask;
    Collection<CEffortImpl> oEffortsToExport;
    CTasksInTreeFilter oThisFilter;
    
    oCurrentViewController = s_oInstance._getCurrentViewController();
    
    if (oCurrentViewController != null)
    {
      oDisplayedColumns = oCurrentViewController.getColumns().getVisibleColumTypes();
      oSelectedProject = oCurrentViewController.getSelectedProject();
      oSelectedTask = oCurrentViewController.getSelectedTask();
      oThisFilter = new CTasksInTreeFilter();
      oThisFilter.setUseThisFilter(false);
      
      if (oSelectedProject != null)
      {
        oThisFilter.addTasksOfProject(oSelectedProject);        
        oEffortsToExport = ProjectList.getInstance().getContainedEfforts(oThisFilter);
        
        s_oInstance._incomingEffortExportEfforts(oEffortsToExport, oDisplayedColumns);
        
        return;
      }
      else if (oSelectedTask != null)
      {
        oThisFilter.addTask(oSelectedTask);      
        oEffortsToExport = ProjectList.getInstance().getContainedEfforts(oThisFilter);

        s_oInstance._incomingEffortExportEfforts(oEffortsToExport, oDisplayedColumns);

        return;
      }
    }
    
    Shout.newError(s_oInstance.getClass().getName() + "#incomingEffortExportEffortsContext",
    "should not be usable in this configuration");
  }

  private void _incomingEffortExportEfforts(Collection<CEffortImpl>  p_oEffortsToExport,
                                            Vector<EColumnType>      p_oDisplayedColumns)
  {
    String strExportDestination;
    CDlgEffortExportEfforts oDialog = null;
    EExportFormat eExportFormat;
    boolean bExportSuccessful;
    
    // Führe den Export durch
    if(p_oEffortsToExport.size() > 0)
    {
      // 1. Dialog anzeigen um Format und Ziel des Exports zu ermitteln.
      oDialog = new CDlgEffortExportEfforts(Configuration.getString(Configuration.EConf.LastExportPath));
      if (oDialog.wasSuccessful())
      {
        // 2. Dialog auswerten
        eExportFormat = oDialog.getDestinationFormat();
        strExportDestination = oDialog.getDestinationFile();
        
        // 3. Vermerke den letzten ExportPath, der sich soeben ergeben hat.
        Configuration.set(Configuration.EConf.LastExportPath, strExportDestination);
      
        if (eExportFormat == CDlgEffortExportEfforts.EExportFormat.CSV)
          bExportSuccessful = Export.exportToCSV(strExportDestination, p_oEffortsToExport, p_oDisplayedColumns);
        else
          bExportSuccessful = Export.exportToHTML(strExportDestination, p_oEffortsToExport, p_oDisplayedColumns);
        
        if (bExportSuccessful)
        {
          FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_EFFORT_EXPORTED"));          
        }
        else
        {
          FredMessageBox.showInfoDlg(Labels.get("INFORMATION_EXPORT_ERROR"),
                                     Labels.get("TITLE_EXPORT_EFFORTS"),
                                     EIcon.ERROR);
        }
      }
    }
    else
    {
      FredMessageBox.showInfoDlg(Labels.get("INFORMATION_EXPORT_NO_EFFORTS"),
                                 Labels.get("TITLE_EXPORT_EFFORTS"),
                                 EIcon.WARNING);
    }
  }
  
  /**
   * Aufwände replizieren
   */
  public static void incomingEffortReplicateEfforts()
  {
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
    
    if (oCurrentViewController != null)
    {
      Collection<CEffortImpl> oReplications = new CEffortTree();
      oReplications.addAll(oCurrentViewController.getSelectedEfforts());
      
      if (oReplications.size() > 0)
      {
        if (FredMessageBox.showOptionDlg(Labels.get("OPTION_REPLICATE_EFFORTS"),
                                         Labels.get("OPTION_REPLICATE_EFFORTS_TITLE"),
                                         EIcon.QUESTION,
                                         EOptionButtons.YES_NO) == EButton.NO)
        {
          return;
        }

        // Buffer für Abschlussnachricht
        Vector<String[]> oNotificationVector = new Vector<String[]>();
        boolean bEffortsReplicated = false;
        
        // es müssen alle Aufwände nach Task sortiert werden, da nur Task-weise repliziert werden kann.
        // es wird ein Mapping angelegt von Task auf eine Liste mit zugehörigen Aufwänden
        TreeMap<CTaskImpl, Vector<CEffortImpl>> oTasksOfEfforts =
          new TreeMap<CTaskImpl, Vector<CEffortImpl>>();
        Iterator<CEffortImpl> oToReplicate = oReplications.iterator();
        while (oToReplicate.hasNext())
        {
          CEffortImpl oThisEffort = oToReplicate.next();
          CTaskImpl oItsTask = oThisEffort.getTask();
          
          // ein Aufwand wird nur repliziert, falls er akzeptiert ist,
          // nicht schon repliziert wurde, der Task abonniert wurde
          if (oThisEffort.isApproved()
            && !oThisEffort.isReplicated()
            && oItsTask.isSubscibed())
          {
            // hole die Liste des Tasks
            Vector<CEffortImpl> oItsTasksList = oTasksOfEfforts.get(oItsTask);
            if (oItsTasksList == null)
            {
              // der Task hatte noch keine Liste -> lege sie an
              oItsTasksList = new Vector<CEffortImpl>();
              oTasksOfEfforts.put(oItsTask, oItsTasksList);
            }
  
            oItsTasksList.add(oThisEffort);
          }
        }
        
        Iterator<Entry<CTaskImpl, Vector<CEffortImpl>>> oTaskLists =
          oTasksOfEfforts.entrySet().iterator();
        
        while (oTaskLists.hasNext())
        {
          Entry<CTaskImpl, Vector<CEffortImpl>> oThisEntry = oTaskLists.next();
          CTaskImpl oThisTask = oThisEntry.getKey();
          CProjectImpl oThisProject = oThisTask.getProject();
          Vector<CEffortImpl> oThisEffortVector = oThisEntry.getValue();
  
          if (oThisProject.getServerURL() != null)
          {
            CReplicationServiceEx oRS = new CReplicationServiceEx();
            oRS.setURL(oThisProject.getServerURL());
            oRS.setObjectFactory(ObjectFactoryImpl.getInstance());
            oRS.setProjects(oThisProject.toVector());
            oRS.setTasks(oThisTask.toVector());
            oRS.setEfforts(oThisEffortVector);
            oRS.setPersons(s_oInstance.getUserVector());
            
            CServerController oServerController = new CServerController(oRS, EReplicationJob.sendNewEfforts);
            oServerController.start();

            if (oServerController.getStatus() != IReplicationService.STATUS_OK)
            {
              s_oInstance._checkAndCloseProject(oServerController.getStatus(), oThisProject);
              
              String[] astrMessage = {oThisProject.getName() + " - " + oThisTask.getName(),
                                      s_oInstance._getErrorMessageByStatusShort(oServerController.getStatus())};
              oNotificationVector.add(astrMessage);
            }
            else
            {
              String[] astrMessage = {oThisProject.getName() + " - " + oThisTask.getName(),
                Integer.toString(oThisEffortVector.size())};
              oNotificationVector.add(astrMessage);
              
              Iterator<CEffortImpl> oReplicated = oThisEffortVector.iterator();
              while (oReplicated.hasNext())
              {
                oReplicated.next().setReplicated(true);
              }
              
              bEffortsReplicated = true;
            }
          }
        } // oTaskLists.hasNext()
        
        if (bEffortsReplicated)
        {
          s_oInstance.notifyControllers(EUpdateMessage.EffortsUpdated, oReplications);
          FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_EFFORT_REPLICATED"));
        }
        
        if (oNotificationVector.size() > 0)
        {
          String[][] aastrNotification = new String[oNotificationVector.size()][];

          for (int i = 0; i < oNotificationVector.size(); i++)
          {
            aastrNotification[i] = oNotificationVector.get(i);
          }
          
          new CDlgEffortReplicateNotification(aastrNotification);
          
          return;
        }
      }
  
      FredMessageBox.showInfoDlg(Labels.get("INFORMATION_NOTHING_TO_REPLICATE"),
                                 Labels.get("TITLE_REPLICATE_EFFORTS"),
                                 EIcon.INFORMATION);
      
      return;
    }
    
    Shout.newError(s_oInstance.getClass().getName() + "#incomingEffortReplicateEfforts",
    "should not be usable in this configuration");
  }

  /**
   * Aufwände akzeptieren
   */
  public static void incomingEffortAcceptEfforts()
  {
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
    
    if (oCurrentViewController != null)
    {
      Collection<CEffortImpl> oCollectionAccepted = oCurrentViewController.getSelectedEfforts();
      
      if (oCollectionAccepted.size() > 0)
      {
        EButton eButton = FredMessageBox.showOptionDlg(Labels.get("CONFIRM_ACCEPT_EFFORTS"),
                                                       Labels.get("TITLE_ACCEPT_EFFORTS"),
                                                       EIcon.QUESTION,
                                                       EOptionButtons.YES_NO);
        
        if (eButton == EButton.YES)
        {
          Iterator<CEffortImpl> oToAccept = oCollectionAccepted.iterator();
          Vector<CEffortImpl> oVectorAccepted = new Vector<CEffortImpl>(oCollectionAccepted.size());
  
          while (oToAccept.hasNext())
          {
            CEffortImpl oThisEffort = oToAccept.next();
            if (oThisEffort.getAccepted() != EAccepted.Accepted)
            {
              oThisEffort.setAccepted(EAccepted.Accepted);
              oVectorAccepted.add(oThisEffort);
            }
          }
  
          s_oInstance.notifyControllers(EUpdateMessage.EffortsUpdated, oVectorAccepted);
        }
      }
      
      return;
    } // oCurrentViewController != null
    
    Shout.newError(s_oInstance.getClass().getName() + "#incomingEffortAcceptEfforts",
    "should not be usable in this configuration");
  }

  /**
   * Aufwände akzeptieren
   */
  public static void incomingEffortDiscardEfforts()
  {
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
    
    if (oCurrentViewController != null)
    {
      Collection<CEffortImpl> oCollectionDiscard = oCurrentViewController.getSelectedEfforts();
      
      if (oCollectionDiscard.size() > 0)
      {
        EButton eButton = FredMessageBox.showOptionDlg(Labels.get("CONFIRM_DISCARD_EFFORTS"),
                                                       Labels.get("TITLE_DISCARD_EFFORTS"),
                                                       EIcon.QUESTION,
                                                       EOptionButtons.YES_NO);
        
        if (eButton == EButton.YES)
        {
          Iterator<CEffortImpl> oToDiscard = oCollectionDiscard.iterator();
          Vector<CEffortImpl> oVectorDiscarded = new Vector<CEffortImpl>(oCollectionDiscard.size());
  
          while (oToDiscard.hasNext())
          {
            CEffortImpl oThisEffort = oToDiscard.next();
            if (oThisEffort.getAccepted() != EAccepted.NotAccepted)
            {
              oThisEffort.setAccepted(EAccepted.NotAccepted);
              oVectorDiscarded.add(oThisEffort);
            }
          }
  
          s_oInstance.notifyControllers(EUpdateMessage.EffortsUpdated, oVectorDiscarded);
        }
      }
      
      return;
    } // oCurrentViewController != null
    
    Shout.newError(s_oInstance.getClass().getName() + "#incomingEffortDiscardEfforts",
    "should not be usable in this configuration");
  }
  
  /**
   * Aufwände einsammeln
   */
  @SuppressWarnings("unchecked")
  public static void incomingEffortCollectEfforts()
  {
    CViewController oCurrentViewController = s_oInstance._getCurrentViewController();
    CProjectImpl oProjectToCollect = null;
    
    if (oCurrentViewController != null)
    {
      oProjectToCollect = oCurrentViewController.getSelectedProject();
      
      if (oProjectToCollect != null)
      {
        if (oProjectToCollect.isClosed()
          || !oProjectToCollect.isUserProjectLeader()
          || !oProjectToCollect.isTeamproject())
          oProjectToCollect = null;
      }
    }

    Vector<CProjectImpl> oBroadcasted = ProjectList.getInstance().getBroadcastedProjects();
    CDlgEffortCollectEffortsProjects oDialog = new CDlgEffortCollectEffortsProjects(oBroadcasted, oProjectToCollect);
    if (oDialog.showDialog() == EReturnValue.OK)
    {
      Vector<CProjectImpl> oProjectsToCollect = oDialog.getProjectVector();
      ProjectList oProjectList = ProjectList.getInstance();
      
      // Buffer für Abschlussnachricht
      String[][] aastrNotification = new String[oProjectsToCollect.size()][2];
      // Collection für _notify
      CEffortTree oNotificationEffortTree = new CEffortTree();
      
      for (int i = 0; i < oProjectsToCollect.size(); i++)
      {
        CProjectImpl oThisProject = oProjectsToCollect.get(i);
        CReplicationServiceEx oRS = new CReplicationServiceEx();
        oRS.setURL(oThisProject.getServerURL());
        oRS.setObjectFactory(ObjectFactoryImpl.getInstance());
        oRS.setProjects(oThisProject.toVector());
        oRS.setPersons(s_oInstance.getUserVector());
        
        CServerController oServerController = new CServerController(oRS, EReplicationJob.receiveEfforts);
        oServerController.start();
        
        aastrNotification[i] = new String[2];
        aastrNotification[i][0] = oThisProject.getName();

        if (oServerController.getStatus() != IReplicationService.STATUS_OK)
        {
          s_oInstance._checkAndCloseProject(oServerController.getStatus(), oThisProject);
          
          aastrNotification[i][1] = s_oInstance._getErrorMessageByStatusShort(oServerController.getStatus());
        }
        else
        {
          oThisProject.resetCollectedLastAt();
          Vector<CEffortImpl> oCollected = (Vector<CEffortImpl>) oRS.getEfforts();
          
          if (oCollected.size() == 0)
          {
            aastrNotification[i][1] = Labels.get("TITLE_EFFORTS_NONE_SHORT");
          }
          else
          {
            int iCount = 0;
            
            Iterator<CEffortImpl> oIteratorCollected = oCollected.iterator();
            while (oIteratorCollected.hasNext())
            {
              CEffortImpl oThisCollectedEffort = oIteratorCollected.next();
              oThisCollectedEffort.setAccepted(EAccepted.NotYetAccepted);
              oThisCollectedEffort.setReplicated(true);
              oThisCollectedEffort.setApproved(true);

              if (oProjectList.addEffort(oThisCollectedEffort))
              {
                iCount++;
                oNotificationEffortTree.add(oThisCollectedEffort);
              }
            }
            
            aastrNotification[i][1] = String.valueOf(iCount);
                                 
          } // oCollected.size() >= 0
        } // oRS.getStatus() == IReplicationService.STATUS_OK
      } // for (int i = 0
      
      CDlgEffortCollectNotification oDialogNotification = new CDlgEffortCollectNotification(aastrNotification);
      if (oDialogNotification.showCollectedEfforts() == true)
      {
        // Eingesammelte Aufwände in Ansicht anzeigen
        CTasksInTreeFilter oFilterToShowAfter = new CTasksInTreeFilter();
        oFilterToShowAfter.setUseThisFilter(true);
        oFilterToShowAfter.setRelevantMaskBit(AFilter.BIT_ACCEPTIONSTATE_SET, true);
        oFilterToShowAfter.setDataMaskBit(AFilter.BIT_ACCEPTIONSTATE_SET, false);
        oFilterToShowAfter.addAllTasks();
        oFilterToShowAfter.addAllCategories();
        oFilterToShowAfter.addAllPersons();
        s_oInstance._newViewByFilter(oFilterToShowAfter);
      }
      
      if (oNotificationEffortTree.size() > 0)
      {
        s_oInstance.notifyControllers(EUpdateMessage.EffortsCreated, oNotificationEffortTree);
        FredStatusBar.setTextForInterval(Labels.get("STATUSBAR_EFFORT_COLLECTED"));
      }
    } // oDialog.wasSuccessful()
  }


  /**
   * {Kurzbeschreibung}<br>
   * <br>
   * {Beschreibung}
   * 
   * @remarks {Anmerkungen}
   * @pre
   * @post
   */
  public static void incomingExtrasOptions()
  {
    if (s_oInstance.m_eWindowContent == EWindowContent.Views)
    {
      CDlgExtrasOptions oDialog = null;
      
      oDialog = new CDlgExtrasOptions();
      
      if (oDialog.showDialog() == EReturnValue.OK)
      {
        CPersonImpl oCurrentUser = MainController.getInstance().getUser();
        
        oCurrentUser.setFirstName(oDialog.getFirstName());
        oCurrentUser.setName(oDialog.getUserName());
        oCurrentUser.setInitials(oDialog.getInitials());
        
        s_oInstance.m_oWorkfile.setChanged();
        s_oInstance.notifyObservers();
      }
      
      return;
    }
    
    Shout.newError(s_oInstance.getClass().getName() + "#incomingExtrasOptions",
    "should not be usable in this configuration");
  }

  public static void incomingExtrasGlobalSettings()
  {
    CDlgExtrasGlobalSettings oDialog = new CDlgExtrasGlobalSettings();

    if (oDialog.showDialog() == EReturnValue.OK)
    {
      /*
       * CPersonImpl oCurrentUser = MainController.getInstance().getUser();
       * oCurrentUser.setFirstName(oDialog.getFirstName());
       * oCurrentUser.setName(oDialog.getUserName());
       * oCurrentUser.setInitials(oDialog.getInitials());
       * this.m_oWorkfile.setChanged(); notifyObservers();
       */
    }
  }
  
  /**
   * Die Hilfe wird angezeigt.
   */
  public static void incomingHelpShowHelp()
  {
    String strManualPath = GlobalSettings.getManualPath();
    
    if ("".equals(strManualPath) == true)
    {
      //
      // Pfad zur PDF-Datei soll automatisch bestimmt werden.
      //
      strManualPath = ManualHelper.getDefaultManualPath();
      
      if (new File(strManualPath).exists() == false)
      {
        FredMessageBox.showInfoDlg(Labels.get("INFORMATION_HELP_AUTOMANUALPATH_ERROR") + strManualPath,
                                   Labels.get("TITLE_HELP"),
                                   MessageBox.EIcon.ERROR);
        
        return;
      }
    }
    else
    {
      //
      // Pfad zur PDF-Datei ist gegeben
      //
      if (new File(strManualPath).exists() == false)
      {
        FredMessageBox.showInfoDlg(Labels.get("INFORMATION_HELP_PATH_ERROR"),
                                   Labels.get("TITLE_HELP"),
                                   MessageBox.EIcon.ERROR);
        
        return;
      }
    }

    try
    {
      ManualHelper.showManual(GlobalSettings.getPDFReaderPath(), strManualPath);
    }
    catch (Exception oException)
    {
      FredMessageBox.showInfoDlg(Labels.get("INFORMATION_HELP_SHOW_ERROR"),
                                 Labels.get("TITLE_HELP"),
                                 MessageBox.EIcon.ERROR,
                                 oException);
    }
  }
  
  /**
   * Die Quick-Start-Anleitung wird angezeigt.
   */
  public static void incomingHelpQuickstart()
  {
    CDlgHelpQuickStart.showQuickstartDlg();
  }

  /**
   * Der Help-About-Dialog wird angezeigt.
   */
  public static void incomingHelpShowAbout()
  {
    new CDlgHelpShowAbout();
  }

  /**
   * Eine neue View wird angezeigt.
   * 
   * @param p_eState Der Status des Tabs. 
   */
  public static void incomingTabChange(EState p_eState)
  {
    if (p_eState == EState.ACTIVATED)
    {
      CView oCurrentView = s_oInstance.m_oMainWindow.getCurrentView();
      if (oCurrentView != null)
      {
        CViewController oCurrentViewController = oCurrentView.getViewController();
        CViewController oLastViewController = s_oInstance._getCurrentViewController();

        if (oLastViewController == null || oLastViewController != oCurrentViewController)
          s_oInstance._addViewControllerToTop(oCurrentViewController);
      }
    }
  }

  /**
   * Diese Funktion wird aufgerufen, wenn man im About-Dialog Strg+Doppelklick macht
   * 
   * Sie dient als Hintertür und macht z.Z.:
   * <ul>
   * <li>"Anja ist die Beste!<br>Julia auch!" ausgeben
   * </ul>
   * 
   */
  public static void incomingAnjaFunction()
  {
    FredMessageBox.showInfoDlg("Anja ist die Beste!\nJulia auch!",
                               "Hinweis von den Programmierern!",
                               MessageBox.EIcon.INFORMATION);
  }
  
  /**
   * Diese Funktion wird aufgerufen, wenn man im About-Dialog Alt+Doppelklick macht
   * 
   * Sie dient als Hintertür und macht z.Z.:
   * <ul>
   * <li>nicht den inoffiziellen Import starten
   * </ul>
   */
  @SuppressWarnings("unused")
  public static void incomingInofficialImport()
  {
    /*ProjectList oPL = ProjectList.getInstance();

    int iReturnResult = JOptionPane.showConfirmDialog(
        getMainWindow(),
        "Du betrittst den inoffiziellen Importbereich!\nDieser ist nicht dokumentiert und wird nicht supported!",
        "Hinweis vom Programmierer!",
        JOptionPane.OK_CANCEL_OPTION,
        JOptionPane.QUESTION_MESSAGE);

    if (iReturnResult == JOptionPane.CANCEL_OPTION)
      return;
    
    CProjectImpl[] aoProjects = new CProjectImpl[oPL.getProjectCount()];
    int i = 0;
    
    for (CProjectImpl oThisProject : oPL)
    {
      aoProjects[i++] = oThisProject;
    }
    
    Object oSelectedValue = JOptionPane.showInputDialog(getMainWindow(),
      "In welches Projekt importieren?",
      "Frage vom Programmierer!",
      JOptionPane.QUESTION_MESSAGE,
      null,
      aoProjects,
      null);
    
    if (oSelectedValue == null)
      return;

    CProjectImpl oSelectedProject = (CProjectImpl) oSelectedValue;
    
    // Es wird eine Hashmap mit Name >> Task erzeugt, diese wird zum späteren Suchen benötigt
    HashMap<String, CTaskImpl> oAvailableTasks = new HashMap<String, CTaskImpl>();
    for (CTaskImpl oThisTask : oSelectedProject)
    {
      oAvailableTasks.put(oThisTask.getName(), oThisTask);
    }
    
    //Es wird die Quelldatei gesucht
    //  FileChooser Instanz erzeugen falls nötig
    FileChooser oFileChooser = FileChooser.getFileChooser(true);
    
    // Stelle das Verzeichnis ein, dessen Inhalt der FileChooser
    // zu begin, nach dem öffnen, anzeigen wird.
    oFileChooser.setCurrentDirectory(null);
    
    // Erlaube nur das Auswählen von Dateien, verbiete das
    // Auswählen von Verzeichnissen.
    oFileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    
    // Verbiete den Filter, der alle Dateiendungen und Verzeichnisse anzeigt.
    oFileChooser.setAcceptAllFileFilterUsed(false);
    
    // Erlaube nur die das Anzeigen von Dateien mit der Endung .html, .HTML
    // und Verzeichnissen.
    oFileChooser.addChoosableFileFilter(new CDlgEffortExportEfforts.CCSVFileFilter());

    // Zeige einen FileChooser mit besonderer Beschriftung des Submit
    // Buttons und mit besonderen Titel.
    int iDecision = oFileChooser.showDialog(MainController.getInstance().getMainWindow(), 
                                            "Importieren");
    
    if (iDecision != JFileChooser.APPROVE_OPTION)
      return;
    
    String strSourcePath = oFileChooser.getSelectedFile().toString().trim().toLowerCase();
    
    try
    {
      FileReader oFileReader = new FileReader(strSourcePath);
      BufferedReader oBufferedReader = new BufferedReader(oFileReader);

      String strRead;
      String[] astrReadTokens;
      String strTask;
      String strBeginDate;
      String strBeginHour;
      String strEndDate;
      String strEndHour;
      String strDescription;
      
      CTaskImpl oTask;
      GregorianCalendar oBegin;
      GregorianCalendar oEnd;
      CEffortImpl oEffort;
      
      while (oBufferedReader.ready())
      {
        strRead = oBufferedReader.readLine();
        astrReadTokens = strRead.split(";");
        strTask = astrReadTokens[7];
        strBeginDate = astrReadTokens[2];
        strBeginHour = astrReadTokens[3];
        strEndDate = astrReadTokens[4];
        strEndHour = astrReadTokens[5];
        strDescription = astrReadTokens[8];
        
        oTask = oAvailableTasks.get(strTask);
        if (oTask == null)
        {
          System.out.println("Unbekannter Taskname: \"" + strTask + "\" | \"" + strRead + "\" ignoriert");
          continue;
        }
        
        oBegin = DateTime.getCalendar(strBeginDate, strBeginHour);
        if (oBegin == null)
        {
          System.out.println("Datum/Uhrzeit nicht lesbar: \"" + strBeginDate + " " + strBeginHour + "\" | \"" + strRead + "\" ignoriert");
          continue;
        }        
        
        oEnd = DateTime.getCalendar(strEndDate, strEndHour);
        if (oEnd == null)
        {
          System.out.println("Datum/Uhrzeit nicht lesbar: \"" + strEndDate + " " + strEndHour + "\" | \"" + strRead + "\" ignoriert");
          continue;
        }
        
        // NB: Der Task ist erkannt worden, die Daten und Uhrzeiten sind geladen worden
        
        oEffort = CEffortImpl.create();
        
        oEffort.setTask(oTask);
        oEffort.setCategory(oTask.getDefaultCCategory());
        oEffort.setBegin(oBegin);
        oEffort.setEnd(oEnd); // TODO Ändern
        oEffort.setDescription(strDescription);
        oEffort.setApproved(true);
        
        ProjectList.getInstance().addEffort(oEffort);
        
        notifyControllers(EUpdateMessage.EffortsCreated, new CSingleCol<CEffortImpl>(oEffort));
      }
      
      FredStatusBar.setTextForInterval("Import abgeschlossen");
    }
    catch (FileNotFoundException p_oException)
    {
      Shout.printException(p_oException);
      
      JOptionPane.showMessageDialog(
        getMainWindow(),
        p_oException.getMessage(),
        "Hinweis vom Programmierer!",
        JOptionPane.ERROR_MESSAGE);
    }
    catch (IOException p_oException)
    {
      Shout.printException(p_oException);
      
      JOptionPane.showMessageDialog(
        getMainWindow(),
        p_oException.getMessage(),
        "Hinweis vom Programmierer!",
        JOptionPane.ERROR_MESSAGE);
    }*/
  }
  
  /**
   * Schließen des Splash-Screens
   * 
   * @param p_oMainWindow Das referenzierte Hauptfenster.
   */
  @SuppressWarnings("synthetic-access")
  public void prepareSplashScreenClose(JFrame p_oMainWindow)
  {
    p_oMainWindow.addWindowListener(new IMainWindowListener());
  }
  
  private class IMainWindowListener extends WindowAdapter
  {
    @Override
    public void windowOpened(WindowEvent p_oEvent)
    {
      SplashScreen.closeSplashScreen(0);
    }
  }
  
  /**
   * Updatenachricht, dass sich an dem Model etwas geändert hat.
   * 
   * @author cm
   * @version 1.0 19.06.2006 erstellt<br>
   * @see MainController#notifyControllers(EUpdateMessage, Collection)
   */
  public enum EUpdateMessage
  {
    /** Ein oder mehrere Projekte wurden erstellt */
    ProjectCreated,
    /** Ein oder mehrere Projekte wurden verändert */
    ProjectUpdated,
    /** Ein oder mehrere Projekte wurden gelöscht */
    ProjectDeleted,
    /** Ein oder mehrere Tasks wurden erstellt */
    TasksCreated,
    /** Ein oder mehrere Tasks wurden verändert */
    TaskUpdated,
    /** Ein oder mehrere Tasks wurden gelöscht */
    TaskDeleted,
    /** Ein oder mehrere Aufwände wurden erstellt */
    EffortsCreated,
    /** Ein oder mehrere Aufwände wurden verändert */
    EffortsUpdated,
    /** Ein oder mehrere Aufwände wurden gelöscht */
    EffortsDeleted,
    /** Es wurden Personen hinzugefügt */
    PersonsAdded,
    /** Es wurden Personen gelöscht */
    PersonsDeleted,
    /** Die GUI muss sich updaten */
    VisualUpdate,
    /** Der ViewController wird angezeigt. */
    BecomesVisible,
  }

  /**
   * {Kurzbeschreibung}<br>
   * <br>
   * {Beschreibung}
   * 
   * @author sk
   * @version 1.0 20.05.2006 erstellt<br>
   */
  private static class CExitListener implements IWindowCloseListener
  {
    @SuppressWarnings("synthetic-access")
    public boolean windowClosingPerformed()
    {
      return MainController.getInstance()._shutDown();
    }
  }

  /** Factory zum Erstellen des Benutzers */
  // FIXME Rename
  public class CUserFactory extends AChildlessXMLObjectContainer<CPersonImpl>
  {
    @SuppressWarnings("synthetic-access")
    @Override
    protected CPersonImpl createContainedObject(Attributes p_oAtts, int p_iFileVersion)
    {
      String strID = p_oAtts.getValue(CXMLElementUser.XML_USER_ATT_ID);
      CPersonImpl oUser = ObjectFactoryImpl.createCPersonImpl(strID);
      MainController.this.m_oUser = oUser;
      return oUser;
    }
  }

  /** Factory zum Erstellen des DefaultTasks */
  // FIXME Rename
  public class CDefaultTaskFactory extends AChildlessXMLObjectContainer<CTaskImpl>
  {
    @SuppressWarnings("synthetic-access")
    @Override
    protected CTaskImpl createContainedObject(Attributes p_oAtts, int p_iFileVersion)
    {
      String strID = p_oAtts.getValue(CXMLElementDefaultTask.XML_DEFAULT_TASK_ATT_ID);
      CTaskImpl oDefaultTask = ObjectFactoryImpl.createCTaskImpl(strID);
      MainController.this.m_oDefaultTask = oDefaultTask;
      return oDefaultTask;
    }
  }

  /** Factory zum Erstellen der Quickbarposition */
  // FIXME Rename
  public class CQuickbarFactory extends AChildlessXMLObjectContainer<EDockBorder>
  {
    @SuppressWarnings("synthetic-access")
    @Override
    protected EDockBorder createContainedObject(Attributes p_oAtts, int p_iFileVersion)
    {
      String strIName = p_oAtts.getValue(CXMLRootElementView.XML_QUICKBAR_ATT_POS);
      return EDockBorder.valueOf(strIName);
    }
  }
  
  /**
   * Erzeugt aus einer XML-Datei die ViewController.<br>
   * <br>
   *
   * @author cm
   * @version 1.0 28.06.2006 erstellt<br>
   */
  // FIXME Rename und statische Kind-Container
  public class CViewsFactory extends AXMLObjectContainer<Object>
  {
    @SuppressWarnings("synthetic-access")
    @Override
    protected Object createContainedObject(Attributes p_oAtts, int p_iFileVersion)
    {
      // TODO Häh? Ist das sicher, dass diese Methode nur einmal aufgerufen wird?
      MainController.this.m_oViewControllers = new Vector<CViewController>();
      
      return null;
    }
    
    @Override
    protected AXMLObjectContainer<?> newChildContainer(String p_strLocalName, int p_iFileVersion)
    {
      if (p_strLocalName.equals(CViewController.XML_TAG_NAME))
        return new CViewController.CViewStoreContainer();

      return null;
    }

    @SuppressWarnings("synthetic-access")
    @Override
    protected void childContainerCompleted(String p_strLocalName, Object oCreated, int p_iFileVersion)
    {
      MainController.getInstance()._addViewControllerToTop((CViewController) oCreated);
    }
  }
  
  /**
   * XMLizer für das Model.
   *
   * @author cm
   * @version 1.0 22.07.2006 erstellt<br>
   */
  public class CXMLRootElementModel implements IXMLizable
  {
    /**
     * Speichert das gesamte Model.<br>
     * <br>
     * Zur Konfiguration gehört:
     * <ul>
     * <li>ObjectFactory-Dimension (ProjectCount, TaskCount, EffortCount,
     * PersonCount)
     * <li>Personenliste
     * <li>Benuter des Programms
     * <li>URLListe
     * <li>Projekte, Arbeitspakete, Aufwände
     * <li>DefaultTask
     * </ul>
     * 
     * @param p_oXMLStream Der ToXMLStrom.
     * @throws SAXException Falls es XML-Fehler beim Erzeugen gab.
     */
    @SuppressWarnings("synthetic-access")
    public void xmlize(CSimpleXMLStream p_oXMLStream) throws SAXException
    {
      // Zahl der Projekte, Taks, Aufwände und Personen
      ObjectFactoryImpl.getInstance().xmlize(p_oXMLStream);
  
      // Personen
      ObjectFactoryImpl.getInstance().xmlizePersons(p_oXMLStream);
  
      // der User der Arbeitsdatei
      MainController.this.new CXMLElementUser().xmlize(p_oXMLStream);
  
      // URL-Liste
      MainController.this.getURLList().xmlize(p_oXMLStream);
  
      // Projekte, Tasks, Aufwände
      ProjectList.getInstance().xmlize(p_oXMLStream);
    }
  }
  
  /**
   * XMLizer für den Benutzer.
   *
   * @author cm
   * @version 1.0 22.07.2006 erstellt<br>
   */
  public class CXMLElementUser implements IXMLizable
  {
    /** XMLTag für den Benutzer */
    public static final String XML_USER_TAG_NAME = "user";
  
    private static final String XML_USER_ATT_ID = "id";
    
    public void xmlize(CSimpleXMLStream p_oXMLStream) throws SAXException
    {
      // der User der Arbeitsdatei
      p_oXMLStream.startElement(XML_USER_TAG_NAME);
      p_oXMLStream.addAttribute(XML_USER_ATT_ID, MainController.this.getUser().getID());
      p_oXMLStream.endElement(XML_USER_TAG_NAME);
    }
  }
  
  /**
   * XMLizer für den DefaultTask.
   *
   * @author cm
   * @version 1.0 22.07.2006 erstellt<br>
   */
  public class CXMLElementDefaultTask implements IXMLizable
  {
    /** XMLTag für den DefaultTask */
    public static final String XML_DEFAULT_TASK_TAG_NAME = "defaulttask";
  
    private static final String XML_DEFAULT_TASK_ATT_ID = "id";
    
    public void xmlize(CSimpleXMLStream p_oXMLStream) throws SAXException
    {
      // der DefaultTask
      if (MainController.this.getDefaultTask() != null)
      {
        p_oXMLStream.startElement(XML_DEFAULT_TASK_TAG_NAME);
        p_oXMLStream.addAttribute(XML_DEFAULT_TASK_ATT_ID, MainController.this.getDefaultTask().getID());
        p_oXMLStream.endElement(XML_DEFAULT_TASK_TAG_NAME);
      }
    }
  }
  
  /**
   * XMLizer für die Konfiguration und die View.
   *
   * @author cm
   * @version 1.0 22.07.2006 erstellt<br>
   */
  public class CXMLRootElementView implements IXMLizable
  {
    public static final String XML_GUI_TAG_NAME = "guidata";
    
    /** XMLTag für die Quickbar */
    public static final String XML_QUICKBAR_TAG_NAME = "quickbar";
    
    /** XMLTag für die Position der Quickbar */
    public static final String XML_QUICKBAR_ATT_POS = "dockborder";
    
    /** XMLTag für die Views */
    public static final String XML_VIEWS_TAG_NAME = "views";
  
    /**
     * Speichert die gesamte View.<br>
     * <br>
     * Dazu gehört:
     * <ul>
     * <li>Ansichten
     * <ul>
     * <li>Filter im Baum
     * <li>Filter im Dialog </lu>
     * </ul>
     * 
     * @param p_oXMLStream Der ToXMLStrom.
     * @throws SAXException Falls es XML-Fehler beim Erzeugen gab.
     */
    @SuppressWarnings("synthetic-access")
    public void xmlize(CSimpleXMLStream p_oXMLStream) throws SAXException
    {
      //
      // der DefaultTask
      //
      MainController.this.new CXMLElementDefaultTask().xmlize(p_oXMLStream);
      
      //
      // Konfiguration sichern
      //
      Configuration.xmlizeConfiguration(p_oXMLStream);      
      
      //
      // GUI-Data sichern
      //
      p_oXMLStream.startElement(XML_GUI_TAG_NAME);
      
      // Die Position der Quickbar
      p_oXMLStream.startElement(XML_QUICKBAR_TAG_NAME);
      String strDockboder = CQuickBar.getInstance().getDockBorder().name();
      p_oXMLStream.addAttribute(XML_QUICKBAR_ATT_POS, strDockboder);
      p_oXMLStream.endElement(XML_QUICKBAR_TAG_NAME);
      
      // die views
      if (Configuration.getBoolean(EConf.SaveViews))
      {
        p_oXMLStream.startElement(XML_VIEWS_TAG_NAME);
        Iterator<CViewController> oITeratorViewControllers =
          MainController.this.m_oViewControllers.iterator();
        while (oITeratorViewControllers.hasNext())
        {
          oITeratorViewControllers.next().xmlize(p_oXMLStream);
        }
        p_oXMLStream.endElement(XML_VIEWS_TAG_NAME);
      }
      
      p_oXMLStream.endElement(XML_GUI_TAG_NAME);
    }
  }
}
