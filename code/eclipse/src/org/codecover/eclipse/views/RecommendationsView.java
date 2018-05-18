package org.codecover.eclipse.views;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.xml.parsers.ParserConfigurationException;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.utils.EclipseMASTLinkage;
import org.codecover.eclipse.utils.EclipseMASTLinkage.MAST;
import org.codecover.eclipse.utils.recommendationgenerator.Cache;
import org.codecover.eclipse.utils.recommendationgenerator.ChangeErrorDataSourceWeightWizard;
import org.codecover.eclipse.utils.recommendationgenerator.ErrorDataSource;
import org.codecover.eclipse.utils.recommendationgenerator.ErrorDataWizard;
import org.codecover.eclipse.utils.recommendationgenerator.ErrorIndicator;
import org.codecover.eclipse.utils.recommendationgenerator.ErrorIndicatorMode;
import org.codecover.eclipse.utils.recommendationgenerator.MalformedErrorIndicatorFileException;
import org.codecover.eclipse.utils.recommendationgenerator.Parameter;
import org.codecover.eclipse.utils.recommendationgenerator.RecommendationGenerator;
import org.codecover.eclipse.utils.recommendationgenerator.UncoveredBranch;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.HierarchyLevel;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IPackageDeclaration;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.xml.sax.SAXException;

public class RecommendationsView extends CodeCoverView {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "org.codecover.eclipse.views.RecommendationsView"; //$NON-NLS-1$

	private TableViewer viewer;
	private Action openWeightWizardAction;
	private Action openCodeWizardAction;

	RecommendationGenerator recommendationGenerator;

	private Action openVersionWizardAction;

	private Action openCCWizardAction;

	private Action openProcessWizardAction;

	private Action openExpertWizardAction;

	private Action openQSWizardAction;

	RecViewViewContentProvider recViewViewContentProvider;

	private Action executeSortingAction;

	private Action packageFilterAction;

	private Action filterClassAction;

	private Action delClassFilterAction;

	private Action showDetailsAction;

	private Action createInfoFileAction;

	private Action exportAction;

	private RecViewViewLabelProvider recViewViewLabelProvider;

	public RecommendationsView() {
		TestSessionContainer testSessionContainer = CodeCoverPlugin.getDefault().getTSContainerManager().getActiveTSContainer()
				.getTestSessionContainer();

		recommendationGenerator = new RecommendationGenerator(testSessionContainer, null);
	}

	@Override
	public void init(IViewSite site, IMemento memento) throws PartInitException {
		super.init(site, memento);
		if (memento == null)
			return;
		try {
			for (ErrorDataSource eds : this.recommendationGenerator.getErrorDataSources()) {
				Float float1 = memento.getFloat(fix(eds.getName()) + "-weight"); //$NON-NLS-1$
				try {
					eds.setWeight(float1);
				} catch (Exception e) {
					eds.setWeight(1.0);
				}
				for (ErrorIndicator ei : eds.getErrorIndicators()) {
					String mode = memento.getString(fix(ei.getName()) + "-mode");
					if (mode == null) {
						ei.setMode(ErrorIndicatorMode.OFF);
					} else {
						ei.setMode(ErrorIndicatorMode.valueOf(mode));
					}
					try {
						ei.setXmlErrorDataInformation(new File(memento.getString(fix(ei.getName()) + "-xmlfile")));
					} catch (NumberFormatException e) {
						e.printStackTrace();
					} catch (ParserConfigurationException e) {
						e.printStackTrace();
					} catch (SAXException e) {
						e.printStackTrace();
					} catch (IOException e) {
						e.printStackTrace();
					} catch (MalformedErrorIndicatorFileException e) {
						e.printStackTrace();
					} catch (NullPointerException e) {
					}

					for (Parameter p : ei.getParameters()) {
						try {
							if (p.getType() instanceof String) {
								p.setValue(unfix2(memento.getString(fix(ei.getName() + "-" + p.getName()) + "-value")));
							} else if (p.getType() instanceof Boolean) {
								p.setValue(Boolean.valueOf(memento.getBoolean(fix(ei.getName() + "-" + p.getName()) + "-value")));
							} else if (p.getType() instanceof Integer) {
								p.setValue(Integer.valueOf(memento.getInteger(fix(ei.getName() + "-" + p.getName()) + "-value")));
							} else if (p.getType() instanceof File) {
								try {
									String filepath = unfix2(memento.getString(fix(ei.getName() + "-" + p.getName()) + "-value"));
									p.setValue(new File(filepath));
								} catch (NullPointerException e) {
								}
							}
						} catch (NullPointerException e) {

						}
					}
				}
			}
		} catch (NullPointerException e) {
			e.printStackTrace();
		}
	}

	@Override
	public void saveState(IMemento memento) {
		super.saveState(memento);
		for (ErrorDataSource eds : this.recommendationGenerator.getErrorDataSources()) {
			memento.putFloat(fix(eds.getName()) + "-weight", (float) eds.getWeight()); //$NON-NLS-1$
			for (ErrorIndicator ei : eds.getErrorIndicators()) {
				memento.putString(fix(ei.getName()) + "-mode", ei.getMode().name());
				if (ei.getXmlErrorDataInformation() != null) {
					memento.putString(fix(ei.getName()) + "-xmlfile", ei.getXmlErrorDataInformation().getAbsolutePath());
				}
				for (Parameter p : ei.getParameters()) {
					if (p.getType() instanceof String) {
						memento.putString(fix(ei.getName()) + "-" + fix(p.getName()) + "-value", fix2((String) p.getValue())); //$NON-NLS-1$//$NON-NLS-2$
					} else if (p.getType() instanceof Boolean) {
						memento.putBoolean(fix(ei.getName()) + "-" + fix(p.getName()) + "-value", (Boolean) p.getValue());
					} else if (p.getType() instanceof Integer) {
						memento.putInteger(fix(ei.getName()) + "-" + fix(p.getName()) + "-value", (Integer) p.getValue());
					} else if (p.getType() instanceof File) {
						try {
							String filepath = fix2(((File) p.getValue()).getAbsolutePath());
							memento.putString(fix(ei.getName()) + "-" + fix(p.getName()) + "-value", filepath);
						} catch (NullPointerException e) {
						}
					}
				}
			}
		}

	}

	private String fix(String s) {
		// Das saudoofe Memento frisst keine Lücken in Strings. Natürlich
		// undokumentiert....
		return s.replaceAll(" ", "");
	}

	private String fix2(String s) {
		// Das saudoofe Memento frisst keine Lücken in Strings. Natürlich
		// undokumentiert....
		return s.replaceAll(" ", "XXXXXXX");
	}

	private String unfix2(String s) {
		return s.replaceAll("XXXXXXX", " ");
	}

	/**
	 * This is a callback that will allow us to create the viewer and initialize
	 * it.
	 */
	@Override
	public void createPartControl(Composite parent) {

		performUpdate();
		viewer = new TableViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		recViewViewContentProvider = null;
		recViewViewContentProvider = new RecViewViewContentProvider(this, viewer, this.recommendationGenerator);

		viewer.setContentProvider(recViewViewContentProvider);
		recViewViewLabelProvider = new RecViewViewLabelProvider(this);
		viewer.setLabelProvider(recViewViewLabelProvider);
		viewer.setInput(getViewSite());
		viewer.setComparator(null);

		Table table = viewer.getTable();
		table.setLayoutData(new GridData(GridData.FILL_BOTH));
		TableColumn column = new TableColumn(table, SWT.LEFT);
		column.setText("Methode");
		column.setWidth(485);

		column = new TableColumn(table, SWT.LEFT);
		column.setText("Statement");
		column.setWidth(340);
		
		column = new TableColumn(table, SWT.LEFT);
		column.setText("Typ");
		column.setWidth(90);
		
		column = new TableColumn(table, SWT.LEFT);
		column.setText("Summe");
		column.setWidth(45);

		column = new TableColumn(table, SWT.LEFT);
		column.setText("Code");
		column.setWidth(45);

		column = new TableColumn(table, SWT.LEFT);
		column.setText("Vers.gesch.");
		column.setWidth(45);

		column = new TableColumn(table, SWT.LEFT);
		column.setText("CC-Test");
		column.setWidth(45);

		column = new TableColumn(table, SWT.LEFT);
		column.setText("Experten");
		column.setWidth(45);

		column = new TableColumn(table, SWT.LEFT);
		column.setText("Prozess");
		column.setWidth(45);

		column = new TableColumn(table, SWT.LEFT);
		column.setText("QS");
		column.setWidth(45);
		
		column = new TableColumn(table, SWT.LEFT);
		column.setText("# LOC");
		column.setWidth(45);

		column = new TableColumn(table, SWT.LEFT);
		column.setText("# Testfälle");
		column.setWidth(45);


		

		// // Pack the columns
		// for (int i = 0, n = table.getColumnCount(); i < n; i++) {
		// table.getColumn(i).pack();
		// }

		// Turn on the header and the lines
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
		table.pack();

		viewer.refresh();

		// Create the help context id for the viewer's control
		PlatformUI.getWorkbench().getHelpSystem().setHelp(viewer.getControl(), "org.codecover.eclipse.viewer");
		makeActions();
		hookContextMenu();
		hookDoubleClickAction();
		ActiveTSContainerInfo activeTSContainer = CodeCoverPlugin.getDefault().getTSContainerManager().getActiveTSContainer();
		this.setViewerInput(activeTSContainer);

		contributeToActionBars();
	}

	private void hookContextMenu() {
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			@Override
			public void menuAboutToShow(IMenuManager manager) {
				RecommendationsView.this.fillContextMenu(manager);
			}
		});
		Menu menu = menuMgr.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(menu);
		getSite().registerContextMenu(menuMgr, viewer);
	}

	private void contributeToActionBars() {
		IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}

	private void fillLocalPullDown(IMenuManager manager) {
		manager.add(openWeightWizardAction);
		manager.add(new Separator());
		manager.add(openCodeWizardAction);
	}

	private void fillContextMenu(IMenuManager manager) {
		// manager.add(openWeightWizardAction);
		// manager.add(openCodeWizardAction);
		manager.add(filterClassAction);
		manager.add(showDetailsAction);
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}

	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(exportAction);
		manager.add(createInfoFileAction);
		manager.add(delClassFilterAction);
		manager.add(packageFilterAction);
		manager.add(executeSortingAction);
		manager.add(openWeightWizardAction);
		manager.add(openCodeWizardAction);
		manager.add(openVersionWizardAction);
		manager.add(openCCWizardAction);
		manager.add(openProcessWizardAction);
		manager.add(openExpertWizardAction);
		manager.add(openQSWizardAction);
	}

	private void makeActions() {
		
		exportAction = new Action() {
			@Override
			public void run() {
				StringBuilder csv = new StringBuilder();
				csv.append("\"Methode\",\"Statement\",\"LOC\",\"# TF\",\"Typ\",\"Code\",\"Version\",\"CC\",\"Experten\",\"Prozess\",\"QS\"\n");
				FileDialog fg = new FileDialog(new Shell(), SWT.SAVE);
				fg.setText("Datei auswählen...");
				String file = fg.open();
				if (file != null && file.length() > 2) {
					File selectedFile = new File(file);
					int row = 0;
					while (viewer.getElementAt(row) != null) {
						for (int col = 0; col < viewer.getTable().getColumnCount(); col++) {
							csv.append("\""+RecommendationsView.this.recViewViewLabelProvider.getColumnText(viewer.getElementAt(row), col)+"\",");
						}
						row++;
						csv.append("\n");
					}
					String csvOut = csv.toString().replaceAll(",\n", "\n");
					try {
						new FileWriter(selectedFile).append(csv.toString());
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}
		};
		exportAction.setText("CSV-Export");
		
		createInfoFileAction = new Action() {
			@Override
			public void run() {
				List<String> classes = new ArrayList<String>();
				TestSessionContainer tsc = RecommendationsView.this.getVisTSC();
				for (HierarchyLevel lev : getClasses(tsc.getCode())) {
					ICompilationUnit iCompilationUnit = Cache.hLevToCompUnitCache.get(lev);
					if (iCompilationUnit == null) {
						String fqn = MAST.getFQName(lev, getVisTSC());
						Set<ICompilationUnit> cuSet;
						cuSet = EclipseMASTLinkage.Eclipse.findCompilationUnit(fqn);
						if (cuSet.size() > 0) {
							iCompilationUnit = cuSet.iterator().next();
							if (iCompilationUnit != null) {
								Cache.hLevToCompUnitCache.put(lev, iCompilationUnit);
							}
						}
					}
					if (iCompilationUnit == null) {
						continue;
					}
					String name = lev.getName();
					String packagge = "";
					try {
						IPackageDeclaration[] packageDeclarations = iCompilationUnit.getPackageDeclarations();
						for (IPackageDeclaration ipd : packageDeclarations) {
							packagge = ipd.getElementName();
						}
					} catch (JavaModelException e) {
						e.printStackTrace();
					}
					if (name.length() > 0 && packagge.length() > 0) {
						String ganz = packagge+"."+name;
						System.out.println("ADD: "+ganz);
						classes.add(ganz);
					}
					
					
					
				}
				
				
				for (String c : classes) {
					System.out.println(c);
				}
				new CreateErrorInfoFileDialog(new Shell(), classes);
				
			}
			private List<HierarchyLevel> getClasses(HierarchyLevel lev) {
				List<HierarchyLevel> list = new ArrayList<HierarchyLevel>();
				if (lev.getType().getInternalName().equals("class")) {
					list.add(lev);
				}
				for (HierarchyLevel child : lev.getChildren()) {
					list.addAll(getClasses(child));
				}
				return list;
			}
			
			/*
			private List<IJavaElement> getClasses(IJavaElement e) throws JavaModelException {
				List<IJavaElement> list = new ArrayList<IJavaElement>();
				if (e instanceof PackageFragmentRoot) {
					for (IJavaElement ee : ((PackageFragmentRoot)e).getChildren()) {
						System.out.println(ee.getClass().getCanonicalName());
						list.addAll(getClasses(ee));
					}
				}
				if (e instanceof PackageFragment) {
					for (ICompilationUnit cu : ((PackageFragment)e).getCompilationUnits()) {
						System.out.println("ADD - "+cu.getElementName());
						list.add(cu.getElementAt(0));
					}
				}
				return list;
			}*/
		};
		createInfoFileAction.setText("Neue Info-Datei anlegen");

		showDetailsAction = new Action() {
			@Override
			public void run() {
				ISelection selection = RecommendationsView.this.viewer.getSelection();
				if (selection instanceof IStructuredSelection) {
					IStructuredSelection sS = (IStructuredSelection) selection;
					for (Object o : sS.toArray()) {
						if (o instanceof UncoveredBranch) {
							String message = "Ergebnisse der ErrorDataSources und ErrorIndicators:\n"
									+ ((UncoveredBranch) o).info;
							showMessage(message);
						}
					}
				}

			}
		};
		showDetailsAction.setText("Details anzeigen");

		executeSortingAction = new Action() {
			@Override
			public void run() {
				RecommendationsView.this.recViewViewContentProvider.sort();
			}
		};
		executeSortingAction.setText("Sortieren");
		executeSortingAction.setToolTipText("Sortierung durchführen");
		executeSortingAction.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry()
				.getDescriptor(CodeCoverPlugin.Image.REC_SORT.getPath()));

		packageFilterAction = new Action() {
			@Override
			public void run() {
				ConfigureFiltersWizard wizard = new ConfigureFiltersWizard(RecommendationsView.this);
				WizardDialog dialog = new WizardDialog(RecommendationsView.this.getSite().getShell(), wizard);
				dialog.create();
				dialog.setBlockOnOpen(true);
				dialog.open();
			}
		};
		packageFilterAction.setText("Filter auswählen");
		packageFilterAction.setToolTipText("Filter auswählen");
		packageFilterAction.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry()
				.getDescriptor(CodeCoverPlugin.Image.REC_FILTER.getPath()));

		openWeightWizardAction = new Action() {
			@Override
			public void run() {
				ChangeErrorDataSourceWeightWizard wizard = new ChangeErrorDataSourceWeightWizard(
						RecommendationsView.this.recommendationGenerator);
				WizardDialog dialog = new WizardDialog(RecommendationsView.this.getSite().getShell(), wizard);
				dialog.create();
				dialog.setBlockOnOpen(true);
				dialog.open();
				RecommendationsView.this.recViewViewContentProvider.refreshViewWithoutInvoking();
			}
		};

		openWeightWizardAction.setText("Gewichtung einstellen");
		openWeightWizardAction.setToolTipText("Gewichtung einstellen");
		openWeightWizardAction.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry()
				.getDescriptor(CodeCoverPlugin.Image.REC_WIZ_WEIGHT.getPath()));

		openCodeWizardAction = new Action() {
			@Override
			public void run() {
				ErrorDataWizard wizard = new ErrorDataWizard(
						RecommendationsView.this.recommendationGenerator.getCodeErrorDataSource());
				WizardDialog dialog = new WizardDialog(RecommendationsView.this.getSite().getShell(), wizard);
				dialog.create();
				dialog.open();
			}
		};
		openCodeWizardAction.setText("Datenquelle Code konfigurieren");
		openCodeWizardAction.setToolTipText("Datenquelle Code konfigurieren");
		openCodeWizardAction.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry()
				.getDescriptor(CodeCoverPlugin.Image.REC_WIZ_CODE.getPath()));

		openVersionWizardAction = new Action() {
			@Override
			public void run() {
				ErrorDataWizard wizard = new ErrorDataWizard(
						RecommendationsView.this.recommendationGenerator.getVersionErrorDataSource());
				WizardDialog dialog = new WizardDialog(RecommendationsView.this.getSite().getShell(), wizard);
				dialog.create();
				dialog.open();
			}
		};
		openVersionWizardAction.setText("Datenquelle Versionsgeschichte konfigurieren");
		openVersionWizardAction.setToolTipText("Datenquelle Versionsgeschichte konfigurieren");
		openVersionWizardAction.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry()
				.getDescriptor(CodeCoverPlugin.Image.REC_WIZ_VERSION.getPath()));

		openCCWizardAction = new Action() {
			@Override
			public void run() {
				ErrorDataWizard wizard = new ErrorDataWizard(
						RecommendationsView.this.recommendationGenerator.getCcErrorDataSource());
				WizardDialog dialog = new WizardDialog(RecommendationsView.this.getSite().getShell(), wizard);
				dialog.create();
				dialog.open();
			}
		};
		openCCWizardAction.setText("Datenquelle CC-Test konfigurieren");
		openCCWizardAction.setToolTipText("Datenquelle CC-Test konfigurieren");
		openCCWizardAction.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry()
				.getDescriptor(CodeCoverPlugin.Image.REC_WIZ_CC.getPath()));

		openProcessWizardAction = new Action() {
			@Override
			public void run() {
				ErrorDataWizard wizard = new ErrorDataWizard(
						RecommendationsView.this.recommendationGenerator.getProcessDataSource());
				WizardDialog dialog = new WizardDialog(RecommendationsView.this.getSite().getShell(), wizard);
				dialog.create();
				dialog.open();
			}
		};
		openProcessWizardAction.setText("Datenquelle Prozess konfigurieren");
		openProcessWizardAction.setToolTipText("Datenquelle Prozess konfigurieren");
		openProcessWizardAction.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry()
				.getDescriptor(CodeCoverPlugin.Image.REC_WIZ_PROCESS.getPath()));

		openExpertWizardAction = new Action() {
			@Override
			public void run() {
				ErrorDataWizard wizard = new ErrorDataWizard(
						RecommendationsView.this.recommendationGenerator.getExpertErrorDataSource());
				WizardDialog dialog = new WizardDialog(RecommendationsView.this.getSite().getShell(), wizard);
				dialog.create();
				dialog.open();
			}
		};
		openExpertWizardAction.setText("Datenquelle Expertenwissen konfigurieren");
		openExpertWizardAction.setToolTipText("Datenquelle Expertenwissen konfigurieren");
		openExpertWizardAction.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry()
				.getDescriptor(CodeCoverPlugin.Image.REC_WIZ_EXPERT.getPath()));

		openQSWizardAction = new Action() {
			@Override
			public void run() {
				ErrorDataWizard wizard = new ErrorDataWizard(
						RecommendationsView.this.recommendationGenerator.getQualityErrorDataSource());
				WizardDialog dialog = new WizardDialog(RecommendationsView.this.getSite().getShell(), wizard);
				dialog.create();
				dialog.open();
			}
		};
		openQSWizardAction.setText("Datenquelle Qualitätssicherung konfigurieren");
		openQSWizardAction.setToolTipText("Datenquelle Qualitätssicherung konfigurieren");
		openQSWizardAction.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry()
				.getDescriptor(CodeCoverPlugin.Image.REC_WIZ_QS.getPath()));

		filterClassAction = new Action() {
			// FIXME. Actions ins Contextmenu für die Filterung der ausgewählten
			// klasse.
			// RecGenerator informieren, ansicht neu aufbauen
			@Override
			public void run() {
				ISelection selection = RecommendationsView.this.viewer.getSelection();
				if (selection instanceof IStructuredSelection) {
					IStructuredSelection sS = (IStructuredSelection) selection;
					for (Object o : sS.toArray()) {
						if (o instanceof UncoveredBranch) {
							RecommendationsView.this.recommendationGenerator.excludeClass(((UncoveredBranch) o));
						}
					}
				}
				recViewViewContentProvider.refreshViewWithoutInvoking();
			};
		};
		filterClassAction.setText("Diese Klasse ausschließen");

		delClassFilterAction = new Action() {
			@Override
			public void run() {
				RecommendationsView.this.recommendationGenerator.resetClassFilter();
				recViewViewContentProvider.refreshViewWithoutInvoking();
			};
		};
		delClassFilterAction.setText("Klassenfilter löschen");
		delClassFilterAction.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry()
				.getDescriptor(CodeCoverPlugin.Image.REC_DEL_CLASS_FILTER.getPath()));

	}

	private void hookDoubleClickAction() {
		this.viewer.addDoubleClickListener(new IDoubleClickListener() {
			@Override
			public void doubleClick(DoubleClickEvent event) {
				ActiveTSContainerInfo activeTSContainer = CodeCoverPlugin.getDefault().getTSContainerManager()
						.getActiveTSContainer();
				RecommendationsView.this.setViewerInput(activeTSContainer);
				performUpdate();
				ISelection sel;
				Object element;
				if ((sel = RecommendationsView.this.viewer.getSelection()) instanceof IStructuredSelection) {
					element = ((IStructuredSelection) sel).getFirstElement();
					if (element != null && element instanceof UncoveredBranch) {
						/* element is the selected hierarchy level */
						HierarchyLevel hLev = ((UncoveredBranch) element).m_branchInfo.m_hierarchyLevel;

						/*
						 * only react on classes, interfaces, enums, annotations
						 * and methods
						 */
						if (Type.typeOf(hLev) == Type.CLASS || Type.typeOf(hLev) == Type.METHOD) {
							showHierarchyLevelInEditor(hLev);
						}
					}
				}
			}
		});
	}

	private void showMessage(String message) {
		MessageDialog.openInformation(viewer.getControl().getShell(), "RecommendationsView", message);
	}

	/**
	 * Passing the focus request to the viewer's control.
	 */
	@Override
	public void setFocus() {
		viewer.getControl().setFocus();
	}

}