package org.codecover.eclipse.views;

import java.util.ArrayList;
import java.util.List;

import org.codecover.eclipse.utils.recommendationgenerator.RecommendationGenerator;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.internal.core.JavaProject;
import org.eclipse.jdt.internal.ui.dialogs.PackageSelectionDialog;
import org.eclipse.jdt.internal.ui.util.BusyIndicatorRunnableContext;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

public class ConfigureFilterPackagePage extends WizardPage {

	private Button blacklistButton;
	private Button whitelistButton;
	private Button selectPackagesButton;
	private int selectPackageDialogStatus;
	private Object[] result = null;
	public Object[] getResult() {
		return result;
	}

	private Button usePackageFilterCheckbox;
	private RecommendationGenerator generator;
	
	public ConfigureFilterPackagePage(RecommendationGenerator generator) {
		super("Paketfilter");
		setTitle("Paketfilter");
		setDescription("Wählen Sie hier die Paketfilter aus. Sie können nach Blacklist- oder Whitelistmethode vorgehen und eine Liste von Paketen auswählen.");
		this.generator = generator;
	}

	@Override
	public void createControl(Composite parent) {
		Composite mainComposite = new Composite(parent, SWT.NONE);
		mainComposite.setLayout(new GridLayout());
		setControl(mainComposite);

		GridLayout grid = new GridLayout(1, false);
		mainComposite.setLayout(grid);
		
		Composite row = new Composite(mainComposite, SWT.NONE);
		row.setLayout(new GridLayout(1, false));
		this.usePackageFilterCheckbox = new Button(row, SWT.CHECK);
		this.usePackageFilterCheckbox.setText("Nach Paketen filtern");
		this.usePackageFilterCheckbox.setSelection(this.generator.getPackageFilterMode() != null && this.generator.getPackageFilterMode() != PackageFilterMode.NONE);
		this.usePackageFilterCheckbox.setVisible(true);
	
		row = new Composite(mainComposite, SWT.NONE);
		row.setLayout(new GridLayout(2, false));
		this.blacklistButton = new Button(row, SWT.RADIO);
		this.whitelistButton = new Button(row, SWT.RADIO);
		
		this.blacklistButton.setText("Pakete ausschließen");
		this.whitelistButton.setText("Pakete auswählen");
		
		row = new Composite(mainComposite, SWT.NONE);
		row.setLayout(new GridLayout(1, false));
		this.selectPackagesButton = new Button(row, SWT.PUSH);
		this.selectPackagesButton.setText("Pakete auswählen...");
		this.selectPackagesButton.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				IJavaSearchScope searchScope = SearchEngine.createJavaSearchScope(ConfigureFilterPackagePage.this.getSourceFolders());
				BusyIndicatorRunnableContext context= new BusyIndicatorRunnableContext();
				PackageSelectionDialog packageSelectionDialog = new PackageSelectionDialog(new Shell(), context, 0, searchScope);
				packageSelectionDialog.setMessage("Pakete auswählen, die je nach Auswahl in der Black- oder Whitelist sein sollen");
				packageSelectionDialog.setTitle("Pakete auswählen");
				packageSelectionDialog.setMultipleSelection(true);
				packageSelectionDialog.setBlockOnOpen(true);
				selectPackageDialogStatus = packageSelectionDialog.open();
				if (Window.OK == selectPackageDialogStatus) {
					result = packageSelectionDialog.getResult();
					if (result != null && result.length == 0) {
						result = null;
					}
				} else {
					result = null;
				}
				getContainer().updateButtons();
			}
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		
		this.usePackageFilterCheckbox.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				ConfigureFilterPackagePage.this.blacklistButton.setVisible(usePackageFilterCheckbox.getSelection());
				ConfigureFilterPackagePage.this.whitelistButton.setVisible(usePackageFilterCheckbox.getSelection());
				ConfigureFilterPackagePage.this.selectPackagesButton.setVisible(usePackageFilterCheckbox.getSelection());
				if (!ConfigureFilterPackagePage.this.selectPackagesButton.getSelection()) {
					result = null;
				}
				getContainer().updateButtons();
			}
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		
		this.blacklistButton.setSelection(this.generator.getPackageFilterMode() == PackageFilterMode.BLACKLIST);
		this.whitelistButton.setSelection(this.generator.getPackageFilterMode() == PackageFilterMode.WHITELIST);
		if (this.generator.getPackageFilterMode() == null || this.generator.getPackageFilterMode() == PackageFilterMode.NONE) {
			this.blacklistButton.setSelection(true);
		}
		
		this.blacklistButton.setVisible(usePackageFilterCheckbox.getSelection());
		this.whitelistButton.setVisible(usePackageFilterCheckbox.getSelection());
		this.selectPackagesButton.setVisible(usePackageFilterCheckbox.getSelection());
		
		
	}
	
	public Button getBlacklistButton() {
		return blacklistButton;
	}

	public Button getUsePackageFilterCheckbox() {
		return usePackageFilterCheckbox;
	}

	@Override
	public boolean isPageComplete() {
		boolean complete = super.isPageComplete();
		// Entweder kein Filter oder Filter && was_ausgewählt
		complete &= (!this.usePackageFilterCheckbox.getSelection() || (this.usePackageFilterCheckbox.getSelection() && (result != null && result.length > 0)));
		
		return complete;
	}
	
	private IPackageFragmentRoot[] getSourceFolders() {
		List<IPackageFragmentRoot> ret = new ArrayList<IPackageFragmentRoot>();
		IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		
		IProject[] projects = wsRoot.getProjects();

		// FindBugs auf jedes Projekt laufen lassen
		for (IProject project : projects) {
			if (project.isOpen() && JavaProject.hasJavaNature(project)) { 
			  IJavaProject javaProject = JavaCore.create(project);
				try {
					for (IPackageFragmentRoot root : javaProject.getPackageFragmentRoots()) {
						if (root.getKind() == IPackageFragmentRoot.K_SOURCE) {
							ret.add(root);
						}
					}
				} catch (JavaModelException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				
			}
		}
		return ret.toArray(new IPackageFragmentRoot[]{});
	}

}
