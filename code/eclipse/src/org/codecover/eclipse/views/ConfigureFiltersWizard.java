package org.codecover.eclipse.views;

import java.util.ArrayList;

import org.codecover.eclipse.utils.recommendationgenerator.BranchType;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Button;

public class ConfigureFiltersWizard extends Wizard {

	private RecommendationsView view;
	private ConfigureFilterPackagePage filterPackagePage;
	private ConfigureFilterCCTypePage typePage;

	public ConfigureFiltersWizard(RecommendationsView view) {
		super();
		this.view = view;
	}
	
	@Override
	public void addPages() {
		filterPackagePage = new ConfigureFilterPackagePage(view.recommendationGenerator);
		addPage(filterPackagePage);
		typePage = new ConfigureFilterCCTypePage(this.view.recommendationGenerator);
		addPage(typePage);
	}
	
	@Override
	public boolean performFinish() {
		PackageFilterMode mode = PackageFilterMode.NONE;
		if (this.filterPackagePage.getUsePackageFilterCheckbox().getSelection())
		if (filterPackagePage.getBlacklistButton().getSelection()) {
			mode = PackageFilterMode.BLACKLIST;
		} else {
			mode = PackageFilterMode.WHITELIST;
		}
		
		
		ArrayList<BranchType> typeFilter = new ArrayList<BranchType>();
		for (Button b : this.typePage.getTypeCheckboxes()) {
			if (b.getSelection()) {
				typeFilter.add((BranchType) b.getData());
			}
		}
		this.view.recommendationGenerator.setFilterBranches(typeFilter);

		
		this.view.recommendationGenerator.setPackageFilterMode(mode);
		this.view.recommendationGenerator.setPackageFilter(filterPackagePage.getResult());
		this.view.recViewViewContentProvider.refreshViewWithoutInvoking();
		return true;
	}

}
