package org.codecover.eclipse.views;

import java.util.ArrayList;

import org.codecover.eclipse.utils.recommendationgenerator.BranchType;
import org.codecover.eclipse.utils.recommendationgenerator.RecommendationGenerator;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

public class ConfigureFilterCCTypePage extends WizardPage {

	private RecommendationGenerator generator;
	private ArrayList<Button> typeCheckboxes;

	protected ConfigureFilterCCTypePage(RecommendationGenerator generator) {
		super("Typfilter");
		setTitle("Typfilter");
		setDescription("Stellen Sie hier ein, zu welchen Blocktypen Sie Empfehlungen erhalten möchten.");
		this.generator = generator;
	}

	@Override
	public void createControl(Composite parent) {
		Composite mainComposite = new Composite(parent, SWT.NONE);
		mainComposite.setLayout(new GridLayout());
		setControl(mainComposite);

		GridLayout grid = new GridLayout(1, false);
		mainComposite.setLayout(grid);
		
		this.typeCheckboxes = new ArrayList<Button>();
		for (BranchType type : BranchType.values()) {
			Composite row = new Composite(mainComposite, SWT.NONE);
			row.setLayout(new GridLayout(1, false));
			Button button = new Button(row, SWT.CHECK);
			typeCheckboxes.add(button);
			button.setText(type.getNiceName());
			button.setData(type);
			button.setSelection(this.generator.getFilterBranches().contains(type));
		}
		
	}

	public ArrayList<Button> getTypeCheckboxes() {
		return typeCheckboxes;
	}

}
