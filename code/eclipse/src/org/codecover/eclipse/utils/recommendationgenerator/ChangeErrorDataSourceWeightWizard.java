package org.codecover.eclipse.utils.recommendationgenerator;

import org.eclipse.jface.wizard.Wizard;

public class ChangeErrorDataSourceWeightWizard extends Wizard {

	private ChangeErrorDataSourceWeightPage changeErrorDateSourcePage;
	private RecommendationGenerator generator;

	public ChangeErrorDataSourceWeightWizard(RecommendationGenerator generator) {
		super();
		this.generator = generator;
	}
	
	@Override
	public void addPages() {
		changeErrorDateSourcePage = new ChangeErrorDataSourceWeightPage("Gewichtung einstellen");
		addPage(changeErrorDateSourcePage);
		
	}
	
	@Override
	public boolean performFinish() {
		int selection = this.changeErrorDateSourcePage.getCcTestSlider().getSelection();
		double weight = selection / 100.0;
		this.generator.getCcErrorDataSource().setWeight(weight);
		
		selection = this.changeErrorDateSourcePage.getCodeSlider().getSelection();
		weight = selection / 100.0;
		this.generator.getCodeErrorDataSource().setWeight(weight);
		
		selection = this.changeErrorDateSourcePage.getVersionSlider().getSelection();
		weight = selection / 100.0;
		this.generator.getVersionErrorDataSource().setWeight(weight);
		
		selection = this.changeErrorDateSourcePage.getExpertKnowledgeSlider().getSelection();
		weight = selection / 100.0;
		this.generator.getExpertErrorDataSource().setWeight(weight);
		
		selection = this.changeErrorDateSourcePage.getQaActionsSlider().getSelection();
		weight = selection / 100.0;
		this.generator.getQualityErrorDataSource().setWeight(weight);
		
		selection = this.changeErrorDateSourcePage.getProcessSlider().getSelection();
		weight = selection / 100.0;
		this.generator.getProcessDataSource().setWeight(weight);
		
		return true;
	}

	public RecommendationGenerator getGenerator() {
		return generator;
	}

}
