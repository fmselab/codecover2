package org.codecover.eclipse.utils.recommendationgenerator;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Slider;

public class ChangeErrorDataSourceWeightPage extends WizardPage {

	private Label codeDisplayLabel;
	private Label versionDisplayLabel;
	private Label ccTestDisplayLabel;
	private Label codeLabel;
	private Slider codeSlider;
	private Label versionLabel;
	private Slider versionSlider;
	private Label ccTestLabel;
	private Slider ccTestSlider;
	private Label workPressureLabel;
	private Slider workPressureSlider;
	private Label workPressureDisplayLabel;
	private Label expertKnowledgeLabel;
	private Slider expertKnowledgeSlider;
	private Label expertKnowledgeDisplayLabel;
	private Label qaActionsLabel;
	private Slider qaActionsSlider;
	private Label qaActionsDisplayLabel;

	protected ChangeErrorDataSourceWeightPage(String pageName) {
		super(pageName);
		setTitle("Gewichtung einstellen");
		setDescription("Bitte die zu verwendende Gewichtung der einzelnen Fehlerdatenquellen einstellen");
	}

	@Override
	public void createControl(Composite parent) {
		Composite mainComposite = new Composite(parent, SWT.NONE);
		mainComposite.setLayout(new GridLayout());
		setControl(mainComposite);
		
		GridLayout grid = new GridLayout(3, false);
		mainComposite.setLayout(grid);
		
		RecommendationGenerator generator = ((ChangeErrorDataSourceWeightWizard)getWizard()).getGenerator();
		
		
		codeLabel = new Label(mainComposite, SWT.LEFT);
		Label label = codeLabel;
		label.setText("Gewichtung Code:");
		codeSlider = new Slider(mainComposite, SWT.HORIZONTAL);
		Slider slider = codeSlider;
		slider.setIncrement(1);
		slider.setThumb(1);
		slider.setMinimum(0);
		slider.setMaximum(1000);
		int selection = (int) (generator.getCodeErrorDataSource().getWeight() * 100);
		System.out.println("Setze selection auf "+selection);
		slider.setSelection(selection);
		selection = slider.getSelection();
		System.out.println("Selection ist nun "+selection);
		slider.setVisible(true);
		slider.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				ChangeErrorDataSourceWeightPage.this.codeDisplayLabel.setText("Wert: " + codeSlider.getSelection()/100.0); //$NON-NLS-1$
			}
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		codeDisplayLabel = new Label(mainComposite, SWT.RIGHT);
		Label displayLabel = codeDisplayLabel;
		displayLabel.setText("Wert: "+slider.getSelection()/100.0);
		displayLabel.setVisible(true);
		
		
		
		
		
		
		versionLabel = new Label(mainComposite, SWT.LEFT);
		label = versionLabel;
		label.setText("Gewichtung Versionsgeschichte:");
		versionSlider = new Slider(mainComposite, SWT.HORIZONTAL);
		slider = versionSlider;
		slider.setIncrement(1);
		slider.setThumb(1);
		slider.setMinimum(0);
		slider.setMaximum(1000);
		slider.setSelection((int) (generator.getVersionErrorDataSource().getWeight() * 100));
		slider.setVisible(true);
		slider.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				ChangeErrorDataSourceWeightPage.this.versionDisplayLabel.setText("Wert: " + versionSlider.getSelection()/100.0); //$NON-NLS-1$
			}
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		versionDisplayLabel = new Label(mainComposite, SWT.RIGHT);
		displayLabel = versionDisplayLabel;
		displayLabel.setText("Wert: "+slider.getSelection()/100.0);
		displayLabel.setVisible(true);
		
		
		

		
		
		ccTestLabel = new Label(mainComposite, SWT.LEFT);
		label = ccTestLabel;
		label.setText("Gewichtung CC-Test:");
		ccTestSlider = new Slider(mainComposite, SWT.HORIZONTAL);
		slider = ccTestSlider;
		slider.setIncrement(1);
		slider.setThumb(1);
		slider.setMinimum(0);
		slider.setMaximum(1000);
		slider.setSelection((int) (generator.getCcErrorDataSource().getWeight() * 100));
		slider.setVisible(true);
		slider.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				ChangeErrorDataSourceWeightPage.this.ccTestDisplayLabel.setText("Wert: " + ccTestSlider.getSelection()/100.0); //$NON-NLS-1$
			}
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		ccTestDisplayLabel = new Label(mainComposite, SWT.RIGHT);
		displayLabel = ccTestDisplayLabel;
		displayLabel.setText("Wert: "+slider.getSelection()/100.0);
		displayLabel.setVisible(true);
		
		

		
		
		workPressureLabel = new Label(mainComposite, SWT.LEFT);
		label = workPressureLabel;
		label.setText("Gewichtung Arbeitsdruck:");
		workPressureSlider = new Slider(mainComposite, SWT.HORIZONTAL);
		slider = workPressureSlider;
		slider.setIncrement(1);
		slider.setThumb(1);
		slider.setMinimum(0);
		slider.setMaximum(1000);
		slider.setSelection((int) (generator.getProcessDataSource().getWeight() * 100));
		slider.setVisible(true);
		slider.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				ChangeErrorDataSourceWeightPage.this.workPressureDisplayLabel.setText("Wert: " + workPressureSlider.getSelection()/100.0); //$NON-NLS-1$
			}
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		workPressureDisplayLabel = new Label(mainComposite, SWT.RIGHT);
		displayLabel = workPressureDisplayLabel;
		displayLabel.setText("Wert: "+slider.getSelection()/100.0);
		displayLabel.setVisible(true);
		
		

		
		
		expertKnowledgeLabel = new Label(mainComposite, SWT.LEFT);
		label = expertKnowledgeLabel;
		label.setText("Gewichtung Expertenwissen:");
		expertKnowledgeSlider = new Slider(mainComposite, SWT.HORIZONTAL);
		slider = expertKnowledgeSlider;
		slider.setIncrement(1);
		slider.setThumb(1);
		slider.setMinimum(0);
		slider.setMaximum(1000);
		slider.setSelection((int) (generator.getExpertErrorDataSource().getWeight() * 100));
		slider.setVisible(true);
		slider.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				ChangeErrorDataSourceWeightPage.this.expertKnowledgeDisplayLabel.setText("Wert: " + expertKnowledgeSlider.getSelection()/100.0); //$NON-NLS-1$
			}
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		expertKnowledgeDisplayLabel = new Label(mainComposite, SWT.RIGHT);
		displayLabel = expertKnowledgeDisplayLabel;
		displayLabel.setText("Wert: "+slider.getSelection()/100.0);
		displayLabel.setVisible(true);
		
		

		
		
		qaActionsLabel = new Label(mainComposite, SWT.LEFT);
		label = qaActionsLabel;
		label.setText("Gewichtung QS-Maßnahmen:");
		qaActionsSlider = new Slider(mainComposite, SWT.HORIZONTAL);
		slider = qaActionsSlider;
		slider.setIncrement(1);
		slider.setThumb(1);
		slider.setMinimum(0);
		slider.setMaximum(1000);
		slider.setSelection((int) (generator.getQualityErrorDataSource().getWeight() * 100));
		slider.setVisible(true);
		slider.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				ChangeErrorDataSourceWeightPage.this.qaActionsDisplayLabel.setText("Wert: " + qaActionsSlider.getSelection()/100.0); //$NON-NLS-1$
			}
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		qaActionsDisplayLabel = new Label(mainComposite, SWT.RIGHT);
		displayLabel = qaActionsDisplayLabel;
		displayLabel.setText("Wert: "+slider.getSelection()/100.0);
		displayLabel.setVisible(true);
		
		

		
		
		
	}

	public Slider getCodeSlider() {
		return codeSlider;
	}

	public Slider getVersionSlider() {
		return versionSlider;
	}

	public Slider getCcTestSlider() {
		return ccTestSlider;
	}

	public Slider getProcessSlider() {
		return workPressureSlider;
	}

	public Slider getExpertKnowledgeSlider() {
		return expertKnowledgeSlider;
	}

	public Slider getQaActionsSlider() {
		return qaActionsSlider;
	}

}
