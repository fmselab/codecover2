package org.codecover.eclipse.utils.recommendationgenerator;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

public class ErrorDataWizardFirstPage extends WizardPage {

	private ErrorDataWizard wizard;
	private ErrorDataSource errorDataSource;

	protected ErrorDataWizardFirstPage(ErrorDataSource errorDataSource) {
		super(errorDataSource.getName());
		super.setTitle("Konfiguration der Fehlerdatenquelle "+errorDataSource.getName());
		super.setDescription("In diesem Dialog werden die einzelnen Fehlerindikatoren der Fehlerdatenquelle \""+errorDataSource.getName()+"\" konfiguriert. Einzelne \n" +
				"Fehlerindikatoren können automatisch ermittelt werden, deaktiviert werden oder es kann eine XML-Datei mit \nden Fehlerdaten angegeben werden.");
		this.errorDataSource = errorDataSource;
	}

	@Override
	public void createControl(Composite parent) {
		this.wizard = (ErrorDataWizard) getWizard();
		Composite mainComposite = new Composite(parent, SWT.NONE);
		mainComposite.setLayout(new GridLayout());
		setControl(mainComposite);
		
		GridLayout grid = new GridLayout(1, false);
		mainComposite.setLayout(grid);
		
		Label label = new Label(mainComposite, SWT.WRAP);
		label.setText(errorDataSource.getDescription());
	}

}
