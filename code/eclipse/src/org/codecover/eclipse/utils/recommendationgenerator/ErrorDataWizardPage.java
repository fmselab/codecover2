package org.codecover.eclipse.utils.recommendationgenerator;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

public class ErrorDataWizardPage extends WizardPage {

	private ErrorDataWizard wizard;
	private ErrorIndicator ei;
	
	private List<Composite> useIfAutoRows = new ArrayList<Composite>();
	private List<Composite> useIfXmlRows = new ArrayList<Composite>();
	private IndicatorModeSelector indicatorModeSelector;

	public ErrorDataWizardPage(ErrorIndicator ei) {
		super(ei.getName());
		setTitle(ei.getName());
		setDescription(ei.getDescription());
		this.ei = ei;
	}

	@Override
	public void createControl(Composite parent) {
		this.wizard = (ErrorDataWizard) getWizard();
		Composite mainComposite = new Composite(parent, SWT.NONE);
		mainComposite.setLayout(new GridLayout());
		setControl(mainComposite);

		GridLayout grid = new GridLayout(1, false);
		mainComposite.setLayout(grid);
		
		
		Composite row = new Composite(mainComposite, SWT.NONE);
		indicatorModeSelector = new IndicatorModeSelector(row, ei);
		this.wizard.addControl(indicatorModeSelector);

		indicatorModeSelector.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				setControlVisibility();
			}

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		

		for (Parameter p : ei.getParameters()) {
			row = new Composite(mainComposite, SWT.NONE);
			if (p.isUsedIfAuto()) {
				this.useIfAutoRows.add(row);
			}
			if (p.isUsedIfXml()) {
				this.useIfXmlRows.add(row);
			}
			
			if (p.getType() instanceof Boolean) {
				row.setLayout(new GridLayout(1, false));
				Button checkbox = new Button(row, SWT.CHECK);
				this.wizard.addControl(checkbox);
				checkbox.setData(p);
				checkbox.setSelection((Boolean) p.getValue());
				checkbox.setText(p.getName());
			} else if (p.getType() instanceof Integer) {
				row.setLayout(new GridLayout(2, false));

				Label label = new Label(row, SWT.NONE);
				label.setText(p.getName());
				label.setToolTipText(p.getDescription());
				final Text textfield = new Text(row, SWT.LEFT);
				this.wizard.addControl(textfield);
				GridData gridData = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
				gridData.minimumWidth = 80;
				textfield.setLayoutData(gridData);
				if (p.getValue() != null) {
					String initialText = ((Integer) p.getValue()).toString();
					textfield.setText(initialText);
				} else {
					textfield.setText("0");
				}
				textfield.setData(p);
				textfield.addVerifyListener(new VerifyListener() {
					@Override
					public void verifyText(VerifyEvent e) {
						switch (e.keyCode) {
						case SWT.BS: // Backspace
						case SWT.DEL: // Delete
						case SWT.HOME: // Home
						case SWT.END: // End
						case SWT.ARROW_LEFT: // Left arrow
						case SWT.ARROW_RIGHT: // Right arrow
							return;
						}

						if (!Character.isDigit(e.character)) {
							e.doit = false; // disallow the action
						}
					}
				});
			} else if (p.getType() instanceof String) {
				row.setLayout(new GridLayout(2, false));

				Label label = new Label(row, SWT.NONE);
				label.setText(p.getName());
				label.setToolTipText(p.getDescription());
				final Text textfield = new Text(row, SWT.LEFT);
				this.wizard.addControl(textfield);
				GridData gridData = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
				gridData.minimumWidth = 160;
				textfield.setLayoutData(gridData);

				textfield.setText((String) p.getValue());
				textfield.setData(p);
			} else if (p.getType() instanceof File) {
				FileSelector fileSelector = new FileSelector(row, p, wizard);
				
				wizard.addControl(fileSelector);
			}
			
		}

		
		setControlVisibility();
	}
	
	
	private void setControlVisibility() {
		for (Composite c : ErrorDataWizardPage.this.useIfAutoRows) {
			c.setVisible(indicatorModeSelector.getSelectedMode() == ErrorIndicatorMode.AUTO);
			for (Control cc : c.getChildren()) {
				cc.setEnabled(indicatorModeSelector.getSelectedMode() == ErrorIndicatorMode.AUTO);
			}
		}
		for (Composite c : ErrorDataWizardPage.this.useIfXmlRows) {
			c.setVisible(indicatorModeSelector.getSelectedMode() == ErrorIndicatorMode.MANUAL);
			for (Control cc : c.getChildren()) {
				cc.setEnabled(indicatorModeSelector.getSelectedMode() == ErrorIndicatorMode.MANUAL);
			}
		}
	}
	
	
	

}
