package org.codecover.eclipse.utils.recommendationgenerator;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

public class ErrorDataWizard extends Wizard {

	private ErrorDataSource errorDataSource;
	
	private List<Object> controls = new ArrayList<Object>();
	
	

	public ErrorDataWizard(ErrorDataSource eDS) {
		this.errorDataSource = eDS;
	}
	
	@Override
	public boolean performFinish() {
		for (Object o : this.controls) {
			// Einfaches SWT-Widget
			if (o instanceof Control) {
				Control c = (Control) o;
				// Controls, die einen Parameter konfigurieren
				if (c.getData() instanceof Parameter) {
					Parameter parameter = (Parameter) c.getData();
					// Textfeld für Nummern
					if (c instanceof Text && parameter.getType() instanceof Integer) {
						parameter.setValue(Integer.parseInt(((Text) c).getText()));
						continue;
					} else if (c instanceof Text) {
						// Textfeld für Text
						parameter.setValue(((Text)c).getText());
						continue;
					} else if (c instanceof Button) {
						// Checkbox
						parameter.setValue(((Button)c).getSelection());
						continue;
					}
				}
			} else if (o instanceof FileSelector ) {
				// Dateiauswahl
				FileSelector fs = (FileSelector) o;
				Parameter parameter = fs.getParameter();
				parameter.setValue(fs.getSelectedFile());
				continue;
			} else if (o instanceof IndicatorModeSelector) {
				IndicatorModeSelector ims = (IndicatorModeSelector) o;
				ims.getEi().setMode(ims.getSelectedMode());
				continue;
			}
			MessageDialog.openInformation(this.getShell(), 
					"Problem", 
					"Implementierungsfehler. Nicht alle Eingaben wurden verarbeitet. Es fehlt: "+o.toString());
		}
		
		return true;
	}
	
	@Override
	public void addPages() {
		controls = new ArrayList<Object>();
		addPage(new ErrorDataWizardFirstPage(errorDataSource));
		for (ErrorIndicator ei : errorDataSource.getErrorIndicators()) {
			ErrorDataWizardPage errorDataWizardPage = new ErrorDataWizardPage(ei);
			addPage(errorDataWizardPage);
		}
		
	}
	
	public boolean addControl(Object arg0) {
		return controls.add(arg0);
	}

}
