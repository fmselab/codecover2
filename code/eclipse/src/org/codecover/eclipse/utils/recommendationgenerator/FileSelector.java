package org.codecover.eclipse.utils.recommendationgenerator;

import java.io.File;

import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;

public class FileSelector {

	private Composite row;
	
	private File selectedFile = null;

	private Parameter parameter; 
	
	public FileSelector(Composite row, Parameter p, final Wizard wizard) {
		this.parameter = p;
		this.row = row;
		row.setLayout(new GridLayout(3, false));
		Label label = new Label(row, SWT.NONE);
		label.setText(p.getName());
		
		GridData gridData = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
		gridData.minimumWidth = 150;
		final Label fileLabel = new Label(row, SWT.NONE);
		fileLabel.setLayoutData(gridData);
		if (p.getValue() != null) {
			fileLabel.setText(((File)p.getValue()).getName());
		}

		final Button chooseFileButton = new Button(row, SWT.PUSH);
		chooseFileButton.setText("Datei auswählen...");
		chooseFileButton.setToolTipText(p.getDescription());
		chooseFileButton.setData(null);
		
		chooseFileButton.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				FileDialog fg = new FileDialog(wizard.getShell());
				fg.setText("Datei auswählen...");
				String file = fg.open();
				if (file != null && file.length() > 2) {
					selectedFile = new File(file);
					fileLabel.setText(selectedFile.getName());
				}
			}
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
	}

	public Parameter getParameter() {
		return this.parameter;
	}

	public File getSelectedFile() {
		return selectedFile;
	}
}
