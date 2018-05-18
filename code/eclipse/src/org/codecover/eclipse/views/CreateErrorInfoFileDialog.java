package org.codecover.eclipse.views;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;

public class CreateErrorInfoFileDialog extends Dialog {

	private Table table;
	private Shell shell;
	private TableViewer tableViewer;
	private ArrayList<String> columnNames = new ArrayList<String>();
	private List<ClassRating> classRatings = new ArrayList<ClassRating>();
	private Button okButton;
	private Text text;
	
	public CreateErrorInfoFileDialog(final Shell shell, List<String> classes) {
		super(shell);
		
		for (String clazz : classes) {
			if (clazz != null) {
				classRatings.add(new ClassRating(clazz, 0));
			}
		}
		
		columnNames.add("Klasse");
		columnNames.add("Fehlerpunkte");
		
		this.shell = shell;
		this.shell.setLayout(new GridLayout());
		
		Composite upper = new Composite(shell, SWT.NONE);
		upper.setLayout(new GridLayout());
		GridData gridData = new GridData();
		gridData.heightHint = 400;
		
		this.tableViewer = new TableViewer(upper, SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION);
		this.table = this.tableViewer.getTable();
		this.table.setLayoutData(gridData);
		this.table.setHeaderVisible(true);
		this.tableViewer.setUseHashlookup(true);
		this.tableViewer.setColumnProperties(columnNames.toArray(new String[]{}));
		TableColumn column = new TableColumn(table, SWT.LEFT);
		column.setText("Klasse");
		column.setWidth(485);
		column = new TableColumn(table, SWT.LEFT);
		column.setText("Fehlerpunkte");
		column.setWidth(70);
		
		CellEditor[] editors = new CellEditor[2];
		editors[0] = new TextCellEditor(this.table);
		editors[1] = new TextCellEditor(this.table);
		((Text) editors[1].getControl()).setTextLimit(5);
		this.tableViewer.setCellEditors(editors);
		this.tableViewer.setCellModifier(new ErrorValEditor());
		this.tableViewer.setContentProvider(new TableContentProvider());
		this.tableViewer.setLabelProvider(new TableLabelProvider());
		this.tableViewer.setInput(classRatings);
		
		/*
		
		Composite nameComp = new Composite(shell, SWT.NONE);
		nameComp.setLayout(new GridLayout(2, false));
		Label label = new Label(nameComp, SWT.LEFT);
		label.setText("Name: ");
		text = new Text(nameComp, SWT.LEFT);
		*/
		okButton = new Button(shell, SWT.PUSH);
		okButton.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				outputErrorInfoFile();
				shell.close();
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		okButton.setText("OK");
		
		shell.pack();
		shell.open();
		
	}
	
	private void outputErrorInfoFile() {
		boolean relevant = false;
		for (ClassRating cr : this.classRatings) {
			if (cr.rating > 0) {
				relevant = true;
			}
		}
		if (relevant) {
			
			FileDialog fg = new FileDialog(shell, SWT.SAVE);
			fg.setOverwrite(true);
			fg.setFilterExtensions(new String[]{"xml"});
			fg.setText("Zieldatei auswählen...");
			String file = fg.open();
			File selectedFile = null;
			if (file != null && file.length() > 2) {
				selectedFile = new File(file);
			}
			try {
				if (selectedFile != null && ((selectedFile.exists() && selectedFile.canWrite()) || selectedFile.createNewFile())) {
					StringBuilder sb = new StringBuilder(); // Hier tuts ein einfacher StringBuilder wirklich.
					sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<errorIndicator>\n");
					for (ClassRating cr : this.classRatings) {
						if (cr.rating > 0) {
							// <file class="de.mailacs.intern.actions.MemberActions" value="18" />
							sb.append("\t<file class=\""+cr.clazz+"\" value=\""+cr.rating+"\" />\n");
						}
					}
					sb.append("</errorIndicator>\n");
					
					if (selectedFile.exists()) {
						selectedFile.delete();
					}
					selectedFile.createNewFile();
					
					FileWriter writer = new FileWriter(selectedFile);
					writer.append(sb.toString());
					writer.close();
					
				}
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
	
	class TableContentProvider implements IStructuredContentProvider {

		@Override
		public Object[] getElements(Object inputElement) {
			return classRatings.toArray(new ClassRating[]{});
		}

		@Override
		public void dispose() {
		}

		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		}
	}
	
	class TableLabelProvider extends LabelProvider implements ITableLabelProvider {

		@Override
		public Image getColumnImage(Object element, int columnIndex) {
			return null;
		}

		@Override
		public String getColumnText(Object element, int columnIndex) {
			switch (columnIndex) {
			case 0:
				return ((ClassRating)element).clazz+"";
			case 1:
				return ((ClassRating)element).rating+"";
			}
			return "";
		}
		
	}
	
	class ErrorValEditor implements ICellModifier {

		@Override
		public boolean canModify(Object element, String property) {
			int columnIndex = columnNames.indexOf(property);
			if (columnIndex == 1) {
				return true;
			}
			return false;
		}

		@Override
		public Object getValue(Object element, String property) {
			int columnIndex = columnNames.indexOf(property);
			Object result = null;
			ClassRating classRating = (ClassRating) element;
			if (columnIndex == 0) {
				result = classRating.clazz;
			}
			if (columnIndex == 1) {
				result = classRating.rating+"";
			}
			
			return result;
		}

		@Override
		public void modify(Object element, String property, Object value) {
			if (value instanceof String && ((String)value).length() == 0) {
				return;
			}
			int columnIndex = columnNames.indexOf(property);
			try {
				ClassRating classRating = (ClassRating) ((Widget) element).getData();
				if (columnIndex == 1) {
					classRating.rating = Integer.valueOf((String)value);
				}
				if (columnIndex == 2) {
					classRating.clazz = ((String)value);
				}
			}
			catch (NullPointerException e) {
				
			}
			
			tableViewer.refresh();
		}
		
	}
	
	class ClassRating {
		public ClassRating(String clazz2, int i) {
			this.clazz = clazz2;
			this.rating = i;
		}
		String clazz;
		int rating;
	}

	
}
