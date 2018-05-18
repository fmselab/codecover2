package org.codecover.eclipse.utils.recommendationgenerator;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

public class IndicatorModeSelector {

	
	private ErrorIndicatorMode mode;
	private Button offRadio;
	private Button autoRadio;
	private Button manualRadio;
	private List<SelectionListener> selectionListeners = new ArrayList<SelectionListener>();
	private ErrorIndicator ei;
	
	public ErrorIndicator getEi() {
		return ei;
	}

	public IndicatorModeSelector(Composite row, ErrorIndicator ei) {
		this.mode = ei.getMode();
		this.ei = ei;
		row.setLayout(new GridLayout(3, false));
		offRadio = new Button(row, SWT.RADIO);
		offRadio.setText("Indikator nicht verwenden");
		if (ei.canDetermineAutomatically()) {
			autoRadio = new Button(row, SWT.RADIO);
			autoRadio.setText("Automatisch ermitteln");
			
		}
		manualRadio = new Button(row, SWT.RADIO);
		manualRadio.setText("Manuelle Dateieingabe (XML)");
		
		SelectionListener modeSelectionListener = new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (offRadio.getSelection()) {
					IndicatorModeSelector.this.mode = ErrorIndicatorMode.OFF;
				}
				if (IndicatorModeSelector.this.ei.canDetermineAutomatically()) {
					if (autoRadio.getSelection()) {
						IndicatorModeSelector.this.mode = ErrorIndicatorMode.AUTO;
					}
				}
				if (manualRadio.getSelection()) {
					IndicatorModeSelector.this.mode = ErrorIndicatorMode.MANUAL;
				}
				for (SelectionListener sl : IndicatorModeSelector.this.selectionListeners) {
					sl.widgetSelected(e);
				}
			}
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		};
		manualRadio.addSelectionListener(modeSelectionListener);
		offRadio.addSelectionListener(modeSelectionListener);
		
		if (ei.canDetermineAutomatically()) { 
			autoRadio.addSelectionListener(modeSelectionListener); 
			autoRadio.setSelection(ei.getMode()==ErrorIndicatorMode.AUTO);
		}
		manualRadio.setSelection(ei.getMode()==ErrorIndicatorMode.MANUAL);
		offRadio.setSelection(ei.getMode()==ErrorIndicatorMode.OFF);
		
		if (ei.isAutoOnly()) {
			manualRadio.setEnabled(false);
			manualRadio.setVisible(false);
		}
		
	}
	
	public ErrorIndicatorMode getSelectedMode() {
		return this.mode;
	}

	public void addSelectionListener(SelectionListener selectionListener) {
		this.selectionListeners.add(selectionListener);
	}
}
