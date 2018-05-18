/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.eclipse.preferences;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import org.codecover.eclipse.Messages;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;

/**
 * This class is used to display the range preferences of the correlation
 * matrix. It is possible to add and remove ranges and manipulate the colors of
 * those ranges.
 * 
 * @author Michael Starzmann, Markus Wittlinger
 * @version 1.0 ($Id: RGBWithBoundariesEditor.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class RGBWithBoundariesEditor extends Composite {

    private final List<TextPart> textParts = new Vector<TextPart>();

    private final List<BetweenPart> betweenParts = new Vector<BetweenPart>();

    private String preferenceName;

    private IPreferenceStore preferenceStore;

    private BetweenPart lowerBorder;

    private BetweenPart upperBorder;

    private final Set<RefreshListener> refreshListeners;

    private final SelectionListener addListener = new SelectionAdapter() {
        @Override
        public void widgetSelected(SelectionEvent e) {
            handleAddBoundary((BetweenPart) e.widget);
        }
    };

    private final SelectionListener removeListener = new SelectionAdapter() {
        @Override
        public void widgetSelected(SelectionEvent e) {
            handleRemoveBoundary((BetweenPart) e.widget);
        }
    };

    private final ModifyListener spinnerListener = new ModifyListener() {

        @Override
		public void modifyText(ModifyEvent e) {
            handleSpinnerModifiedEvent(e);
        }
    };

    /**
     * Constructor
     * 
     * @param preferenceName
     *                the key for the preferences in the store
     * @param preferenceStore
     *                the {@link IPreferenceStore} containing the preferences
     * @param parent
     *                the parent of the editor.
     */
    public RGBWithBoundariesEditor(String preferenceName,
            IPreferenceStore preferenceStore, Composite parent) {
        super(parent, SWT.NONE);
        this.refreshListeners = new HashSet<RefreshListener>();
        this.preferenceName = preferenceName;
        this.preferenceStore = preferenceStore;
        createContents();
    }

    /**
     * Adds the given {@link RefreshListener}
     * 
     * @param refreshListener
     *                the to be added {@link RefreshListener}
     */
    public void addRefreshListener(RefreshListener refreshListener) {
        this.refreshListeners.add(refreshListener);
    }

    /**
     * Removes the given {@link RefreshListener}
     * 
     * @param refreshListener
     *                the to be removed{@link RefreshListener}
     */
    public void removeRefreshListener(RefreshListener refreshListener) {
        this.refreshListeners.remove(refreshListener);
    }

    /**
     * @param widget
     */
    protected void handleRemoveBoundary(BetweenPart widget) {
        int index = this.betweenParts.indexOf(widget);

        if (index == 0) {
            return;
        }

        this.setRedraw(false);

        this.betweenParts.remove(index);

        // index is always at least 1, since the top most betweenpart can't be
        // removed.
        TextPart removedTextPart = this.textParts.remove(index - 1);

        widget.dispose();
        removedTextPart.dispose();

        this.setRedraw(true);
        this.layout(true, true);

        updateSpinnerBoundaries();
        updateButtonEnabledStates();

        notifyRefreshListeners();
    }

    /**
     * @param widget
     */
    protected void handleAddBoundary(BetweenPart widget) {
        int index = this.betweenParts.indexOf(widget);

        this.setRedraw(false);

        int selectionAbove = 0;
        if (index == 0) {
            selectionAbove = 0;
        } else {
            selectionAbove = this.textParts.get(index - 1).spinner
                    .getSelection();
        }

        TextPart textPart = new TextPart(this, SWT.NONE);
        textPart.moveBelow(widget);
        textPart.spinner.setSelection(selectionAbove + 1);
        this.textParts.add(index, textPart);

        BetweenPart betweenPart = new BetweenPart(this, SWT.NONE);
        betweenPart.moveBelow(textPart);
        this.betweenParts.add(index + 1, betweenPart);

        this.setRedraw(true);
        this.layout(true, true);

        updateSpinnerBoundaries();
        updateButtonEnabledStates();

        notifyRefreshListeners();
    }

    private void handleSpinnerModifiedEvent(ModifyEvent e) {
        if (!(e.widget instanceof Spinner)) {
            return;
        }

        updateSpinnerBoundaries();
        updateButtonEnabledStates();
    }

    /**
     * Loads the default boundaries.
     */
    protected void loadDefault() {
        loadDefaultsIntoControls();
    }

    /**
     * Stores the boundaries into the preferences.
     */
    protected void store() {
        String oldValue = this.preferenceStore.getString(this.preferenceName);
        String newValue = this.convertToSetOfRGBWithBoundaries().toString();
        this.preferenceStore.setValue(this.preferenceName, newValue);
        this.preferenceStore.firePropertyChangeEvent(this.preferenceName,
                oldValue, newValue);
    }

    private SetOfRGBWithBoundaries convertToSetOfRGBWithBoundaries() {
        SetOfRGBWithBoundaries set = new SetOfRGBWithBoundaries();

        set.add(new RGBWithBoundaries(this.lowerBorder.colorSelect.getRGB(), 0,
                0));

        for (int i = 0, a = -1; i < this.betweenParts.size()
                && a < this.textParts.size(); i++, a++) {
            RGB color = this.betweenParts.get(i).colorSelect.getRGB();
            int lower;
            int upper;

            if (a == -1) {
                lower = 0;
            } else {
                lower = this.textParts.get(a).spinner.getSelection();
            }

            if (a == this.textParts.size() - 1) {
                upper = 100;
            } else {
                upper = this.textParts.get(a + 1).spinner.getSelection();
            }

            set.add(new RGBWithBoundaries(color, lower, upper));
        }

        set.add(new RGBWithBoundaries(this.upperBorder.colorSelect.getRGB(),
                100, 100));

        return set;
    }

    /**
     * @param set
     */
    private void createContents() {
        List<RGBWithBoundaries> list = new Vector<RGBWithBoundaries>(
                new SetOfRGBWithBoundaries(this.preferenceStore
                        .getString(this.preferenceName)));

        this.setLayout(new GridLayout());

        this.lowerBorder = new BetweenPart(this, SWT.NONE);
        this.lowerBorder.text.setText("0"); //$NON-NLS-1$
        this.lowerBorder.colorSelect.setRGB(list.get(0).getRGB());
        this.lowerBorder.buttonAdd.setVisible(false);
        this.lowerBorder.buttonRemove.setVisible(false);

        BetweenPart betweenPart;

        betweenPart = new BetweenPart(this, SWT.NONE);
        this.betweenParts.add(betweenPart);
        betweenPart.colorSelect.setRGB(list.get(1).getRGB());
        betweenPart.buttonRemove.setEnabled(false);

        List<RGBWithBoundaries> betweenList = list.subList(2, list.size() - 1);
        for (int i = 0; i < betweenList.size(); i++) {
            TextPart textPart = new TextPart(this, SWT.NONE);
            this.textParts.add(textPart);
            textPart.spinner
                    .setSelection(betweenList.get(i).getLowerBoundary());

            betweenPart = new BetweenPart(this, SWT.NONE);
            this.betweenParts.add(betweenPart);
            betweenPart.colorSelect.setRGB(betweenList.get(i).getRGB());
        }

        this.upperBorder = new BetweenPart(this, SWT.NONE);
        this.upperBorder.text.setText("100"); //$NON-NLS-1$
        this.upperBorder.colorSelect.setRGB(list.get(list.size() - 1).getRGB());
        this.upperBorder.buttonAdd.setVisible(false);
        this.upperBorder.buttonRemove.setVisible(false);

        updateSpinnerBoundaries();
        updateButtonEnabledStates();

        notifyRefreshListeners();
    }

    private void updateSpinnerBoundaries() {
        for (int i = 0; i < this.textParts.size(); i++) {
            Spinner currentSpinner = this.textParts.get(i).spinner;

            if (i == 0) {
                currentSpinner.setMinimum(1);
            } else {
                currentSpinner.setMinimum(this.textParts.get(i - 1).spinner
                        .getSelection() + 1);
            }

            if (i == this.textParts.size() - 1) {
                currentSpinner.setMaximum(99);
            } else {
                currentSpinner.setMaximum(this.textParts.get(i + 1).spinner
                        .getSelection() - 1);
            }
        }
    }

    private void updateButtonEnabledStates() {
        this.betweenParts.get(0).buttonRemove.setEnabled(false);

        for (int i = 0, a = -1; i < this.betweenParts.size()
                && a < this.textParts.size(); i++, a++) {
            BetweenPart betweenPart = this.betweenParts.get(i);

            int lower;
            int upper;

            if (a == -1) {
                lower = 0;
            } else {
                lower = this.textParts.get(a).spinner.getSelection();
            }

            if (a == this.textParts.size() - 1) {
                upper = 100;
            } else {
                upper = this.textParts.get(a + 1).spinner.getSelection();
            }

            if (upper - lower == 1) {
                betweenPart.buttonAdd.setEnabled(false);
            } else {
                betweenPart.buttonAdd.setEnabled(true);
            }
        }
    }

    private void loadDefaultsIntoControls() {
        this.setRedraw(false);

        List<RGBWithBoundaries> list = new Vector<RGBWithBoundaries>(
                new SetOfRGBWithBoundaries(this.preferenceStore
                        .getDefaultString(this.preferenceName)));

        this.lowerBorder.colorSelect.setRGB(list.get(0).getRGB());

        BetweenPart betweenPart;
        TextPart textPart;
        betweenPart = this.betweenParts.get(0);
        betweenPart.colorSelect.setRGB(list.get(1).getRGB());

        List<RGBWithBoundaries> betweenList = list.subList(2, list.size() - 1);
        List<BetweenPart> betweenPartSubList = this.betweenParts.subList(1,
                this.betweenParts.size());

        for (int i = 0; i < betweenList.size(); i++) {
            if (i < betweenPartSubList.size()) {
                betweenPart = betweenPartSubList.get(i);
                textPart = this.textParts.get(i);
            } else {
                textPart = new TextPart(this, SWT.NONE);
                this.textParts.add(textPart);

                betweenPart = new BetweenPart(this, SWT.NONE);
                this.betweenParts.add(betweenPart);

                betweenPart.moveAbove(this.upperBorder);
                textPart.moveAbove(betweenPart);
            }

            RGBWithBoundaries boundaries = betweenList.get(i);

            betweenPart.colorSelect.setRGB(boundaries.getRGB());
            textPart.spinner
                    .setSelection(betweenList.get(i).getLowerBoundary());
        }

        this.upperBorder.colorSelect.setRGB(list.get(list.size() - 1).getRGB());

        if (this.betweenParts.size() > betweenList.size() + 1) {
            for (BetweenPart part : this.betweenParts.subList(betweenList
                    .size() + 1, this.betweenParts.size())) {
                part.dispose();
            }
            this.betweenParts.subList(betweenList.size() + 1,
                    this.betweenParts.size()).clear();
        }

        if (this.textParts.size() > betweenList.size()) {
            for (TextPart part : this.textParts.subList(betweenList.size(),
                    this.textParts.size())) {
                part.dispose();
            }
            this.textParts.subList(betweenList.size(), this.textParts.size())
                    .clear();
        }

        this.setRedraw(true);
        this.layout(true, true);

        updateSpinnerBoundaries();
        updateButtonEnabledStates();

        notifyRefreshListeners();
    }

    private void notifyRefreshListeners() {
        for (RefreshListener listener : this.refreshListeners) {
            listener.contentChanged(this);
        }
    }

    private final class ColorSelect extends Composite {

        private ColorSelector colorSelector;

        /**
         * Constructor
         * 
         * @param parent
         */
        public ColorSelect(Composite parent) {
            super(parent, SWT.FILL);
            FillLayout layout = new FillLayout();
            layout.type = SWT.HORIZONTAL;
            this.setLayout(layout);
            this.colorSelector = new ColorSelector(this);
        }

        /**
         * Gets the {@link RGB} of the selector.
         * 
         * @return the {@link RGB}
         */
        public RGB getRGB() {
            return this.colorSelector.getColorValue();
        }

        /**
         * Sets the {@link RGB} of the selector.
         * 
         * @param rgb
         *                the {@link RGB} to set.
         */
        public void setRGB(RGB rgb) {
            this.colorSelector.setColorValue(rgb);
        }
    }

    private final class TextPart extends Composite {
        final Spinner spinner;

        /**
         * @param parent
         * @param style
         */
        public TextPart(Composite parent, int style) {
            super(parent, style);
            this.setLayout(new GridLayout(1, false));
            final GridData gridData = new GridData(SWT.FILL, SWT.FILL, true,
                    false);
            this.setLayoutData(gridData);

            this.spinner = new Spinner(this, SWT.BORDER | SWT.READ_ONLY);
            this.spinner
                    .addModifyListener(RGBWithBoundariesEditor.this.spinnerListener);
        }
    }

    private final class BetweenPart extends Composite {
        final Label text;

        final ColorSelect colorSelect;

        final Button buttonAdd;

        final Button buttonRemove;

        /**
         * @param parent
         * @param style
         */
        public BetweenPart(Composite parent, int style) {
            super(parent, style);

            this.setLayout(new GridLayout(4, false));
            final GridData gridData = new GridData(SWT.FILL, SWT.FILL, true,
                    false);
            this.setLayoutData(gridData);

            this.text = new Label(this, SWT.NONE);
            this.text.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                    false));
            this.text.setText(Messages
                    .getString("PreferencePage.CORRALATION_BETWEEN")); //$NON-NLS-1$

            this.colorSelect = new ColorSelect(this);
            this.colorSelect.setRGB(new RGB(0, 0, 0));

            this.buttonRemove = new Button(this, SWT.PUSH);
            this.buttonRemove.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    e.widget = BetweenPart.this;
                    RGBWithBoundariesEditor.this.removeListener
                            .widgetSelected(e);
                }
            });
            this.buttonRemove.setText("-"); //$NON-NLS-1$

            this.buttonAdd = new Button(this, SWT.PUSH);
            this.buttonAdd.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    e.widget = BetweenPart.this;
                    RGBWithBoundariesEditor.this.addListener.widgetSelected(e);
                }
            });

            this.buttonAdd.setText("+"); //$NON-NLS-1$
        }
    }

    /**
     * A listener, which will be called, when the content of the
     * {@link RGBWithBoundariesEditor} changed.
     * 
     * @author Markus Wittlinger
     * @version 1.0 ($Id: RGBWithBoundariesEditor.java 1870 2007-08-15 21:45:23Z
     *          wittlims $)
     */
    public interface RefreshListener {
        /**
         * Is called, when the content of this {@link RGBWithBoundariesEditor}
         * changed
         * 
         * @param boundariesEditor
         *                the editor, whose content changed.
         */
        public void contentChanged(RGBWithBoundariesEditor boundariesEditor);
    }
}
