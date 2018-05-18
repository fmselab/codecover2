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

package org.codecover.eclipse.views.controls;

import java.util.List;
import java.util.Vector;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;

/**
 * This class extends a {@link Composite} and displays the legend of the
 * {@link MatrixControl}.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: LegendControl.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class LegendControl extends Composite {

    private ILegendContentProvider legendContentProvider;

    private final GridLayout gridLayout;

    private final List<Label> labelList;

    private final List<GridObject> gridObjectList;

    private final MouseListener mouseListener;

    /**
     * Constructor.
     * 
     * @param parent
     * @param style
     */
    public LegendControl(Composite parent, int style) {
        super(parent, style);
        this.gridLayout = new GridLayout(0, false);
        this.gridLayout.horizontalSpacing = 0;
        this.gridLayout.verticalSpacing = 0;
        super.setLayout(this.gridLayout);

        this.labelList = new Vector<Label>();
        this.gridObjectList = new Vector<GridObject>();

        this.mouseListener = new MatrixMouseListener();
        addMouseListener(this.mouseListener);
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.swt.widgets.Composite#setLayout(org.eclipse.swt.widgets.Layout)
     */
    @Override
    public void setLayout(Layout layout) {
        // We have our own layout
    }

    /**
     * Set the {@link ILegendContentProvider} of this {@link LegendControl}
     * 
     * @param legendContentProvider
     *                the given {@link ILegendContentProvider}
     */
    public void setLegendContentProvider(
            ILegendContentProvider legendContentProvider) {
        this.legendContentProvider = legendContentProvider;
    }

    /**
     * Refreshes the Legend with the data from the set
     * {@link ILegendContentProvider}.
     */
    public void refreshLegend() {
        if (this.legendContentProvider == null) {
            return;
        }
        // We don't redraw to reduce any flicker
        // setRedraw(false);

        // Clear the old widgets.
        disposeOldWidgets();

        List<Color> colors = this.legendContentProvider.getColors();
        List<String> labels = this.legendContentProvider.getLabels();

        if (!checkContentProviderData(colors, labels)) {
            throw new IllegalArgumentException(
                    "ILegendContentProvider data is not valid"); //$NON-NLS-1$
        }

        // We can stop here, if the data of the content provider is empty
        if (colors.size() == 0) {
            return;
        }

        this.gridLayout.numColumns = 2;

        for (int i = 0; i < colors.size(); i++) {
            Color color = colors.get(i);
            String labelText = labels.get(i);

            GridObject gridObject = new GridObject(this, SWT.NONE, color);
            gridObject.setShowMouseOver(false);
            gridObject.addMouseListener(this.mouseListener);
            gridObject.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false,
                    false));
            gridObject.setToolTipText(null);

            // Save for later disposal.
            this.gridObjectList.add(gridObject);

            Label label = new Label(this, SWT.NONE);
            label.setText(labelText);
            label.addMouseListener(this.mouseListener);
            GridData gridData = new GridData(SWT.LEFT, SWT.CENTER, true, true);
            label.setLayoutData(gridData);

            // Save for later disposal.
            this.labelList.add(label);
        }

        // setRedraw(true);
    }

    /**
     * Checks, if the sizes of the given lists are valid.
     * 
     * @param colors
     *                the list to check
     * @param labels
     *                the list to check
     * @return true, if the size of the colors is the same as the size of the
     *         labels
     */
    private boolean checkContentProviderData(List<Color> colors,
            List<String> labels) {
        if (colors == null || labels == null) {
            return false;
        }

        if (colors.size() != labels.size()) {
            return false;
        }

        return true;
    }

    private void disposeOldWidgets() {
        for (Label label : this.labelList) {
            label.dispose();
        }

        for (GridObject gridObject : this.gridObjectList) {
            gridObject.dispose();
        }

        this.labelList.clear();
        this.gridObjectList.clear();
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.swt.widgets.Widget#dispose()
     */
    @Override
    public void dispose() {
        disposeOldWidgets();
        super.dispose();
    }

    /**
     * This interface represents a content provider for this
     * {@link LegendControl} and is meant to be implemented by anyone, who
     * wishes to use this {@link LegendControl}.
     * 
     * @author Markus Wittlinger
     * @version 1.0 ($Id: LegendControl.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public interface ILegendContentProvider {
        /**
         * Sets the client of this {@link ILegendContentProvider}
         * 
         * @param legendControl
         *                the client.
         */
        public void setClient(LegendControl legendControl);

        /**
         * Gets the {@link List} of {@link Color}s, that are to be displayed
         * <p>
         * Note: {@link #getColors()}.size() == {@link #getLabels()}.size()
         * must apply
         * 
         * @return the {@link List}
         */
        public abstract List<Color> getColors();

        /**
         * Gets the {@link List} of {@link String}s, that are the labels for
         * the {@link Color}s
         * <p>
         * Note: {@link #getColors()}.size() == {@link #getLabels()}.size()
         * must apply
         * 
         * @return the {@link List}
         */
        public abstract List<String> getLabels();
    }

    private final class MatrixMouseListener extends MouseAdapter {

        /**
         * (non-Javadoc)
         * 
         * @see org.eclipse.swt.events.MouseAdapter#mouseDown(org.eclipse.swt.events.MouseEvent)
         */
        @Override
        public void mouseDown(MouseEvent e) {
            LegendControl.this.forceFocus();
        }
    }
}
