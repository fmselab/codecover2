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

import java.text.DecimalFormat;

import org.codecover.eclipse.Messages;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

/**
 * This class extends {@link Canvas} and represents an item in the
 * {@link MatrixControl}.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: GridObject.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class GridObject extends Canvas {
    private static final String TOTAL_ITEMS_FORMAT_PLURAL = Messages
            .getString("GridObject.TOTAL_ITEMS_FORMAT_PLURAL"); //$NON-NLS-1$

    private static final String TOTAL_ITEMS_FORMAT_SINGULAR = Messages
            .getString("GridObject.TOTAL_ITEMS_FORMAT_SINGULAR"); //$NON-NLS-1$

    private static final String COVERAGE_FORMAT = Messages
            .getString("GridObject.COVERAGE_FORMAT"); //$NON-NLS-1$

    private static final String SHARE_ITEMS_FORMAT_SINGULAR = Messages
            .getString("GridObject.SHARE_ITEMS_FORMAT_SINGULAR"); //$NON-NLS-1$

    private static final String SHARE_ITEMS_FORMAT_PLURAL = Messages
            .getString("GridObject.SHARE_ITEMS_FORMAT_PLURAL"); //$NON-NLS-1$

    private boolean isMouseOver = false;

    private final String leftName;

    private final String leftTooltip;

    private final String topName;

    private final String topTooltip;

    private final double correlation;

    private final int leftAmountCoverableItems;

    private final int topAmountCoverableItems;

    private final int sharedAmountCoverableItems;

    private final Color mainColor;

    private final Color borderColor;

    private boolean showMouseOver = true;

    private Image mainImage;

    private Image mouseOverImage;

    /**
     * Constructor
     * 
     * @param parent
     * @param style
     * @param leftName
     * @param leftTooltip
     * @param topName
     * @param topTooltip
     * @param correlation
     * @param leftAmountCoverableItems
     * @param topAmountCoverableItems
     * @param sharedAmountCoverableItems
     * @param mainColor
     */
    public GridObject(Composite parent, int style, String leftName,
            String leftTooltip, String topName, String topTooltip,
            double correlation, int leftAmountCoverableItems,
            int topAmountCoverableItems, int sharedAmountCoverableItems,
            Color mainColor) {
        super(parent, style);
        this.leftName = leftName;
        this.leftTooltip = leftTooltip;
        this.topName = topName;
        this.topTooltip = topTooltip;
        this.correlation = correlation;
        this.leftAmountCoverableItems = leftAmountCoverableItems;
        this.topAmountCoverableItems = topAmountCoverableItems;
        this.sharedAmountCoverableItems = sharedAmountCoverableItems;
        this.mainColor = mainColor;
        this.borderColor = getDisplay().getSystemColor(SWT.COLOR_BLACK);

        setTooltip();
        addPaintListener(new GridPaintListener());
        addMouseTrackListener(new GridMouseTrackListener());
        addControlListener(new GridControlListener());
    }

    /**
     * Constructor
     * 
     * @param parent
     * @param style
     * @param mainColor
     */
    public GridObject(Composite parent, int style, Color mainColor) {
        this(parent, style, "", "", "", "", 0.0, 0, 0, 0, mainColor); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$//$NON-NLS-4$
    }

    /**
     * Sets the showMouseOver property, which determines, if it is to be
     * indicated, that the mouse if over the object.
     * 
     * @param showMouseOver
     */
    public void setShowMouseOver(boolean showMouseOver) {
        this.showMouseOver = showMouseOver;
    }

    private void setTooltip() {
        DecimalFormat df = new DecimalFormat("0.0 %"); //$NON-NLS-1$

        StringBuilder sb = new StringBuilder();

        sb.append(String.format(COVERAGE_FORMAT, this.leftName, df
                .format(this.correlation), this.topName));

        sb.append("\n\n"); //$NON-NLS-1$

        if (this.leftAmountCoverableItems == 1) {
            sb.append(String.format(TOTAL_ITEMS_FORMAT_SINGULAR, this.leftName,
                    this.leftAmountCoverableItems));
        } else {
            sb.append(String.format(TOTAL_ITEMS_FORMAT_PLURAL, this.leftName,
                    this.leftAmountCoverableItems));
        }

        sb.append("\n"); //$NON-NLS-1$

        if (this.topAmountCoverableItems == 1) {
            sb.append(String.format(TOTAL_ITEMS_FORMAT_SINGULAR, this.topName,
                    this.topAmountCoverableItems));
        } else {
            sb.append(String.format(TOTAL_ITEMS_FORMAT_PLURAL, this.topName,
                    this.topAmountCoverableItems));
        }

        sb.append("\n"); //$NON-NLS-1$

        if (this.sharedAmountCoverableItems == 1) {
            sb.append(String.format(SHARE_ITEMS_FORMAT_SINGULAR,
                    this.sharedAmountCoverableItems));
        } else {
            sb.append(String.format(SHARE_ITEMS_FORMAT_PLURAL,
                    this.sharedAmountCoverableItems));
        }

        sb.append("\n\n"); //$NON-NLS-1$

        sb.append(this.leftTooltip);
        sb.append("\n"); //$NON-NLS-1$
        sb.append(this.topTooltip);
        sb.append("\n"); //$NON-NLS-1$

        super.setToolTipText(sb.toString());
    }

    private final class GridPaintListener implements PaintListener {

        /**
         * (non-Javadoc)
         * 
         * @see org.eclipse.swt.events.PaintListener#paintControl(org.eclipse.swt.events.PaintEvent)
         */
        @Override
		public void paintControl(PaintEvent e) {
            GC gc = e.gc;

            Canvas canvas = (Canvas) e.widget;

            if (GridObject.this.mainImage == null) {
                GridObject.this.mainImage = createMainImage(gc, canvas);
            }
            gc.drawImage(GridObject.this.mainImage, 0, 0);

            if (GridObject.this.isMouseOver && GridObject.this.showMouseOver) {
                if (GridObject.this.mouseOverImage == null) {
                    GridObject.this.mouseOverImage = createMouseOverImage(gc,
                            canvas);
                }
                gc.drawImage(GridObject.this.mouseOverImage, 0, 0);
            }
        }

        private Image createMainImage(GC gc, Canvas canvas) {
            Image image = new Image(canvas.getDisplay(), canvas.getSize().x,
                    canvas.getSize().y);

            // Initializes the graphics context of the image.
            GC imageGC = new GC(image);
            imageGC.setBackground(gc.getBackground());
            imageGC.setForeground(gc.getForeground());
            imageGC.setFont(gc.getFont());

            Rectangle imageSize = image.getBounds();

            imageGC.setBackground(GridObject.this.mainColor);
            imageGC.fillRectangle(0, 0, imageSize.width - 1,
                    imageSize.height - 1);

            imageGC.setForeground(GridObject.this.borderColor);
            imageGC.drawRectangle(0, 0, imageSize.width - 1,
                    imageSize.height - 1);

            imageGC.dispose();

            return image;
        }

        private Image createMouseOverImage(GC gc, Canvas canvas) {
            Image image = new Image(canvas.getDisplay(), canvas.getSize().x,
                    canvas.getSize().y);

            // Initializes the graphics context of the image.
            GC imageGC = new GC(image);
            imageGC.setBackground(gc.getBackground());
            imageGC.setForeground(gc.getForeground());
            imageGC.setFont(gc.getFont());

            Rectangle imageSize = image.getBounds();

            final Color transparentColor = canvas.getDisplay().getSystemColor(
                    SWT.COLOR_GRAY);
            final Color mouseOverBorderColor = canvas.getDisplay()
                    .getSystemColor(SWT.COLOR_WHITE);

            // Fill the image with the transparent color, for later use.
            imageGC.setBackground(transparentColor);
            imageGC.fillRectangle(imageSize);

            imageGC.setBackground(mouseOverBorderColor);

            // Draw the individual borders of the image.
            imageGC.fillRectangle(0, 0, imageSize.width, 2);
            imageGC.fillRectangle(0, 0, 2, imageSize.height);
            imageGC.fillRectangle(0, imageSize.height - 2, imageSize.width, 2);
            imageGC.fillRectangle(imageSize.width - 2, 0, 2, imageSize.height);

            imageGC.dispose();

            ImageData imageData = image.getImageData();
            imageData.transparentPixel = imageData.palette
                    .getPixel(transparentColor.getRGB());

            return new Image(canvas.getDisplay(), imageData);
        }
    }

    private final class GridMouseTrackListener extends MouseTrackAdapter {
        /**
         * (non-Javadoc)
         * 
         * @see org.eclipse.swt.events.MouseTrackAdapter#mouseEnter(org.eclipse.swt.events.MouseEvent)
         */
        @Override
        public void mouseEnter(MouseEvent e) {
            GridObject.this.isMouseOver = true;
            GridObject.this.redraw();
        }

        /**
         * (non-Javadoc)
         * 
         * @see org.eclipse.swt.events.MouseTrackAdapter#mouseExit(org.eclipse.swt.events.MouseEvent)
         */
        @Override
        public void mouseExit(MouseEvent e) {
            GridObject.this.isMouseOver = false;
            GridObject.this.redraw();
        }
    }

    private final class GridControlListener extends ControlAdapter {
        /**
         * (non-Javadoc)
         * 
         * @see org.eclipse.swt.events.ControlAdapter#controlResized(org.eclipse.swt.events.ControlEvent)
         */
        @Override
        public void controlResized(ControlEvent e) {
            GridObject.this.mainImage = null;
            GridObject.this.mouseOverImage = null;
        }
    }
}
