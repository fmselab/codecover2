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

package org.codecover.eclipse.utils;

import java.util.HashMap;
import java.util.Map;

import org.codecover.metrics.coverage.CoverageResult;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.RGB;

/**
 * Provides the images for the instrumentation decorator used in the package
 * explorer and the coverage indicator bars used in the Coverage view.
 * 
 * @author Markus Wittlinger, Robert Hanussek
 * @version 1.0 ($Id: ImageProvider.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ImageProvider {

    private static ImageDescriptor decorator;

    private static Map<CoverageResult, Image> covIndCache
            = new HashMap<CoverageResult, Image>();
    
    private static final Object covIndCacheLock = new Object();
    
    private static final int covIndIconHeight = 16;

    private static final int covIndIconWidth = 16;

    //
    // private static final int covIndBarHeight = 7;
    //
    // private static final int covIndBarWidth = 16;
    //
    // private static final int covIndBarXPos = 0;
    //
    // private static final int covIndBarYPos = 5;

    private static final RGB covIndCoveredColor = new RGB(20, 210, 0);

    private static final RGB covIndUncoveredColor = new RGB(186, 0, 0);

    private static final RGB covIndUnknownColor = new RGB(238, 238, 238);

    /**
     * The number of steps of the coverage indicator bar. For each step an image
     * is created, plus the image for the 0% step.
     */
    private static final int covIndSteps = covIndIconWidth; 
    
    /**
     * If <code>true</code> only 0% is represented by the
     * 0%-coverage-indicator and only 100% is represented by the
     * 100%-coverage-indicator. If <code>false</code>, it is possible that a
     * value which is not 0 (but close to 0) is represented by a
     * 0%-coverage-indicator depending on the number of {@link #covIndSteps}
     * 0% and 100% are interpreted relative to the number of significant
     * fraction digits for percentages, i.e. if this number is 1:
     * 100% == 100.04% and 100% == 99.95% but 100% != 100.06% and
     * 100% != 99.94%, which in turn means that 99.95% is represented by a
     * 100%-coverage-indicator but not 99.94%.
     */
    private static final boolean covIndTrueLimits = true;
    
    /**
     * The number of significant fraction digits when comparing the
     * <em>percentages</em> of coverage results. The percentages to compare
     * are rounded to this number of digits. 
     */
    private static final int covIndNumberOfSignificantFractionDigits = 1;
    
    private static final int covIndPrecision
            = (int)Math.pow(10, covIndNumberOfSignificantFractionDigits+2);
    
    private static final float covIndMinQuotientFor100Percent
            = 1f-(0.5f/covIndPrecision);
    
    private static final float covIndMaxQuotientFor0Percent
            = 0f+(0.5f/covIndPrecision);
    
    /**
     * A coverage indicator visualizes the result of a coverage measurement by
     * displaying a green bar which represents the percentage of covered code.
     * This method creates an image of this bar from a given
     * <code>CoverageResult</code> and caches it for subsequent calls, thus the
     * returned image must not be disposed.
     * 
     * @param result
     *            the <code>CoverageResult</code> to create the coverage
     *            indicator from
     * @return  an image which represents the coverage indicator or
     *          <code>null</code> if the given <code>CoverageResult</code> is
     *          <code>null</code> or if the image could not be created
     */
    public static Image generateCoverageIndicator(CoverageResult result) {
        if(result == null) {
            return null;
        }
        CoverageResult representingCovResult
                = calculateRepresentingCoverageResult(result); 
        Image img;
        synchronized(covIndCacheLock) {
            img = covIndCache.get(representingCovResult);
            if(img != null) {
                return img;
            } else {
                img = generateCoverageIndicator(
                        representingCovResult.getCoveredItems(),
                        representingCovResult.getTotalItems()).createImage();
                if(img != null) {
                    covIndCache.put(representingCovResult, img);
                }
                return img;
            }
        }
    }

    private static CoverageResult calculateRepresentingCoverageResult(
            CoverageResult result) {
        int cov = result.getCoveredItems();
        int tot = result.getTotalItems();
        float quotient = cov / (float)tot; 
        int newCov;
        if(tot > 0) {
            newCov = Math.round(cov * (covIndSteps / (float)tot));
            /*
             * make sure that only 0% is represented by the
             * 0%-representing-coverage-result and only 100% is represented by
             * the 100%-representing-coverage-result 
             */
            if(covIndTrueLimits) {
                if(newCov == 0 && quotient > covIndMaxQuotientFor0Percent) {
                    newCov = 1;
                } else if(newCov == covIndSteps
                        && quotient < covIndMinQuotientFor100Percent) {
                    newCov = covIndSteps - 1;
                }
            }
            return new CoverageResult(newCov, covIndSteps);
        } else {
            return CoverageResult.NULL;
        }
    }
    
    /**
     * A coverage indicator visualizes the result of a coverage measurement by
     * displaying a green bar which represents the percentage of covered code.
     * This method creates an image of this bar from a given
     * <code>CoverageResult</code>.
     * 
     * @param coveredCount
     *            the amount of covered coverableItems
     * @param totalCount
     *            the total amount of coverableItems
     * @return an image which represents the coverage indicator.
     */
    private static ImageDescriptor generateCoverageIndicator(int coveredCount,
            int totalCount) {
        // Display display = Display.getCurrent();
        // Image img = new Image(display, covIndIconWidth, covIndIconHeight);
        // img.setBackground(display.getSystemColor(SWT.COLOR_RED));
        // GC gc = new GC(img);
        // // draw background
        // gc.setBackground(new Color(display, covIndUncoveredColor));
        // gc.fillRectangle(covIndBarXPos,
        // covIndBarYPos,
        // covIndBarWidth,
        // covIndBarHeight);
        // // draw bar
        // float barWidth = covIndBarWidth;
        // // if there are no items to be covered, bar width stays at 100
        // percent
        // if (totalCount > 0) {
        // barWidth *= ((float) coveredCount / totalCount);
        // gc.setBackground(new Color(display, covIndCoveredColor));
        // } else {
        // gc.setBackground(new Color(display, covIndUnknownColor));
        // }
        // gc.fillRectangle(covIndBarXPos,
        // covIndBarYPos,
        // Math.round(barWidth),
        // covIndBarHeight);
        // gc.dispose();
        // // Now let's make the indicator image transparent.
        // ImageData imgData = img.getImageData();
        // img.dispose(); // get rid of the old image
        // imgData.transparentPixel = imgData.palette.getPixel(new RGB(255,
        // 255,
        // 255));
        // return the transparent image
        // return new Image(display, imgData);
        return generateDecorator(coveredCount,
                                 totalCount,
                                 covIndIconWidth,
                                 covIndIconHeight);
    }

    private static ImageDescriptor generateDecorator(int coveredCount,
            int totalCount, final int covIndIconWidth,
            final int covIndIconHeight) {
        final int covBarHeight = covIndIconHeight / 2;
        final int yPosOffset = (covIndIconHeight - covBarHeight) / 2 + 1;

        ImageData imageData = new ImageData(covIndIconWidth,
                                            covIndIconHeight,
                                            32,
                                            new PaletteData(0xFF0000,
                                                            0xFF00,
                                                            0xFF));

        if (totalCount <= 0) {
            for (int i = 0; i < covIndIconWidth; i++) {
                for (int a = yPosOffset; a < covIndIconHeight - yPosOffset; a++) {
                    imageData.setPixel(i,
                                       a,
                                       imageData.palette.getPixel(covIndUnknownColor));
                }
            }
        } else {
            int dividerPos = Math.round(covIndIconWidth
                    * ((float) coveredCount / (float) totalCount));

            for (int i = 0; i < covIndIconWidth; i++) {
                for (int a = yPosOffset; a < covIndIconHeight - yPosOffset; a++) {
                    RGB rgb;
                    if (i < dividerPos) {
                        rgb = covIndCoveredColor;
                    } else {
                        rgb = covIndUncoveredColor;
                    }

                    imageData.setPixel(i, a, imageData.palette.getPixel(rgb));
                }
            }
        }

        imageData.transparentPixel = imageData.palette.getPixel(new RGB(0, 0, 0));

        return ImageDescriptor.createFromImageData(imageData);
    }

    /**
     * The image to be used in the packageExplorer as a decorator for to be
     * instrumented Items.
     * 
     * @return the {@link ImageDescriptor}
     */
    public static ImageDescriptor getDecorator() {
        if (decorator == null) {
            decorator = generateDecorator(1,
                                          2,
                                          ImageProvider.covIndIconWidth / 2,
                                          2 * ImageProvider.covIndIconHeight / 3);
        }
        return decorator;
    }
    
    /**
     * Disposes all provided images (provided via
     * {@link #generateCoverageIndicator(CoverageResult)}). Be sure that they
     * aren't in use anymore.
     */
    public static void dispose() {
        synchronized(covIndCacheLock) {
            for(Image img : covIndCache.values()) {
                img.dispose();
            }
            covIndCache.clear();
        }
    }
}
