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

package org.codecover.eclipse.annotation.hotpath;

import org.codecover.eclipse.preferences.PreferencePageRoot;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.texteditor.IAnnotationImageProvider;

/**
 * Generate icons to visualize line execution count per
 * {@link EclLineExecutionAnnotation}.
 * <p>
 * The mapping from execution count to color is realized in two steps:
 * <ol>
 * <li>Map annotation to heat, which is an int in the range 0..MAX_HEAT linear
 * to the color the user sees. Edit
 * <LineExecutionImageProvider#getHeat(EclLineExecutionAnnotation)}
 * to change the color.
 * <li>Map unique heat ID to color. 
 * </ol>
 * @author  Johannes Langauf
 * @version $Id: LineExecutionImageProvider.java 1 2007-12-12 17:37:26Z t-scheller $
 */
public class LineExecutionImageProvider implements IAnnotationImageProvider {

    private final static int WIDTH = 8;
    private final static int HEIGHT = 16;
    
    /**
     * The value to normalize heat to. We produce: MAX_HEAT + 1 images.
     */
    private final static int MAX_HEAT = 255;
    
    @Override
	public ImageDescriptor getImageDescriptor(String imageDescritporId) {
        //solid icon with RGB-Color encoded as hex in imageDescriptorId
        return generateDescriptor(hexToRgb(imageDescritporId));
    }

    @Override
	public String getImageDescriptorId(Annotation a) {
        if (a instanceof EclLineExecutionAnnotation) {
            // id is color as a hexadecimal string
            return rgbToHex(getColor(getHeat((EclLineExecutionAnnotation) a)));
        } else {
            throw new IllegalArgumentException("unsupported annotation type: " //$NON-NLS-1$
                    + a.getType());
        }
    }

    private static RGB hexToRgb(String hex) {
        int r = Integer.parseInt(hex.substring(0, 2), 16);
        int g = Integer.parseInt(hex.substring(2, 4), 16);
        int b = Integer.parseInt(hex.substring(4, 6), 16);
        
        return new RGB(r, g, b);
    }
    
    private static String rgbToHex(RGB color) {
        return unsignedByteToString(color.red)
             + unsignedByteToString(color.green)
             + unsignedByteToString(color.blue);
                
    }
    
    private static String unsignedByteToString(int i) {
        //assert that i is in 0..255
//        if (i < 0 || i > 255) {
//           throw new IllegalArgumentException("i not in range 0..255: " + i);
//        }
        return "" + Character.forDigit(i / 16, 16) //$NON-NLS-1$
                  + Character.forDigit(i % 16, 16);
    }
    
    @Override
	public Image getManagedImage(Annotation annotation) {
        return null; // force external management of images (See code above)
    }

    /**
     * Get the appropriate heat to show for an annotation. The heat is a uid
     * that defines which discrete level of heat the user sees.
     * <p>
     * To be displayed the heat is mapped to an icon with a solid color using
     * linear  interpolation in RGB-Space from the color for cold(heat=0) to
     * hot(heat=MAX_HEAT) in <code>getColor(int)</code>
     * 
     * @param annotation
     * the annotation to calculate heat for
     * @return the heat in the range 0..MAX_HEAT, 0 is coldest, higher is hotter
     * @see LineExecutionImageProvider.getColor(final int)
     */
    private static int getHeat(EclLineExecutionAnnotation annotation) {
        int result;

        result = (int) Math.floor((double) MAX_HEAT * annotation.getExecutions()
                                / annotation.getMaxExecutions());
        
        return result;
    }    
    
    /**
     * Get the appropriate color to show for the given heat.
     * <p>
     * Mapping is linear interpolation on RGB-Space from cold to hot. More
     * sophisticated mappings must be done in
     * {@link #getHeat(EclLineExecutionAnnotation)}.
     *   
     * @param heat
     * the heat to map color to
     * @return the color to show for <code>heat</code>
     */
    private static RGB getColor(final int heat) {
        RGB colorCold = PreferencePageRoot.getHotPathCold();
        RGB colorHot = PreferencePageRoot.getHotPathHot();

        /* do linear blending between colorCold and colorHot */
        int coldness = MAX_HEAT - heat;
        int r = (heat * colorHot.red + coldness * colorCold.red) / MAX_HEAT;
        int g = (heat * colorHot.green + coldness * colorCold.green) / MAX_HEAT;
        int b = (heat * colorHot.blue + coldness * colorCold.blue) / MAX_HEAT;
        
        return new RGB (r, g, b);
    }
    
    /**
     * Generate an Icon with a solid color.
     */
    private static ImageDescriptor generateDescriptor(final RGB color) {        
        ImageData imageData = generateColoredBox(WIDTH, HEIGHT, color);
        return ImageDescriptor.createFromImageData(imageData);
    }

    /**
     * Generate an Icon with a solid color.
     * 
     * @param width
     * width in pixels
     * @param height
     * height in pixels
     * @param color
     * color of the icon
     * @return
     * the icon
     */
    private static ImageData generateColoredBox(final int width,
            final int height, final RGB color) {
        ImageData imageData = new ImageData(width,
                                            height,
                                            32,
                                            new PaletteData(0xFF0000,
                                                            0xFF00,
                                                            0xFF));
        final int pixelValue = imageData.palette.getPixel(color);

        for (int i = 0; i < width; i++) {
            for (int a = 0; a < height; a++) {
                imageData.setPixel(i,
                        a,
                        pixelValue);
            }
        }
        return imageData;
    }
}
