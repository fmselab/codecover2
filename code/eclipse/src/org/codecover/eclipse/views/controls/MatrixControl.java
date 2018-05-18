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
import java.util.List;
import java.util.Vector;

import org.codecover.eclipse.Messages;
import org.codecover.eclipse.views.controls.LegendControl.ILegendContentProvider;
import org.codecover.metrics.correlation.CorrelationMetric;
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
 * This class extends {@link Composite} and displays the results of a
 * {@link CorrelationMetric} in a matrix.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: MatrixControl.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MatrixControl extends Composite {
    private static final String TOOLTIP_FORMAT = Messages
            .getString("MatrixControl.TOOLTIP_FORMAT"); //$NON-NLS-1$

    private IMatrixContentProvider matrixContentProvider;

    private ILegendContentProvider legendContentProvider;

    private LegendControl legendControl;

    private final List<Label> labelList;

    private final List<GridObject> gridObjectList;

    private final GridLayout gridLayout;

    private final MouseListener mouseListener;

    private boolean showLegend = true;

    /**
     * Constructor
     * 
     * @param parent
     * @param style
     */
    public MatrixControl(Composite parent, int style) {
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
     * Set the {@link IMatrixContentProvider} of this {@link MatrixControl}
     * 
     * @param matrixContentProvider
     *                the given {@link IMatrixContentProvider}
     */
    public void setMatrixContentProvider(
            IMatrixContentProvider matrixContentProvider) {
        this.matrixContentProvider = matrixContentProvider;
    }

    /**
     * Set the {@link ILegendContentProvider} of this {@link MatrixControl}
     * 
     * @param legendContentProvider
     *                the given {@link ILegendContentProvider}
     */
    public void setLegendContentProvider(
            ILegendContentProvider legendContentProvider) {
        this.legendContentProvider = legendContentProvider;
    }

    /**
     * Sets whether or not the legend is displayed.
     * <p>
     * Note: {@link #refreshMatrix()} still needs to be called, for the change
     * to have any effect.
     * 
     * @param showLegend
     *                indicates, if the legend is shown.
     */
    public void setShowLegend(boolean showLegend) {
        this.showLegend = showLegend;
    }

    /**
     * Indicates whether or not the legend is displayed.
     * 
     * @return indicates, if the legend is shown.
     */
    public boolean isShowLegend() {
        return this.showLegend;
    }

    /**
     * Refreshes the Matrix with the data from the set
     * {@link IMatrixContentProvider}.
     */
    public void refreshMatrix() {
        if (this.matrixContentProvider == null) {
            return;
        }
        // We don't redraw to reduce any flicker
        setRedraw(false);

        // Clear the old widgets.
        disposeOldWidgets();

        List<TestCaseInfo> infos = this.matrixContentProvider
                .getTestCaseInfos();
        List<CorrelationInfo> correlationInfos = this.matrixContentProvider
                .getCorrelationInfos();

        if (!checkContentProviderData(infos, correlationInfos)) {
            throw new IllegalArgumentException(
                    "IMatrixContentProvider data is not valid"); //$NON-NLS-1$
        }

        // We can stop here, if the data of the content provider is empty
        if (infos.size() == 0) {
            setRedraw(true);
            return;
        }

        if (this.showLegend) {
            this.gridLayout.numColumns = infos.size() + 2;
        } else {
            this.gridLayout.numColumns = infos.size() + 1;
        }

        // A blank label for the upper left corner of the matrix
        Label blankLabel = new Label(this, SWT.NONE);
        blankLabel.addMouseListener(this.mouseListener);
        // Save for later disposal.
        this.labelList.add(blankLabel);

        for (int i = 0; i < infos.size(); i++) {
            TestCaseInfo testCaseInfo = infos.get(i);
            Label label = new Label(this, SWT.NONE);
            label.setText(testCaseInfo.shortTestCaseName);
            label.setToolTipText(getLabelTooltip(testCaseInfo, i));
            label.addMouseListener(this.mouseListener);
            label.setLayoutData(new GridData(SWT.CENTER, SWT.BOTTOM, false,
                    false));
            // Save for later disposal.
            this.labelList.add(label);
        }

        if (this.showLegend) {
            Label blankLabel2 = new Label(this, SWT.NONE);
            blankLabel2.addMouseListener(this.mouseListener);
            // Save for later disposal.
            this.labelList.add(blankLabel2);
        }

        for (int i = 0; i < correlationInfos.size(); i++) {
            final int infoDivIndex = i / infos.size();
            final int infoModIndex = i % infos.size();

            TestCaseInfo leftInfo = infos.get(infoDivIndex);
            TestCaseInfo topInfo = infos.get(infoModIndex);

            // At the beginning of every row, we insert a label.
            if (infoModIndex == 0) {
                Label label = new Label(this, SWT.NONE);
                label.setText(leftInfo.shortTestCaseName);
                label.setToolTipText(getLabelTooltip(leftInfo, infoDivIndex));
                label.addMouseListener(this.mouseListener);
                label.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                        false));
                // Save for later disposal.
                this.labelList.add(label);
            }

            CorrelationInfo correlationInfo = correlationInfos.get(i);
            Color mainColor = this.matrixContentProvider
                    .getColorForCorrelation(correlationInfo.correlation);

            GridObject gridObject = new GridObject(this, SWT.NONE,
                    leftInfo.testCaseName, getLabelTooltip(leftInfo,
                            infoDivIndex), topInfo.testCaseName,
                    getLabelTooltip(topInfo, infoModIndex),
                    correlationInfo.correlation,
                    correlationInfo.firstTestCaseAmountCoverableItems,
                    correlationInfo.secondTestCaseAmountCoverableItems,
                    correlationInfo.sharedAmountCoverableItems, mainColor);
            gridObject.addMouseListener(this.mouseListener);
            gridObject.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false,
                    false));

            // Save for later disposal.
            this.gridObjectList.add(gridObject);

            if (this.showLegend) {
                if (infoDivIndex == 0 && infoModIndex == infos.size() - 1) {
                    this.legendControl = new LegendControl(this, SWT.NONE);
                    this.legendContentProvider.setClient(this.legendControl);
                    this.legendControl
                            .setLegendContentProvider(this.legendContentProvider);
                    GridData gridData = new GridData(SWT.RIGHT, SWT.BOTTOM,
                            false, false);
                    gridData.verticalSpan = infos.size() + 1;
                    this.legendControl.setLayoutData(gridData);
                }
            }
        }

        for (int i = 0; i < infos.size() + 1; i++) {
            Label label = new Label(this, SWT.NONE);
            label.addMouseListener(this.mouseListener);
            label.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false));
            // Save for later disposal.
            this.labelList.add(label);
        }

        if (this.showLegend) {
            this.legendControl.refreshLegend();
        }
        // We are finished, so we redraw again.
        setRedraw(true);

        layout();
    }

    private String getLabelTooltip(TestCaseInfo testCaseInfo, int index) {
        StringBuilder sb = new StringBuilder();

        sb.append(String.format(TOOLTIP_FORMAT, testCaseInfo.testCaseName,
                testCaseInfo.testSessionName));

        return sb.toString();
    }

    private void disposeOldWidgets() {
        if (this.legendControl != null) {
            this.legendControl.dispose();
            this.legendControl = null;
        }

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
     * Checks, if the sizes of the given infos are valid.
     * 
     * @param infos
     *                the infos to check
     * @param correlationInfos
     *                the infos to check
     * @return true, if the size of the infos is the same as the square of the
     *         size of the correlationInfos
     */
    private boolean checkContentProviderData(List<TestCaseInfo> infos,
            List<CorrelationInfo> correlationInfos) {
        if (infos == null || correlationInfos == null) {
            return false;
        }

        final int testCaseInfoLength = infos.size();
        final int correlationInfosLength = correlationInfos.size();

        if (correlationInfosLength != (int) Math.pow(testCaseInfoLength, 2.0)) {
            return false;
        }

        return true;
    }

    /**
     * Creates a csv export string from the currently displayed matrix.
     * 
     * @return the csv export string.
     */
    public final String createCSVExportString() {
        List<CorrelationInfo> correlationInfos = this.matrixContentProvider
                .getCorrelationInfos();
        List<TestCaseInfo> testCaseInfos = this.matrixContentProvider
                .getTestCaseInfos();

        final String seperator = ","; //$NON-NLS-1$
        final String newLine = "\r\n"; //$NON-NLS-1$
        final DecimalFormat df = new DecimalFormat("0.0"); //$NON-NLS-1$

        StringBuilder sb = new StringBuilder();

        // Add initial blank cell.
        sb.append(seperator);

        for (int i = 0; i < testCaseInfos.size(); i++) {
            TestCaseInfo testCaseInfo = testCaseInfos.get(i);

            if (i != 0) {
                sb.append(seperator);
            }

            sb.append(getCSVString(testCaseInfo.testCaseName));
        }

        sb.append(newLine);

        for (int i = 0; i < correlationInfos.size(); i++) {
            final int infoDivIndex = i / testCaseInfos.size();
            final int infoModIndex = i % testCaseInfos.size();

            TestCaseInfo leftInfo = testCaseInfos.get(infoDivIndex);

            // At the beginning of every row, we insert a test case name.
            if (infoModIndex == 0) {
                sb.append(getCSVString(leftInfo.testCaseName));
            }

            CorrelationInfo correlationInfo = correlationInfos.get(i);

            sb.append(seperator);
            sb.append(getCSVString(df.format(correlationInfo.correlation)));

            // At the end of every row, we insert a newline
            if (infoModIndex == testCaseInfos.size() - 1) {
                sb.append(newLine);
            }

        }
        return sb.toString();
    }

    private final String getCSVString(String oldString) {
        final String quotationMark = "\""; //$NON-NLS-1$
        final String doubleQuotationMark = quotationMark + quotationMark;
        final String regExpQuotationMark = "\\" + quotationMark; //$NON-NLS-1$
        StringBuilder sb = new StringBuilder();

        // Surround the given String with ".

        sb.append(quotationMark);

        // Replace all ", that were already present in the given string with "".
        sb.append(oldString
                .replaceAll(regExpQuotationMark, doubleQuotationMark));

        sb.append(quotationMark);

        return sb.toString();
    }

    /**
     * A struct holding the information about the test cases
     * 
     * @author Markus Wittlinger
     * @version 1.0 ($Id: MatrixControl.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public class TestCaseInfo {
        final String shortTestCaseName;

        final String testCaseName;

        final String testSessionName;

        /**
         * Constructor
         * 
         * @param testCaseName
         * @param shortTestCaseName
         * @param testSessionName
         */
        public TestCaseInfo(final String testCaseName,
                final String shortTestCaseName, final String testSessionName) {
            this.testCaseName = testCaseName;
            this.testSessionName = testSessionName;
            this.shortTestCaseName = shortTestCaseName;
        }
    }

    /**
     * A struct holding the information of the correlation of two test cases
     * 
     * @author Markus Wittlinger
     * @version 1.0 ($Id: MatrixControl.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public class CorrelationInfo {
        final double correlation;

        final int sharedAmountCoverableItems;

        final int firstTestCaseAmountCoverableItems;

        final int secondTestCaseAmountCoverableItems;

        /**
         * Constructor
         * 
         * @param correlation
         * @param sharedAmountCoverableItems
         * @param firstTestCaseAmountCoverableItems
         * @param secondTestCaseAmountCoverableItems
         */
        public CorrelationInfo(final double correlation,
                final int sharedAmountCoverableItems,
                final int firstTestCaseAmountCoverableItems,
                final int secondTestCaseAmountCoverableItems) {
            this.correlation = correlation;
            this.sharedAmountCoverableItems = sharedAmountCoverableItems;
            this.firstTestCaseAmountCoverableItems = firstTestCaseAmountCoverableItems;
            this.secondTestCaseAmountCoverableItems = secondTestCaseAmountCoverableItems;
        }
    }

    /**
     * This interface represents a content provider for this
     * {@link MatrixControl} and is meant to be implemented by anyone, who
     * wishes to use this {@link MatrixControl}.
     * 
     * @author Markus Wittlinger
     * @version 1.0 ($Id: MatrixControl.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public interface IMatrixContentProvider {
        /**
         * Gets the {@link TestCaseInfo} objects, that hold the information
         * about the to be displayed test cases
         * 
         * @return the List of {@link TestCaseInfo}s
         */
        public abstract List<TestCaseInfo> getTestCaseInfos();

        /**
         * Gets the {@link CorrelationInfo} objects, that hold the information
         * about the correlation between the test cases.
         * <p>
         * Note: The length of the this {@link List} equal to the square of the
         * length of the list of {@link TestCaseInfo}s received in
         * {@link #getTestCaseInfos()}
         * <p>
         * The {@link CorrelationInfo}s are to be ordered in the following way
         * <br>
         * <code>
         * -------<br>
         * |1 2 3|<br>
         * |4 5 6|<br>
         * |7 8 9|<br>
         * -------<br>
         * </code>
         * 
         * @return the list representing the matrix.
         */
        public abstract List<CorrelationInfo> getCorrelationInfos();

        /**
         * Returns the {@link Color}, that is assigned to the given correlation
         * value.
         * 
         * @param correlation
         *                the correlation, whose Color is desired.
         * @return the Color for the given correlation.
         */
        public abstract Color getColorForCorrelation(double correlation);
    }

    private final class MatrixMouseListener extends MouseAdapter {

        /**
         * (non-Javadoc)
         * 
         * @see org.eclipse.swt.events.MouseAdapter#mouseDown(org.eclipse.swt.events.MouseEvent)
         */
        @Override
        public void mouseDown(MouseEvent e) {
            MatrixControl.this.forceFocus();
        }
    }
}
