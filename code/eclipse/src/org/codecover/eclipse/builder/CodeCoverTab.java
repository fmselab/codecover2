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

package org.codecover.eclipse.builder;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

/**
 * 
 * @author Tilmann Scheller
 * @version 1.0 ($Id: CodeCoverTab.java 63 2009-09-27 12:13:06Z ahija $)
 */
public class CodeCoverTab extends AbstractLaunchConfigurationTab {

    private static final String RUN_WITH_CODE_COVER_BUTTON_LABEL = Messages
            .getString("CodeCoverTab.RUN_WITH_CODE_COVER_BUTTON_LABEL"); //$NON-NLS-1$

    private Button codeCoverState;

    @Override
	public void createControl(Composite parent) {
        parent = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout();
        parent.setLayout(layout);
        codeCoverState = createCheckButton(parent, RUN_WITH_CODE_COVER_BUTTON_LABEL);

        codeCoverState.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setDirty(true);
                updateLaunchConfigurationDialog();
            }
        });
        setControl(parent);
    }

    @Override
	public String getName() {
        return CodeCoverPlugin.NAME;
    }

    @Override
	public void initializeFrom(ILaunchConfiguration config) {
        try {
            if (CodeCoverClasspathProvider.isRunningWithCodeCover(config)) {
                codeCoverState.setSelection(true);
            }
            IJavaProject asJavaProject = JavaRuntime.getJavaProject(config);
            if (asJavaProject != null && !CodeCoverPlugin.isCodeCoverActivated(asJavaProject.getProject())) {
                codeCoverState.setEnabled(false);
            }
        } catch (CoreException e) {
            e.printStackTrace();
        }
    }

    @Override
	public void performApply(ILaunchConfigurationWorkingCopy config) {
        CodeCoverClasspathProvider.setRunWithCodeCover(config, codeCoverState.getSelection());
    }

    @Override
	public void setDefaults(ILaunchConfigurationWorkingCopy configuration) {
    }

}
