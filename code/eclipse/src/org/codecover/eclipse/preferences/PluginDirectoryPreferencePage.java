/**
 * 
 */
package org.codecover.eclipse.preferences;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * @author Michael
 * 
 */
public class PluginDirectoryPreferencePage extends PreferencePage implements
        IWorkbenchPreferencePage {

    public static final String PREFERENCE_PLUGINS_DIR = "org.codecover.plugins.dir"; //$NON-NLS-1$

    private DirectoryFieldEditor directoryEditor;

    /**
     * 
     */
    public PluginDirectoryPreferencePage() {
        setPreferenceStore(CodeCoverPlugin.getDefault().getPreferenceStore());
        setDescription(Messages.getString("PluginPreferences.PAGE_DESCRIPTION")); //$NON-NLS-1$
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected Control createContents(Composite parent) {
        final Composite composite = new Composite(parent, SWT.NULL);
        GridLayout layout = new GridLayout();
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        composite.setLayout(layout);
        composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        composite.setFont(parent.getFont());
        directoryEditor = new DirectoryFieldEditor(
                PREFERENCE_PLUGINS_DIR,
                Messages.getString("PluginPreferences.DIRECTORY_FIELD_CAPTION"), //$NON-NLS-1$
                composite);
        directoryEditor.setPreferenceStore(CodeCoverPlugin.getDefault()
                .getPreferenceStore());
        directoryEditor.setEmptyStringAllowed(true);
        directoryEditor.load();
        return composite;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
	public void init(IWorkbench workbench) {
        // Do nothing
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        if (!CodeCoverPlugin.getDefault().getPreferenceStore()
                .getString(PREFERENCE_PLUGINS_DIR).equals(directoryEditor.getStringValue())) {
            directoryEditor.store();
            MessageDialog.openInformation(null,
                    Messages.getString("PluginPreferences.NEEDS_RESTART_TITLE"), //$NON-NLS-1$
                    Messages.getString("PluginPreferences.NEEDS_RESTART_MESSAGE")); //$NON-NLS-1$
            //TODO instead of this unergonomic message, you could: save the
            //     session container, dispose the EclipsePluginManager, create a
            //     new one and redraw all CodeCover Views as many things may
            //     have changed. NB:  processes running in the background, like
            //     the report generation, may crash, so take care to do this
            //     when nothing is running.
            //     EASYER, BUT ONLY MINOR improvement: provide a restart-button
            //     in the MessageDialog, which restarts Eclipse in exactly the 
            //     same state (except the CodeCover plugins).

        } 
        return super.performOk();
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
     */
    @Override
    protected void performDefaults() {
        this.directoryEditor.loadDefault();
        super.performDefaults();
    }

}
