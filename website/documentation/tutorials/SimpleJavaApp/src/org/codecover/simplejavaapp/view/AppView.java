package org.codecover.simplejavaapp.view;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.codecover.simplejavaapp.controller.AppController;
import org.codecover.simplejavaapp.model.AppFile;
import org.codecover.simplejavaapp.model.AppModel;
import org.codecover.simplejavaapp.view.AppView.ViewListener.ViewEvent;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: AppView.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class AppView {
    private static final AppModel model = AppModel.getInstance();

    private static AppView instance;

    private final List<ViewListener> listeners;

    private final Map<String, FrameMain> windowList;

    /**
     * Singleton-Pattern method to get the instance of {@link AppView}.
     * 
     * @return the instance of the {@link AppView}
     */
    public static AppView getInstance() {
        if (instance == null) {
            instance = new AppView();
        }

        return instance;
    }

    private AppView() {
        this.windowList = new HashMap<String, FrameMain>();
        this.listeners = new LinkedList<ViewListener>();
    }

    void notifyViewListener(ViewEvent viewEvent, String fileId, Object args) {
        for (ViewListener listener : this.listeners) {
            listener.update(viewEvent, fileId, args);
        }
    }

    /**
     * Adds the given {@link ViewListener} to the list of listeners.
     * 
     * @param listener
     *            the given {@link ViewListener}
     */
    public void addViewListener(ViewListener listener) {
        this.listeners.add(listener);
    }

    /**
     * Removes the given {@link ViewListener} from the list of listeners.
     * 
     * @param listener
     *            the given {@link ViewListener}
     */
    public void removeViewListener(ViewListener listener) {
        this.listeners.remove(listener);
    }

    /**
     * Opens a new window with the {@link AppFile}, as denoted by the given id,
     * as its content.
     * 
     * @param fileId
     *            the id of the {@link AppFile}.
     */
    public void openWindow(String fileId) {
        FrameMain frameMain = new FrameMain(model.getPathOfFile(fileId), fileId);

        this.windowList.put(fileId, frameMain);
        frameMain.setVisible(true);
    }

    /**
     * Opens a file with the given id in the window currently still displaying
     * another file.
     * 
     * @param oldFileId
     *            the id of the currently displayed file.
     * @param newFileId
     *            the id of the to be displayed file.
     */
    public void openFileInWindow(String oldFileId, String newFileId) {
        FrameMain frameMain = getWindowFromId(oldFileId);

        this.windowList.remove(oldFileId);

        this.windowList.put(newFileId, frameMain);

        frameMain.setFileId(newFileId);
    }

    /**
     * Closes the window displaying the file with the given id.
     * 
     * @param fileId
     *            the id of the displayed file.
     */
    public void closeWindow(String fileId) {
        FrameMain frameMain = getWindowFromId(fileId);

        this.windowList.remove(fileId);

        frameMain.setVisible(false);
        frameMain.dispose();
    }

    /**
     * Displays the given error message.
     * 
     * @param message
     *            the error message.
     * @param fileId
     *            the id of the currently displayed file.
     */
    public void showErrorDialog(String message, String fileId) {
        JOptionPane.showMessageDialog(getWindowFromId(fileId), message,
                "Error", JOptionPane.ERROR_MESSAGE);
    }

    /**
     * Displays the given error message.
     * 
     * @param message
     *            the error message.
     * @param fileId
     *            the id of the currently displayed file.
     * @return the result
     */
    public int showConfirmationDialog(String message, String fileId) {
        return JOptionPane.showConfirmDialog(getWindowFromId(fileId), message,
                "Please Confirm", JOptionPane.YES_NO_CANCEL_OPTION);
    }

    /**
     * Shows a save as… dialog.
     * 
     * @param fileId
     *            the id of the currently displayed file.
     * @return the path of the file, or <code>null</code> if the action was
     *         canceled.
     */
    public String showSaveAsDialog(String fileId) {
        JFileChooser chooser = new JFileChooser();
        chooser.setDialogType(JFileChooser.SAVE_DIALOG);
        chooser.setDialogTitle("Save As…");
        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

        if (chooser.showSaveDialog(getWindowFromId(fileId)) == JFileChooser.APPROVE_OPTION) {
            return chooser.getSelectedFile().getAbsolutePath();
        }

        return null;
    }

    /**
     * Shows a open dialog.
     * 
     * @param fileId
     *            the id of the currently displayed file.
     * @return the path of the file, or <code>null</code> if the action was
     *         canceled.
     */
    public String showOpenDialog(String fileId) {
        JFileChooser chooser = new JFileChooser();
        chooser.setDialogType(JFileChooser.OPEN_DIALOG);
        chooser.setDialogTitle("Open");
        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

        if (chooser.showOpenDialog(getWindowFromId(fileId)) == JFileChooser.APPROVE_OPTION) {
            return chooser.getSelectedFile().getAbsolutePath();
        }

        return null;
    }

    private FrameMain getWindowFromId(String fileId) {
        return this.windowList.get(fileId);
    }

    String[] getAllFileIds() {
        Set<String> idSet = this.windowList.keySet();
        String[] ids = new String[idSet.size()];
        idSet.toArray(ids);

        return ids;
    }

    /**
     * A listener to notify the {@link AppController} of any events, that are
     * relevant to it.
     * 
     * @author Markus Wittlinger
     * @version 1.0 ($Id: AppView.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public interface ViewListener {
        /**
         * An enum denoting the events the AppView can send.
         * 
         * @author Markus Wittlinger
         * @version 1.0 ($Id: AppView.java 1 2007-12-12 17:37:26Z t-scheller $)
         */
        enum ViewEvent {
            /**
             * EVENT_NEW_WINDOW
             */
            EVENT_NEW_WINDOW,
            /**
             * EVENT_NEW_BOOK
             */
            EVENT_NEW_BOOK,
            /**
             * EVENT_OPEN (args contains the ids of the selected books as a
             * String array)
             */
            EVENT_OPEN,
            /**
             * EVENT_CLOSE_WINDOW
             */
            EVENT_CLOSE_WINDOW,
            /**
             * EVENT_SAVE
             */
            EVENT_SAVE,
            /**
             * EVENT_SAVE_AS
             */
            EVENT_SAVE_AS,
            /**
             * EVENT_DELETE
             */
            EVENT_DELETE,
            /**
             * EVENT_QUIT (args contains the ids of the open files as a String
             * array)
             */
            EVENT_QUIT;
        }

        /**
         * Is called, when an event occurs.
         * 
         * @param viewEvent
         *            the type of event
         * @param fileId
         *            the id of the file and windows the event occurred in.
         * @param args
         *            the additional arguments, that are passed.
         */
        public void update(ViewEvent viewEvent, String fileId, Object args);
    }
}
