package org.codecover.simplejavaapp.controller;

import java.io.File;
import java.io.IOException;

import javax.swing.JOptionPane;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import org.codecover.simplejavaapp.model.AppModel;
import org.codecover.simplejavaapp.model.Book;
import org.codecover.simplejavaapp.view.AppView;
import org.codecover.simplejavaapp.view.AppView.ViewListener;
import org.xml.sax.SAXException;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: AppController.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class AppController {
    private static final AppModel model = AppModel.getInstance();

    private static final AppView view = AppView.getInstance();

    private static AppController instance;

    /**
     * Singleton-Pattern method to get the instance of {@link AppController}.
     * 
     * @return the instance of the {@link AppController}
     */
    public static AppController getInstance() {
        if (instance == null) {
            instance = new AppController();
        }

        return instance;
    }

    private AppController() {
        view.addViewListener(new AppViewListener());
    }

    /**
     * Opens the application in its initial state.
     */
    public static void initialize() {
        AppController appController = AppController.getInstance();

        appController.onOpenNewWindow();
    }

    private void onOpenNewWindow() {
        String fileId = model.newFile();

        view.openWindow(fileId);
    }

    private void onNewBook(String fileId) {
        Book book = new Book();

        model.addBookToFile(fileId, book);
    }

    private boolean onCloseWindow(String fileId) {
        if (model.isFileModified(fileId)) {
            int result = view.showConfirmationDialog(
                    "The file has modifications. Save before proceeding?",
                    fileId);

            switch (result) {
                case JOptionPane.YES_OPTION:
                    if (!onSave(fileId)) {
                        /* Saving was aborted or failed, so we don't proceed */
                        return false;
                    }
                    break;
                case JOptionPane.NO_OPTION:
                    break;
                case JOptionPane.CANCEL_OPTION:
                    /* Saving was aborted or failed, so we don't proceed */
                    return false;
            }
        }

        view.closeWindow(fileId);
        model.closeFile(fileId, true);
        return true;
    }

    private void onDelete(String fileId, String[] bookIds) {
        if(bookIds.length == 0) {
            return;
        }
        
        int result = view.showConfirmationDialog(
                "Are you sure you want to delete these books?", fileId);
        switch (result) {
            case JOptionPane.YES_OPTION:
                for (String bookId : bookIds) {
                    model.removeBookFromFile(fileId, bookId);
                }
                break;
            case JOptionPane.NO_OPTION:
            case JOptionPane.CANCEL_OPTION:
        }
    }

    private void onOpen(String fileId) {
        if (model.isFileModified(fileId)) {
            int result = view.showConfirmationDialog(
                    "The file has modifications. Save before proceeding?",
                    fileId);

            switch (result) {
                case JOptionPane.YES_OPTION:
                    if (!onSave(fileId)) {
                        /* Saving was aborted or failed, so we don't proceed */
                        return;
                    }
                    break;
                case JOptionPane.NO_OPTION:
                    break;
                case JOptionPane.CANCEL_OPTION:
                    /* Saving was aborted or failed, so we don't proceed */
                    return;
            }
        }

        String path = view.showOpenDialog(fileId);

        if (path == null) {
            return;
        }

        try {
            String newFileId = model.loadFile(new File(path));

            view.openFileInWindow(fileId, newFileId);
            model.closeFile(fileId, true);
        } catch (ParserConfigurationException e) {
            view.showErrorDialog("Error loading file", fileId);
        } catch (SAXException e) {
            view.showErrorDialog("Error loading file", fileId);
        } catch (IOException e) {
            view.showErrorDialog("Error loading file", fileId);
        }
    }

    private void onQuit(String fileId, String[] allFileIds) {
        for (String id : allFileIds) {
            if (!onCloseWindow(id)) {
                return;
            }
        }

        /* All windows were closed so we can exit the application. */
        System.exit(0);
    }

    private boolean onSave(String fileId) {
        String path = model.getPathOfFile(fileId);

        if (path == null) {
            return onSaveAs(fileId);
        } else {
            save(fileId, new File(path));
            return true;
        }
    }

    private boolean onSaveAs(String fileId) {
        String path = view.showSaveAsDialog(fileId);

        if (path == null) {
            return false;
        } else {
            save(fileId, new File(path));
            return true;
        }
    }

    private void save(String fileId, File file) {
        try {
            model.saveFile(fileId, file);
        } catch (ParserConfigurationException e) {
            view.showErrorDialog("Error saving file", fileId);
        } catch (TransformerException e) {
            view.showErrorDialog("Error saving file", fileId);
        } catch (IOException e) {
            view.showErrorDialog("Error saving file", fileId);
        }
    }

    private final class AppViewListener implements ViewListener {

        /**
         * (non-Javadoc)
         * 
         * @see org.codecover.simplejavaapp.view.AppView.ViewListener#update(org.codecover.simplejavaapp.view.AppView.ViewListener.ViewEvent,
         *      java.lang.String, java.lang.Object)
         */
        public void update(ViewEvent viewEvent, String fileId, Object args) {
            switch (viewEvent) {
                case EVENT_CLOSE_WINDOW:
                    onCloseWindow(fileId);
                    break;
                case EVENT_DELETE:
                    onDelete(fileId, (String[]) args);
                    break;
                case EVENT_NEW_BOOK:
                    onNewBook(fileId);
                    break;
                case EVENT_NEW_WINDOW:
                    onOpenNewWindow();
                    break;
                case EVENT_OPEN:
                    onOpen(fileId);
                    break;
                case EVENT_QUIT:
                    onQuit(fileId, (String[]) args);
                    break;
                case EVENT_SAVE:
                    onSave(fileId);
                    break;
                case EVENT_SAVE_AS:
                    onSaveAs(fileId);
                    break;
            }
        }
    }
}
