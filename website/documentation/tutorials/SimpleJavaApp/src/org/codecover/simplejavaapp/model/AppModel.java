package org.codecover.simplejavaapp.model;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import org.xml.sax.SAXException;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: AppModel.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class AppModel {
    private static AppModel instance;

    private final Map<String, AppFile> appFiles;

    /**
     * Singleton-Pattern method to get the instance of {@link AppModel}.
     * 
     * @return the instance of the {@link AppModel}
     */
    public static AppModel getInstance() {
        if (instance == null) {
            instance = new AppModel();
        }

        return instance;
    }

    private AppModel() {
        this.appFiles = new HashMap<String, AppFile>();
    }

    /**
     * Adds a new {@link AppFile} to the model and returns its id.
     * 
     * @return the id of the {@link AppFile}.
     */
    public String newFile() {
        AppFile appFile = AppFile.newFile();
        putAppFile(appFile);

        return appFile.getId();
    }

    /**
     * Loads an {@link AppFile} from the given location.
     * 
     * @param file
     *            the location of the persistently saved {@link AppFile}
     * @return the read {@link AppFile}.
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws IOException
     */
    public String loadFile(File file) throws ParserConfigurationException,
            SAXException, IOException {
        if (file == null) {
            throw new NullPointerException("file == null");
        }

        AppFile appFile = AppFile.loadAppFile(file);
        putAppFile(appFile);

        return appFile.getId();
    }

    /**
     * Saves the {@link AppFile} with the given id as a XML file to the given
     * location.
     * 
     * @param fileId
     *            the id of the {@link AppFile}
     * @param file
     *            the given location.
     * @throws ParserConfigurationException
     * @throws TransformerException
     * @throws IOException
     */
    public void saveFile(String fileId, File file)
            throws ParserConfigurationException, TransformerException,
            IOException {
        if (file == null) {
            throw new NullPointerException("file == null");
        }

        AppFile appFile = getAppFile(fileId);

        appFile.saveAppFile(file);
    }

    /**
     * Closes the {@link AppFile} with the given id.
     * 
     * @param fileId
     *            the id of the {@link AppFile}.
     * @param force
     *            if this is set to true, the file is closed even with
     *            modification
     * @return true &rarr; the {@link AppFile} was not modified and is now
     *         closed. </br> false &rarr; the {@link AppFile} was modified and
     *         is still open.
     */
    public boolean closeFile(String fileId, boolean force) {
        if (isFileModified(fileId) && !force) {
            return false;
        }

        this.appFiles.remove(fileId);
        return true;
    }

    /**
     * @param appFile
     */
    private void putAppFile(AppFile appFile) {
        this.appFiles.put(appFile.getId(), appFile);
    }

    /**
     * Gets the {@link Book}s stored in the {@link AppFile} with the given id.
     * 
     * @param fileId
     *            the id of the {@link AppFile}.
     * @return the {@link Collection} of {@link Book}s.
     */
    public List<Book> getBooksInFile(String fileId) {
        AppFile appFile = getAppFile(fileId);

        return appFile.getBooks();
    }

    /**
     * Gets the {@link Book} stored in the {@link AppFile} with the given id.
     * 
     * @param fileId
     *            the id of the {@link AppFile}.
     * @param bookId
     *            the id of the {@link Book}.
     * @return the {@link Book} with the given id, or <code>null</code>, if
     *         no such book existed.
     */
    public Book getBooksInFile(String fileId, String bookId) {
        if (bookId == null) {
            throw new NullPointerException("bookId == null");
        }

        AppFile appFile = getAppFile(fileId);

        return appFile.getBook(bookId);
    }

    /**
     * Adds the given {@link Book} to the {@link AppFile} with the given id.
     * 
     * @param fileId
     *            the id of the {@link AppFile}
     * @param book
     *            the {@link Book} to be added.
     */
    public void addBookToFile(String fileId, Book book) {
        if (book == null) {
            throw new NullPointerException("book == null");
        }

        AppFile appFile = getAppFile(fileId);

        appFile.addBook(book);
    }

    /**
     * Removes the {@link Book} with the given id from the {@link AppFile} with
     * the given id.
     * 
     * @param fileId
     *            the id of the {@link AppFile}
     * @param bookId
     *            the id of the {@link Book}.
     */
    public void removeBookFromFile(String fileId, String bookId) {
        if (bookId == null) {
            throw new NullPointerException("bookId == null");
        }

        AppFile appFile = getAppFile(fileId);

        appFile.removeBook(bookId);
    }

    /**
     * Returns, whether or not the {@link AppFile} with the given id, is in it's
     * modified state.
     * 
     * @param fileId
     *            the id of the {@link AppFile}.
     * @return the modified state of the {@link AppFile}.
     */
    public boolean isFileModified(String fileId) {
        AppFile appFile = getAppFile(fileId);

        return appFile.isModified();
    }

    /**
     * Gets the previously set path used to save the {@link AppFile}.
     * 
     * @param fileId
     *            the if of the {@link AppFile}
     * @return the path or <code>null</code>, if the {@link AppFile} was
     *         never saved before.
     */
    public String getPathOfFile(String fileId) {
        AppFile appFile = getAppFile(fileId);

        return appFile.getPath();
    }

    /**
     * Adds the given {@link AppModelModifyListener} to the {@link AppFile} with
     * the given id.
     * 
     * @param fileId
     *            the id of the {@link AppFile}.
     * @param listener
     *            the given {@link AppModelModifyListener}
     */
    public void addAppModelModifyListenerToFile(String fileId,
            AppModelModifyListener listener) {
        if (listener == null) {
            throw new NullPointerException("listener == null");
        }

        AppFile appFile = getAppFile(fileId);

        appFile.addAppModelModifyListener(listener);
    }

    /**
     * Removes the given {@link AppModelModifyListener} from the {@link AppFile}
     * with the given id.
     * 
     * @param fileId
     *            the id of the {@link AppFile}.
     * @param listener
     *            the given {@link AppModelModifyListener}
     */
    public void removeAppModelModifyListener(String fileId,
            AppModelModifyListener listener) {
        if (listener == null) {
            throw new NullPointerException("listener == null");
        }

        AppFile appFile = getAppFile(fileId);

        appFile.removeAppModelModifyListener(listener);
    }

    /**
     * Gets the {@link AppFile} with the given id and handles all exceptions
     * 
     * @param fileId
     *            the id of the {@link AppFile}
     * @return the {@link AppFile};
     */
    private AppFile getAppFile(String fileId) {
        if (fileId == null) {
            throw new NullPointerException("fileId == null");
        }

        AppFile appFile = this.appFiles.get(fileId);

        if (appFile == null) {
            throw new IllegalArgumentException(
                    "No file with the given id in the model!");
        }
        return appFile;
    }

    /**
     * @author Markus Wittlinger
     * @version 1.0 ($Id: AppModel.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public interface AppModelModifyListener {
        /**
         * Notifies, that the modified state of the {@link AppFile} with the
         * given id, has changed.
         * 
         * @param fileId
         *            the id of the {@link AppFile}.
         * @param modified
         *            the new modified state of {@link AppFile}.
         */
        public void appFileModified(String fileId, boolean modified);
    }
}
