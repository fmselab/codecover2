package org.codecover.simplejavaapp.model;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.UUID;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.codecover.simplejavaapp.model.AppModel.AppModelModifyListener;
import org.codecover.simplejavaapp.model.Book.BookModifyListener;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: AppFile.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class AppFile {
    private final String id;

    private final Map<String, Book> books;

    private final BookListener bookListener;

    private final List<AppModelModifyListener> modifyListener;

    private boolean modified = false;

    private String path;

    /**
     * Constructor.
     */
    private AppFile() {
        this.id = UUID.randomUUID().toString();
        this.bookListener = new BookListener();
        this.books = new TreeMap<String, Book>(new Comparator<String>() {
            public int compare(String o1, String o2) {
                return o1.compareTo(o2);
            }
        });
        this.modifyListener = new LinkedList<AppModelModifyListener>();
    }

    /**
     * Creates and returns a new {@link AppFile}.
     * 
     * @return the new {@link AppFile}.
     */
    static AppFile newFile() {
        return new AppFile();
    }

    /**
     * Gets all {@link Book Books} contained in this {@link AppFile}.
     * 
     * @return the {@link Collection} of books.
     */
    List<Book> getBooks() {
        return new LinkedList<Book>(this.books.values());
    }

    /**
     * Gets the {@link Book} with the given id.
     * 
     * @param id
     *            the given id.
     * @return the {@link Book} with the given id, or <code>null</code>, if
     *         no such book existed.
     */
    Book getBook(String id) {
        return this.books.get(id);
    }

    /**
     * Adds the given {@link Book} to this {@link AppFile}
     * 
     * @param book
     *            the given {@link Book}, that is to be added.
     */
    void addBook(Book book) {
        this.books.put(book.getId(), book);
        book.setBookModifyListener(this.bookListener);

        setModified(true);
    }

    /**
     * Removes the {@link Book} with the given id.
     * 
     * @param bookId
     *            the id of the book {@link Book}.
     */
    void removeBook(String bookId) {
        this.books.remove(bookId);

        setModified(true);
    }

    /**
     * Gets the id of this {@link AppFile}.
     * 
     * @return the id.
     */
    String getId() {
        return this.id;
    }

    /**
     * Gets the modified.
     * 
     * @return the modified
     */
    boolean isModified() {
        return this.modified;
    }

    /**
     * Sets the modified.
     * 
     * @param modified
     *            the modified to set
     */
    void setModified(boolean modified) {
        this.modified = modified;
        notifyListeners();
    }

    /**
     * Adds the given {@link AppModelModifyListener} to the list of listeners
     * 
     * @param listener
     *            the given {@link AppModelModifyListener}
     */
    void addAppModelModifyListener(AppModelModifyListener listener) {
        this.modifyListener.add(listener);
    }

    /**
     * Removes the given {@link AppModelModifyListener} from the list of
     * listeners
     * 
     * @param listener
     *            the given {@link AppModelModifyListener}
     */
    void removeAppModelModifyListener(AppModelModifyListener listener) {
        this.modifyListener.remove(listener);
    }

    private void notifyListeners() {
        for (AppModelModifyListener listener : this.modifyListener) {
            listener.appFileModified(getId(), isModified());
        }
    }

    /**
     * Sets the path.
     * 
     * @param path
     *            the path to set
     */
    private void setPath(String path) {
        this.path = path;
        notifyListeners();
    }

    /**
     * Gets the path.
     * 
     * @return the path
     */
    String getPath() {
        return this.path;
    }

    /**
     * Saves this {@link AppFile} as a XML file to the given location.
     * 
     * @param file
     *            the given location.
     * @throws ParserConfigurationException
     * @throws TransformerException
     * @throws IOException
     */
    void saveAppFile(File file) throws ParserConfigurationException,
            TransformerException, IOException {
        Document xmlDoc = null;
        DocumentBuilderFactory docBFac;
        DocumentBuilder docBuild;

        docBFac = DocumentBuilderFactory.newInstance();
        docBFac.setNamespaceAware(true);
        docBuild = docBFac.newDocumentBuilder();
        xmlDoc = docBuild.newDocument();

        if (xmlDoc != null) {
            Element documentElement = xmlDoc.createElement("SimpleJavaApp");
            xmlDoc.appendChild(documentElement);

            Element bookListElement = xmlDoc.createElement("BookList");

            for (Book book : getBooks()) {
                bookListElement.appendChild(book.getXMLRepresentation(xmlDoc));
            }

            documentElement.appendChild(bookListElement);
        }

        FileOutputStream out = new FileOutputStream(file);
        try {
            DOMSource domSource = new DOMSource(xmlDoc);
            StreamResult streamResult = new StreamResult(out);
            TransformerFactory tf = TransformerFactory.newInstance();
            Transformer serializer = tf.newTransformer();
            serializer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
            serializer.setOutputProperty(OutputKeys.INDENT, "yes");
            serializer.transform(domSource, streamResult);
            out.flush();

            /*
             * All saving operations were successful, so the AppFile can be set
             * to 'not modified'.
             */
            this.setModified(false);
            this.setPath(file.getAbsolutePath());
        } finally {
            out.close();
        }
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
    static AppFile loadAppFile(File file) throws ParserConfigurationException,
            SAXException, IOException {
        AppFile appFile = AppFile.newFile();

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        DocumentBuilder DocumentBuilder = factory.newDocumentBuilder();
        Document xmlDoc = DocumentBuilder.parse(file);

        Element element = xmlDoc.getDocumentElement();

        NodeListIterable nodeListIterable = appFile.new NodeListIterable(
                element.getElementsByTagName("BookList"));
        for (Node node : nodeListIterable) {
            for (Node subNode : appFile.new NodeListIterable(((Element) node)
                    .getElementsByTagName("Book"))) {
                appFile.addBook(Book.getBookFromXML((Element) subNode));
            }
        }

        appFile.setModified(false);
        appFile.setPath(file.getAbsolutePath());

        return appFile;
    }

    private final class NodeListIterable implements Iterable<Node> {
        private final List<Node> list;

        /**
         * Initializes this class with the {@link NodeList} it is supposed to
         * encapsulate.
         * 
         * @param nodeList
         *            the {@link NodeList} to encapsulate.
         */
        public NodeListIterable(NodeList nodeList) {
            this.list = new LinkedList<Node>();

            for (int i = 0; i < nodeList.getLength(); i++) {
                this.list.add(nodeList.item(i));
            }
        }

        /**
         * (non-Javadoc)
         * 
         * @see java.lang.Iterable#iterator()
         */
        public Iterator<Node> iterator() {
            return this.list.iterator();
        }
    }

    private final class BookListener implements BookModifyListener {

        /**
         * (non-Javadoc)
         * 
         * @see org.codecover.simplejavaapp.model.Book.BookModifyListener#stateModified()
         */
        public void stateModified() {
            AppFile.this.setModified(true);
        }
    }
}
