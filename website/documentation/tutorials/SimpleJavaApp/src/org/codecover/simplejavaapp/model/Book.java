package org.codecover.simplejavaapp.model;

import java.util.UUID;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * A model class holding the data of a book. Also contains methods for
 * persistent storage.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: Book.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class Book {
    private static final String YEAR = "year";

    private static final String NAME = "name";

    private static final String ISBN = "isbn";

    private static final String ID = "id";

    private static final String AUTHOR = "author";

    private final String id;

    private String name = "name";

    private String author = "author";

    private int year = 2007;

    private String isbn = "isbn";

    private BookModifyListener listener;

    /**
     * Constructor, which creates a {@link Book} with the given id.
     * 
     * @param id
     *            the id of the {@link Book}
     */
    public Book(String id) {
        this.id = id;
    }

    /**
     * Constructor, which gives this {@link Book} a random id.
     */
    public Book() {
        this(UUID.randomUUID().toString());
    }

    /**
     * Gets the id.
     * 
     * @return the id
     */
    public String getId() {
        return this.id;
    }

    /**
     * Gets the name.
     * 
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    /**
     * Gets the author.
     * 
     * @return the author
     */
    public String getAuthor() {
        return this.author;
    }

    /**
     * Gets the year.
     * 
     * @return the year
     */
    public int getYear() {
        return this.year;
    }

    /**
     * Gets the isbn.
     * 
     * @return the isbn
     */
    public String getIsbn() {
        return this.isbn;
    }

    /**
     * Sets the name.
     * 
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
        notifiyChange();
    }

    /**
     * Sets the author.
     * 
     * @param author
     *            the author to set
     */
    public void setAuthor(String author) {
        this.author = author;
        notifiyChange();
    }

    /**
     * Sets the year.
     * 
     * @param year
     *            the year to set
     */
    public void setYear(int year) {
        this.year = year;
        notifiyChange();
    }

    /**
     * Sets the isbn.
     * 
     * @param isbn
     *            the isbn to set
     */
    public void setIsbn(String isbn) {
        this.isbn = isbn;
        notifiyChange();
    }

    private void notifiyChange() {
        if (this.listener == null) {
            return;
        }

        this.listener.stateModified();
    }

    /**
     * Sets the {@link BookModifyListener}, that is notified, when a change
     * occurs.
     * 
     * @param listener
     *            the {@link BookModifyListener}
     */
    public void setBookModifyListener(BookModifyListener listener) {
        this.listener = listener;
    }

    /**
     * Return a DOM {@link Element}, which holds all the data of this instance
     * of {@link Book}.
     * 
     * @param xmlDoc
     *            the document, in which the {@link Element} is to be created.
     * @return the created {@link Element}.
     */
    public Element getXMLRepresentation(Document xmlDoc) {
        if (xmlDoc == null) {
            throw new NullPointerException("xmlDoc == null");
        }

        Element bookElement = xmlDoc.createElement("Book");

        bookElement.setAttribute(AUTHOR, getAuthor());
        bookElement.setAttribute(ID, getId());
        bookElement.setAttribute(ISBN, getIsbn());
        bookElement.setAttribute(NAME, getName());
        bookElement.setAttribute(YEAR, Integer.toString(getYear()));

        return bookElement;
    }

    /**
     * Gets the book, that is contained this the given DOM {@link Element}
     * 
     * @param element
     *            the DOM {@link Element} holding the data.
     * @return the extracted {@link Book}
     */
    public static Book getBookFromXML(Element element) {
        String author = element.getAttribute(AUTHOR);
        String id = element.getAttribute(ID);
        String isbn = element.getAttribute(ISBN);
        String name = element.getAttribute(NAME);
        String year = element.getAttribute(YEAR);

        Book book = new Book(id);

        book.setAuthor(author);
        book.setIsbn(isbn);
        book.setName(name);
        book.setYear(Integer.parseInt(year));

        return book;
    }

    /**
     * A listener to monitor changes in the {@link Book}.
     * 
     * @author Markus Wittlinger
     * @version 1.0 ($Id: Book.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public interface BookModifyListener {
        /**
         * Is called every time a property of the book is changed.
         */
        public void stateModified();
    }
}
