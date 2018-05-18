package org.codecover.simplejavaapp.view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.table.AbstractTableModel;

import org.codecover.simplejavaapp.model.AppModel;
import org.codecover.simplejavaapp.model.Book;
import org.codecover.simplejavaapp.model.AppModel.AppModelModifyListener;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: FrameMain.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
@SuppressWarnings("serial")
public class FrameMain extends JFrame {
    private final String[] colNames = new String[] {"Author",
                                                    "Name",
                                                    "Year",
                                                    "ISBN"};

    private static final AppModel model = AppModel.getInstance();

    private static final AppView view = AppView.getInstance();

    private String fileId;

    private JTable table;

    private final AppAction newWindowAction = new AppAction("New Window", null,
            "Creates a new file and opens it in a new window.", KeyStroke
                    .getKeyStroke('N', InputEvent.META_DOWN_MASK)) {

        /**
         * (non-Javadoc)
         * 
         * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
         */
        public void actionPerformed(ActionEvent e) {
            view.notifyViewListener(
                    AppView.ViewListener.ViewEvent.EVENT_NEW_WINDOW,
                    FrameMain.this.fileId, null);
        }
    };

    private final AppAction newBookAction = new AppAction("New Book",
            new ImageIcon("icons/new.png"), "Creates a new book.", KeyStroke
                    .getKeyStroke('N', InputEvent.META_DOWN_MASK
                            | InputEvent.SHIFT_DOWN_MASK)) {

        public void actionPerformed(ActionEvent e) {
            view.notifyViewListener(
                    AppView.ViewListener.ViewEvent.EVENT_NEW_BOOK,
                    FrameMain.this.fileId, null);
        }
    };

    private final AppAction closeWindowAction = new AppAction("Close Window",
            null, "Closes the window.", KeyStroke.getKeyStroke('W',
                    InputEvent.META_DOWN_MASK)) {

        public void actionPerformed(ActionEvent e) {
            view.notifyViewListener(
                    AppView.ViewListener.ViewEvent.EVENT_CLOSE_WINDOW,
                    FrameMain.this.fileId, null);
        }
    };

    private final AppAction deleteAction = new AppAction("Delete",
            new ImageIcon("icons/delete.png"), "Deletes the selected element.",
            KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0)) {

        public void actionPerformed(ActionEvent e) {
            view.notifyViewListener(
                    AppView.ViewListener.ViewEvent.EVENT_DELETE,
                    FrameMain.this.fileId, FrameMain.this.getSelectedBooks());
        }
    };

    private final AppAction openAction = new AppAction("Open…", null,
            "Opens a previosly saved file.", KeyStroke.getKeyStroke('O',
                    InputEvent.META_DOWN_MASK)) {

        public void actionPerformed(ActionEvent e) {
            view.notifyViewListener(AppView.ViewListener.ViewEvent.EVENT_OPEN,
                    FrameMain.this.fileId, null);
        }
    };

    private final AppAction quitAction = new AppAction("Quit", null,
            "Closes the application", KeyStroke.getKeyStroke('Q',
                    InputEvent.META_DOWN_MASK)) {

        public void actionPerformed(ActionEvent e) {
            view.notifyViewListener(AppView.ViewListener.ViewEvent.EVENT_QUIT,
                    FrameMain.this.fileId, FrameMain.view.getAllFileIds());
        }
    };

    private final AppAction saveAction = new AppAction("Save", new ImageIcon(
            "icons/save.png"), "Saves the current file.", KeyStroke
            .getKeyStroke('S', InputEvent.META_DOWN_MASK)) {

        public void actionPerformed(ActionEvent e) {
            view.notifyViewListener(AppView.ViewListener.ViewEvent.EVENT_SAVE,
                    FrameMain.this.fileId, null);
        }
    };

    private final AppAction saveAsAction = new AppAction("Save As…",
            new ImageIcon("icons/save.png"),
            "Saves the current file to the specified location.", KeyStroke
                    .getKeyStroke('S', InputEvent.META_DOWN_MASK
                            | InputEvent.SHIFT_DOWN_MASK)) {

        public void actionPerformed(ActionEvent e) {
            view.notifyViewListener(
                    AppView.ViewListener.ViewEvent.EVENT_SAVE_AS,
                    FrameMain.this.fileId, null);
        }
    };

    final AppModelModifyListener appModelModifyListener = new AppModelModifyListener() {
        public void appFileModified(String fileId, boolean modified) {
            FrameMain.this.updateTitle(modified);
            FrameMain.this.table.setModel(new BookTableModel());
        }
    };

    FrameMain(String title, String fileId) {
        super(title == null ? "Untitled" : title);
        this.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        this.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                FrameMain.view.notifyViewListener(
                        AppView.ViewListener.ViewEvent.EVENT_CLOSE_WINDOW,
                        FrameMain.this.fileId, null);
            }
        });
        this.fileId = fileId;

        this.setSize(600, 500);
        this.setLocationRelativeTo(null);

        initComponents();
    }

    private final void initComponents() {
        this.setLayout(new BorderLayout());

        createToolbar();
        createMenuBar();

        this.table = new JTable(new BookTableModel());
        this.table.setShowGrid(true);
        this.table.setGridColor(Color.LIGHT_GRAY);
        JScrollPane scrollPane = new JScrollPane(this.table);

        this.add(scrollPane, BorderLayout.CENTER);


        model.addAppModelModifyListenerToFile(this.fileId,
                this.appModelModifyListener);
    }

    private final void createToolbar() {
        JToolBar toolBar = new JToolBar("Toolbar");
        toolBar.setFloatable(false);
        toolBar.setRollover(true);
        this.add(toolBar, BorderLayout.NORTH);

        final JButton newWindowButton = new JButton(this.newBookAction);
        newWindowButton.setText("");
        toolBar.add(newWindowButton);

        final JButton savebutton = new JButton(this.saveAction);
        savebutton.setText("");
        toolBar.add(savebutton);

        toolBar.addSeparator();

        final JButton deleteButton = new JButton(this.deleteAction);
        deleteButton.setText("");
        toolBar.add(deleteButton);
    }

    private final void createMenuBar() {
        JMenuBar menuBar = new JMenuBar();
        this.setJMenuBar(menuBar);

        JMenu fileMenu = new JMenu("File");
        menuBar.add(fileMenu);
        fileMenu.add(new JMenuItem(this.newWindowAction));

        final JMenuItem newBookItem = new JMenuItem(this.newBookAction);
        newBookItem.setIcon(null);
        fileMenu.add(newBookItem);

        fileMenu.add(new JMenuItem(this.openAction));
        fileMenu.add(new JMenuItem(this.closeWindowAction));
        fileMenu.addSeparator();

        final JMenuItem saveItem = new JMenuItem(this.saveAction);
        saveItem.setIcon(null);
        fileMenu.add(saveItem);

        final JMenuItem saveAsItem = new JMenuItem(this.saveAsAction);
        saveAsItem.setIcon(null);
        fileMenu.add(saveAsItem);

        fileMenu.addSeparator();
        fileMenu.add(new JMenuItem(this.quitAction));

        JMenu editMenu = new JMenu("Edit");
        menuBar.add(editMenu);

        final JMenuItem deleteItem = new JMenuItem(this.deleteAction);
        deleteItem.setIcon(null);
        editMenu.add(deleteItem);
    }

    private List<Book> getBookList() {
        return model.getBooksInFile(FrameMain.this.fileId);
    }

    private String[] getSelectedBooks() {
        List<Book> bookList = getBookList();
        int[] indices = this.table.getSelectedRows();
        String[] bookIds = new String[indices.length];
        for (int i = 0; i < indices.length; i++) {
            bookIds[i] = bookList.get(indices[i]).getId();
        }

        return bookIds;
    }

    private void updateTitle(boolean modified) {
        String title = (model.getPathOfFile(this.fileId) == null ? "Untitled"
                : model.getPathOfFile(this.fileId));

        if (modified) {
            title += "*";
        }

        setTitle(title);
    }

    /**
     * Sets the file id.
     * 
     * @param fileId
     *            the given file id.
     */
    public void setFileId(String fileId) {
        model.removeAppModelModifyListener(this.fileId,
                this.appModelModifyListener);
        this.fileId = fileId;
        model.addAppModelModifyListenerToFile(this.fileId,
                this.appModelModifyListener);
        this.updateTitle(false);
        this.table.updateUI();
    }

    private abstract class AppAction extends AbstractAction {
        /**
         * Constructor.
         * 
         * @param name
         *            the name of the action.
         * @param icon
         *            the icon of the action.
         * @param desc
         *            the description of the action.
         * @param keyStroke
         *            the keystroke of the action.
         */
        protected AppAction(String name, ImageIcon icon, String desc,
                KeyStroke keyStroke) {
            super(name, icon);
            this.putValue(Action.SHORT_DESCRIPTION, desc);
            if (keyStroke != null) {
                this.putValue(Action.ACCELERATOR_KEY, keyStroke);
            }
        }
    }

    private final class BookTableModel extends AbstractTableModel {

        /**
         * (non-Javadoc)
         * 
         * @see javax.swing.table.TableModel#getColumnClass(int)
         */
        @Override
        public Class<?> getColumnClass(int columnIndex) {
            switch (columnIndex) {
                case 0:
                    return String.class;
                case 1:
                    return String.class;
                case 2:
                    return Integer.class;
                case 3:
                    return String.class;
            }
            return null;
        }

        /**
         * (non-Javadoc)
         * 
         * @see javax.swing.table.AbstractTableModel#getColumnName(int)
         */
        @Override
        public String getColumnName(int column) {
            return FrameMain.this.colNames[column];
        }

        /**
         * (non-Javadoc)
         * 
         * @see javax.swing.table.TableModel#getColumnCount()
         */
        public int getColumnCount() {
            return FrameMain.this.colNames.length;
        }

        /**
         * (non-Javadoc)
         * 
         * @see javax.swing.table.TableModel#getRowCount()
         */
        public int getRowCount() {
            return getBookList().size();
        }

        /**
         * (non-Javadoc)
         * 
         * @see javax.swing.table.TableModel#getValueAt(int, int)
         */
        public Object getValueAt(int rowIndex, int columnIndex) {
            Book book = getBookList().get(rowIndex);

            switch (columnIndex) {
                case 0:
                    return book.getAuthor();
                case 1:
                    return book.getName();
                case 2:
                    return book.getYear();
                case 3:
                    return book.getIsbn();
            }
            return null;
        }

        /**
         * (non-Javadoc)
         * 
         * @see javax.swing.table.TableModel#isCellEditable(int, int)
         */
        @Override
        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return true;
        }

        /**
         * (non-Javadoc)
         * 
         * @see javax.swing.table.TableModel#setValueAt(java.lang.Object, int,
         *      int)
         */
        @Override
        public void setValueAt(Object value, int rowIndex, int columnIndex) {
            Book book = getBookList().get(rowIndex);

            switch (columnIndex) {
                case 0:
                    book.setAuthor((String) value);
                    break;
                case 1:
                    book.setName((String) value);
                    break;
                case 2:
                    book.setYear(((Integer) value).intValue());
                    break;
                case 3:
                    book.setIsbn((String) value);
                    break;
            }
        }
    }
}
