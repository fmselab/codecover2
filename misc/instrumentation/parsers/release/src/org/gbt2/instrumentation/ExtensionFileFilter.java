package org.gbt2.instrumentation;

import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.util.Set;

/**
 * This is a simple {@link FilenameFilter}.<br>
 * <br>
 * It is bases on three information:
 * <ul>
 * <li>whether or whether not subdirectories are accepted</li>
 * <li>which files and folders are ignored using a name pattern</li>
 * <li>which files are accepted using an extension pattern</li>
 * </ul>
 * 
 * @author Christoph Müller
 * @version 1.0 - 25.02.2007
 * 
 */
public class ExtensionFileFilter implements FileFilter {

    private Set<String> extensions;

    private boolean acceptFolders;

    private Set<String> ignoredNames;

    /**
     * Constructs a new {@link FilenameFilter}.
     * 
     * @param extensions
     *            Sets the extensions, the filter uses for files.
     * @param acceptFolders
     *            Are subdirectories supported?
     * @param ignoredNames
     *            Which file and folder names should be ignored.
     */
    public ExtensionFileFilter(Set<String> extensions, boolean acceptFolders,
            Set<String> ignoredNames) {
        super();
        this.extensions = extensions;
        this.acceptFolders = acceptFolders;
        this.ignoredNames = ignoredNames;
    }

    public boolean accept(File fileOrDirectory) {
        if (fileOrDirectory == null) {
            return false;
        }
        // fileOrDirectory != null
        
        String fileName = fileOrDirectory.getName();

        if (this.ignoredNames.contains(fileName)) {
            return false;
        }
        // fileOrDirectory name is not an ignored name

        if (fileOrDirectory.isDirectory()) {
            return this.acceptFolders;
        }
        // fileOrDirectory is a file

        return (this.extensions == null)
                || this.extensions.contains(getExtension(fileOrDirectory,
                        fileName));
    }

    /**
     * Get the extension of a file.<br>
     * <br>
     * An extension is definded as the String after the last dot. The extensions
     * is always returned in lower case. If there is no dot, the extensions is
     * defined as "".
     * 
     * <pre>
     *      getExtension(Main.java) == &quot;java&quot;
     *      getExtension(Main.java.in) == &quot;in&quot;
     *      getExtension(Main) == &quot;&quot;
     * </pre>
     * 
     * @param file
     *            The given file.
     * @param name
     *            The name of the given file.
     * @return The extension of the file.
     */
    public static String getExtension(File file, String name) {
        if (file == null) {
            return "";
        }
        // file != null

        if (name == null) {
            name = file.getName();
        }
        // name != null

        String extensions = "";
        int i = name.lastIndexOf('.');
        if (i >= 0 && i < name.length() - 1) {
            extensions = name.substring(i + 1).toLowerCase();
        }
        return extensions;
    }
}